;;; build-farm.el --- Interface for Nix and Guix build farms (Hydra and Cuirass)  -*- lexical-binding: t -*-

;; Copyright © 2015–2018 Alex Kost <alezost@gmail.com>

;; Author: Alex Kost <alezost@gmail.com>
;; Version: 0.1
;; URL: https://gitlab.com/alezost-emacs/build-farm
;; Keywords: tools
;; Package-Requires: ((emacs "24.3") (bui "1.1.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides Emacs interface for Hydra and Cuirass (Nix and
;; Guix build farms):
;;
;;   https://hydra.nixos.org            (Hydra)
;;   https://hydra.gnu.org              (Hydra)
;;   https://berlin.guixsd.org          (Cuirass)
;;
;; Set `build-farm-url' variable to choose what build farm you wish to
;; use.

;; The following M-x commands display builds, jobsets and projects:
;;
;; - `build-farm-latest-builds'
;; - `build-farm-queued-builds'
;; - `build-farm-jobsets'
;; - `build-farm-projects'
;;
;; You can press RET in a list (of builds, etc.) to see more info on the
;; current entry.  You can also select several entries in the list (with
;; "m" key) and press RET to "describe" them.

;;; Code:

(require 'bui)
(require 'build-farm-utils)
(require 'build-farm-url)

(defgroup build-farm nil
  "Interface for Hydra and Cuirass build farms used by Guix and Nix."
  :prefix "build-farm-"
  :group 'external)

(defgroup build-farm-faces nil
  "Faces for build-farm interfaces."
  :group 'build-farm
  :group 'faces)


;;; System types

;; XXX I don't like this hard-coding very much.  But it looks like there
;; is no way to receive system types from a build farm.

(defvar build-farm-guix-system-types
  '("x86_64-linux" "i686-linux" "armhf-linux" "mips64el-linux")
  "List of systems supported by Guix build farms.")

(defvar build-farm-nix-system-types
  '("x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux")
  "List of systems supported by Nix build farms.")

(defun build-farm-system-types (&optional url)
  "Return a list of systems supported by URL.
If URL is nil, use variable `build-farm-url'."
  (cl-case (build-farm-url-package-manager url)
    (nix  build-farm-nix-system-types)
    (guix build-farm-guix-system-types)
    (t    (delete-dups
           (append build-farm-nix-system-types
                   build-farm-guix-system-types)))))


;;; Cache

(defvar build-farm-cache nil
  "Cache of various data received from build farms.")

(defun build-farm-cache-get (url data-type)
  "Return DATA-TYPE data from the cache of build farm URL."
  (bui-assoc-value build-farm-cache url data-type))

(defun build-farm-cache-set (url data-type data)
  "Add DATA-TYPE DATA to the cache of build farm URL."
  (let ((data-assoc (cons data-type data))
        (url-assoc (assoc url build-farm-cache)))
    (if url-assoc
        (setf (cdr url-assoc)
              (cons data-assoc (cdr url-assoc)))
      (setq build-farm-cache
            (cons (list url data-assoc)
                  build-farm-cache)))))

(defun build-farm-clear-cache ()
  "Remove all cached data received from the build farms."
  (interactive)
  (setq build-farm-cache nil)
  (message "The build farm cache has been cleared."))


(defvar build-farm-job-regexp ".+\\.[^.]+"
  "Regexp matching full name of a job (including system).")

(defun build-farm-job-name-specification (name version)
  "Return job name specification by NAME and VERSION."
  (concat name "-" version))

(bui-define-current-args-accessors build-farm-current
  url search-type search-args)

(defun build-farm-get-entries (root-url entry-type search-type
                                        &rest args)
  "Receive ENTRY-TYPE entries from cache or build farm.
See `build-farm-search-url' for the meaning of ROOT-URL,
SEARCH-TYPE and ARGS."
  (unless (eq search-type 'fake)
    (if (eq entry-type 'project)
        (or (build-farm-cache-get root-url 'projects)
            (let ((entries (apply #'build-farm-get-entries-1
                                  root-url entry-type search-type args)))
              (build-farm-cache-set root-url 'projects entries)
              (build-farm-cache-set root-url 'projects-received t)
              entries))
      (apply #'build-farm-get-entries-1
             root-url entry-type search-type args))))

(defun build-farm-get-entries-1 (root-url entry-type search-type
                                          &rest args)
  "Receive ENTRY-TYPE entries from build farm.
See `build-farm-search-url' for the meaning of ROOT-URL,
SEARCH-TYPE and ARGS."
  (let* ((url         (apply #'build-farm-search-url
                             root-url entry-type search-type args))
         (raw-entries (build-farm-receive-data url))
         (entries     (apply #'build-farm-modify-objects
                             raw-entries
                             (build-farm-filters entry-type))))
    entries))

(defun build-farm-get-display (root-url entry-type search-type
                                        &rest args)
  "Search for ENTRY-TYPE entries and show results.
ENTRY-TYPE should be `build', `jobset', etc.
See `build-farm-search-url' for the meaning of ROOT-URL,
SEARCH-TYPE and ARGS."
  (apply #'bui-list-get-display-entries
         (build-farm-symbol entry-type)
         root-url
         search-type args))

(defun build-farm-message (entries _root-url search-type &rest _)
  "Display a message after showing ENTRIES of SEARCH-TYPE."
  ;; XXX Add more messages maybe.
  (when (null entries)
    (if (eq search-type 'fake)
        (message "The update is impossible due to lack of the build farm API.")
      (message "The build farm has returned no results."))))

(defun build-farm-list-describe (&rest ids)
  "Describe 'build-farm' entries with IDS (list of identifiers)."
  (bui-display-entries
   (bui-entries-by-ids (bui-current-entries) ids)
   (bui-current-entry-type) 'info
   ;; Hydra and Cuirass do not provide an API to receive builds/jobsets
   ;; by IDs/names, so we use a 'fake' search type.
   (list (build-farm-current-url) 'fake)
   'add))


;;; Readers

(defvar build-farm-projects
  '("gnu" "guix")
  "List of available projects.")

(build-farm-define-readers
 :require-match nil
 :completions-var build-farm-projects
 :single-reader build-farm-read-project
 :single-prompt "Project: ")

(build-farm-define-readers
 :require-match nil
 :single-reader build-farm-read-jobset
 :single-prompt "Jobset: ")

(build-farm-define-readers
 :require-match nil
 :single-reader build-farm-read-job
 :single-prompt "Job: ")

(build-farm-define-readers
 :require-match nil
 :completions-getter build-farm-system-types
 :single-reader build-farm-read-system
 :single-prompt "System: ")


;;; Filters for processing raw entries

(defun build-farm-filter-names (entry name-alist)
  "Replace names of ENTRY parameters using NAME-ALIST.
Each element of NAME-ALIST is (OLD-NAME . NEW-NAME) pair."
  (mapcar (lambda (param)
            (pcase param
              (`(,name . ,val)
               (let ((new-name (bui-assq-value name-alist name)))
                 (if new-name
                     (cons new-name val)
                   param)))))
          entry))

(defun build-farm-filter-boolean (entry params)
  "Convert number PARAMS (0/1) of ENTRY to boolean values (nil/t)."
  (mapcar (lambda (param)
            (pcase param
              (`(,name . ,val)
               (if (memq name params)
                   (cons name (build-farm-number->bool val))
                 param))))
          entry))


;;; Wrappers for defined variables

(defun build-farm-symbol (&rest symbols)
  "Return `build-farm-...' symbol.
Where '...' is made from SYMBOLS."
  (apply #'bui-make-symbol 'build-farm symbols))

(defun build-farm-symbol-value (entry-type symbol)
  "Return SYMBOL's value for ENTRY-TYPE."
  (symbol-value (build-farm-symbol entry-type symbol)))

(defun build-farm-search-url (root-url entry-type search-type
                                       &rest args)
  "Return URL to receive ENTRY-TYPE entries from build farm.
ROOT-URL is the url of the build farm (from `build-farm-url-alist').
SEARCH-TYPE is one of the types defined by `build-farm-define-entry-type'.
ARGS are passed to the according URL function."
  (apply (bui-assq-value (build-farm-symbol-value
                          entry-type 'search-types)
                         search-type)
         ;; `:root-url' should be the last argument because `args' may
         ;; contain non-keyword arguments.
         (append args `(:root-url ,root-url))))

(defun build-farm-filters (entry-type)
  "Return a list of filters for ENTRY-TYPE."
  (build-farm-symbol-value entry-type 'filters))


;;; Interface definers

(defmacro build-farm-define-entry-type (entry-type &rest args)
  "Define general code for ENTRY-TYPE.
Remaining arguments (ARGS) should have a form [KEYWORD VALUE] ...

Required keywords:

  - `:search-types' - default value of the generated
    `build-farm-ENTRY-TYPE-search-types' variable.

Optional keywords:

  - `:filters' - default value of the generated
    `build-farm-ENTRY-TYPE-filters' variable.

  - `:filter-names' - if specified, a generated
    `build-farm-ENTRY-TYPE-filter-names' function for filtering
    these names will be added to `build-farm-ENTRY-TYPE-filters'
    variable.

  - `:filter-boolean-params' - if specified, a generated
    `build-farm-ENTRY-TYPE-filter-boolean' function for filtering
    these names will be added to `build-farm-ENTRY-TYPE-filters'
    variable.

The rest keyword arguments are passed to
`bui-define-entry-type' macro."
  (declare (indent 1))
  (let* ((entry-type-str     (symbol-name entry-type))
         (full-entry-type    (build-farm-symbol entry-type))
         (prefix             (concat "build-farm-" entry-type-str))
         (search-types-var   (intern (concat prefix "-search-types")))
         (filters-var        (intern (concat prefix "-filters")))
         (get-fun            (intern (concat prefix "-get-entries"))))
    (bui-plist-let args
        ((search-types-val   :search-types)
         (filters-val        :filters)
         (filter-names-val   :filter-names)
         (filter-bool-val    :filter-boolean-params))
      `(progn
         (defvar ,search-types-var ,search-types-val
           ,(format "\
Alist of search types and according URL functions.
Functions are used to define URL to receive '%s' entries."
                    entry-type-str))

         (defvar ,filters-var ,filters-val
           ,(format "\
List of filters for '%s' parameters.
Each filter is a function that should take an entry as a single
argument, and should also return an entry."
                    entry-type-str))

         ,(when filter-bool-val
            (let ((filter-bool-var (intern (concat prefix
                                                   "-filter-boolean-params")))
                  (filter-bool-fun (intern (concat prefix
                                                   "-filter-boolean"))))
              `(progn
                 (defvar ,filter-bool-var ,filter-bool-val
                   ,(format "\
List of '%s' parameters that should be transformed to boolean values."
                            entry-type-str))

                 (defun ,filter-bool-fun (entry)
                   ,(format "\
Run `build-farm-filter-boolean' with `%S' variable."
                            filter-bool-var)
                   (build-farm-filter-boolean entry ,filter-bool-var))

                 (setq ,filters-var
                       (cons ',filter-bool-fun ,filters-var)))))

         ;; Do not move this clause up!: name filtering should be
         ;; performed before any other filtering, so this filter should
         ;; be consed after the boolean filter.
         ,(when filter-names-val
            (let* ((filter-names-var (intern (concat prefix
                                                     "-filter-names")))
                   (filter-names-fun filter-names-var))
              `(progn
                 (defvar ,filter-names-var ,filter-names-val
                   ,(format "\
Alist of '%s' parameter names returned by the build farm API and
names used internally by the elisp code of this package."
                            entry-type-str))

                 (defun ,filter-names-fun (entry)
                   ,(format "\
Run `build-farm-filter-names' with `%S' variable."
                            filter-names-var)
                   (build-farm-filter-names entry ,filter-names-var))

                 (setq ,filters-var
                       (cons ',filter-names-fun ,filters-var)))))

         (defun ,get-fun (root-url search-type &rest args)
           ,(format "\
Receive '%s' entries.
See `build-farm-get-entries' for details."
                    entry-type-str)
           (apply #'build-farm-get-entries
                  root-url ',entry-type search-type args))

         (bui-define-groups ,full-entry-type
           :parent-group build-farm
           :parent-faces-group build-farm-faces)

         (bui-define-entry-type ,full-entry-type
           :message-function 'build-farm-message
           ,@%foreign-args)))))

(defmacro build-farm-define-interface (entry-type buffer-type &rest args)
  "Define BUFFER-TYPE interface for displaying ENTRY-TYPE entries.

This macro should be called after calling
`build-farm-define-entry-type' with the same ENTRY-TYPE.

ARGS are passed to `bui-define-interface' macro."
  (declare (indent 2))
  `(bui-define-interface ,(build-farm-symbol entry-type) ,buffer-type
     :get-entries-function ',(build-farm-symbol entry-type 'get-entries)
     ,@args))

(provide 'build-farm)

;;; build-farm.el ends here
