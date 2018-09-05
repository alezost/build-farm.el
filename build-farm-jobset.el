;;; build-farm-jobset.el --- Interface for jobsets  -*- lexical-binding: t -*-

;; Copyright © 2015–2018 Alex Kost <alezost@gmail.com>

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

;; This file provides an interface for displaying jobsets of a build
;; farm in 'list' and 'info' buffers.

;; Unlike builds, jobsets for Cuirass and Hydra have very few in common,
;; so there are 2 different interfaces for these 2 types of jobsets.

;;; Code:

(require 'cl-lib)
(require 'bui)
(require 'build-farm)
(require 'build-farm-build)
(require 'build-farm-url)


;;; Common for Hydra and Cuirass

(defun build-farm-jobset-info-insert-url (entry)
  "Insert URL for the jobset ENTRY."
  (bui-insert-button (build-farm-jobset-url
                      :root-url (build-farm-current-url)
                      :jobset-id (bui-entry-id entry))
                     'bui-url)
  (bui-newline))


;;; Hydra common

(build-farm-define-entry-type hydra-jobset
  :search-types '((project . build-farm-hydra-jobset-api-url))
  :filters '(build-farm-hydra-jobset-filter-id)
  :filter-names '((nrscheduled . scheduled)
                  (nrsucceeded . succeeded)
                  (nrfailed . failed)
                  (nrtotal . total)))

(defun build-farm-hydra-jobset-id (project jobset)
  "Return jobset ID from PROJECT name and JOBSET name."
  (concat project "/" jobset))

(defun build-farm-hydra-jobset-filter-id (entry)
  "Add ID parameter to Hydra jobset ENTRY."
  (cons `(id . ,(build-farm-hydra-jobset-id
                 (bui-entry-non-void-value entry 'project)
                 (bui-entry-non-void-value entry 'name)))
        entry))


;;; Hydra Jobset 'info'

(build-farm-define-interface hydra-jobset info
  :mode-name "Hydra-Jobset-Info"
  :buffer-name "*Farm Jobset Info*"
  :format '((name nil (simple bui-info-heading))
            nil
            build-farm-jobset-info-insert-url
            nil
            (project   format build-farm-hydra-jobset-info-insert-project)
            (scheduled format (format build-farm-hydra-jobset-info-scheduled))
            (succeeded format (format build-farm-hydra-jobset-info-succeeded))
            (failed    format (format build-farm-hydra-jobset-info-failed))
            (total     format (format build-farm-hydra-jobset-info-total))))

(defface build-farm-hydra-jobset-info-scheduled
  '((t))
  "Face used for the number of scheduled builds."
  :group 'build-farm-hydra-jobset-info-faces)

(defface build-farm-hydra-jobset-info-succeeded
  '((t :inherit build-farm-build-status-succeeded))
  "Face used for the number of succeeded builds."
  :group 'build-farm-hydra-jobset-info-faces)

(defface build-farm-hydra-jobset-info-failed
  '((t :inherit build-farm-build-status-failed))
  "Face used for the number of failed builds."
  :group 'build-farm-hydra-jobset-info-faces)

(defface build-farm-hydra-jobset-info-total
  '((t))
  "Face used for the total number of builds."
  :group 'build-farm-hydra-jobset-info-faces)

(defun build-farm-hydra-jobset-info-insert-project (project entry)
  "Insert PROJECT button for the jobset ENTRY."
  (let ((jobset (bui-entry-non-void-value entry 'name)))
    (bui-insert-button project 'build-farm-project)
    (bui-insert-indent)
    (build-farm-info-insert-builds-button
     :project project
     :jobset jobset)))


;;; Hydra Jobset 'list'

(build-farm-define-interface hydra-jobset list
  :describe-function 'build-farm-list-describe
  :mode-name "Hydra-Jobset-List"
  :buffer-name "*Farm Jobsets*"
  :format '((name build-farm-hydra-jobset-list-get-name 25 t)
            (project nil 10 t)
            (scheduled nil 12 t)
            (succeeded nil 12 t)
            (failed nil 9 t)
            (total nil 10 t))
  :hint 'build-farm-hydra-jobset-list-hint)

(let ((map build-farm-hydra-jobset-list-mode-map))
  (define-key map (kbd "B") 'build-farm-hydra-jobset-list-latest-builds))

(defface build-farm-hydra-jobset-list-status-scheduled
  '((t))
  "Face used for a jobset name if there are scheduled jobs."
  :group 'build-farm-hydra-jobset-list-faces)

(defface build-farm-hydra-jobset-list-status-succeeded
  '((t :inherit build-farm-build-status-succeeded))
  "Face used for a jobset name if there are no failed or scheduled jobs."
  :group 'build-farm-hydra-jobset-list-faces)

(defface build-farm-hydra-jobset-list-status-failed
  '((t :inherit build-farm-build-status-failed))
  "Face used for a jobset name if there are failed jobs."
  :group 'build-farm-hydra-jobset-list-faces)

(defvar build-farm-hydra-jobset-list-default-hint
  '(("\\[build-farm-hydra-jobset-list-latest-builds]")
    " show latest builds of the current jobset;\n"))

(defun build-farm-hydra-jobset-list-hint ()
  "Return hint string for a jobset-list buffer."
  (bui-format-hints
   build-farm-hydra-jobset-list-default-hint
   (bui-default-hint)))

(defun build-farm-hydra-jobset-list-get-name (name entry)
  "Return NAME of the jobset ENTRY.
Colorize it with an appropriate face if needed."
  (bui-get-string
   name
   (cond ((> (bui-entry-value entry 'failed) 0)
          'build-farm-hydra-jobset-list-status-failed)
         ((> (bui-entry-value entry 'scheduled) 0)
          'build-farm-hydra-jobset-list-status-scheduled)
         ((= (bui-entry-value entry 'total)
             (bui-entry-value entry 'succeeded))
          'build-farm-hydra-jobset-list-status-succeeded))))

(defun build-farm-hydra-jobset-list-latest-builds (number &rest args)
  "Display latest NUMBER of builds of the current jobset.
Interactively, use `build-farm-number-of-builds' variable for
NUMBER.  With prefix argument, prompt for it and for the other
ARGS."
  (interactive
   (let ((entry (bui-list-current-entry)))
     (build-farm-build-latest-prompt-args
      :project (bui-entry-non-void-value entry 'project)
      :jobset  (bui-entry-non-void-value entry 'name))))
  (apply #'build-farm-get-display
         (build-farm-current-url) 'build 'latest number args))


;;; Cuirass common

(build-farm-define-entry-type cuirass-jobset
  :search-types '((all . build-farm-cuirass-jobsets-url))
  :filters '(build-farm-cuirass-jobset-filter-id)
  :titles '((proc . "Procedure")
            (proc-input . "Procedure input")
            (proc-file . "Procedure file")
            (proc-args . "Procedure arguments")))

(defun build-farm-cuirass-jobset-filter-id (entry)
  "Add ID parameter to Cuirass jobset ENTRY if needed."
  ;; In the past, Cuirass returned jobset ID but not anymore (is it
  ;; temporary?).
  (if (bui-void-value? (bui-entry-id entry))
      (cons `(id . ,(bui-entry-non-void-value entry 'name))
            entry)
    entry))

(defface build-farm-cuirass-jobset-file
  '((t :inherit bui-file-name))
  "Face used for file name of a jobset's procedure."
  :group 'build-farm-cuirass-jobset-faces)

(declare-function guix-directory "guix-repl" t)

(defun build-farm-cuirass-jobset-file-action (button)
  "Find file of BUTTON.
The BUTTON file name is relative to guix source tree."
  (let ((file-name (or (button-get button 'file-name)
                       (button-label button))))
    (if (require 'guix-repl nil t)
        (find-file (expand-file-name file-name (guix-directory)))
      (error "Sorry, no idea where '%s' is placed :-)"
             file-name))))

(define-button-type 'build-farm-cuirass-jobset-file
  :supertype 'bui-file
  'face 'build-farm-cuirass-jobset-file
  'action #'build-farm-cuirass-jobset-file-action)


;;; Cuirass Jobset 'info'

(build-farm-define-interface cuirass-jobset info
  :mode-name "Cuirass-Jobset-Info"
  :buffer-name "*Farm Jobset Info*"
  :format '((name nil (simple bui-info-heading))
            nil
            build-farm-jobset-info-insert-url
            nil
            build-farm-cuirass-jobset-info-insert-builds
            (load-path-inputs format (format))
            (package-path-inputs format (format))
            nil
            (proc-input format (format))
            (proc-file format (format build-farm-cuirass-jobset-file))
            (proc format (format))
            (proc-args simple
                       (build-farm-cuirass-jobset-info-insert-proc-args))
            (inputs simple (build-farm-cuirass-jobset-info-insert-inputs))))

(bui-define-interface build-farm-cuirass-jobset-args info
  :format '((subset format (format))
            (systems format (format build-farm-system)))
  :reduced? t)

(bui-define-interface build-farm-cuirass-jobset-inputs info
  :format '((name format (format))
            (url format (format bui-url))
            (branch format (format))
            (load-path format (format))
            (tag format (format))
            (commit format (format))
            (no-compile? format (format)))
  :reduced? t)

(defun build-farm-cuirass-jobset-info-insert-builds (entry)
  "Insert 'Builds' button for the jobset ENTRY."
  (let ((jobset (bui-entry-non-void-value entry 'name)))
    (build-farm-info-insert-builds-button
     :jobset jobset))
  (bui-newline))

(defun build-farm-cuirass-jobset-info-insert-file (file-name)
  "Insert FILE-NAME of a jobset's procedure at point."
  (bui-insert-non-nil file-name
    (bui-info-insert-value-indent
     file-name 'build-farm-jobset-proc-file)))

(defun build-farm-cuirass-jobset-info-insert-proc-args (args)
  "Insert procedure ARGS at point."
  (bui-newline)
  (bui-info-insert-entry args 'build-farm-cuirass-jobset-args 1))

(defun build-farm-cuirass-jobset-info-insert-inputs (inputs)
  "Insert jobset INPUTS at point."
  (dolist (input inputs)
    (bui-newline)
    (bui-info-insert-entry input 'build-farm-cuirass-jobset-inputs 1)))


;;; Cuirass Jobset 'list'

(build-farm-define-interface cuirass-jobset list
  :describe-function 'build-farm-list-describe
  :mode-name "Cuirass-Jobset-List"
  :buffer-name "*Farm Jobsets*"
  :hint 'build-farm-cuirass-jobset-list-hint
  :format '((name nil 30 t)
            (proc-input nil 20 t)
            (proc-file build-farm-cuirass-jobset-list-get-file 20 t)))

(let ((map build-farm-cuirass-jobset-list-mode-map))
  (define-key map (kbd "B") 'build-farm-cuirass-jobset-list-latest-builds))

(defvar build-farm-cuirass-jobset-list-default-hint
  '(("\\[build-farm-cuirass-jobset-list-latest-builds]")
    " show latest builds of the current jobset;\n"))

(defun build-farm-cuirass-jobset-list-hint ()
  "Return hint string for a jobset-list buffer."
  (bui-format-hints
   build-farm-cuirass-jobset-list-default-hint
   (bui-default-hint)))

(defun build-farm-cuirass-jobset-list-get-file (file-name &optional _)
  "Return FILE-NAME button specification for `tabulated-list-entries'."
  (bui-get-non-nil file-name
    (list file-name
          :type 'build-farm-cuirass-jobset-file
          'file-name file-name)))

(defun build-farm-cuirass-jobset-list-latest-builds (number &rest args)
  "Display latest NUMBER of builds of the current jobset.
Interactively, use `build-farm-number-of-builds' variable for
NUMBER.  With prefix argument, prompt for it and for the other
ARGS."
  (interactive
   (let ((entry (bui-list-current-entry)))
     (build-farm-build-latest-prompt-args
      :jobset (bui-entry-non-void-value entry 'name))))
  (apply #'build-farm-get-display
         (build-farm-current-url) 'build 'latest number args))


;;; Interactive commands

;;;###autoload
(defun build-farm-jobsets (&optional project)
  "Display jobsets of PROJECT.
PROJECT is required for Hydra build farm and is not needed for
Cuirass."
  (interactive
   (when (eq 'hydra (build-farm-url-type))
     (list (build-farm-read-project))))
  (if (eq 'cuirass (build-farm-url-type))
      (build-farm-get-display build-farm-url 'cuirass-jobset 'all)
    (build-farm-get-display build-farm-url 'hydra-jobset
                            'project project)))

;; Info returned for multiple jobsets (from "api/jobsets") and for a
;; single jobset (from "jobset") are completely different!  Compare:
;;
;;   (build-farm-receive-data "https://hydra.nixos.org/jobset/hydra/master")
;;   (build-farm-receive-data "https://hydra.nixos.org/api/jobsets?project=hydra")
;;
;; How this duality can be supported?  Maybe make another
;; "jobset-configuration" interface?  Anyway, `build-farm-jobset'
;; command is not available yet.

;; (defun build-farm-jobset (project jobset)
;;   "Display JOBSET of PROJECT."
;;   (interactive (list (build-farm-read-project)
;;                      (build-farm-read-jobset project)))
;;   (build-farm-jobset-get-display
;;    'id (build-farm-jobset-id project jobset)))

(provide 'build-farm-jobset)

;;; build-farm-jobset.el ends here
