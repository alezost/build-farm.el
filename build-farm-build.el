;;; build-farm-build.el --- Interface for builds  -*- lexical-binding: t -*-

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

;; This file provides an interface for displaying builds of a build farm
;; in 'list' and 'info' buffers.

;;; Code:

(require 'cl-lib)
(require 'bui)
(require 'build-farm)
(require 'build-farm-utils)
(require 'build-farm-url)

(build-farm-define-entry-type build
  :search-types '((latest . build-farm-build-latest-api-url)
                  (queue  . build-farm-build-queue-api-url))
  :filters '(build-farm-build-filter-status)
  :filter-names '((nixname . name)
                  (buildstatus . build-status)
                  (timestamp . time))
  :filter-boolean-params '(finished busy))

(defcustom build-farm-number-of-builds 64
  "Default number of builds to display.
This variable is used by '\\[build-farm-latest-builds]' and'
'\\[build-farm-queued-builds]' commands.  If nil, always prompt
for the number of builds."
  :type 'integer
  :group 'build-farm-build)

(defun build-farm-build-get-display (search-type &rest args)
  "Search for builds and show results.
See `build-farm-search-url' for the meaning of SEARCH-TYPE and ARGS."
  (apply #'bui-list-get-display-entries
         'build-farm-build search-type args))

(cl-defun build-farm-build-latest-prompt-args (&key project jobset
                                                    job system)
  "Prompt for and return a list of 'latest builds' arguments."
  (let* ((number      (if (or current-prefix-arg
                              (null build-farm-number-of-builds))
                          (read-number "Number of latest builds: "
                                       build-farm-number-of-builds)
                        build-farm-number-of-builds))
         (project     (if current-prefix-arg
                          (build-farm-read-project nil project)
                        project))
         (jobset      (if current-prefix-arg
                          (build-farm-read-jobset nil jobset)
                        jobset))
         (job-or-name (if current-prefix-arg
                          (build-farm-read-job nil job)
                        job))
         (job         (and job-or-name
                           (string-match-p build-farm-job-regexp
                                           job-or-name)
                           job-or-name))
         (system      (if (and (not job)
                               (or current-prefix-arg
                                   (and job-or-name (not system))))
                          (if job-or-name
                              (build-farm-while-null
                                (build-farm-read-system
                                 (concat job-or-name ".") system))
                            (build-farm-read-system nil system))
                        system))
         (job         (or job
                          (and job-or-name
                               (concat job-or-name "." system)))))
    (list number
          :project project
          :jobset  jobset
          :job     job
          :system  system)))

(declare-function guix-build-log-find-file "guix-build-log" (file))

(defun build-farm-build-view-log (id)
  "View build log of a build ID."
  (require 'guix-build-log)
  (guix-build-log-find-file (build-farm-build-log-url id)))


;;; Filters for processing raw entries

(defun build-farm-build-filter-status (entry)
  "Add 'status' parameter to 'hydra-build' ENTRY."
  (let ((status (if (bui-entry-non-void-value entry 'finished)
                    (build-farm-build-status-number->name
                     (bui-entry-non-void-value entry 'build-status))
                  (if (bui-entry-non-void-value entry 'busy)
                      'running
                    'scheduled))))
    (cons `(status . ,status)
          entry)))


;;; Build status

(defface build-farm-build-status-running
  '((t :inherit bold))
  "Face used if a build is not finished."
  :group 'build-farm-build-faces)

(defface build-farm-build-status-scheduled
  '((t))
  "Face used if a build is scheduled."
  :group 'build-farm-build-faces)

(defface build-farm-build-status-succeeded
  '((t :inherit success))
  "Face used if a build succeeded."
  :group 'build-farm-build-faces)

(defface build-farm-build-status-cancelled
  '((t :inherit warning))
  "Face used if a build was cancelled."
  :group 'build-farm-build-faces)

(defface build-farm-build-status-failed
  '((t :inherit error))
  "Face used if a build failed."
  :group 'build-farm-build-faces)

(defface build-farm-build-status-unknown
  '((t))
  "Face used if a build has an unknown status."
  :group 'build-farm-build-faces)

(defvar build-farm-build-status-alist
  ;; "buildstatus" numbers can be looked at:
  ;; <https://github.com/NixOS/hydra/blob/master/src/root/common.tt>.
  '((0 . succeeded)
    (1 . failed-build)
    (2 . failed-dependency)
    (3 . failed-other)
    (6 . failed-output)
    (4 . cancelled))
  "Alist of build status numbers and status names.
Status numbers are returned by build farms APIs; names (symbols)
are used internally by the elisp code of this package.")

(defun build-farm-build-status-number->name (number)
  "Convert build status NUMBER to a name.
See `build-farm-build-status-alist'."
  (bui-assq-value build-farm-build-status-alist number))

(defun build-farm-build-status-string (status)
  "Return a human readable string for build STATUS."
  (cl-case status
    (scheduled
     (bui-get-string "Scheduled" 'build-farm-build-status-scheduled))
    (running
     (bui-get-string "Running" 'build-farm-build-status-running))
    (succeeded
     (bui-get-string "Succeeded" 'build-farm-build-status-succeeded))
    (cancelled
     (bui-get-string "Cancelled" 'build-farm-build-status-cancelled))
    (failed-build
     (build-farm-build-status-fail-string))
    (failed-dependency
     (build-farm-build-status-fail-string "dependency"))
    (failed-other
     (build-farm-build-status-fail-string "other"))
    (failed-output
     (build-farm-build-status-fail-string "with output"))
    (t
     (bui-get-string "Unknown" 'build-farm-build-status-unknown))))

(defun build-farm-build-status-fail-string (&optional reason)
  "Return a string for a failed build using REASON."
  (let ((base (bui-get-string "Failed" 'build-farm-build-status-failed)))
    (if reason
        (concat base " (" reason ")")
      base)))

(defun build-farm-build-finished? (entry)
  "Return non-nil, if build ENTRY was finished."
  (bui-entry-non-void-value entry 'finished))

(defun build-farm-build-running? (entry)
  "Return non-nil, if build ENTRY is running."
  (eq (bui-entry-non-void-value entry 'status)
      'running))

(defun build-farm-build-scheduled? (entry)
  "Return non-nil, if build ENTRY is scheduled."
  (eq (bui-entry-non-void-value entry 'status)
      'scheduled))

(defun build-farm-build-succeeded? (entry)
  "Return non-nil, if build ENTRY succeeded."
  (eq (bui-entry-non-void-value entry 'status)
      'succeeded))

(defun build-farm-build-cancelled? (entry)
  "Return non-nil, if build ENTRY was cancelled."
  (eq (bui-entry-non-void-value entry 'status)
      'cancelled))

(defun build-farm-build-failed? (entry)
  "Return non-nil, if build ENTRY failed."
  (memq (bui-entry-non-void-value entry 'status)
        '(failed-build failed-dependency failed-other)))


;;; Build 'info'

(build-farm-define-interface build info
  :mode-name "Farm-Build-Info"
  :buffer-name "*Farm Build Info*"
  :format '((name nil (simple bui-info-heading))
            nil
            build-farm-build-info-insert-url
            (time     format (time))
            (status   format build-farm-build-info-insert-status)
            (project  format (format build-farm-build-project))
            (jobset   format (format build-farm-build-jobset))
            (job      format (format build-farm-build-job))
            (system   format (format build-farm-build-system))
            (priority format (format))))

(defface build-farm-build-info-project
  '((t :inherit link))
  "Face for project names."
  :group 'build-farm-build-info-faces)

(defface build-farm-build-info-jobset
  '((t :inherit link))
  "Face for jobsets."
  :group 'build-farm-build-info-faces)

(defface build-farm-build-info-job
  '((t :inherit link))
  "Face for jobs."
  :group 'build-farm-build-info-faces)

(defface build-farm-build-info-system
  '((t :inherit link))
  "Face for system names."
  :group 'build-farm-build-info-faces)

(defmacro build-farm-build-define-button (name)
  "Define `build-farm-build-NAME' button."
  (let* ((name-str    (symbol-name name))
         (button-name (intern (concat "build-farm-build-" name-str)))
         (face-name   (intern (concat "build-farm-build-info-" name-str)))
         (keyword     (intern (concat ":" name-str))))
    `(define-button-type ',button-name
       :supertype 'bui
       'face ',face-name
       'help-echo ,(format "\
Show latest builds for this %s (with prefix, prompt for all parameters)"
                           name-str)
       'action (lambda (btn)
                 (let ((args (build-farm-build-latest-prompt-args
                              ,keyword (button-label btn))))
                   (apply #'build-farm-build-get-display
                          'latest args))))))

(build-farm-build-define-button project)
(build-farm-build-define-button jobset)
(build-farm-build-define-button job)
(build-farm-build-define-button system)

(defun build-farm-build-info-insert-url (entry)
  "Insert URL for the build ENTRY."
  (bui-insert-button (build-farm-build-url (bui-entry-id entry))
                     'bui-url)
  (when (build-farm-build-finished? entry)
    (bui-insert-indent)
    (bui-insert-action-button
     "Build log"
     (lambda (btn)
       (build-farm-build-view-log (button-get btn 'id)))
     "View build log"
     'id (bui-entry-id entry)))
  (bui-newline))

(defun build-farm-build-info-insert-status (status &optional _)
  "Insert a string with build STATUS."
  (insert (build-farm-build-status-string status)))


;;; Build 'list'

(build-farm-define-interface build list
  :describe-function 'build-farm-list-describe
  :mode-name "Farm-Build-List"
  :buffer-name "*Farm Builds*"
  :format '((name nil 30 t)
            (system nil 16 t)
            (status build-farm-build-list-get-status 20 t)
            (project nil 10 t)
            (jobset nil 17 t)
            (time bui-list-get-time 20 t))
  :hint 'build-farm-build-list-hint)

(let ((map build-farm-build-list-mode-map))
  (define-key map (kbd "B") 'build-farm-build-list-latest-builds)
  (define-key map (kbd "L") 'build-farm-build-list-view-log))

(defvar build-farm-build-list-default-hint
  '(("\\[build-farm-build-list-latest-builds]")
    " show latest builds of the current job;\n"
    ("\\[build-farm-build-list-view-log]") " show build log;\n"))

(defun build-farm-build-list-hint ()
  "Return hint string for a build-list buffer."
  (bui-format-hints
   build-farm-build-list-default-hint
   (bui-default-hint)))

(defun build-farm-build-list-get-status (status &optional _)
  "Return a string for build STATUS."
  (build-farm-build-status-string status))

(defun build-farm-build-list-latest-builds (number &rest args)
  "Display latest NUMBER of builds of the current job.
Interactively, prompt for NUMBER.  With prefix argument, prompt
for all ARGS."
  (interactive
   (let ((entry (bui-list-current-entry)))
     (build-farm-build-latest-prompt-args
      :project (bui-entry-non-void-value entry 'project)
      :jobset  (bui-entry-non-void-value entry 'name)
      :job     (bui-entry-non-void-value entry 'job)
      :system  (bui-entry-non-void-value entry 'system))))
  (apply #'build-farm-latest-builds number args))

(defun build-farm-build-list-view-log ()
  "View build log of the current build."
  (interactive)
  (build-farm-build-view-log (bui-list-current-id)))


;;; Interactive commands

;;;###autoload
(defun build-farm-latest-builds (number &rest args)
  "Display latest NUMBER of builds.
ARGS are the same arguments as for `build-farm-build-latest-api-url'.
Interactively, use `build-farm-number-of-builds' variable for
NUMBER.  With prefix argument, prompt for it and for the other
ARGS."
  (interactive (build-farm-build-latest-prompt-args))
  (apply #'build-farm-build-get-display
         'latest number args))

;;;###autoload
(defun build-farm-queued-builds (number)
  "Display the NUMBER of queued builds.
Interactively, use `build-farm-number-of-builds' variable for
NUMBER.  With prefix argument, prompt for it."
  (interactive
   (list (if (or current-prefix-arg
                 (null build-farm-number-of-builds))
             (read-number "Number of queued builds: "
                          build-farm-number-of-builds)
           build-farm-number-of-builds)))
  (build-farm-build-get-display 'queue number))

(provide 'build-farm-build)

;;; build-farm-build.el ends here
