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
  :search-types '((id     . build-farm-build-url)
                  (latest . build-farm-build-latest-api-url)
                  (queue  . build-farm-build-queue-api-url))
  :filters '(build-farm-build-filter-status)
  :filter-names '((nixname . name)
                  (buildstatus . build-status)
                  (buildmetrics . build-metrics)
                  (buildoutputs . outputs)
                  (drvpath . derivation)
                  (releasename . release-name)
                  (starttime . start-time)
                  (stoptime . stop-time)
                  (timestamp . queued-time))
  :filter-boolean-params '(finished busy)
  :titles '((queued-time . "Queued at")
            (start-time . "Started at")
            (stop-time . "Stopped at")))

(defcustom build-farm-number-of-builds 64
  "Default number of builds to display.
This variable is used by '\\[build-farm-latest-builds]' and'
'\\[build-farm-queued-builds]' commands.  If nil, always prompt
for the number of builds."
  :type 'integer
  :group 'build-farm-build)

(defun build-farm-set-number-of-builds (number)
  "Set `build-farm-number-of-builds' to NUMBER."
  (interactive (list (build-farm-build-read-number)))
  (setq build-farm-number-of-builds number))

(defun build-farm-build-read-number (&optional prompt)
  "Read from minibuffer (using PROMPT) a number of builds."
  (read-number (or prompt "Number of builds: ")
               build-farm-number-of-builds))

(defun build-farm-build-read-number-maybe (&optional prompt)
  "Read from minibuffer (using PROMPT) a number of builds.
If `current-prefix-arg' is specified, just return
`build-farm-number-of-builds' without reading."
  (if (or current-prefix-arg
          (null build-farm-number-of-builds))
      (build-farm-build-read-number prompt)
    build-farm-number-of-builds))

(cl-defun build-farm-build-latest-prompt-args (&key project jobset
                                                    job system)
  "Prompt for and return a list of 'latest builds' arguments."
  (let* ((number      (build-farm-build-read-number-maybe))
         (url         (build-farm-current-url))
         (url-type    (build-farm-url-type url))
         (project     (unless (eq 'cuirass url-type)
                        (if current-prefix-arg
                            (build-farm-read-project
                             :url url
                             :initial-input project)
                          project)))
         (jobset      (if current-prefix-arg
                          (build-farm-read-jobset
                           :url url
                           :project project
                           :initial-input jobset)
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

(defun build-farm-build-button-action (button)
  "Display latest builds according to BUTTON."
  (let ((args (build-farm-build-latest-prompt-args
               :project (button-get button 'project)
               :jobset  (button-get button 'jobset)
               :job     (button-get button 'job)
               :system  (button-get button 'system))))
    (apply #'build-farm-get-display
           (build-farm-current-url)
           'build 'latest args)))

(cl-defun build-farm-info-insert-builds-button
    (&key project jobset job system)
  "Insert 'Builds' button for PROJECT, JOBSET, JOB, SYSTEM."
  (bui-insert-action-button
   "Builds"
   #'build-farm-build-button-action
   (concat "Show latest builds"
           (let ((thing (cond (job "job")
                              (system "system")
                              (jobset "jobset")
                              (project "project"))))
             (if thing
                 (concat " for this " thing)
               ""))
           " (with prefix, prompt for all parameters)")
   'project project
   'jobset jobset
   'job job
   'system system))

(declare-function guix-build-log-mode "guix-build-log" t)

(defun build-farm-build-view-log (id &optional root-url)
  "View build log of a build ID from ROOT-URL."
  (let ((pkg-manager (build-farm-url-package-manager root-url))
        (url (or root-url (build-farm-current-url))))
    (if (eq pkg-manager 'nix)
        ;; Logs from hydra.nixos.org (which are actually kept on
        ;; amazonaws) are stored in a compressed form that is not
        ;; supported by Emacs yet: a raw log page returns
        ;; "Content-Encoding: br" heading ("Brotli" compression).  So
        ;; instead of opening the log in Emacs (it would be displayed as
        ;; an arbitrary binary data), open it in a browser.
        (browse-url (build-farm-build-log-url id :root-url url))
      (browse-url-emacs (build-farm-build-log-url
                         id :root-url url :raw t))
      (when (and (eq pkg-manager 'guix)
                 (require 'guix-build-log nil t))
        (guix-build-log-mode)))))


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
  :mode-name "Build-Info"
  :buffer-name "*Farm Build Info*"
  :format '((name nil (simple bui-info-heading))
            nil
            build-farm-build-info-insert-more-button
            build-farm-build-info-insert-url
            nil
            (queued-time format (time))
            (start-time format (time))
            (stop-time format (time))
            (release-name format (format))
            (status   format build-farm-build-info-insert-status)
            (priority format (format))
            (derivation simple (indent bui-file))
            (outputs simple (build-farm-build-info-insert-outputs))
            nil
            (project  format build-farm-build-info-insert-project)
            (jobset   format build-farm-build-info-insert-jobset)
            (job      format build-farm-build-info-insert-job)
            (system   format build-farm-build-info-insert-system)))

(defvar build-farm-build-info-output-format "%-6s  "
  "String for formatting output names of builds.
It should be a '%s'-sequence.")

(defun build-farm-build-info-insert-more-button (entry)
  "Insert 'More info' button for build ENTRY at point."
  (when (and (eq 'hydra (build-farm-current-url-type))
             (bui-void-value? (bui-entry-value entry 'start-time)))
    (bui-insert-action-button
     "More info"
     (lambda (btn)
       (build-farm-build-info-update-build (button-get btn 'id)))
     "Receive more info on the current build"
     'id (bui-entry-id entry))
    (bui-newline 2)))

(defun build-farm-build-info-update-build (id)
  "Update build with ID in the current build info buffer."
  (let ((new-entry (car (bui-get-entries
                         'build-farm-build 'info
                         (list (build-farm-current-url) 'id id)))))
    (or new-entry
        ;; Actually, this shouldn't happen.
        (error "Couldn't receive more info for build %d" id))
    (setf (bui-item-entries bui-item)
          (bui-replace-entry (bui-current-entries) id new-entry))
    (bui-redisplay)))

(defun build-farm-build-info-insert-project (project entry)
  "Insert PROJECT for build ENTRY at point."
  (bui-insert-button project 'build-farm-project)
  (bui-insert-indent)
  (build-farm-info-insert-builds-button
   :project (bui-entry-non-void-value entry 'project)))

(defun build-farm-build-info-insert-jobset (jobset entry)
  "Insert JOBSET for build ENTRY at point."
  (if (eq 'hydra (build-farm-current-url-type))
      (build-farm-info-insert-hydra-jobset
       (bui-entry-non-void-value entry 'project)
       jobset)
    (build-farm-info-insert-cuirass-jobset jobset))
  (bui-insert-indent)
  (build-farm-info-insert-builds-button
   :project (bui-entry-non-void-value entry 'project)
   :jobset  (bui-entry-non-void-value entry 'jobset)))

(defun build-farm-build-info-insert-job (job entry)
  "Insert JOB for build ENTRY at point."
  (bui-format-insert job 'build-farm-info-job)
  (bui-insert-indent)
  (build-farm-info-insert-builds-button
   :project (bui-entry-non-void-value entry 'project)
   :jobset  (bui-entry-non-void-value entry 'jobset)
   :job     (bui-entry-non-void-value entry 'job)))

(defun build-farm-build-info-insert-system (system entry)
  "Insert SYSTEM for build ENTRY at point."
  (bui-format-insert system 'build-farm-info-system)
  (bui-insert-indent)
  (build-farm-info-insert-builds-button
   :system  (bui-entry-non-void-value entry 'system)))

(defun build-farm-build-info-insert-url (entry)
  "Insert URL for the build ENTRY."
  (bui-insert-button (build-farm-build-url
                      (bui-entry-id entry)
                      :root-url (build-farm-current-url))
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

(defun build-farm-build-info-insert-outputs (outputs)
  "Insert build OUTPUTS at point."
  (bui-insert-non-nil outputs
    (dolist (output outputs)
      (bui-newline)
      (bui-insert-indent)
      (build-farm-build-info-insert-output output))))

(defun build-farm-build-info-insert-output (output)
  "Insert build OUTPUT at point."
  (let* ((name (symbol-name (car output)))
         (alist (cdr output))
         (file-name (bui-assq-value alist 'path)))
    (bui-format-insert name nil
                       build-farm-build-info-output-format)
    (bui-insert-button file-name 'bui-file)))


;;; Build 'list'

(build-farm-define-interface build list
  :describe-function 'build-farm-list-describe
  :mode-name "Build-List"
  :buffer-name "*Farm Builds*"
  :format '((name nil 30 t)
            (system nil 16 t)
            (status build-farm-build-list-get-status 20 t)
            (project nil 10 t)
            (jobset nil 17 t)
            (queued-time bui-list-get-time 20 t))
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
Interactively, use `build-farm-number-of-builds' variable for
NUMBER.  With prefix argument, prompt for it and for the other
ARGS."
  (interactive
   (let ((entry (bui-list-current-entry)))
     (build-farm-build-latest-prompt-args
      :project (bui-entry-non-void-value entry 'project)
      :jobset  (bui-entry-non-void-value entry 'jobset)
      :job     (bui-entry-non-void-value entry 'job)
      :system  (bui-entry-non-void-value entry 'system))))
  (apply #'build-farm-get-display
         (build-farm-current-url) 'build 'latest number args))

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
  (apply #'build-farm-get-display
         build-farm-url 'build 'latest number args))

;;;###autoload
(defun build-farm-queued-builds (number)
  "Display the NUMBER of queued builds.
Interactively, use `build-farm-number-of-builds' variable for
NUMBER.  With prefix argument, prompt for it."
  (interactive
   (list (build-farm-build-read-number-maybe)))
  (build-farm-get-display build-farm-url 'build 'queue number))

;;;###autoload
(defun build-farm-build (id)
  "Find build by its ID and display it."
  (interactive "nBuild ID: ")
  (build-farm-get-display build-farm-url 'build 'id id))

(provide 'build-farm-build)

;;; build-farm-build.el ends here
