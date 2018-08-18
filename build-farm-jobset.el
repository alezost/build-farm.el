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
  "Add 'ID' parameter to 'jobset' ENTRY."
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
    (build-farm-build-info-insert-builds-button
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


;;; Interactive commands

;;;###autoload
(defun build-farm-jobsets (project)
  "Display jobsets of PROJECT."
  (interactive (list (build-farm-read-project)))
  (build-farm-get-display build-farm-url 'hydra-jobset
                          'project project))

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
