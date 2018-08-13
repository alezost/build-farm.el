;;; build-farm-project.el --- Interface for projects  -*- lexical-binding: t -*-

;; Copyright © 2018 Alex Kost <alezost@gmail.com>

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

;; This file provides an interface for displaying projects of a build
;; farm in 'list' and 'info' buffers.

;;; Code:

(require 'bui)
(require 'build-farm)
(require 'build-farm-url)
(require 'build-farm-build)
(require 'build-farm-jobset)

(build-farm-define-entry-type project
  :search-types '((all . build-farm-project-url))
  :filter-names '((name . id)
                  (displayname . name))
  ;; Also there is `hidden' boolean field, but it is useless because it
  ;; is the same thing as `enabled': when `hidden' is 1, `enabled' is 0,
  ;; and vice versa.  So `hidden' is not going to be used anywhere.
  :filter-boolean-params '(enabled))


;;; Project 'info'

(build-farm-define-interface project info
  :mode-name "Farm-Project-Info"
  :buffer-name "*Farm Project Info*"
  :format '((name nil (simple bui-info-heading))
            nil
            (description nil (simple build-farm-project-info-description))
            nil
            build-farm-project-info-insert-url
            (id format (format))
            (owner format (format build-farm-project-info-owner))
            (enabled format build-farm-project-info-insert-enabled)
            (releases format (format))
            (jobsets simple build-farm-project-info-insert-jobsets)))

(defface build-farm-project-info-description
  '((t :inherit font-lock-doc-face))
  "Face used for a project description."
  :group 'build-farm-project-info-faces)

(defface build-farm-project-info-owner
  '((t :inherit font-lock-constant-face))
  "Face used for the owner of a project."
  :group 'build-farm-project-info-faces)

(defface build-farm-project-info-enabled
  '((t :inherit success))
  "Face used for enabled projects."
  :group 'build-farm-project-info-faces)

(defface build-farm-project-info-disabled
  '((t :inherit shadow))
  "Face used for disabled projects."
  :group 'build-farm-project-info-faces)

(defun build-farm-project-info-insert-url (entry)
  "Insert URL for the project ENTRY."
  (bui-insert-button (build-farm-project-url
                      :root-url (build-farm-current-url)
                      :project (bui-entry-id entry))
                     'bui-url)
  (bui-newline))

(defun build-farm-project-info-insert-enabled (value _entry)
  "Insert boolean VALUE showing whether this project is enabled."
  (if value
      (bui-info-insert-value-format "Yes" 'build-farm-project-info-enabled)
    (bui-info-insert-value-format "No" 'build-farm-project-info-disabled)))

(defun build-farm-project-info-insert-jobsets (jobsets entry)
  "Insert JOBSETS of the project ENTRY."
  (let ((project (bui-entry-id entry)))
    (dolist (jobset jobsets)
      (bui-newline)
      (bui-insert-indent)
      (build-farm-project-info-insert-jobset project jobset))))

(defun build-farm-project-info-insert-jobset (project jobset)
  "Insert info about JOBSET of the PROJECT at point."
  (build-farm-info-insert-jobset project jobset)
  (bui-insert-indent)
  (build-farm-build-info-insert-builds-button
   :project project
   :jobset jobset))


;;; Project 'list'

(build-farm-define-interface project list
  :describe-function 'build-farm-list-describe
  :mode-name "Farm-Project-List"
  :buffer-name "*Farm Projects*"
  :format '((name build-farm-project-list-get-name 25 t)
            (owner nil 22 t)
            (description bui-list-get-one-line 30 t))
  :hint 'build-farm-project-list-hint)

(let ((map build-farm-project-list-mode-map))
  (define-key map (kbd "B") 'build-farm-project-list-latest-builds)
  (define-key map (kbd "J") 'build-farm-project-list-jobsets))

(defface build-farm-project-list-disabled
  '((t :inherit build-farm-project-info-disabled))
  "Face used for disabled projects."
  :group 'build-farm-project-list-faces)

(defvar build-farm-project-list-default-hint
  '(("\\[build-farm-project-list-jobsets]")
    " show jobsets of the current project;\n"
    ("\\[build-farm-project-list-latest-builds]")
    " show latest builds of the current project;\n"))

(defun build-farm-project-list-hint ()
  "Return hint string for a project-list buffer."
  (bui-format-hints
   build-farm-project-list-default-hint
   (bui-default-hint)))

(defun build-farm-project-list-read-jobset ()
  "Read jobset for the current project."
  (build-farm-completing-read
   "Jobset: "
   (bui-entry-non-void-value (bui-list-current-entry)
                             'jobsets)))

(defun build-farm-project-list-get-name (name entry)
  "Return NAME of the project ENTRY.
Colorize it with an appropriate face if needed."
  (bui-get-string
   name
   (unless (bui-entry-non-void-value entry 'enabled)
     'build-farm-project-list-disabled)))

(declare-function build-farm-jobsets "build-farm-jobset")

(defun build-farm-project-list-jobsets ()
  "Display jobsets of the current project."
  (interactive)
  (build-farm-get-display (build-farm-current-url)
                          'jobset 'project (bui-list-current-id)))

(defun build-farm-project-list-latest-builds (number &rest args)
  "Display latest NUMBER of builds of the current project.
Interactively, use `build-farm-number-of-builds' variable for
NUMBER.  With prefix argument, prompt for it and for the other
ARGS."
  (interactive
   (build-farm-build-latest-prompt-args
    :project (bui-list-current-id)
    :jobset  (build-farm-project-list-read-jobset)))
  (apply #'build-farm-get-display
         (build-farm-current-url) 'build 'latest number args))


;;; Interactive commands

;;;###autoload
(defun build-farm-projects ()
  "Display build farm projects."
  (interactive)
  (build-farm-get-display build-farm-url 'project 'all))

;;;###autoload
(defun build-farm-project (project)
  "Display build farm PROJECT."
  (interactive (list (build-farm-read-project)))
  (bui-get-display-entries 'build-farm-project 'info
                           (list build-farm-url 'id project)))

(provide 'build-farm-project)

;;; build-farm-project.el ends here
