;;; build-farm-popup.el --- Magit-like popup interface  -*- lexical-binding: t -*-

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

;; This file provides popup interface (using `magit-popup' library) for
;; build farm commands.

;;; Code:

(require 'cl-lib)
(require 'magit-popup)
(require 'build-farm-url)
(require 'build-farm-build)
(require 'build-farm-evaluation)

(defgroup build-farm-popup nil
  "Variables for popup interface for build farm commands."
  :group 'build-farm)

;;;###autoload (autoload 'build-farm-popup "build-farm-popup" nil t)
(magit-define-popup build-farm-popup
  "Show popup buffer for build farm commands."
  'build-farm-popup
  :variables '((?u "URL"
                   build-farm-set-url
                   build-farm-popup-format-url))
  :actions '((?p "projects" build-farm-projects)
             (?j "jobsets"  build-farm-jobsets)
             (?b "builds"   build-farm-build-popup)
             (?e "evaluations" build-farm-evaluation-popup-maybe)))

;;;###autoload
(defalias 'build-farm #'build-farm-popup
  "Popup interface for the available build farm commands.")

(defun build-farm-popup-variable-value (var-name)
  "Return string formatted for popup buffer.
String is made of variable VAR-NAME and its value."
  (concat (propertize (symbol-name var-name)
                      'face font-lock-variable-name-face)
          " "
          (propertize (prin1-to-string (symbol-value var-name))
                      'face 'magit-popup-option-value)))

(defun build-farm-popup-format-url ()
  "Return URL string, formatted for '\\[build-farm]'."
  (build-farm-popup-variable-value 'build-farm-url))

(defun build-farm-evaluation-popup-maybe (&optional arg)
  "Display popup for evaluations if it is supported.
ARG is passed to `build-farm-evaluation-popup'."
  (interactive "P")
  (if (eq 'hydra (build-farm-url-type))
      (error "Sorry, Hydra evaluations are not supported because of
<https://github.com/NixOS/hydra/issues/582>")
    (build-farm-evaluation-popup arg)))


;;; Builds

(magit-define-popup build-farm-build-popup
  "Show popup buffer for builds."
  'build-farm-popup
  :variables '("Variable for latest and queued builds"
               (?n "number"
                   build-farm-set-number-of-builds
                   build-farm-popup-format-number-of-builds))
  :options  '("Options for latest builds"
              (?p "project" "project=" build-farm-popup-read-project)
              (?j "jobset"  "jobset=" build-farm-popup-read-jobset)
              (?J "job"     "job=")
              (?s "system"  "system=" build-farm-read-system))
  :actions '((?l "latest" build-farm-popup-latest-builds)
             (?q "queued" build-farm-popup-queued-builds)
             (?i "build by ID" build-farm-build)))

(defun build-farm-popup-read-project (&optional prompt initial-input)
  "Read project from minibuffer.
See `completing-read' for PROMPT and INITIAL-INPUT."
  (build-farm-read-project :prompt prompt
                           :initial-input initial-input))

(defun build-farm-popup-read-jobset (&optional prompt initial-input)
  "Read jobset for the current project from minibuffer.
See `completing-read' for PROMPT and INITIAL-INPUT."
  (build-farm-read-jobset
   :prompt prompt
   :initial-input initial-input
   :project (plist-get (build-farm-popup-parse-build-args
                        (magit-popup-get-args))
                       :project)))

(defun build-farm-popup-format-number-of-builds ()
  "Return number of builds, formatted for '\\[build-farm-build-popup]'."
  (build-farm-popup-variable-value 'build-farm-number-of-builds))

(defun build-farm-popup-build-args ()
  "Return arguments of the current build popup buffer."
  (and (eq magit-current-popup 'build-farm-build-popup)
       magit-current-popup-args))

(defun build-farm-popup-parse-build-args (args)
  "Convert popup ARGS to a form suitable for `build-farm-latest-builds'."
  (cl-mapcan (lambda (string)
               (cl-multiple-value-bind (key value)
                   (split-string string "=")
                 (list (intern (concat ":" key))
                       value)))
             args))

(defun build-farm-popup-latest-builds (&rest args)
  "Display `build-farm-number-of-builds' of latest builds.
ARGS are read from the current popup buffer."
  (interactive (build-farm-popup-build-args))
  (apply #'build-farm-latest-builds
         (apply #'build-farm-build-latest-prompt-args
                (build-farm-popup-parse-build-args args))))

(defun build-farm-popup-queued-builds ()
  "Display `build-farm-number-of-builds' of queued builds."
  (interactive)
  (build-farm-queued-builds
   (or build-farm-number-of-builds
       (build-farm-build-read-number))))


;;; Evaluations

(magit-define-popup build-farm-evaluation-popup
  "Show popup buffer for evaluations."
  'build-farm-popup
  :variables '((?n "number"
                   build-farm-set-number-of-evaluations
                   build-farm-popup-format-number-of-evaluations))
  :actions '((?l "latest" build-farm-popup-latest-evaluations)))

(defun build-farm-popup-format-number-of-evaluations ()
  "Return number of evaluations for '\\[build-farm-evaluation-popup]'."
  (build-farm-popup-variable-value 'build-farm-number-of-evaluations))

(defun build-farm-popup-latest-evaluations ()
  "Display `build-farm-number-of-evaluations' of latest evaluations."
  (interactive)
  (build-farm-latest-evaluations
   (or build-farm-number-of-evaluations
       (build-farm-evaluation-read-number))))

(provide 'build-farm-popup)

;;; build-farm-popup.el ends here
