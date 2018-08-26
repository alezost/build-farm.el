;;; build-farm-evaluation.el --- Interface for evaluations  -*- lexical-binding: t -*-

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

;; This file provides an interface for displaying jobset evaluations of
;; a build farm in 'list' and 'info' buffers.

;;; Code:

(require 'bui)
(require 'build-farm)
(require 'build-farm-url)
(require 'build-farm-build)

(defgroup build-farm-evaluation nil
  "Interface for Hydra and Cuirass evaluations."
  :group 'build-farm)

(defcustom build-farm-number-of-evaluations 32
  "Default number of latest evaluations to display.
This variable is used by '\\[build-farm-latest-evaluations]'
command.  If nil, always prompt for the number of evaluations."
  :type 'integer
  :group 'build-farm-evaluation)

(defun build-farm-set-number-of-evaluations (number)
  "Set `build-farm-number-of-evaluations' to NUMBER."
  (interactive (list (build-farm-evaluation-read-number)))
  (setq build-farm-number-of-evaluations number))

(defun build-farm-evaluation-read-number (&optional prompt)
  "Read from minibuffer (using PROMPT) a number of evaluations."
  (read-number (or prompt "Number of evaluations: ")
               build-farm-number-of-evaluations))

(defun build-farm-evaluation-info-insert-id (id)
  "Insert title for evaluation ID at point."
  (bui-format-insert
   (concat "Evaluation " (number-to-string id))
   'bui-info-heading))

(defun build-farm-evaluation-info-insert-url (entry)
  "Insert URL for the evaluation ENTRY at point."
  (bui-insert-button (build-farm-evaluation-url
                      :root-url (build-farm-current-url)
                      :evaluation (bui-entry-id entry))
                     'bui-url)
  (bui-newline))


;;; Cuirass common

(build-farm-define-entry-type cuirass-evaluation
  :search-types '((latest . build-farm-latest-evaluations-api-url))
  :filter-names '((specification . jobset)))


;;; Cuirass Evaluation 'info'

(build-farm-define-interface cuirass-evaluation info
  :mode-name "Cuirass-Evaluation-Info"
  :buffer-name "*Farm Evaluation Info*"
  :format '((id nil (build-farm-evaluation-info-insert-id))
            nil
            build-farm-evaluation-info-insert-url
            nil
            (jobset format (build-farm-cuirass-evaluation-info-insert-jobset))
            (commits format (format))))

(defun build-farm-cuirass-evaluation-info-insert-jobset (jobset)
  "Insert info about Cuirass JOBSET at point."
  (build-farm-info-insert-cuirass-jobset jobset)
  (bui-insert-indent)
  (build-farm-info-insert-builds-button
   :jobset jobset))


;;; Cuirass Evaluation 'list'

(build-farm-define-interface cuirass-evaluation list
  :describe-function 'build-farm-list-describe
  :mode-name "Cuirass-Evaluation-List"
  :buffer-name "*Farm Evaluations*"
  :format '((id nil 10 bui-list-sort-numerically-0)
            (jobset nil 30 t)
            (commits nil 30 t)))


;;; Interactive commands

;;;###autoload
(defun build-farm-latest-evaluations (number)
  "Display latest NUMBER of evaluations.
Interactively, use `build-farm-number-of-builds' variable for
NUMBER.  With prefix argument, prompt for it."
  (interactive (list (build-farm-evaluation-read-number)))
  (if (eq 'hydra (build-farm-url-type))
      (error "Hydra API does not support latest evaluations")
    (build-farm-get-display build-farm-url
                            'cuirass-evaluation 'latest number)))

(provide 'build-farm-evaluation)

;;; build-farm-evaluation.el ends here
