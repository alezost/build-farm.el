;;; build-farm-utils.el --- General utility functions  -*- lexical-binding: t -*-

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

;; This file provides auxiliary general code for build-farm package.

;;; Code:

(require 'bui)

(defun build-farm-hexify (value)
  "Convert VALUE to string and hexify it."
  (url-hexify-string (bui-get-string value)))

(defun build-farm-number->bool (number)
  "Convert NUMBER to boolean value.
Return nil, if NUMBER is 0; return t otherwise."
  (/= 0 number))

(defmacro build-farm-while-null (&rest body)
  "Evaluate BODY until its result becomes non-nil."
  (declare (indent 0) (debug t))
  (let ((result-var (make-symbol "result")))
    `(let (,result-var)
       (while (null ,result-var)
         (setq ,result-var ,@body))
       ,result-var)))

(defun build-farm-modify (object &rest modifiers)
  "Apply MODIFIERS to OBJECT.
OBJECT is passed as an argument to the first function from
MODIFIERS list, the returned result is passed to the second
function from the list and so on.  Return result of the last
modifier call."
  (if (null modifiers)
      object
    (apply #'build-farm-modify
           (funcall (car modifiers) object)
           (cdr modifiers))))

(defun build-farm-modify-objects (objects &rest modifiers)
  "Apply MODIFIERS to each object from a list of OBJECTS.
See `build-farm-modify' for details."
  (mapcar (lambda (object)
            (apply #'build-farm-modify object modifiers))
          objects))


;;; Completing readers

(defun build-farm-completing-read (prompt table &optional predicate
                                          require-match initial-input
                                          hist def inherit-input-method)
  "Same as `completing-read' but return nil instead of an empty string."
  (let ((res (completing-read prompt table predicate
                              require-match initial-input
                              hist def inherit-input-method)))
    (unless (string= "" res) res)))

(defmacro build-farm-define-reader (name read-fun completions prompt
                                         &optional require-match default)
  "Define NAME function to read from minibuffer.
READ-FUN may be `completing-read', `completing-read-multiple' or
another function that takes COMPLETIONS, PROMPT, REQUIRE-MATCH, and
DEFAULT."
  (declare (indent 1))
  `(defun ,name (&optional prompt initial-contents)
     (,read-fun (or prompt ,prompt)
                ,completions nil ,require-match
                initial-contents nil ,default)))

(defmacro build-farm-define-readers (&rest args)
  "Define reader functions.

ARGS should have a form [KEYWORD VALUE] ...  The following
keywords are available:

  - `completions-var' - variable used to get completions.

  - `completions-getter' - function used to get completions.

  - `require-match' - if the match is required (see
    `completing-read' for details); default is t.

  - `default' - default value.

  - `single-reader', `single-prompt' - name of a function to read
    a single value, and a prompt for it.

  - `multiple-reader', `multiple-prompt' - name of a function to
    read multiple values, and a prompt for it.

  - `multiple-separator' - if specified, another
    `<multiple-reader-name>-string' function returning a string
    of multiple values separated the specified separator will be
    defined."
  (bui-plist-let args
      ((completions-var    :completions-var)
       (completions-getter :completions-getter)
       (require-match      :require-match t)
       (default            :default)
       (single-reader      :single-reader)
       (single-prompt      :single-prompt)
       (multiple-reader    :multiple-reader)
       (multiple-prompt    :multiple-prompt)
       (multiple-separator :multiple-separator))
    (let ((completions
           (cond ((and completions-var completions-getter)
                  `(or ,completions-var
                       (setq ,completions-var
                             (funcall ',completions-getter))))
                 (completions-var
                  completions-var)
                 (completions-getter
                  `(funcall ',completions-getter)))))
      `(progn
         ,(when (and completions-var
                     (not (boundp completions-var)))
            `(defvar ,completions-var nil))

         ,(when single-reader
            `(build-farm-define-reader ,single-reader
               build-farm-completing-read ,completions ,single-prompt
               ,require-match ,default))

         ,(when multiple-reader
            `(build-farm-define-reader ,multiple-reader
               completing-read-multiple ,completions ,multiple-prompt
               ,require-match ,default))

         ,(when (and multiple-reader multiple-separator)
            (let ((name (intern (concat (symbol-name multiple-reader)
                                        "-string"))))
              `(defun ,name (&optional prompt initial-contents)
                 (mapconcat #'identity
                            (,multiple-reader prompt initial-contents)
                            ,multiple-separator))))))))


(provide 'build-farm-utils)

;;; build-farm-utils.el ends here
