;;; build-farm-url.el --- Build farm URLs  -*- lexical-binding: t -*-

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

;; This file provides the code to determine various URLs of the build
;; farms and to receive data from them.

;;; Code:

(require 'url-handlers)
(require 'json)
(require 'build-farm-utils)

(defvar build-farm-url-alist
  '(("https://hydra.nixos.org" . hydra)
    ("https://hydra.gnu.org" . hydra)
    ("https://berlin.guixsd.org" . cuirass))
  "Alist of URLs and their types of the available build farms.")

(defun build-farm-guess-url ()
  "Return URL of a build farm that a user probably wants to use."
  (if (eq 'guix build-farm-preferred-package-manager)
      "https://hydra.gnu.org"
    "https://hydra.nixos.org"))

(defun build-farm-urls ()
  "Return a list of available build farm URLs."
  (mapcar #'car build-farm-url-alist))

(defcustom build-farm-url (build-farm-guess-url)
  "URL of the default build farm."
  :type `(choice ,@(mapcar (lambda (url) (list 'const url))
                           (build-farm-urls))
                 (string :tag "Other URL"))
  :group 'build-farm)

(defun build-farm-read-url ()
  "Read from minibuffer and return build farm URL."
  (completing-read "Build farm URL: "
                   (build-farm-urls)
                   nil nil nil nil
                   build-farm-url))

;;;###autoload
(defun build-farm-set-url (url)
  "Set `build-farm-url' to URL.
Interactively, prompt for URL"
  (interactive (list (build-farm-read-url)))
  (setq build-farm-url url))

(defun build-farm-url-type (&optional url)
  "Return build farm type by its URL (`build-farm-url' by default)."
  (or (bui-assoc-value build-farm-url-alist
                       (or url build-farm-url))
      (let ((type (if (string-match-p "cuirass" url)
                      'cuirass
                    'hydra)))
        (message "Unknown URL: <%s>.
Consider adding it to `build-farm-url-alist'.
Arbitrarily choosing `%S' type for this URL."
                 url type)
        type)))

(defun build-farm-url (&rest url-parts)
  "Return build farm URL using URL-PARTS.
URL-PARTS are added to `build-farm-url'."
  (apply #'concat build-farm-url "/" url-parts))

(defun build-farm-api-url (type args)
  "Return URL for receiving data using build farm API.
TYPE is the name of an allowed method.
ARGS is alist of (KEY . VALUE) pairs.
Skip ARG, if VALUE is nil or an empty string."
  (declare (indent 1))
  (let* ((fields (mapcar
                  (lambda (arg)
                    (pcase arg
                      (`(,key . ,value)
                       (unless (or (null value)
                                   (equal "" value))
                         (concat (build-farm-hexify key) "="
                                 (build-farm-hexify value))))
                      (_ (error "Wrong argument '%s'" arg))))
                  args))
         (fields (mapconcat #'identity (delq nil fields) "&")))
    (build-farm-url "api/" type "?" fields)))

(defun build-farm-build-url (id)
  "Return URL of a build ID."
  (build-farm-url "build/" (number-to-string id)))

(defun build-farm-build-log-url (id)
  "Return URL of the log file of a build ID."
  (concat (build-farm-build-url id) "/log/raw"))

(cl-defun build-farm-build-latest-api-url
    (number &key project jobset job system)
  "Return API URL to receive latest NUMBER of builds."
  (build-farm-api-url "latestbuilds"
    `(("nr" . ,number)
      ("project" . ,project)
      ("jobset" . ,jobset)
      ("job" . ,job)
      ("system" . ,system))))

(defun build-farm-build-queue-api-url (number)
  "Return API URL to receive the NUMBER of queued builds."
  (build-farm-api-url "queue"
    `(("nr" . ,number))))

(defun build-farm-jobset-url (project jobset)
  "Return URL of a PROJECT's JOBSET."
  (build-farm-url "jobset/" project "/" jobset))

(defun build-farm-jobset-api-url (project)
  "Return API URL for jobsets by PROJECT."
  (build-farm-api-url "jobsets"
    `(("project" . ,project))))


;;; Receiving data from a build farm

(defvar url-http-codes)

(defun build-farm-retrieve-url (url)
  "Retrieve URL synchronously and return buffer containing the data.
This function is similar to `url-retrieve-synchronously' but it
also raises an error if URL has not been retrieved properly."
  ;; This code is taken from `url-insert-file-contents'.
  (let ((buffer (url-retrieve-synchronously url)))
    (unless buffer
      (signal 'file-error (list url "No Data")))
    (with-current-buffer buffer
      (when (bound-and-true-p url-http-response-status)
        (unless (and (>= url-http-response-status 200)
                     (< url-http-response-status 300))
          (let ((desc (nth 2 (assq url-http-response-status
                                   url-http-codes))))
            (kill-buffer buffer)
            (signal 'file-error (list url desc))))))
    buffer))

(defun build-farm-receive-data (url)
  "Return output received from URL and processed with `json-read'."
  (let* ((url-request-extra-headers '(("Accept" . "application/json")))
         (url-buffer (build-farm-retrieve-url url))
         (content-type (buffer-local-value 'url-http-content-type
                                           url-buffer)))
    ;; Do not use `string=' here because the content type may look like
    ;; this: "application/json;charset=utf-8".
    (unless (string-match-p "application/json" content-type)
      ;; Currently Cuirass does not support "Accept" extra header, so it
      ;; does not return json data from "non-api" URLs.
      (if (eq (build-farm-url-type) 'cuirass)
          (error "Sorry, Cuirass does not support this API")
          (error "\
The server has not returned 'application/json' content type.
Perhaps, API has changed:\n%s"
             url)))
    (with-temp-buffer
      (url-insert-buffer-contents url-buffer url)
      (goto-char (point-min))
      (let ((json-key-type 'symbol)
            (json-array-type 'list)
            (json-object-type 'alist))
        (json-read)))))

(provide 'build-farm-url)

;;; build-farm-url.el ends here
