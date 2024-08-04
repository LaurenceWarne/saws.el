;;; saws-logs.el --- saws logs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Laurence Warne

;; Author: Laurence Warne

;; Local variables:
;; package-lint-main-file: "saws.el"
;; end:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Utilities for interacting with AWS Secrets.

;;; Code:

(require 'dash)
(require 'transient)
(require 's)
(require 'saws-core)

(transient-define-prefix saws-secrets ()
  "Transient for interacting with secrets."

  ["Actions"
   ("c" "Copy secret value" saws-secrets-copy-value)
   ("o" "Open aws console" ignore)])

(defun saws-secrets-get-secret (secret-name)
  "Return the secret value for SECRET-NAME."
  (string-trim
   (saws-aws-command (format "secretsmanager get-secret-value --query='SecretString' --secret-id=%s" secret-name))
   "[\"\n]+"
   "[\"\n]+"))

(defun saws-secrets-get-secret-names ()
  "Return a list of secret names."
  (append (saws-aws-command-to-json "secretsmanager list-secrets --query='SecretList[*]' --no-paginate") nil))

(defun saws--secrets-secrets-annotate (secret max-secret-name-len)
  "Annotate SECRET given preview given MAX-SECRET-NAME-LEN."
  (let ((fill-length (- max-secret-name-len (length (alist-get 'Name secret)))))
    (format "%s  %s"
            (s-repeat fill-length " ")
            (car (s-split "T" (alist-get 'CreatedDate secret))))))

;;;###autoload
(defun saws-secrets-copy-value (secret-name)
  "Copy the secret SECRET-NAME to the kill ring."
  (interactive
   (list
    (let* ((secrets (saws-secrets-get-secret-names))
           (secrets-alist (--map (cons (alist-get 'Name it) it) secrets))
           (mx-secret-name-len (-max (--map (length (alist-get 'Name it)) secrets)))
           (completion-extra-properties
            '(:annotation-function
              (lambda (k) (saws--secrets-secrets-annotate (alist-get k secrets-alist nil nil #'string=)
                                                          mx-secret-name-len)))))
      (completing-read "Secret name: " (-map #'car secrets-alist)))))
  (kill-new (saws-secrets-get-secret secret-name))
  (message "Copied '%s' to the kill ring" secret-name))

(provide 'saws-secrets)
;;; saws-secrets.el ends here
