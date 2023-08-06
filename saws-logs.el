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

;; Log utilities.

;;; Code:

(require 'ansi-color)
(require 'transient)
(require 'dash)
(require 'saws)

(defcustom saws-logs-since "1h"
  "By default, from what time to begin displaying logs.

The value provided can be an ISO 8601 timestamp or a relative time."
  :group 'saws
  :type 'string)

(defvar-local saws--log-group-name nil)

(transient-define-prefix saws-logs ()
  "Transient for interacting with logs."
  ["Actions"
   ("l" "open log group" saws-logs-open-log-group)
   ("o" "open aws console" saws-logs-open-console)
   ("O" "open documentation for resource" ignore)])

(defvar saws-logs-output-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "o" #'saws-logs-open-console)
    (define-key map "q" #'saws-logs-cloudwatch-query)
    (define-key map "t" #'saws-logs-change-time-period)
    map))

(define-derived-mode saws-logs-output-mode saws-command-output-mode "Saws-Logs-Output"
  "Major mode for AWS logs output."
  :group 'saws)

(defun saws-logs-log-group-names ()
  "Return a list of all log group names."
  (--map (alist-get 'logGroupName it)
         (append (cdar (saws-aws-command-to-json "logs describe-log-groups")))))

;;;###autoload
(defun saws-logs-open-console (&optional log-group-name)
  "Open the log group with LOG-GROUP-NAME in the AWS console."
  ;; Note `read-extended-command-predicate' has to be set for the second arg to
  ;; `interactive' to matter
  (interactive nil saws-logs-output-mode)
  (browse-url "https://console.aws.amazon.com/cloudwatch"))

;;;###autoload
(defun saws-logs-cloudwatch-query (&optional log-group-name query-string)
  "Open the cloudwatch console for LOG-GROUP-NAME"
  (interactive "query: " saws-logs-output-mode)
  (browse-url "https://console.aws.amazon.com/cloudwatch"))

;;;###autoload
(defun saws-logs-change-time-period (&optional period)
  (interactive "period: " saws-logs-output-mode))

;;;###autoload
(defun saws-logs-open-log-group (log-group-name)
  "Open the log group with LOG-GROUP-NAME."
  (interactive
   (list
    ;; TODO figure out how to show last written to on another completing read column
    (completing-read "Log group name: " (saws-logs-log-group-names))))
  ;; https://awscli.amazonaws.com/v2/documentation/api/latest/reference/logs/tail.html
  (let* ((buf (saws-async-aws-process
               log-group-name
               "logs"
               (list "tail"
                     log-group-name
                     "--since" saws-logs-since
                     "--color" "on"
                     "--format" "short"
                     "--follow")
               #'saws-logs-output-mode)))
    (display-buffer buf '(display-buffer-reuse-window . nil))))

(provide 'saws-logs)
;;; saws-logs.el ends here
