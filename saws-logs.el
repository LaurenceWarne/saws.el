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
(require 'saws-core)

(defcustom saws-logs-since "1h"
  "By default, from what time to begin displaying logs.

The value provided can be an ISO 8601 timestamp or a relative time."
  :group 'saws
  :type 'string)

(defvar-local saws--log-group-name nil)

(transient-define-prefix saws-logs ()
  "Transient for interacting with logs."
  ["Actions"
   ("l" "Open log group" saws-logs-open-log-group)
   ("o" "Open aws console" saws-logs-open-console)
   ("O" "Open documentation for resource" ignore)])

(transient-define-prefix saws-logs-mode-menu ()
  "Transient for interacting with logs."
  ["AWS Console"
   ("o" "Open log group on the AWS console" saws-logs-open-console)
   ("q" "Run a Cloudwatch query" saws-logs-cloudwatch-query-dwim)]
  ["Change Parameters"
   ("t" "Change the time period" saws-logs-change-time-period)])

(defvar saws-logs-output-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "o" #'saws-logs-open-console)
    (define-key map "q" #'saws-logs-cloudwatch-query)
    (define-key map "t" #'saws-logs-change-time-period)
    (define-key map "?" #'saws-logs-mode-menu)
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
  "Open the log group with LOG-GROUP-NAME in the AWS console.

If LOG-GROUP-NAME is nil, fallback to `saws--log-group-name', if that is nil
open the cloudwatch console."
  ;; Note `read-extended-command-predicate' has to be set for the second arg to
  ;; `interactive' to matter
  (interactive (list saws--log-group-name) saws-logs-output-mode)
  (message "Opening '%s'" log-group-name)
  (browse-url (format "https://%s.console.aws.amazon.com/cloudwatch/?region=%1$s#logsV2:log-groups/log-group/%s"
                      saws-region
                      ;; https://stackoverflow.com/questions/60796991/is-there-a-way-to-generate-the-aws-console-urls-for-cloudwatch-log-group-filters
                      (s-replace "/" "$252F" log-group-name))))

;;;###autoload
(defun saws-logs-cloudwatch-query-dwim (&optional log-group-name query-string)
  "Open the cloudwatch console for LOG-GROUP-NAME.

If QUERY-STRING is specified, preset the query to filter on it."
  (interactive
   "query: "
   (list saws--log-group-name (when (region-active-p) (buffer-substring-no-properties
                                                       (region-beginning) (region-end))))
   saws-logs-output-mode)
  (when query-string (message "Using preset filter sting '%s'" query-string))
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
    (with-current-buffer buf (setq-local saws--log-group-name log-group-name))
    (display-buffer buf '(display-buffer-reuse-window . nil))))

(provide 'saws-logs)
;;; saws-logs.el ends here
