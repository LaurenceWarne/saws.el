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

(defface saws-logs-log-group-headerline
  '((t (:foreground "SlateBlue" :bold t :underline t)))
  "Face used in log buffer headerlines for \"Log Group\".")

(defface saws-logs-log-group-value-headerline
  '((t (:foreground "purple")))
  "Face used in log buffer headerlines for the log group value.")

(defface saws-logs-since-headerline
  '((t (:foreground "SlateBlue" :bold t :underline t)))
  "Face used in log buffer headerlines for \"Since\".")

(defface saws-logs-since-value-headerline
  '((t (:foreground "purple")))
  "Face used in log buffer headerlines for the value of since.")

(defconst saws-logs-time-strings '("1m" "5m" "1h" "2h" "1d"))

(defvar-local saws--log-group-name nil)

(transient-define-prefix saws-logs (&optional args)
  "Transient for interacting with logs."
  ["Context"
   (saws--aws-region-infix)
   (saws--aws-profile-infix)]

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
    (define-key map "Q" #'saws-logs-cloudwatch-query-dwim)
    (define-key map "t" #'saws-logs-change-time-period)
    (define-key map "q" #'kill-this-buffer)
    (define-key map "k" #'kill-this-buffer)
    (define-key map "?" #'saws-logs-mode-menu)
    map))

(define-derived-mode saws-logs-output-mode saws-command-output-mode "Saws-Logs-Output"
  "Major mode for AWS logs output."
  :group 'saws)

(defun saws-logs-log-group-names ()
  "Return a list of all log group names."
  (--map (alist-get 'logGroupName it)
         (append (cdar (saws-aws-command-to-json "logs describe-log-groups")))))


;; https://repost.aws/questions/QUkdGEQP7rQZmDBUaB2Ai2Qg/aws-cloudwatch-log-insights-generate-url
(defun get-cloudwatch-insights-url (log-group-name)
  "Generate an AWS CloudWatch Insights URL for the given log group name."
  (let ((query-url (format "https://console.aws.amazon.com/cloudwatch/home?region=%s#logsV2:logs-insights/query" saws-region))
        (encoded-log-group (url-hexify-string log-group-name)))
    (setq encoded-log-group (replace-regexp-in-string "%" "$" encoded-log-group))
    (format "%s?queryDetail=%s" query-url encoded-log-group)))

(defun saws-logs--read-time-period ()
  "Read a time period compatible with aws logs tail --since."
  (completing-read "Since: "
                   (append saws-logs-time-strings
                           (and (member saws-logs-since saws-logs-time-strings)
                                (list saws-logs-since)))
                   nil
                   nil
                   saws-logs-since))

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
                      saws--region
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
(defun saws-logs-change-time-period (since)
  "Change the time period in the current saws-logs buffer to SINCE."
  (interactive (list (saws-logs--read-time-period)) saws-logs-output-mode)
  (let ((log-group saws--log-group-name)
        (display-buffer-overriding-action '(display-buffer-same-window . nil))
        ;; Don't ask for confirmation
        (kill-buffer-query-functions nil))
    (kill-buffer)
    (saws-logs-open-log-group log-group since)))

;;;###autoload
(defun saws-logs-open-log-group (log-group-name since)
  "Open the log group with LOG-GROUP-NAME from SINCE."
  (interactive
   (list
    ;; TODO figure out how to show last written to on another completing read column
    (completing-read "Log group name: " (saws-logs-log-group-names))
    (saws-logs--read-time-period)))
  ;; https://awscli.amazonaws.com/v2/documentation/api/latest/reference/logs/tail.html
  (let* ((buf (saws-async-aws-process
               log-group-name
               "logs"
               (list "tail"
                     log-group-name
                     "--since" since
                     "--color" "on"
                     "--format" "short"
                     "--follow"
                     "--no-paginate")
               #'saws-logs-output-mode)))
    (with-current-buffer buf
      (setq-local saws--log-group-name log-group-name)
      (setq header-line-format
            `("" header-line-indent ,(format "%s: %s %s: %s"
                                             (propertize "Log Group"
                                                         'font-lock-face
                                                         'saws-logs-log-group-headerline)
                                             (propertize log-group-name
                                                         'font-lock-face
                                                         'saws-logs-log-group-value-headerline)
                                             (propertize "Since"
                                                         'font-lock-face
                                                         'saws-logs-since-headerline)
                                             (propertize since
                                                         'font-lock-face
                                                         'saws-logs-since-value-headerline)))))
    (display-buffer buf '(display-buffer-reuse-window . nil))))

(provide 'saws-logs)
;;; saws-logs.el ends here
