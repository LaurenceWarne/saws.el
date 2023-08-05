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

(transient-define-prefix saws-logs ()
  "Transient for interacting with logs."
  ["Actions"
   ("l" "open log group" saws-open-log-group)
   ("o" "open aws console" saws-logs-open-console)
   ("O" "open documentation for resource" ignore)])

(defun saws-log-group-names ()
  "Return a list of all log group names."
  (--map (alist-get 'logGroupName it)
         (append (cdar (saws-aws-command-to-json "logs describe-log-groups")))))

(defun saws-open-log-group ()
  "Open a log group."
  (interactive)
  (let* ((log-group-name (completing-read "Log group name: " (saws-log-group-names)))
         (name (format "AWS logs for '%s'" log-group-name))
         (buf (generate-new-buffer name))
         ;; https://awscli.amazonaws.com/v2/documentation/api/latest/reference/logs/tail.html
         (proc (start-process name
                              buf
                              "aws"
                              "logs"
                              "tail"
                              log-group-name
                              "--since" "1d"
                              "--color" "on"
                              "--format" "short"
                              "--follow"
                              "--region" saws-region
                              "--profile" saws-profile)))
    (with-current-buffer buf
      (ansi-color-for-comint-mode-on)
      (comint-mode)
      (set-process-filter proc 'comint-output-filter))
    (display-buffer buf '(display-buffer-reuse-window . nil))))

(provide 'saws-logs)
;;; saws-logs.el ends here
