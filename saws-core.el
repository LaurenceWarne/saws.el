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

(require 'comint)
(require 'dash)
(require 'json)
(require 's)

(defgroup saws nil
  "AWS convenience tools."
  :group 'applications)

(defcustom saws-region "us-east-1"
  "The AWS region to use for commands."
  :group 'saws
  :type 'string)

(defcustom saws-profile "default"
  "The AWS profile to use for commands."
  :group 'saws
  :type 'string)

(defcustom saws-echo-commands nil
  "If non-nil echo any aws commands used to the minibuffer."
  :group 'saws
  :type 'string)

(defcustom saws-sso-autologin t
  "If non-nil, attempt to re-authenticate if SSO credentials are expired.

Note this requires the `password-store' package."
  :group 'saws
  :type 'boolean)

(defcustom saws-sso-pass-entry nil
  "If non-nil, copy this entry to the kill ring prior to attempting to login with SSO."
  :group 'saws
  :type 'string)

(defun saws-aws-command (command &rest args)
  "Run the aws command COMMAND with ARGS."
  (let ((cmd (format "aws %s %s --profile %s --region %s"
                     command
                     (s-join " " args)
                     saws-profile
                     saws-region)))
    (when saws-echo-commands (message cmd))
    (let ((result (shell-command-to-string cmd)))
      (if (and saws-sso-autologin
               (s-contains-p "Error when retrieving token from sso: Token has expired and refresh failed"
                             result t))
          (progn (message "Logging in with aws sso...")
                 (when saws-sso-pass-entry
                   (if (require 'password-store nil t)
                       (progn (password-store-copy saws-sso-pass-entry)
                              (message "Copied password entry %s to kill ring" saws-sso-pass-entry))
                     (user-error "saws-sso-pass-entry is set, but password-store is not installed")))
                 (saws-sso-login saws-profile)
                 (shell-command-to-string cmd))
        result))))

(defun saws-sso-login (profile)
  "Retrieve and cache an AWS SSO access token for profile PROFILE."
  (shell-command-to-string (format "aws sso login --no-paginate --profile %s" profile)))

(defun saws-aws-command-to-json (command)
  "Run the aws command COMMAND and read into a json object."
  (let ((command-output (saws-aws-command command "--output" "json")))
    (condition-case err
        (json-read-from-string command-output)
      (error (error
              "Encountered error '%s' whilst running command '%s', output: '%s'"
              err
              command
              command-output)))))

(defun saws--get-regions ()
  "Return a list of all user profiles."
  (append (saws-aws-command-to-json "account list-regions --query='Regions[*].RegionName' --no-paginate") nil))

(defcustom saws-regions (or (ignore-errors (saws--get-regions)) '("us-east-1" "eu-west-1" "eu-west-2"))
  "Regions which will be available for selection in transients."
  :group 'saws
  :type 'string)

(defun saws--get-profiles ()
  "Return a list of all user profiles."
  (split-string (shell-command-to-string "aws configure list-profiles")))

(defcustom saws-profiles (saws--get-profiles)
  "Profiles which will be available for selection in transients."
  :group 'saws
  :type '(repeat string))

(defconst saws--region-key "r")
(defconst saws--profile-key "p")

(define-derived-mode saws-command-output-mode comint-mode "Saws-Command"
  "Major mode for AWS cli command output."
  :group 'saws
  (ansi-color-for-comint-mode-on)
  (comint-mode)
  (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter)
  (buffer-disable-undo)
  (setq buffer-read-only t)
  ;; https://emacs.stackexchange.com/questions/42152/enable-dir-locals-el-variables-inside-just-opened-buffer
  (hack-dir-local-variables-non-file-buffer))

(defun saws-async-aws-process (name command args &optional mode)
  "Run the aws command COMMAND with ARGS, and return the process buffer.

The buffer and process will have name NAME.  If MODE is non-nil set the output
buffer mode to MODE, else `saws-command-output-mode' will be used as the major
mode."
  (let* ((name (format "AWS %s for '%s'" command name))
         (buf (generate-new-buffer name)))
    (apply #'start-process
           name
           buf
           "aws"
           command
           (append args
                   (list "--region" saws-region
                         "--profile" saws-profile)))
    (with-current-buffer buf (if mode (funcall mode) (saws-command-output-mode)))
    buf))

(provide 'saws-core)
;;; saws-core.el ends here
