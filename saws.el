;;; saws.el --- some aws management stuff -*- lexical-binding: t -*-

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Homepage: https://github.com/laurencewarne/saws.el
;; Package-Requires: ((emacs "27.0") (tablist "1.0") (transient "0.3.7") (dash "2.17.0"))

;;; Commentary:

;; some aws management stuff

;;; Code:

(require 'json)

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

(defun saws-aws-command (command)
  "Run the aws command COMMAND."
  (let ((cmd (format "aws %s --profile %s --region %s"
                     command
                     saws-profile
                     saws-region)))
    (shell-command-to-string cmd)))

(defun saws-async-aws-process (name command args)
  "Run the aws command COMMAND with ARGS, and return the process buffer.

The buffer and process will have NAME."
  (let* ((name (format "AWS %s for '%s'" command name))
         (buf (generate-new-buffer name))
         ;; https://awscli.amazonaws.com/v2/documentation/api/latest/reference/logs/tail.html
         (proc (apply #'start-process
                      name
                      buf
                      "aws"
                      command
                      (append args
                              (list"--region" saws-region
                                   "--profile" saws-profile)))))
    (with-current-buffer buf
      (ansi-color-for-comint-mode-on)
      (comint-mode)
      (set-process-filter proc 'comint-output-filter))
    buf))


(defun saws-aws-command-to-json (command)
  "Run the aws command COMMAND and read into a json object."
  (json-read-from-string (saws-aws-command command)))

(provide 'saws)

;;; saws.el ends here
