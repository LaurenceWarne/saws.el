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
  (let ((cmd (format "AWS_PROFILE=%s AWS_REGION=%s aws %s"
                     saws-profile
                     saws-region
                     command)))
    (shell-command-to-string cmd)))

(defun saws-aws-command-to-json (command)
  "Run the aws command COMMAND and read into a json object."
  (json-read-from-string (saws-aws-command command)))

(provide 'saws)

;;; saws.el ends here
