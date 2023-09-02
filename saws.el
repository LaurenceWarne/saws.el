;;; saws.el --- some aws management stuff -*- lexical-binding: t -*-

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Homepage: https://github.com/laurencewarne/saws.el
;; Package-Requires: ((emacs "28.0") (tablist "1.0") (transient "0.3.7") (dash "2.17.0"))

;;; Commentary:

;; some aws management stuff

;;; Code:

(require 'saws-cloudformation)
(require 'saws-logs)

(defun saws--console-base-url (region service)
  "Return console base url for REGION and SERVICE."
  (format "https://%s.console.aws.amazon.com/%s/home?region=%1$s"
          region
          service))

;;;###autoload (autoload 'saws "saws" nil t)
(transient-define-prefix saws ()
  "Transient for everything saws."
  ["Context"
   ("R" "Change Region" ignore)
   ("P" "Change Profile" ignore)]
  [["Logs"
    ("l" "Logs" saws-logs)
    ("L" "Console" saws-console-open-logs)]
   ["Cloudformation"
    ("c" "Cloudformation" saws-cloudformation)
    ("C" "Console" saws-console-open-cloudformation)]])

(defun saws-console-open-logs ()
  "Open logs in the AWS Console."
  (interactive)
  (saws-console-open 'logs))

(defun saws-console-open-cloudformation ()
  "Open the Cloudformation stacks page in the AWS Console."
  (interactive)
  (saws-console-open 'cloudformation))

(defun saws-console-open (service)
  "Open SERVICE in the AWS Console."
  (interactive)
  (browse-url
   (cond
    ((eq service 'logs)
     (format "%s#logsV2:log-groups" (saws--console-base-url saws-region "cloudwatch")))
    ((eq service 'cloudformation)
     (format "%s#stacks" (saws--console-base-url saws-region "cloudformation") ))
    (t (user-error "Service '%s' unknown or not supported" service)))))

(provide 'saws)

;;; saws.el ends here
