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
(require 'saws-secrets)
(require 'transient)

(cl-defun saws--console-base-url (region service &key (include-home t))
  "Return console base url for REGION and SERVICE.

If INCLUDE-HOME is non-nil, include \"/home\" in the url."
  (format "https://%s.console.aws.amazon.com/%s%s?region=%1$s"
          region
          service
          (if include-home "/home" "")))

(defclass saws--transient-variable (transient-variable)
  ((scope :initarg :scope)))

;; Copied from https://github.com/alphapapa/org-ql/blob/b7d4856f926cb71e01427a940dc948a48b0a702d/org-ql-view.el#L724
;; no clue why this isn't done OOTB by transient
(cl-defmethod transient-infix-set ((obj saws--transient-variable) value)
  "Use VALUE to set the value of OBJ and the variable value."
  (let ((variable (oref obj variable)))
    (oset obj value value)
    (set (make-local-variable (oref obj variable)) value)
    (unless (or value transient--prefix)
      (message "Unset %s" variable))))

;; https://github.com/akirak/emacs-config/blob/e18447bb19059c63cb26188f393718aa7da76f7f/emacs/lisp/akirak-transient.el#L21
(cl-defmethod transient-format-value ((obj saws--transient-variable))
  "Format the value of OBJ using the variable value."
  (let ((value (oref obj value)))
    (concat
     (propertize "(" 'face 'transient-inactive-value)
     (propertize (format "%s" value) 'face 'transient-value)
     (propertize ")" 'face 'transient-inactive-value))))

(transient-define-infix saws--aws-region-infix ()
  :class 'saws--transient-variable
  :choices saws-regions
  :argument ""
  :key saws--region-key
  :description "AWS Region"
  :init-value (lambda (obj) (oset obj value saws-region))
  :always-read t
  :allow-empty nil
  :variable 'saws-region)

(transient-define-infix saws--aws-profile-infix ()
  :class 'saws--transient-variable
  :choices saws-profiles
  :argument ""
  :key saws--profile-key
  :description "AWS Profile"
  :init-value (lambda (obj) (oset obj value saws-profile))
  :always-read t
  :allow-empty nil
  :variable 'saws-profile)

;;;###autoload (autoload 'saws "saws" nil t)
(transient-define-prefix saws ()
  "Transient for everything saws."
  ["Context"
   (saws--aws-region-infix)
   (saws--aws-profile-infix)]

  [["Logs"
    ("l" "Logs" saws-logs)
    ("L" "Console" saws-console-open-logs)]
   ["Cloudformation"
    ("c" "Cloudformation" saws-cloudformation)
    ("C" "Console" saws-console-open-cloudformation)]
   ["IAM"
    ("i" "IAM" ignore)
    ("I" "Console" saws-console-open-iam)]
   ["Health"
    ("h" "health" ignore)
    ("H" "Console" saws-console-open-health)]]

  [["Lambda"
    ("C-l" "Lambda" ignore)
    ("M-l" "Console" saws-console-open-lambda)]
   ["ECS"
    ("e" "ECS" ignore)
    ("E" "Console" saws-console-open-ecs)]
   ["EC2"
    ("C-e" "EC2" ignore)
    ("M-e" "Console" saws-console-open-ec2)]
   ["Cloudfront"
    ("f" "Cloudfront" ignore)
    ("F" "Console" saws-console-open-cloudfront)]]
  
  [["S3"
    ("s" "s3" ignore)
    ("S" "Console" saws-console-open-s3)]
   ["RDS"
    ("d" "RDS" ignore)
    ("D" "Console" saws-console-open-rds)]
   ["Secrets"
    ("#" "Secrets" saws-secrets)
    ("~" "Console" saws-console-open-secrets)]])

(defun saws-console-open-logs ()
  "Open logs in the AWS Console."
  (interactive)
  (saws-console-open 'logs))

(defun saws-console-open-cloudformation ()
  "Open the Cloudformation stacks page in the AWS Console."
  (interactive)
  (saws-console-open 'cloudformation))

(defun saws-console-open-iam ()
  "Open the IAM home page in the AWS Console."
  (interactive)
  (saws-console-open 'iam))

(defun saws-console-open-health ()
  "Open the AWS Health homepage in the AWS Console."
  (interactive)
  (saws-console-open 'health))

(defun saws-console-open-lambda ()
  "Open the Lambda functions page in the AWS Console."
  (interactive)
  (saws-console-open 'lambda))

(defun saws-console-open-ecs ()
  "Open the ECS clusters page in the AWS Console."
  (interactive)
  (saws-console-open 'ecs))

(defun saws-console-open-rds ()
  "Open the RDS databases page in the AWS Console."
  (interactive)
  (saws-console-open 'rds))

(defun saws-console-open-ec2 ()
  "Open the EC2 homepage in the AWS Console."
  (interactive)
  (saws-console-open 'ec2))

(defun saws-console-open-s3 ()
  "Open the S3 homepage in the AWS Console."
  (interactive)
  (saws-console-open 's3))

(defun saws-console-open-cloudfront ()
  "Open the Cloudfront distributions page in the AWS Console."
  (interactive)
  (saws-console-open 'cloudfront))

(defun saws-console-open-secrets ()
  "Open the AWS Secrets page in the AWS Console."
  (interactive)
  (saws-console-open 'secrets))

(defun saws-console-open (service)
  "Open SERVICE in the AWS Console."
  (interactive)
  (browse-url
   (cond
    ((eq service 'logs)
     (format "%s#logsV2:log-groups" (saws--console-base-url saws-region "cloudwatch")))
    ((eq service 'cloudformation)
     (format "%s#stacks" (saws--console-base-url saws-region "cloudformation")))
    ((eq service 'lambda)
     (format "%s#functions" (saws--console-base-url saws-region "lambda")))
    ((eq service 'ecs)
     (saws--console-base-url saws-region "ecs/v2/clusters" :include-home nil))
    ((eq service 'iam)
     (saws--console-base-url saws-region "iamv2"))
    ((eq service 's3)
     (saws--console-base-url saws-region "s3"))
    ((eq service 'rds)
     (format "%s#databases:" (saws--console-base-url saws-region "rds")))
    ((eq service 'ec2)
     (saws--console-base-url saws-region "ec2"))
    ((eq service 'health)
     "https://health.aws.amazon.com/health/home#/account/dashboard/open-issues")
    ((eq service 'secrets)
     (saws--console-base-url saws-region "secrets"))
    (t (user-error "Service '%s' unknown or not supported" service)))))

(provide 'saws)

;;; saws.el ends here
