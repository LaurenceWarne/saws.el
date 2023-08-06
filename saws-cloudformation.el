;;; saws-cloudformation.el --- saws cloudformation -*- lexical-binding: t -*-

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

;; Cloudformation utilities.

;;; Code:

(require 'saws)

(defvar saws-stack-columns [("Name" 20 t) ("Status" 10 t)])

(transient-define-prefix saws-cloudformation ()
  "Transient for running cloudformation commands."
  ;;:man-page "aws cloudformation deploy help"
  ["Actions"
   ("d" "deploy" saws-deploy)
   ("l" "describe stacks" saws-describe-stacks)
   ("o" "open aws console" saws-deploy-open-console)
   ("O" "open documentation for resource" ignore)])

(transient-define-prefix saws-deploy ()
  "Transient for running cloudformation deploy commands."
  ;;:man-page "aws cloudformation deploy help"
  :value (lambda () `("--capabilities CAPABILITY_IAM"
                      ,(concat "--template-file "
                               (file-name-nondirectory (buffer-file-name)))))
  ["Required Arguments"
   ("s" "stack name" "--stack-name " read-string)
   ("t" "template file" "--template-file " read-string)]
  ["Optional Arguments"
   ("c" "capabilities" "--capabilities " read-string)
   ("n" "no execute changeset" "--no-execute-changeset")
   ("p" "parameters" "--parameter-overrides " read-string)]
  ["Actions"
   ("d" "deploy" saws-deploy-run)
   ("o" "open aws console" saws-deploy-open-console)])

(defvar saws-stack-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "D" #'saws-delete-stack)
    (define-key map "O" #'saws-deploy-open-console)
    (define-key map "q" #'kill-this-buffer)
    (define-key map "k" #'kill-this-buffer)
    map))

(define-derived-mode saws-stack-mode tabulated-list-mode "Stacks"
  "Major mode for handling a list of stacks."
  (setq tabulated-list-format saws-stack-columns)
  ;;(setq tabulated-list-sort-key nil)
  ;;(add-hook 'tabulated-list-revert-hook 'refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode)
  (use-local-map saws-stack-mode-map))

(defun saws-default-region ()
  "Get the user's default region."
  (string-trim-right (shell-command-to-string "aws configure get region")))

;;;###autoload
(defun saws-deploy-run (args)
  "Run \\='aws cloudformation deploy\\=' using ARGS."
  (interactive (list (transient-args transient-current-command)))
  (let ((cmd (concat "aws cloudformation deploy "
                     (mapconcat #'identity args " "))))
    (message "Running command: '%s'" cmd)
    (async-shell-command cmd)))

;;;###autoload
(defun saws-describe-stacks (args)
  "Run \\='aws cloudformation describe-stacks\\=' using ARGS."
  (interactive (list (transient-args transient-current-command)))
  (let* ((output (shell-command-to-string
                  "AWS_REGION=us-east-1 aws cloudformation describe-stacks"))
         (json (append (cdar (json-read-from-string output)) nil)))
    (pop-to-buffer (generate-new-buffer "*stacks*"))
    (saws-stack-mode)
    (setq tabulated-list-entries
          (--map (list nil (vector (alist-get 'StackName it)
                                   (alist-get 'StackStatus it)))
                 json))
    (tablist-revert)))

;;;###autoload
(defun saws-delete-stack ()
  "Delete the marked stacks."
  (interactive)
  (let ((stack (tablist-get-marked-items)))
    (--each stack
      (let ((cmd
             (concat
              "AWS_REGION=us-east-1 aws cloudformation delete-stack --stack-name "
              (elt (cdr it) 0))))
        (when (yes-or-no-p (format "Delete stack '%s'?" stack))
          (message "Running '%s'" cmd)
          (set-process-sentinel (start-process-shell-command "" nil cmd)
                                (lambda (&rest _) (tablist-revert))))))))

;;;###autoload
(defun saws-deploy-open-console (&rest args)
  "Open the aws deploy page in the aws console."
  (interactive (list (transient-args transient-current-command)))
  (browse-url "https://console.aws.amazon.com/cloudformation/home?region=us-east-1#/stacks?filteringStatus=active&filteringText=&viewNested=true&hideStacks=false"))

(provide 'saws-cloudformation)
;;; saws-cloudformation.el ends here
