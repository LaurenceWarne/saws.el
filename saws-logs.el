(require 'json)
(require 'transient)
(require 'dash)

(transient-define-prefix saws-logs ()
  "Transient for interacting with logs."
  ["Actions"
   ("d" "deploy" saws-deploy)
   ("l" "describe stacks" saws-describe-stacks)
   ("o" "open aws console" saws-deploy-open-console)
   ("O" "open documentation for resource" ignore)])

(defun saws-describe-logs (args)
  "Run 'aws cloudformation describe-log-groups' using ARGS."
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

(defun saws-open-log-group (log-group &optional since)
  "Open a log group."
  (let* ((cmd (format "AWS_PROFILE=personal AWS_REGION=us-east-1 aws logs describe-log-streams --log-group-name %s"
                      log-group))
         (output (shell-command-to-string cmd))
         (json (cdar (json-read-from-string output))))
    ;; [((logStreamName . "2022/08/30/[$LATEST]57172f7ae86d41d1b4e9a9d2a3e366f5") (creationTime . 1661865010383) (firstEventTimestamp . 1661865001323) (lastEventTimestamp . 1661865006214) (lastIngestionTime . 1661865010391) (uploadSequenceToken . "49619706100273847445281902055162430536550164215279649058") (arn . "arn:aws:logs:us-east-1:894057498223:log-group:/aws/lambda/TestFn:log-stream:2022/08/30/[$LATEST]57172f7ae86d41d1b4e9a9d2a3e366f5") (storedBytes . 0)) ((logStreamName . "2022/08/30/[$LATEST]b129b7d317e64d25baa86bc52288d57a") (creationTime . 1661864932469) (firstEventTimestamp . 1661864923403) (lastEventTimestamp . 1661864928327) (lastIngestionTime . 1661864932480) (uploadSequenceToken . "49611784873594563045266747106529581801608398818424915090") (arn . "arn:aws:logs:us-east-1:894057498223:log-group:/aws/lambda/TestFn:log-stream:2022/08/30/[$LATEST]b129b7d317e64d25baa86bc52288d57a") (storedBytes . 0)) ((logStreamName . "2022/08/30/[$LATEST]b86258602ca241ccb8f418fc4f25dbaf") (creationTime . 1661863857268) (firstEventTimestamp . 1661863848161) (lastEventTimestamp . 1661863848536) (lastIngestionTime . 1661863857276) (uploadSequenceToken . "49632285450278518550343820966484709726672467147960090834") (arn . "arn:aws:logs:us-east-1:894057498223:log-group:/aws/lambda/TestFn:log-stream:2022/08/30/[$LATEST]b86258602ca241ccb8f418fc4f25dbaf") (storedBytes . 0)) ((logStreamName . "2022/08/30/[$LATEST]d499939cc1dc440895b62eeb7a75fc41") (creationTime . 1661865433881) (firstEventTimestamp . 1661865424815) (lastEventTimestamp . 1661865425184) (lastIngestionTime . 1661865433916) (uploadSequenceToken . "49607503438980352751719831280666946770392568203503993618") (arn . "arn:aws:logs:us-east-1:894057498223:log-group:/aws/lambda/TestFn:log-stream:2022/08/30/[$LATEST]d499939cc1dc440895b62eeb7a75fc41") (storedBytes . 0)) ((logStreamName . "2022/08/30/[$LATEST]e351c38a1e9c4ed48e1661ac650e8b09") (creationTime . 1661865305899) (firstEventTimestamp . 1661865296836) (lastEventTimestamp . 1661865297196) (lastIngestionTime . 1661865305906) (uploadSequenceToken . "49631695642622589784263138417404746163856721100201264562") (arn . "arn:aws:logs:us-east-1:894057498223:log-group:/aws/lambda/TestFn:log-stream:2022/08/30/[$LATEST]e351c38a1e9c4ed48e1661ac650e8b09") (storedBytes . 0)) ((logStreamName . "2022/08/30/[$LATEST]e8e1d14d8c8a4dc5b497936b3e9afc36") (creationTime . 1661865224542) (firstEventTimestamp . 1661865215332) (lastEventTimestamp . 1661865219815) (lastIngestionTime . 1661865224551) (uploadSequenceToken . "49631695642622589784263138337563658259047760880084190642") (arn . "arn:aws:logs:us-east-1:894057498223:log-group:/aws/lambda/TestFn:log-stream:2022/08/30/[$LATEST]e8e1d14d8c8a4dc5b497936b3e9afc36") (storedBytes . 0))]
    (print json)))
