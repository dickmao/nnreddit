(When "I kill all rpc processes$"
      (lambda ()
        (nnreddit-request-close)))

(When "I hide tokens$"
      (lambda ()
        (setq nnreddit--python-module-extra-args '("--token-file" "/dev/null"))))

(When "I unhide tokens$"
      (lambda ()
        (setq nnreddit--python-module-extra-args nil)))

(When "^rpc \"\\(.*\\)\" returns \"\\(.*\\)\"$"
      (lambda (command result)
        (should (string= result (nnreddit-rpc-call nil nil command)))))

(When "^I should be in buffer like \"\\(.+\\)\"$"
      (lambda (prefix)
        (should (string-prefix-p prefix (buffer-name)))))

(When "^I goto group \"\\(.*\\)\"$"
      (lambda (group)
        (Given "I start an action chain")
        (And "I press \"R g\"")
        (And "I type \"%s\"" group)
        (And "I execute the action chain")
        (Then "I should be in buffer like \"*Summary nnreddit:\"")))

(When "^I go to string \"\\(.+\\)\"$"
      (lambda (string)
        (goto-char (point-min))
        (let ((search (re-search-forward string nil t))
              (message "Can not go to string '%s' since it does not exist in the current buffer: %s"))
          (cl-assert search nil message string (buffer-string)))
        (backward-char (length string))))

(When "^I clear buffer \"\\(.*\\)\"$"
      (lambda (buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)))))

(When "^I dump buffer"
      (lambda () (message "%s" (buffer-string))))

(Then "^protected see message \"\\(.+\\)\"$"
  (lambda (message)
    (let ((msg "Expected '%s' to be included in the list of printed messages, but was not."))
      (setq message (s-replace "\\\"" "\"" message))
      (cl-assert (-contains? (-map (lambda (s) (if (stringp s) (s-trim s) "")) ecukes-message-log) message) nil msg message))))

(When "^gnus \\(try \\)?start\\(\\)$"
      (lambda (demote _workaround)
        (aif (get-buffer gnus-group-buffer)
            (switch-to-buffer it)
          (if-demote demote
            (When "I call \"gnus\"")
            (Then "I should be in buffer \"%s\"" gnus-group-buffer)))))

(When "^gnus stop$"
      (lambda ()
        (aif (get-buffer gnus-group-buffer)
            (progn (switch-to-buffer it)
                   (And "I press \"q\"")
                   (switch-to-buffer "*scratch*")))))

(When "^begin recording \"\\(.+\\)\"$"
      (lambda (cassette)
        (should (nnreddit-rpc-call nil nil "recording_begin" cassette))))

(When "^end recording \"\\(.+\\)\"$"
      (lambda (cassette)
        (should (nnreddit-rpc-call nil nil "recording_end" cassette))))

(When "^end recordings$"
      (lambda ()
        (should (nnreddit-rpc-call nil nil "recording_end"))))

(When "^I open latest \"\\(.+\\)\"$"
      (lambda (relative-prefix)
        (let* ((prefix (concat (file-name-as-directory gnus-home-directory)
                               relative-prefix))
               (dir (file-name-directory prefix))
               (base (file-name-base prefix))
               (alist
                (directory-files-and-attributes dir t (regexp-quote base) t))
               (sofar (cl-first alist))
               (most-recent (dolist (cand alist (car sofar))
                              (if (> (float-time (nth 5 (cdr cand)))
                                     (float-time (nth 5 (cdr sofar))))
                                  (setq sofar cand)))))
          (find-file most-recent))))

(When "^I wait \\([.0-9]+\\) seconds?$"
      (lambda (seconds)
        (sleep-for (string-to-number seconds))))

(When "^I wait for buffer to\\( not\\)? say \"\\(.+\\)\"$"
      (lambda (negate bogey)
        (nnreddit-test-wait-for
         (lambda ()
           (let* ((says (s-contains? (s-replace "\\n" "\n" bogey) (buffer-string))))
             (revert-buffer :ignore-auto :noconfirm)
             (if negate (not says) says)))
         nil 5000 1000)))


;; (When "^I scuzz \"\\(.+\\)\"$"
;;       (lambda (buffer)
;;         (let ((v (vconcat [?\C-x ?b] (string-to-vector buffer))))
;;           (princ (format "holla %s %s %s" (string-to-vector buffer) v (key-binding buffer)))
;;           (execute-kbd-macro (string-to-vector buffer))
;;           (execute-kbd-macro v))))

(When "^emacs26 cannot do action chain \"\\(.+\\)\"$"
      (lambda (keys)
        (let ((vkeys (seq-concatenate 'vector (mapcar #'string-to-char (split-string keys "[ ]")))))
          (condition-case err
              (execute-kbd-macro vkeys)
            (error (message "emacs26 cannot do action chain: %s"
                            (error-message-string err)))))))
