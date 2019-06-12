(When "^rpc \"\\(.*\\)\" returns \"\\(.*\\)\"$"
      (lambda (command result)
        (should (string= result (nnreddit-rpc-call nil nil command)))))

(When "^I goto group \"\\(.*\\)\"$"
      (lambda (group)
        (Given "I start an action chain")
        (And "I press \"R g\"")
        (And "I type \"%s\"" group)
        (And "I execute the action chain")
        (Then (format "I should be in buffer \"*Summary nnreddit:%s*\"" group))))

(When "^I should be in buffer like \"\\(.+\\)\"$"
      (lambda (prefix)
        (should (string-prefix-p prefix (buffer-name)))))

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

(When "^gnus start$"
      (lambda ()
        (nnreddit-aif (get-buffer gnus-group-buffer)
            (switch-to-buffer it)
          (When "I call \"gnus\"")
          (Then "I should be in buffer \"%s\"" gnus-group-buffer))))

(When "^gnus stop$"
      (lambda ()
        (nnreddit-aif (get-buffer gnus-group-buffer)
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
               (base (file-name-base prefix)))
          (nnreddit-aif (seq-some (lambda (b)
                               (when (cl-search base (buffer-name b)) b))
                             (buffer-list))
              (progn
                (switch-to-buffer it)
                (revert-buffer :ignore-auto :noconfirm))
            (let* ((alist
                    (directory-files-and-attributes dir t (regexp-quote base) t))
                   (sofar (cl-first alist))
                   (most-recent (dolist (cand alist (car sofar))
                                  (if (> (float-time (nth 5 (cdr cand)))
                                         (float-time (nth 5 (cdr sofar))))
                                      (setq sofar cand)))))
              (find-file most-recent)
              )))))

(When "^I wait \\([.0-9]+\\) seconds?$"
      (lambda (seconds)
        (sleep-for (string-to-number seconds))))

(When "^I wait for buffer to\\( not\\)? say \"\\(.+\\)\"$"
      (lambda (negate bogey)
        (test-wait-for
         (lambda ()
           (let* ((says (s-contains? (s-replace "\\n" "\n" bogey) (buffer-string))))
             (revert-buffer :ignore-auto :noconfirm)
             (if negate (not says) says)))
         nil 40000 2000)))

(When "^emacs26 cannot do action chain \"\\(.+\\)\"$"
      (lambda (keys)
        (let ((vkeys (seq-concatenate 'vector
                                      (mapcar #'string-to-char (split-string keys)))))
          (condition-case err
              (execute-kbd-macro vkeys)
            (error (message "emacs26 cannot do action chain: %s"
                            (error-message-string err)))))))
