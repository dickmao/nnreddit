(When "^I query attr \"\\(.*\\)\"$"
      (lambda (attr)
        (should (equal "poop" (nnreddit-rpc-call nil nil "user_attr" attr)))))

(When "^rpc \"\\(.*\\)\" returns \"\\(.*\\)\"$"
      (lambda (command result)
        (should (string= result (nnreddit-rpc-call nil nil command)))))

(When "^gnus$"
      (lambda ()
        (ein:aif (get-buffer gnus-group-buffer)
            (switch-to-buffer it)
          (When "I call \"gnus\"")
          (Then "I should be in buffer \"%s\"" gnus-group-buffer))))
