@refresh_token
Scenario: Do not know how to betamax initial oauth handshake
  When begin recording "refresh_token"
  Given gnus start
  Then end recording "refresh_token"

@random
Scenario: random subreddit
  When begin recording "random"
  Given gnus start
  And rpc "random_subreddit" returns "preppers"
  Then end recording "random"

@subscribe
Scenario: subscribe and unsubscribe
  When begin recording "subscribe"
  Given gnus start
  And I goto group "test"
  And I press "q"
  Then I should be in buffer "*Group*"
  And I go to word "test"
  And I press "u"
  And I open latest "log/test_py"
  Then I wait for buffer to say "('action', 'sub')"
  And I switch to buffer "*Group*"
  And I go to word "test"
  And I press "u"
  And I open latest "log/test_py"
  Then I wait for buffer to say "('action', 'unsub')"
  Then end recording "subscribe"

@scan
Scenario: selecting group does not rescan, but M-g does
  Given gnus stop
  When begin recording "scan"
  Given gnus start
  And I clear buffer "*Messages*"
  And I go to word "emacs"
  And I press "M-g"
  And I switch to buffer "*Messages*"
  And I should see pattern "nnreddit-request-scan: emacs"
  And I switch to buffer "*Group*"
  And I clear buffer "*Messages*"
  And I go to word "emacs"
  And I press "RET"
  And I should be in buffer "*Summary nnreddit:emacs*"
  And I switch to buffer "*Messages*"
  And I should not see pattern "nnreddit-request-scan: emacs"
  And I switch to buffer "*Group*"
  And I go to word "orgmode"
  And I press "RET"
  And I should be in buffer "*Summary nnreddit:orgmode*"
  And I switch to buffer "*Messages*"
  And I should not see pattern "nnreddit-request-scan: orgmode"
  Then end recording "scan"

@vote
Scenario: Voting from summary and article buffers
  Given gnus stop
  When begin recording "vote"
  Given gnus start
  And I go to word "PostPreview"
  And I press "RET"
  And I should be in buffer "*Summary nnreddit:PostPreview*"
  And I go to word "Plasky"
  And I press "R ="
  Then protected see message "Open the article before voting"
  And I press "RET"
  And I press "R ="
  And I switch to buffer "*Article nnreddit:PostPreview*"
  And I should see "Score: 2 +1"
  And I press "R -"
  And I should see "Score: 2 -1"
  Then end recording "vote"

@post
Scenario: message-send-and-exit
  When begin recording "post"
  Given gnus start
  And I go to word "PostPreview"
  And I press "RET"
  And I should be in buffer "*Summary nnreddit:PostPreview*"
  And emacs26 cannot do action chain "a t"
  Then I should be in buffer "*unsent posting on PostPreview*"
  And I type "test baby test baby 123"
  And I press "M->"
  And I type "this is a test"
  And I press "C-c C-c"
  And I should be in buffer "*Summary nnreddit:PostPreview*"
  Then end recording "post"

@loose
Scenario: Reply to a loose thread
  Given gnus stop
  When begin recording "loose"
  Given gnus start
  And I go to word "PostPreview"
  And I press "RET"
  And I should be in buffer "*Summary nnreddit:PostPreview*"
  And I go to string "Re: Testing inline image links"
  And emacs26 cannot do action chain "f r"
  Then I should be in buffer like "*unsent followup"
  And I should see "Reply-Root: yes"
  And I press "M->"
  And I type "this is a test"
  And I press "C-c C-c"
  And I should be in buffer "*Summary nnreddit:PostPreview*"
  Then end recording "loose"

@cancel
Scenario: cancel post
  When begin recording "cancel"
  Given gnus start
  And I go to word "PostPreview"
  And I press "RET"
  And I should be in buffer "*Summary nnreddit:PostPreview*"
  And I go to string "Re: Testing inline image links"
  And I press "C-n"
  And I press "S c"
  And I open latest "log/test_py"
  And I wait for buffer to say "api/del"
  And I wait for buffer to say "('id', 't1_eqwoano')"
  Then end recording "cancel"

@supersede
Scenario: supersede post
  Given gnus stop
  When begin recording "supersede"
  Given gnus start
  And I go to word "PostPreview"
  And I press "RET"
  And I should be in buffer "*Summary nnreddit:PostPreview*"
  And I go to word "aegisninja"
  And I press "C-n"
  And I press "S s"
  Then I should be in buffer "*unsent supersede*"
  And I type "edit: "
  And I press "C-c C-c"
  And I open latest "log/test_py"
  And I wait for buffer to say "api/editusertext"
  And I wait for buffer to say "('thing_id', 't1_eqwe7dx')"
  Then end recording "supersede"

@canonical
Scenario: Going to hongkong make me really go to HongKong
  Given gnus stop
  When begin recording "canonical"
  Given gnus start
  And I goto group "hongkong"
  And I press "q"
  Then I should be in buffer "*Group*"
  And I should see "HongKong"
  Then end recording "canonical"

@browser
Scenario: Verify user message if no refresh token present
  Given gnus stop
  And I hide tokens
  And gnus try start
  And I switch to buffer "*Messages*"
  Then I should see "nnreddit-default: Please check your browser."
  And I unhide tokens
  And I kill all rpc processes
