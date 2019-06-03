@refresh_token
Scenario: Do not know how to betamax initial oauth handshake
  When begin recording "refresh_token"
  Given gnus
  Then end recording "refresh_token"

@random
Scenario: random subreddit
  When begin recording "random"
  Given gnus
  And rpc "random_subreddit" returns "wholesomegifs"
  Then end recording "random"

@subscribe
Scenario: subscribe and unsubscribe
  When begin recording "subscribe"
  Given gnus
  And I goto group "chvrches"
  And I press "q"
  Then I should be in buffer "*Group*"
  And I go to word "chvrches"
  And I press "u"
  And I open latest "log/test_py"
  Then I wait for buffer to say "('action', 'sub')"
  And I switch to buffer "*Group*"
  And I go to word "chvrches"
  And I press "u"
  And I open latest "log/test_py"
  Then I wait for buffer to say "('action', 'unsub')"
  Then end recording "subscribe"