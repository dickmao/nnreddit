Feature: 20201124

@inbox
Scenario: Get some inbox messages
  Given gnus start
  When begin recording "inbox"
  And I go to word "nnreddit:/u/nnreddit-user"
  And I press "RET"
  And I should be in buffer "*Summary nnreddit:/u/nnreddit-user*"
  And I go to word "Roblox"
  And I press "RET"
  And I switch to buffer "*Article nnreddit:/u/nnreddit-user*"
  And I should see "Nice!"
  And emacs26 cannot do action chain "f m"
  Then protected see message "emacs26 cannot do action chain: Followup from inbox not implemented"
  Then end recording "inbox"
