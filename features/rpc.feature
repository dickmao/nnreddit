@refresh_token
Scenario: Do not know how to betamax initial oauth handshake
  Given gnus

Scenario: random subreddit
  Given gnus
  And rpc "random_subreddit" returns "holdmyredbull"
