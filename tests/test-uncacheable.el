(load "utils-test")

(ert-deftest nnreddit-should-not-cache ()
  (should (string-match gnus-uncacheable-groups "nnreddit:emacs")))

