#!/bin/sh -e

. tools/retry.sh

export EMACS="${EMACS:=emacs}"
export BASENAME=$(basename "$1")

( cask emacs -Q --batch \
           --visit "$1" \
           --eval "(checkdoc-eval-current-buffer)" \
           --eval "(princ (with-current-buffer checkdoc-diagnostic-buffer \
                                               (buffer-string)))" \
           2>&1 | egrep -a "^$BASENAME:" ) && false

# Reduce purity via:
# --eval "(fset 'package-lint--check-defs-prefix (symbol-function 'ignore))" \
PKG_MAIN=$(cask files | egrep -- "pkg.el$")
travis_retry cask emacs -Q --batch \
           -l package-lint \
           --eval "(package-initialize)" \
           --eval "(push (quote (\"melpa\" . \"http://melpa.org/packages/\")) \
                         package-archives)" \
           --eval "(setq package-lint-main-file (if (zerop (length \"${PKG_MAIN}\")) nil \"${PKG_MAIN}\"))" \
           --eval "(package-refresh-contents)" \
           -f package-lint-batch-and-exit "$1"
