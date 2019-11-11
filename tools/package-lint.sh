#!/bin/sh -e

EMACS="${EMACS:=emacs}"
BASENAME=$(basename "$1")

!( cask emacs -Q --batch \
           --visit "$1" \
           --eval "(checkdoc-eval-current-buffer)" \
           --eval "(princ (with-current-buffer checkdoc-diagnostic-buffer \
                                               (buffer-string)))" \
           2>&1 | egrep -a "^$BASENAME:" | egrep -v "Messages should start" | grep "." )

# this repo uses datetime versions
( cd /tmp ; curl -OskL https://raw.githubusercontent.com/dickmao/package-lint/datetime/package-lint.el )

cask emacs -Q --batch \
           --eval "(let ((dir (file-name-directory (locate-library \"package-lint\")))) \
                     (ignore-errors (delete-file (expand-file-name \
                                                  \"package-lint.elc\" dir))) \
                     (copy-file (expand-file-name \"package-lint.el\" \"/tmp\") \
                                (expand-file-name \"package-lint.el\" dir) t))"

# Reduce purity via:
# --eval "(fset 'package-lint--check-defs-prefix (symbol-function 'ignore))" \
cask emacs -Q --batch \
           -l package-lint \
           --eval "(package-initialize)" \
           --eval "(push (quote (\"melpa\" . \"http://melpa.org/packages/\")) \
                         package-archives)" \
           --eval "(package-refresh-contents)" \
           -f package-lint-batch-and-exit "$1"
