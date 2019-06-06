#!/bin/sh -e

# The following is a derivative work of
# https://github.com/purcell/package-lint
# licensed under GNU General Public License v3.0.

EMACS="${EMACS:=emacs}"

INIT_PACKAGE_EL="(progn
  (require 'package)
  (push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives)
  (package-initialize))"

# Refresh package archives, because the test suite needs to see at least
# package-lint and cl-lib.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         --eval '(package-refresh-contents)' \
         --eval "(unless (package-installed-p 'cl-lib) (package-install 'cl-lib))" \
         --eval "(unless (package-installed-p 'package-lint) (package-install 'package-lint))"

# Byte compile, failing on byte compiler errors, or on warnings unless ignored
if [ -n "${EMACS_LINT_IGNORE+x}" ]; then
    ERROR_ON_WARN=nil
else
    ERROR_ON_WARN=t
fi

# Lint ourselves
# Lint failures are ignored if EMACS_LINT_IGNORE is defined, so that lint
# failures on Emacs 24.2 and below don't cause the tests to fail, as these
# versions have buggy imenu that reports (defvar foo) as a definition of foo.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l package-lint.el \
         --eval "(fset 'package-lint--check-defs-prefix (symbol-function 'ignore))" \
         -f package-lint-batch-and-exit \
         lisp/nnreddit.el || [ -n "${EMACS_LINT_IGNORE+x}" ]
