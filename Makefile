EMACS ?= $(shell which emacs)
PYTHON ?= python
SRC=$(shell cask files)
PKBUILD=2.3
ELCFILES = $(SRC:.el=.elc)
ifeq ($(GITHUB_REPOSITORY),)
GITHUB_REPOSITORY := $(shell git config user.name)/$(shell basename `git rev-parse --show-toplevel`)
endif
ifeq ($(GITHUB_HEAD_REF),)
GITHUB_HEAD_REF := $(shell git rev-parse --abbrev-ref HEAD)
endif
ifeq ($(GITHUB_SHA),)
GITHUB_SHA := $(shell if git show-ref --quiet --verify origin/$(GITHUB_HEAD_REF) ; then git rev-parse origin/$(GITHUB_HEAD_REF) ; fi))
endif

.DEFAULT_GOAL := test-compile

.PHONY: autoloads
autoloads:
	$(EMACS) -Q --batch --eval "(package-initialize)" --eval "(package-generate-autoloads \"nnreddit\" \"./lisp\")"

README.rst: README.in.rst lisp/nnreddit.el
	grep ';;' lisp/nnreddit.el \
	    | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | sed -e 's/^\s*;;*\s*//g' \
	    | tools/readme-sed.sh "COMMENTARY" README.in.rst > README.rst

.PHONY: clean
clean:
	cask clean-elc
	pyclean nnreddit
	$(PYTHON) setup.py clean
	rm -f tests/log/*
	rm -rf tests/test-install

.PHONY: test-compile
test-compile: autoloads
	pylint nnreddit
	cask install
	sh -e tools/package-lint.sh lisp/nnreddit.el
	! (cask eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):")
	cask clean-elc

.PHONY: test-install
test-install:
	mkdir -p tests/test-install
	if [ ! -s "tests/test-install/$(PKBUILD).tar.gz" ] ; then \
	  cd tests/test-install ; curl -sLOk https://github.com/melpa/package-build/archive/$(PKBUILD).tar.gz ; fi
	cd tests/test-install ; tar xfz $(PKBUILD).tar.gz
	cd tests/test-install ; rm -f $(PKBUILD).tar.gz
	cd tests/test-install/package-build-$(PKBUILD) ; make -s loaddefs
	mkdir -p tests/test-install/recipes
	cd tests/test-install/recipes ; curl -sfLOk https://raw.githubusercontent.com/melpa/melpa/master/recipes/nnreddit || cp -f ../../../tools/recipe ./nnreddit
	! ( $(EMACS) -Q --batch -L tests/test-install/package-build-$(PKBUILD) \
	--eval "(require 'package-build)" \
	--eval "(require 'subr-x)" \
	--eval "(package-initialize)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	--eval "(package-refresh-contents)" \
	--eval "(setq rcp (package-recipe-lookup \"nnreddit\"))" \
	--eval "(unless (file-exists-p package-build-archive-dir) \
	           (make-directory package-build-archive-dir))" \
	--eval "(let* ((my-repo \"$(GITHUB_REPOSITORY)\") \
	               (my-branch \"$(GITHUB_HEAD_REF)\") \
	               (my-commit \"$(GITHUB_SHA)\")) \
	           (oset rcp :repo my-repo) \
	           (oset rcp :branch my-branch) \
	           (oset rcp :commit my-commit))" \
	--eval "(package-build--package rcp (package-build--checkout rcp))" \
	--eval "(package-install-file (car (file-expand-wildcards (concat package-build-archive-dir \"nnreddit*.tar\"))))" 2>&1 | egrep -ia "error: |fatal" )

.PHONY: test-venv
test-venv: test-install
	$(EMACS) -Q --batch --eval "(package-initialize)" \
	                 --eval "(custom-set-variables (quote (gnus-verbose 8)))" \
	                 --eval "(require (quote nnreddit))" \
	                 --eval "nnreddit-venv"

define TESTRUN
--eval "(custom-set-variables \
  (quote (gnus-select-method (quote (nnreddit \"\")))) \
  (backquote (venv-location ,(file-name-as-directory (make-temp-file \"testrun-\" t)))) \
  (quote (gnus-verbose 8)) \
  (quote (nnreddit-log-rpc t)))" \
--eval "(setq debug-on-error t)" \
--eval "(fset (quote gnus-y-or-n-p) (function ignore))"
endef

.PHONY: test-run
test-run:
	cask emacs -Q --batch \
	  $(TESTRUN) \
	  --eval "(require 'nnreddit)" \
	  --eval "(cl-assert (nnreddit-rpc-get))" \
	  --eval "(sleep-for 0 7300)" \
	  -f nnreddit-dump-diagnostics \
	  --eval "(cl-assert nnreddit-processes)"

.PHONY: test-run-interactive
test-run-interactive:
	cask emacs -Q \
	  $(TESTRUN) \
	  -f gnus

.PHONY: test-unit
test-unit:
	PYTHON=$(PYTHON) cask exec ert-runner -L . -L tests tests/test*.el

.PHONY: test
test: test-compile test-unit test-int

.PHONY: test-int
test-int:
	$(PYTHON) -m pytest tests/test_oauth.py
	rm -f tests/.newsrc.eld
	PYTHON=$(PYTHON) cask exec ecukes --debug --reporter magnars

.PHONY: dist-clean
dist-clean:
	rm -rf dist

.PHONY: dist
dist: dist-clean
	cask package

.PHONY: install
install: test-compile dist
	$(EMACS) -Q --batch --eval "(package-initialize)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	  --eval "(package-refresh-contents)" \
	  --eval "(package-install-file (car (file-expand-wildcards \"dist/nnreddit*.tar\")))"
