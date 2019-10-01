EMACS ?= $(shell which emacs)
PYTHON ?= python
SRC=$(shell cask files)
PKBUILD=2.3
ELCFILES = $(SRC:.el=.elc)
ifeq ($(TRAVIS_PULL_REQUEST_SLUG),)
TRAVIS_PULL_REQUEST_SLUG := $(shell git config --global user.name)/$(shell basename `git rev-parse --show-toplevel`)
endif
ifeq ($(TRAVIS_PULL_REQUEST_BRANCH),)
TRAVIS_PULL_REQUEST_BRANCH := $(shell git rev-parse --abbrev-ref HEAD)
endif
ifeq ($(TRAVIS_PULL_REQUEST_SHA),)
TRAVIS_PULL_REQUEST_SHA := $(shell if git show-ref --quiet --verify origin/$(TRAVIS_PULL_REQUEST_BRANCH) ; then git rev-parse origin/$(TRAVIS_PULL_REQUEST_BRANCH) ; fi))
endif

.DEFAULT_GOAL := test-compile

.PHONY: autoloads
autoloads:
	$(EMACS) -Q --batch --eval "(package-initialize)" --eval "(package-generate-autoloads \"nnreddit\" \"./lisp\")"

README.rst: README.in.rst lisp/nnreddit.el
	sed "/CI VERSION/c"`grep -o 'emacs-[0-9][.0-9]*' .travis.yml | sort -n | head -1 | grep -o '[.0-9]*'` README.in.rst > README.rst0
	grep ';;' lisp/nnreddit.el \
	    | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | sed -e 's/^\s*;;*\s*//g' \
	    | tools/readme-sed.sh "COMMENTARY" README.rst0 > README.rst
	rm -f README.rst0 README.rst1

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
	sh -e tools/package-lint.sh lisp/nnreddit.el
	cask install
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
	--eval "(let* ((my-repo \"$(TRAVIS_PULL_REQUEST_SLUG)\") \
	               (my-branch \"$(TRAVIS_PULL_REQUEST_BRANCH)\") \
	               (my-commit \"$(TRAVIS_PULL_REQUEST_SHA)\")) \
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

.PHONY: test-unit
test-unit:
	cask exec ert-runner -L . -L tests tests/test*.el

.PHONY: test
test: test-compile test-unit test-int

.PHONY: test-int
test-int:
	$(PYTHON) -m pytest tests/test_oauth.py
	rm -f tests/.newsrc.eld
	cask exec ecukes --debug --reporter magnars

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
