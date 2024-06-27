SHELL := /bin/bash
EMACS ?= $(shell which emacs)
export PYTHON ?= python
ifeq ($(shell expr $$($(PYTHON) --version 2>&1 | cut -d' ' -f2) \< 3),1)
$(error Set PYTHON to python3)
endif
SRC=$(shell cask files)
PKBUILD=2.3
ELCFILES = $(SRC:.el=.elc)
CASK_DIR := $(shell EMACS=$(EMACS) cask package-directory || exit 1)
export TEST_PYTHON ?= python
ifeq ($(shell expr $$($(TEST_PYTHON) --version 2>&1 | cut -d'.' -f2) \< 9),0)
$(error Set TEST_PYTHON to an older python3)
endif

.DEFAULT_GOAL := test-compile

.PHONY: cask
cask: $(CASK_DIR)

$(CASK_DIR): Cask
	cask install
	touch $(CASK_DIR)

lisp/nnreddit-pkg.el: nnreddit/VERSION lisp/nnreddit-pkg.el.in
	sed 's/VERSION/"$(shell cat $<)"/' lisp/nnreddit-pkg.el.in > $@

.PHONY: autoloads
autoloads: lisp/nnreddit-pkg.el
	cask emacs -Q --batch -l package --eval "(let ((v (format \"%s.%s\" emacs-major-version emacs-minor-version))) (custom-set-variables (backquote (package-user-dir ,(concat \".cask/\" v)))))" -f package-initialize --eval "(package-generate-autoloads \"nnreddit\" \"./lisp\")"

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


.PHONY: pylint
ifeq ($(shell expr $$($(PYTHON) --version 2>&1 | cut -d'.' -f2) \> 9),1)
pylint:
	$(PYTHON) -m pip -q install --user pylint
	$(PYTHON) -m pylint nnreddit --rcfile=nnreddit/pylintrc
else
pylint:
	@echo forgoing pylint
endif

.PHONY: test-compile
test-compile: cask autoloads pylint
	sh -e tools/package-lint.sh lisp/nnreddit.el
	! (cask eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):")
	cask clean-elc

define SET_GITHUB_ACTOR =
GITHUB_ACTOR := $(shell if [ -z ${GITHUB_ACTOR} ]; then git config user.name; else echo ${GITHUB_ACTOR} ; fi)
endef

define SET_GITHUB_ACTOR_REPOSITORY =
GITHUB_ACTOR_REPOSITORY := $(GITHUB_ACTOR)/$(shell basename `git rev-parse --show-toplevel`)
endef

define SET_GITHUB_HEAD_REF =
GITHUB_HEAD_REF := $(shell if [ -z ${GITHUB_HEAD_REF} ]; then git rev-parse --abbrev-ref HEAD; else echo ${GITHUB_HEAD_REF} ; fi)
endef

define SET_GITHUB_SHA =
GITHUB_SHA := $(shell if [ -z ${GITHUB_SHA} ] ; then git rev-parse origin/${GITHUB_HEAD_REF}; else echo ${GITHUB_SHA}; fi)
endef

define SET_GITHUB_COMMIT =
GITHUB_COMMIT := $(shell if git show -s --format=%s "${GITHUB_SHA}" | egrep -q "^Merge .* into" ; then git show -s --format=%s "${GITHUB_SHA}" | cut -d " " -f2 ; else echo "${GITHUB_SHA}" ; fi)
endef

.PHONY: test-install-vars
test-install-vars:
	$(eval $(call SET_GITHUB_ACTOR))
	$(eval $(call SET_GITHUB_ACTOR_REPOSITORY))
	$(eval $(call SET_GITHUB_HEAD_REF))
	$(eval $(call SET_GITHUB_SHA))
	$(eval $(call SET_GITHUB_COMMIT))
	git show -s --format=%s $(GITHUB_COMMIT)
	git show -s --format=%s $(GITHUB_SHA)

.PHONY: test-install-defunct
test-install-defunct: test-install-vars
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
	-f package-initialize \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	--eval "(package-refresh-contents)" \
	--eval "(setq rcp (package-recipe-lookup \"nnreddit\"))" \
	--eval "(unless (file-exists-p package-build-archive-dir) \
	           (make-directory package-build-archive-dir))" \
	--eval "(let* ((my-repo \"$(GITHUB_ACTOR_REPOSITORY)\") \
	               (my-branch \"$(GITHUB_HEAD_REF)\") \
	               (my-commit \"$(GITHUB_COMMIT)\")) \
	           (oset rcp :repo my-repo) \
	           (oset rcp :branch my-branch) \
	           (oset rcp :commit my-commit))" \
	--eval "(package-build--package rcp (package-build--checkout rcp))" \
	--eval "(package-install-file (car (file-expand-wildcards (concat package-build-archive-dir \"nnreddit*.tar\"))))" 2>&1 | egrep -ia "error: |fatal" )

.PHONY: test-venv
test-venv: test-install-defunct
	$(EMACS) -Q --batch -f package-initialize \
	                 --eval "(custom-set-variables (quote (gnus-verbose 8)))" \
	                 --eval "(require (quote nnreddit))" \
	                 --eval "nnreddit-venv"

define TESTRUN
--eval "(custom-set-variables \
  (quote (gnus-select-method (quote (nnreddit \"\")))) \
  (backquote (venv-location ,(file-name-as-directory (make-temp-file \"testrun-\" t)))) \
  (quote (nnreddit-python-command \"$(PYTHON)\"))\
  (quote (gnus-verbose 8)) \
  (quote (nnreddit-log-rpc t)))" \
--eval "(setq debug-on-error t)" \
--eval "(fset (quote gnus-y-or-n-p) (function ignore))"
endef

.PHONY: test-run
test-run: cask autoloads
	cask emacs -Q --batch \
	  $(TESTRUN) \
	  --eval "(require 'nnreddit)" \
	  --eval "(cl-assert (nnreddit-rpc-get))" \
	  --eval "(sleep-for 0 7300)" \
	  -f nnreddit-dump-diagnostics \
	  --eval "(cl-assert nnreddit-processes)"

.PHONY: test-run-interactive
test-run-interactive: cask autoloads
	cask emacs -Q \
	  $(TESTRUN) \
	  -f gnus

.PHONY: test-unit
test-unit:
	PYTHON=$(TEST_PYTHON) cask exec ert-runner -L . -L tests tests/test*.el

.PHONY: test
test: test-compile test-unit test-int

.PHONY: test-int
test-int:
	$(TEST_PYTHON) -m venv venv-nnreddit-test
	( \
	  source venv-nnreddit-test/bin/activate; \
	  python -m pip -q install -r requirements-dev.txt; \
	  python -m pytest tests/test_oauth.py; \
	  rm -f tests/.newsrc.eld; \
	  cask exec ecukes --reporter magnars --tags "~@inbox"; \
	  rm -f tests/.newsrc.eld; \
	  cask exec ecukes --reporter magnars --tags "@inbox"; \
	)

.PHONY: dist-clean
dist-clean:
	rm -rf dist

.PHONY: dist
dist: dist-clean
	cask package

.PHONY: install
install: dist autoloads
	$(EMACS) -Q --batch -f package-initialize \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	  --eval "(package-refresh-contents)" \
	  --eval "(package-install-file (car (file-expand-wildcards \"dist/nnreddit*.tar\")))"
