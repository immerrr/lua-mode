# Makefile for lua-mode

VERSION="$(shell sed -nre '/^;; Version:/ { s/^;; Version:[ \t]+//; p }' lua-mode.el)"
DISTFILE = lua-mode-$(VERSION).zip

# EMACS value may be overridden
EMACS?=emacs

TESTS=
TESTS += ert-tests/test-defun-font-lock.el
TESTS += ert-tests/test-builtin-font-lock.el

default:
	@echo version is $(VERSION)

compile:
	$(EMACS) --batch --no-site-file -f batch-byte-compile lua-mode.el

dist:
	rm -f $(DISTFILE) && \
	git archive --format=zip -o $(DISTFILE) --prefix=lua-mode/ HEAD

.PHONY: test test-compiled test-uncompiled
# check both regular and compiled versions
test: test-compiled test-uncompiled

test-compiled: compile
	$(EMACS) -Q --batch -l ert \
		-l lua-mode.elc -l ert-tests/lua-font-lock-test-helpers.el \
		$(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

test-uncompiled:
	$(EMACS) -Q --batch -l ert \
		-l lua-mode.el -l ert-tests/lua-font-lock-test-helpers.el \
		$(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

release:
	git fetch && \
	git diff remotes/origin/master --exit-code && \
	git tag -a -m "Release tag" rel-$(VERSION) && \
	woger lua-l lua-mode lua-mode "release $(VERSION)" "Emacs major mode for editing Lua files" release-notes-$(VERSION) http://github.com/immerrr/lua-mode/ && \
	git push origin master
	@echo 'Send update to ELPA!'
