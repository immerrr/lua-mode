# Makefile for lua-mode

VERSION="$(shell sed -nre '/^;; Version:/ { s/^;; Version:[ \t]+//; p }' lua-mode.el)"
DISTFILE = lua-mode-$(VERSION).zip

# EMACS value may be overridden
EMACS?=emacs

EMACS_BATCH=cask exec $(EMACS) --batch -Q

TESTS=
TESTS += test/defun-font-lock-test.el
TESTS += test/builtin-font-lock-test.el
TESTS += test/electric-mode-test.el
TESTS += test/indentation-test.el
TESTS += test/strings-and-comments-test.el
TESTS += test/generic-test.el
TESTS += test/inferior-test.el

default:
	@echo version is $(VERSION)

%.elc: %.el
	$(EMACS_BATCH) -f batch-byte-compile $<

compile: lua-mode.elc


dist:
	rm -f $(DISTFILE) && \
	git archive --format=zip -o $(DISTFILE) --prefix=lua-mode/ HEAD

.PHONY: test-compiled test-uncompiled
# check both regular and compiled versions
test: test-compiled test-uncompiled

test-compiled: compile
	$(EMACS_BATCH) -l test/ert.el \
		-l lua-mode.elc \
		$(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

test-uncompiled:
	$(EMACS_BATCH) -l test/ert.el \
		-l lua-mode.el \
		$(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

release:
	git fetch && \
	git diff remotes/origin/master --exit-code && \
	git tag -a -m "Release tag" rel-$(VERSION) && \
	woger lua-l lua-mode lua-mode "release $(VERSION)" "Emacs major mode for editing Lua files" release-notes-$(VERSION) http://github.com/immerrr/lua-mode/ && \
	git push origin master
	@echo 'Send update to ELPA!'


tryout:
	cask exec $(EMACS) -Q -l lua-mode.el test.lua
