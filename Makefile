# Makefile for lua-mode

VERSION="$(shell sed -nre '/^;; Version:/ { s/^;; Version:[ \t]+//; p }' lua-mode.el)"
DISTFILE = lua-mode-$(VERSION).zip

DIST_CONTENTS = \
	lua-mode.el \
	README \
	README.md \
	TODO \
	Makefile

default:
	@echo version is $(VERSION)

dist:
	rm -f $(DISTFILE) && \
	zip $(DISTFILE) -r $(DIST_CONTENTS)

release:
	git diff --exit-code && \
	git tag -a -m "Release tag" rel-$(VERSION) && \
	git push origin master && git pull origin master && \
	woger lua-l lua-mode lua-mode "release $(VERSION)" "Emacs major mode for editing Lua files" release-notes-$(VERSION) http://github.com/immerrr/lua-mode/
	@echo 'Send update to ELPA!'
