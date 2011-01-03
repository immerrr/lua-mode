# Makefile for lua-mode

VERSION=`grep "^;; Version:" lua-mode.el | cut -f 2`
DISTFILE = lua-mode-$(VERSION).zip

dist:
	rm -f $(DISTFILE) && \
	zip $(DISTFILE) -r . -x ".git/*" "*.gitignore" "*.zip"

release:
	git diff --exit-code && \
	git tag -a -m "Release tag" rel-`echo $(VERSION) | sed -e 's/\./-/g'` && \
	git push origin master && git pull && \
	woger lua-l lua-mode lua-mode "release $(VERSION)" "Emacs major mode for editing Lua files" release-notes-$(VERSION) http://github.com/rrthomas/lua-mode/
