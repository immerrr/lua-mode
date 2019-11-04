#!/bin/bash

if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
    if ! [[ "$EVM_EMACS" == "emacs-26.1-travis" ]]; then
        # FIXME: implement EVM_EMACS version translation.
        echo "Only Emacs-26.1 is supported for OSX builds"
        exit 1
    fi
    set -x
    curl -fsSL https://emacsformacosx.com/emacs-builds/Emacs-26.1-universal.dmg -o /tmp/Emacs-26.1-universal.dmg &&
        hdiutil attach /tmp/Emacs-26.1-universal.dmg &&
        mkdir -p "$HOME/bin" &&
        ln -s /Volumes/Emacs/Emacs.app/Contents/MacOS/Emacs "$HOME/bin/emacs" &&
        export PATH="$HOME/bin:$PATH" &&
        emacs --version &&
        curl -fsSL https://raw.github.com/cask/cask/master/go -o /tmp/cask-bootstrap.py &&
        python /tmp/cask-bootstrap.py &&
        export PATH="$HOME/.cask/bin:$PATH"
    set +x
else
    emacs --version
    curl -fsSkL https://gist.github.com/rejeep/ebcd57c3af83b049833b/raw > travis.sh && source ./travis.sh
    evm install "$EVM_EMACS" --use --skip
fi
