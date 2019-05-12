;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc) -*-
(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "utils.el") nil 'nomessage 'nosuffix)
(require 'cl-lib)

(describe "Test lua-complete-function"
  (it "completes top-level globals"
    (with-lua-buffer
     (insert "tabl")
     (lua-get-create-process)
     (completion-at-point)
     (expect (buffer-string) :to-equal "table")))
  (it "completes nested globals"
    (with-lua-buffer
     (insert "table.ins")
     (lua-get-create-process)
     (completion-at-point)
     (expect (buffer-string) :to-equal "table.insert")))
  (it "completes nested globals with spaces"
    (with-lua-buffer
     (insert "table. ins")
     (lua-get-create-process)
     (completion-at-point)
     (expect (buffer-string) :to-equal "table. insert")))
    (it "completes nested globals inside function call"
    (with-lua-buffer
     (insert "print(table.con)")
     (backward-char)
     (lua-get-create-process)
     (completion-at-point)
     (expect (buffer-string) :to-equal "print(table.concat)")))
  (it "completes nested globals with newline"
    (with-lua-buffer
     (insert "table.\n  ins")
     (lua-get-create-process)
     (completion-at-point)
     (expect (buffer-string) :to-equal "table.\n  insert"))))

(describe "Test lua-complete-function with lua-local-require-regexp"
  (it "completes locally-required libraries"
    (with-lua-buffer
     (insert "local xyz = require('file')\n")
     (insert "xy")
     (let ((lua-local-require-completions t))
       (lua-get-create-process)
       (completion-at-point))
     (expect (thing-at-point 'line) :to-equal "xyz")))
  (it "completes values nested in locally-required libraries"
    (with-lua-buffer
     (insert "local xyz = require('file')\n")
     (insert "xyz.ab")
     (let ((lua-local-require-completions t))
       (message "ABOUT TO RUN LUA!!!")
       (lua-get-create-process)
       (message "ABOUT TO COMPLETE!!!")
       (completion-at-point)		;should wait
       (with-current-buffer lua-shell-output-buffer
	 (message (concat "*lua* output buffer: " (buffer-string)))))
     (expect (thing-at-point 'line) :to-equal "xyz.abc"))))
