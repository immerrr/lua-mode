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
  :var(libs locals)

  (before-all
    (with-lua-buffer
     (insert "local xyz = require('file')\n")
     (let ((lua-local-require-completions t))
       (setq libs (lua-local-libs)
             locals (lua-top-level-locals (mapcar 'car libs))))))

  (before-each
    (lua-kill-process))

  (it "initializes libs and locals correctly"
    (expect libs :to-equal '(("xyz" "'file'")))
    (expect locals :to-be nil))

  (it "completes locally-required libraries"
    (expect (lua--get-completions "xy" libs locals)
            :to-equal '("xyz")))

  (it "completes single value from locally-required libraries"
    (expect (lua--get-completions "xyz.ab" libs locals)
            :to-equal '("abc")))

  (it "completes multiple values from locally-required libraries"
    (expect (sort (lua--get-completions "xyz." libs locals) #'string-lessp)
            :to-equal '("abc" "def"))))
