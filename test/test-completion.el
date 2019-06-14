;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc) -*-
(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "utils.el") nil 'nomessage 'nosuffix)
(require 'cl-lib)

(describe "Test lua-complete-function"
  (it "completes top-level globals"
    (with-lua-buffer
     (insert "tabl")
     (run-lua)
     (completion-at-point)
     (expect (buffer-string) :to-equal "table")))
  (it "completes nested globals"
    (with-lua-buffer
     (insert "table.ins")
     (run-lua)
     (completion-at-point)
     (expect (buffer-string) :to-equal "table.insert")))
  (it "completes nested globals with spaces"
    (with-lua-buffer
     (insert "table. ins")
     (run-lua)
     (completion-at-point)
     (expect (buffer-string) :to-equal "table. insert")))
  (it "completes nested globals inside function call"
    (with-lua-buffer
     (insert "print(table.con)")
     (backward-char)
     (run-lua)
     (completion-at-point)
     (expect (buffer-string) :to-equal "print(table.concat)")))
  (it "completes nested globals with newline"
    (with-lua-buffer
     (insert "table.\n  ins")
     (run-lua)
     (completion-at-point)
     (expect (buffer-string) :to-equal "table.\n  insert"))))

(describe "Test lua-complete-function with lua-local-require-regexp"
  :var(libs locals comp-lib comp-lib-value comp-lib-mult)

  (before-all
   (with-lua-buffer
    (insert "local xyz = require('file')\n")
    (setq lua-local-require-completions t
	  libs (lua-local-libs)
	  locals (lua-top-level-locals (mapcar 'car libs)))
    (run-lua)
    (with-current-buffer lua-process-buffer
      (setq comp-lib (sort (lua--get-completions "xy" libs locals) #'string-lessp)
	    comp-lib-value (lua--get-completions "xyz.ab" libs locals)
	    comp-lib-mult (sort (lua--get-completions "xyz." libs locals) #'string-lessp)))))

  (it "initializes libs and locals correctly"
      (expect libs :to-equal '(("xyz" "file")))
      (expect locals :to-be nil))

  (it "completes locally-required libraries"
      (expect comp-lib :to-equal '("xyz" "xyz.abc" "xyz.def")))
      
  (it "completes single value from locally-required libraries"
      (expect comp-lib-value :to-equal '("xyz.abc")))
      
  (it "completes multiple values from locally-required libraries"
      (expect comp-lib-mult :to-equal '("xyz.abc" "xyz.def"))))
