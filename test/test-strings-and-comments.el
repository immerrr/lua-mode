;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc) -*-

(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "utils.el") nil 'nomessage 'nosuffix)


(describe "Test indent-new-comment-line"
  (it "works with -- ..."
    (expect (lua-buffer-strs
             (lua-insert-goto-<> '("-- foobar <>"))
             (execute-kbd-macro (kbd "M-j")))
            :to-equal
            '("-- foobar"
              "-- "))

    (expect (lua-buffer-strs
             (lua-insert-goto-<> '("xyzzy -- foobar <>"))
             (execute-kbd-macro (kbd "M-j")))
            :to-equal
            '("xyzzy -- foobar"
              "-- "))

    (expect (lua-buffer-strs
             (lua-insert-goto-<> '("xyz<> xyzzy -- foobar"))
             (execute-kbd-macro (kbd "M-j")))
            :to-equal
            '("xyz"
              "xyzzy -- foobar")))


  (it "works with ---- ...."
    (expect (lua-buffer-strs
             (lua-insert-goto-<> '("---- foobar <>"))
             (execute-kbd-macro (kbd "M-j")))
            :to-equal
            '("---- foobar"
              "---- "))

    (expect (lua-buffer-strs
             (lua-insert-goto-<> '("xyzzy ---- foobar <>"))
             (execute-kbd-macro (kbd "M-j")))
            :to-equal
            '("xyzzy ---- foobar"
              "---- ")))

  (it "doesn't recognize \"--\" inside strings and comments"
    (expect (lua-buffer-strs
             (lua-insert-goto-<> '("\"-- \" .. foobar <>"))
             (execute-kbd-macro (kbd "M-j")))
            :to-equal
            '("\"-- \" .. foobar"
              ""))

    (expect (lua-buffer-strs
             (lua-insert-goto-<> '("'-- ' .. foobar <>"))
             (execute-kbd-macro (kbd "M-j")))
            :to-equal
            '("'-- ' .. foobar"
              ""))

    (expect (lua-buffer-strs
             (lua-insert-goto-<> '("[[-- ]] .. foobar <>"))
             (execute-kbd-macro (kbd "M-j")))
            :to-equal
            '("[[-- ]] .. foobar"
              ""))

    (expect (lua-buffer-strs
             (lua-insert-goto-<> '("--[[-- ]] .. foobar <>"))
             (execute-kbd-macro (kbd "M-j")))
            :to-equal
            '("--[[-- ]] .. foobar"
              ""))

    (expect (lua-buffer-strs
             (lua-insert-goto-<> '("---[[-- ]] .. foobar <>"))
             (execute-kbd-macro (kbd "M-j")))
            :to-equal
            '("---[[-- ]] .. foobar"
              "---")))

  (it "works when the comment is empty"
    (expect (lua-buffer-strs
             (lua-insert-goto-<> '("-- <>"))
             (execute-kbd-macro (kbd "M-j")))
            :to-equal
            '("--"
              "--"))

    ;; Let's make sure that whitespace is optional.
    (expect (lua-buffer-strs
             (lua-insert-goto-<> '("--<>"))
             (execute-kbd-macro (kbd "M-j")))
            :to-equal
            '("--"
              "--"))))

(describe "lua-comment-start-pos"
  ;; Single-line comments
  (it "returns beginning of single-line comment if inside"
    (with-lua-buffer
     (lua-insert-goto-<> '("--   <>"))
     (expect (lua-comment-start-pos)
             :to-equal 1)))

  (it "returns beginning of single-line comment if between delimiters"
    (with-lua-buffer
     (lua-insert-goto-<> '("-<>-   "))
     (expect (lua-comment-start-pos)
             :to-equal 1)))

  (it "returns nil if before delimiters"
    (with-lua-buffer
     (lua-insert-goto-<> '("<>--   "))
     (expect (lua-comment-start-pos)
             :to-equal nil)))

  (it "returns nil if before single-line comment"
    (with-lua-buffer
     (lua-insert-goto-<> '("<>"
                           "--   "))
     (expect (lua-comment-start-pos)
             :to-equal nil)))

  (it "returns nil if after single-line comment"
    (with-lua-buffer
     (lua-insert-goto-<> '("--"
                           "<>"))
     (expect (lua-comment-start-pos)
             :to-equal nil)))

  ;; Single-line comments + strings
  (it "returns nil if inside single-line string"
    (with-lua-buffer
     (lua-insert-goto-<> '("'--<>'"))
     (expect (lua-comment-start-pos)
             :to-equal nil)))

  (it "returns nil if inside multi-line string"
    (with-lua-buffer
     (lua-insert-goto-<> '("[[--<>]]"))
     (expect (lua-comment-start-pos)
             :to-equal nil)))

  ;; Multi-line comments
  (it "returns beginning of multi-line comment if inside 1"
    (with-lua-buffer
     (lua-insert-goto-<> '("--[[<>   ]]"))
     (expect (lua-comment-start-pos)
             :to-equal 1)))

  (it "returns beginning of multi-line comment if inside 2"
    (with-lua-buffer
     (lua-insert-goto-<> '("--[[<>"
                           "]]"))
     (expect (lua-comment-start-pos)
             :to-equal 1)))

  (it "returns beginning of multi-line comment if inside 3"
    (with-lua-buffer
     (lua-insert-goto-<> '("--[["
                           "<>]]"))
     (expect (lua-comment-start-pos)
             :to-equal 1)))

  (it "returns beginning of multi-line comment if between delimiters 1"
    (with-lua-buffer
     (lua-insert-goto-<> '("-<>-[[   ]]"))
     (expect (lua-comment-start-pos)
             :to-equal 1)))

  (it "returns beginning of single-line comment if between delimiters 2"
    (with-lua-buffer
     (lua-insert-goto-<> '("--<>[[   ]]"))
     (expect (lua-comment-start-pos)
             :to-equal 1)))

  (it "returns beginning of multi-line comment if between delimiters 3"
    (with-lua-buffer
     (lua-insert-goto-<> '("--[<>[  ]]"))
     (expect (lua-comment-start-pos)
             :to-equal 1)))

  (it "returns beginning of multi-line comment if between delimiters 4"
    (with-lua-buffer
     (lua-insert-goto-<> '("--[=<>[  ]]"))
     (expect (lua-comment-start-pos)
             :to-equal 1)))

  (it "returns beginning of multi-line comment if between delimiters 5"
    (with-lua-buffer
     (lua-insert-goto-<> '("--[[  ]<>]"))
     (expect (lua-comment-start-pos)
             :to-equal 1)))

  (it "returns nil if before multi-line opener"
    (with-lua-buffer
     (lua-insert-goto-<> '("<>--[[   ]]"))
     (expect (lua-comment-start-pos)
             :to-equal nil)))

  (it "returns nil if after multi-line closer"
    (with-lua-buffer
     (lua-insert-goto-<> '("--[[   ]]<>"))
     (expect (lua-comment-start-pos)
             :to-equal nil)))

  (it "returns beginning of multi-line comment if after multi-line closer with different opener"
    (with-lua-buffer
     (lua-insert-goto-<> '("--[==[   ]]<>"))
     (expect (lua-comment-start-pos)
             :to-equal 1)))

  ;; Multi-line comments with strings
  (it "returns nil if multi-line opener is inside string 1"
    (with-lua-buffer
     (lua-insert-goto-<> '("'--[['   <>]]"))
     (expect (lua-comment-start-pos)
             :to-equal nil)))

  (it "returns nil if multi-line opener is inside string 2"
    (with-lua-buffer
     (lua-insert-goto-<> '("'--[['   ]]<>"))
     (expect (lua-comment-start-pos)
             :to-equal nil)))

  (it "returns nil if multi-line opener is inside multi-line string 1"
    (with-lua-buffer
     (lua-insert-goto-<> '("[[--[[]]   <>]]"))
     (expect (lua-comment-start-pos)
             :to-equal nil)))

  (it "returns nil if multi-line opener is inside multi-line string 2"
    (with-lua-buffer
     (lua-insert-goto-<> '("[[--[[]]   ]]<>"))
     (expect (lua-comment-start-pos)
             :to-equal nil))))

(defmacro lua--parametrize-tests (&rest args)
  (pcase args
    (`(,variables ,param-values :it ,description-form . ,body)
     `(progn
        ,@(cl-loop
          for params in param-values
          for let-bindings = (cl-loop for var in variables
                                      for param in params
                                      collect `(,var (quote ,param)))
          for description = (eval `(let ,let-bindings ,description-form))
          for test-body = `(let ,let-bindings ,@body)
          collect
          (macroexpand `(it ,description ,test-body)))))))

(describe "lua-comment-or-string-start-p/-pos"
  (lua--parametrize-tests
   (strings expected-result)
   (;; single-line strings: single-quote
    (("<>'foo'") nil)
    (("'<>foo'") 1)
    (("'foo<>'") 1)
    (("'foo'<>") nil)

    ;; single-line strings: double-quote
    (("<>\"foo\"") nil)
    (("\"<>foo\"") 1)
    (("\"foo<>\"") 1)
    (("\"foo\"<>") nil)

    ;; multi-line strings
    (("<>[[foo]]") nil)
    (("[[<>foo]]") 1)
    (("[<>[foo]]") 1)
    (("[=<>[foo]=]") 1)
    (("[<>=[foo]=]") 1)
    (("[[foo<>]]") 1)
    (("[[foo]<>]") 1)
    (("[[foo]<>=]") 1)
    (("[[foo]=<>]") 1)
    (("[[foo]]<>") nil)

    ;; single-line comments
    (("foo <>-- bar") nil)
    (("foo -<>- bar") 5)
    (("foo --<> bar") 5)
    (("foo -- <>bar") 5)
    (("foo -- bar<>") 5)

    ;; multi-line comments
    (("foo <>--[[ bar ]]") nil)
    (("foo -<>-[[ bar ]]") 5)
    (("foo --<>[[ bar ]]") 5)
    (("foo --[<>[ bar ]]") 5)
    (("foo --[[<> bar ]]") 5)
    (("foo --[[ bar <>]]") 5)
    (("foo --[[ bar ]<>]") 5)
    (("foo --[[ bar ]]<>") nil)
    (("foo --[==[ bar ]]<>") 5)

    ;; single-line comment containing multi-line comment
    (("foo <>---[[ bar ]]") nil)
    (("foo --<>-[[ bar ]]") 5)
    (("foo ---<>[[ bar ]]") 5)
    (("foo ---[<>[ bar ]]") 5)
    (("foo ---[[<> bar ]]") 5)
    (("foo ---[[ bar ]]<>") 5)

    ;; multi-line comment containing single-line comment
    (("foo --[[ -- bar ]]<>") nil)

    ;; string containing multi-line comment opener
    (("foo '--[[' <> bar ]]") nil)
    (("foo [[--[[]] <> bar ]]") nil)
    (("foo [[--[==[]] <> bar ]==]") nil)

    ;; single dash: not a comment
    (("foo = bar -<> baz") nil)
    (("foo = bar <>- baz") nil))
   :it (format "returns %s for %S" (if expected-result (format "truthy/%S" expected-result) "nil")  strings)
   (with-lua-buffer
    (lua-insert-goto-<> strings)
    (expect (lua-comment-or-string-start-pos)
            :to-equal expected-result)
    (if expected-result
        (expect (lua-comment-or-string-p) :to-be-truthy)
      (expect (lua-comment-or-string-p) :not :to-be-truthy)))))
