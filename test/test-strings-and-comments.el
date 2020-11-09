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
   :it (format "returns %s for %S"
               (if expected-result (format "truthy/%S" expected-result) "nil")  strings)
   (with-lua-buffer
    (lua-insert-goto-<> strings)
    (expect (lua-comment-or-string-start-pos)
            :to-equal expected-result)
    (if expected-result
        (expect (lua-comment-or-string-p) :to-be-truthy)
      (expect (lua-comment-or-string-p) :not :to-be-truthy)))))


(describe "lua-skip-ws-and-comments-backward"
  (describe "doesn't move point"
    (lua--parametrize-tests
     (navigation-spec test-name)
     (("<>" "empty buffer")
      ("<>   --[[]]foo" "at beginning of non-empty buffer")
      ("   f<>oo" "in the middle of variable")
      ("   foo<>" "at the end of variable")
      ("   foo<>--" "between variable and comment")
      ("   foo 'bar'<>" "at the end of single-quote string literal")
      ("   foo [[bar]]<>" "at the end of multi-line string literal")
      ("   foo '<>bar'" "inside string literal")
      ("   foo (<>bar)" "inside function call literal")
      ("   foo '--   <>  bar'" "within whitespace inside single-line string literal")
      ("   foo [[--   \n<>  bar]]" "within whitespace inside multi-line string literal")
      )
     :it (replace-regexp-in-string "\n" "\\\\n" (format "%s: %S" test-name navigation-spec))
     (expect navigation-spec
             :with-point-at "<"
             :after-executing (lua-skip-ws-and-comments-backward)
             :to-end-up-at ">")))

  (describe "moves point"
    (lua--parametrize-tests
     (navigation-spec test-name)
     (("<2>      <1>" "skip whitespace at the beginning of buffer")
      ("foo<2>     <1>" "skip ws after variable")
      ("foo()<2>     <1>" "skip ws after function call")
      ("foo<2>    \n\t\n<1>" "skip newlines/tabs/spaces after variable")
      ;; single-line comments
      ("foo<2>  --  <1>" "escape single-line comment and skip ws")
      ("foo<2>  -<1>-" "escape single-line comment delimiter")
      ("foo<2>  --  '<1>'" "escape commented out string and skip ws")
      ("foo<2>  --  [[<1>]]" "escape commented out string and skip ws")
      ("foo<2>  --  \n<1>" "skip single-line comment and ws")
      ("foo<2>  --  \n--\n--\n<1>" "skip several single-line comments and ws")
      ;; multi-line
      ("foo<2>  --[[ <1> ]]" "escape multi-line comment and skip ws")
      ("foo<2>  -<1>-[[  ]]" "escape multi-line comment delimiter and skip ws 1")
      ("foo<2>  --<1>[[  ]]" "escape multi-line comment delimiter and skip ws 2")
      ("foo<2>  --[<1>[  ]]" "escape multi-line comment delimiter and skip ws 3")
      ("foo<2>  --[[  ]<1>]" "escape multi-line comment delimiter and skip ws 4")
      ("foo<2>  --[[ \n\n ]]\n\n--[[ ]]<1>" "skip multi-line comments and ws")
      ;; luadoc keywords
      ("foo<2>  --[[ @see foo <1>]]" "escape multi-line comment with luadoc keyword 1")
      ("foo<2>  --[[ @s<1>ee foo ]]" "escape multi-line comment with luadoc keyword 2")
      ("foo<2>  --[[ <1>@see foo ]]" "escape multi-line comment with luadoc keyword 3")
      ("foo<2>  -- @see foo <1>" "escape single-line comment with luadoc keyword 1")
      ("foo<2>  -- @s<1>ee foo " "escape single-line comment with luadoc keyword 2")
      ("foo<2>  -- <1>@see foo " "escape single-line comment with luadoc keyword 3")
      )
     :it (replace-regexp-in-string "\n" "\\\\n" (format "%s: %S" test-name navigation-spec))
     (expect navigation-spec
             :with-point-at "<1>"
             :after-executing (lua-skip-ws-and-comments-backward)
             :to-end-up-at "<2>")))

  (describe "respects limit"
    (lua--parametrize-tests
     (limit navigation-spec test-name)
     ((3 "  <2>   <1>" "respect limit in whitespace")
      (100 "     <2><1>    " "don't move if limit is beyond point")
      (5 "--  <2>   <1>" "respect limit when escaping single-line comment")
      (5 "--[[<2>   <1>]]" "respect limit when escaping multi-line comment")
      (5 "    <2>--   <1>" "respect limit when escaping multi-line comment")
      (5 "    <2>--[[   <1>]]" "respect limit when escaping multi-line comment")

      (5 "--  <2>@see x   <1>" "respect limit when escaping single-line luadoc comment")
      (5 "--[[<2>@see x   <1>]]" "respect limit when escaping multi-line luadoc comment")
      )
     :it (replace-regexp-in-string "\n" "\\\\n" (format "%s: limit=%S %S" test-name limit navigation-spec))
     (expect navigation-spec
             :with-point-at "<1>"
             :after-executing (lua-skip-ws-and-comments-backward limit)
             :to-end-up-at "<2>"))))


(describe "lua-skip-ws-and-comments-forward"
  (describe "doesn't move point"
    (lua--parametrize-tests
     (navigation-spec test-name)
     (("<>" "empty buffer")
      ("   --[[]]<>" "at end of non-empty buffer")
      ("   f<>oo   " "in the middle of variable")
      ("   <>foo   " "at the beginning of variable")
      ("   --[[]]<>foo   " "between variable and comment")
      ("   foo <>'bar'" "at the end of single-quote string literal")
      ("   foo <>[[bar]]" "at the end of multi-line string literal")
      ("   foo 'bar<>'" "inside string literal")
      ("   foo (bar<>)" "inside function call literal")
      ("   foo '--   <>  bar'" "within whitespace inside single-line string literal")
      ("   foo [[--   \n<>\n  bar]]" "within whitespace inside multi-line string literal")
      )
     :it (replace-regexp-in-string "\n" "\\\\n" (format "%s: %S" test-name navigation-spec))
     (expect navigation-spec
             :with-point-at "<"
             :after-executing (lua-skip-ws-and-comments-forward)
             :to-end-up-at ">")))

  (describe "moves point"
    (lua--parametrize-tests
     (navigation-spec test-name)
     (("<1>      <2>" "skip whitespace at the end of buffer")
      ("<1>     <2>bar" "skip ws before variable")
      ("foo<1>  <2>()" "skip ws before function call")
      ("<1>    \n\t\n<2>foo" "skip newlines/tabs/spaces before variable")

      ;; single-line comments
      ("foo  --  <1>\n  <2>bar" "escape single-line comment and skip ws")
      ("foo  -<1>-  \n  <2>bar" "escape single-line comment delimiter")
      ("foo  --  '<1>'  \n  <2>bar" "escape commented out string and skip ws")
      ("foo  --  [[<1>]]  \n  <2>bar" "escape commented out string and skip ws")
      ("foo  <1>--  \n  \n  <2>bar" "skip single-line comment and ws")
      ("foo  <1>--  \n--\n--\n  \n  <2>bar" "skip several single-line comments and ws")
      ;; multi-line
      ("foo  --[[ <1> ]]   <2>bar" "escape multi-line comment and skip ws")
      ("foo  -<1>-[[  ]]   <2>bar" "escape multi-line comment delimiter and skip ws 1")
      ("foo  --<1>[[  ]]   <2>bar" "escape multi-line comment delimiter and skip ws 2")
      ("foo  --[<1>[  ]]   <2>bar" "escape multi-line comment delimiter and skip ws 3")
      ("foo  --[[  ]<1>]   <2>bar" "escape multi-line comment delimiter and skip ws 4")
      ("foo  <1>--[[ \n\n ]]\n\n--[[ ]]   <2>bar" "skip multi-line comments and ws")
      ;; luadoc keywords
      ("foo  --[[ @see foo <1>]]   <2>bar" "escape multi-line comment with luadoc keyword 1")
      ("foo  --[[ @s<1>ee foo ]]   <2>bar" "escape multi-line comment with luadoc keyword 2")
      ("foo  --[[ <1>@see foo ]]   <2>bar" "escape multi-line comment with luadoc keyword 3")
      ("foo  -- @see foo<1> \n   <2>bar" "escape single-line comment with luadoc keyword 1")
      ("foo  -- @s<1>ee foo \n   <2>bar" "escape single-line comment with luadoc keyword 2")
      ("foo  -- <1>@see foo \n   <2>bar" "escape single-line comment with luadoc keyword 3")
      )
     :it (replace-regexp-in-string "\n" "\\\\n" (format "%s: %S" test-name navigation-spec))
     (expect navigation-spec
             :with-point-at "<1>"
             :after-executing (lua-skip-ws-and-comments-forward)
             :to-end-up-at "<2>")))

  (describe "respects limit"
    (lua--parametrize-tests
     (limit navigation-spec test-name)
     ((6 "  <1>   <2>   " "in whitespace")
      (1 "     <2><1>   " "don't move if limit is before point")
      (8 "--  <1>   <2>  \n" "when escaping single-line comment 1")
      (8 "--  <1>  \n<2>  " "when escaping single-line comment 2")
      (8 "--  <1>   <2>\n  " "when escaping single-line comment 3")
      (8 "--[[<1>   <2> ]] \n" "when escaping multi-line comment 1")
      (8 "--[[<1>  ]<2>] \n" "when escaping multi-line comment 1")
      (8 "--[[<1>   <2> ]] \n" "when escaping multi-line comment 1")

      (7 "--  <1>@s<2>ee x   " "when escaping single-line luadoc comment")
      (8 "--  <1>@se<2>e x   " "when escaping single-line luadoc comment")
      (9 "--  <1>@see<2> x   " "when escaping single-line luadoc comment")
      (7 "--[[<1>@s<2>ee x]] " "when escaping multi-line luadoc comment")
      (8 "--[[<1>@se<2>e x]] " "when escaping multi-line luadoc comment")
      (9 "--[[<1>@see<2> x]] " "when escaping multi-line luadoc comment")
      )
     :it (replace-regexp-in-string "\n" "\\\\n" (format "%s: limit=%S %S" test-name limit navigation-spec))
     (expect navigation-spec
             :with-point-at "<1>"
             :after-executing (lua-skip-ws-and-comments-forward limit)
             :to-end-up-at "<2>"))))

(describe "lua-find-regexp"
  (it "does not match open-bracket that is part of multiline string opener: forward"
    (with-lua-buffer
     (lua-insert-goto-<> '("<>foo = [[ bar ]]"))
     (expect (lua-find-regexp 'forward "\\[") :not :to-be-truthy)))

  (it "does not match open-bracket that is part of multiline string opener: backward"
    (with-lua-buffer
     (lua-insert-goto-<> '("foo = [[ bar ]]<>"))
     (expect (lua-find-regexp 'backward "\\[") :not :to-be-truthy)))

  (it "does not match close-bracket that is part of multiline string closer: forward"
    (with-lua-buffer
     (lua-insert-goto-<> '("<>foo = [[ bar ]]"))
     (expect (lua-find-regexp 'forward "]") :not :to-be-truthy)))

  (it "does not match close-bracket that is part of multiline string closer: backward"
    (with-lua-buffer
     (lua-insert-goto-<> '("<>foo = [[ bar ]]"))
     (expect (lua-find-regexp 'backward "]") :not :to-be-truthy)))

  (it "does not match minus that is part of comment starter: forward"
    (with-lua-buffer
     (lua-insert-goto-<> '("<>foo = [[ bar ]] -- baz"))
     (expect (lua-find-regexp 'forward "-") :not :to-be-truthy)))

  (it "does not match minus that is part of comment starter: backward"
    (with-lua-buffer
     (lua-insert-goto-<> '("<>foo = [[ bar ]] -- baz"))
     (expect (lua-find-regexp 'backward "-") :not :to-be-truthy))))
