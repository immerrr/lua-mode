(require 'ert)
(require 'lua-font-lock-test-helpers
         ;; let's try a bit to help Emacs find the helpers, just in case
         (concat (file-name-directory (or load-file-name (buffer-file-name)
                                          default-directory))
                 "lua-font-lock-test-helpers.el"))


(ert-deftest lua-font-lock-builtins ()
  (should-lua-font-lock-equal
   "\
table.sort(foobar)
   table.sort(foobar)
   table  .sort(foobar)
   table.  sort(foobar)"
   '(("table" builtin "sort" builtin)
     ("table" builtin "sort" builtin)
     ("table" builtin "sort" builtin)
     ("table" builtin "sort" builtin)))

  (should-lua-font-lock-equal
   ;; Neither of these should be highlighted, thing that looks like a builtin
   ;; is in fact a member of some user table.
   "\
foo.table.sort(foobar)
foo.  table.sort(foobar)
foo  .table.sort(foobar)
foo:table.sort(foobar)
foo:  table.sort(foobar)
foo  :table.sort(foobar)

_table.sort(foobar)
   table_.sort(foobar)"
   '(nil nil nil nil nil nil nil nil nil))

  (should-lua-font-lock-equal
   "\
   table  ._sort(foobar)
   table.  sort_(foobar)"
   '(("table" builtin) ("table" builtin)))

  (should-lua-font-lock-equal
   ;; After concatenation operator builtins should be highlighted too.
   "a .. table.concat(foobar, delim)"
   '(("table" builtin "concat" builtin))))


(ert-deftest lua-font-lock-builtin-constants()
  (should-lua-font-lock-equal
   "a = { nil, true, false}"
   '(("nil" constant "true" constant "false" constant)))

  ;; Hint user that builtin constants cannot be used like that
  (should-lua-font-lock-equal
   "a = { foo.true, foo:false }"
   '(("true" constant "false" constant))))


(ert-deftest lua-font-lock-keywords ()
  (should-lua-font-lock-equal
   "do foo(5) end"
   '(("do" keyword "end" keyword)))

  (should-lua-font-lock-equal
   "_do foo(5) end_"
   '(nil))

  ;; Hint user that keywords cannot be used like that
  (should-lua-font-lock-equal
   "do foo(5).end end"
   '(("do" keyword "end" keyword "end" keyword)))
  (should-lua-font-lock-equal
   "do foo(5):end end"
   '(("do" keyword "end" keyword "end" keyword))))
