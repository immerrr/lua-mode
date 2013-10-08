(require 'cl)
(require 'ert)
(require 'lua-font-lock-test-helpers
         ;; let's try a bit to help Emacs find the helpers, just in case
         (concat (file-name-directory (or load-file-name (buffer-file-name)
                                          default-directory))
                 "lua-font-lock-test-helpers.el"))

(ert-deftest lua-indentation-assignment ()
  (should-lua-indent '("foo = 10"
                       ""
                       "bar = 20"))
  (should-lua-indent '("foo"
                       "   = 10"
                       ""
                       "bar = 20"))
  (should-lua-indent '("foo ="
                       "   10"
                       ""
                       "bar = 20"))
  (should-lua-indent '("foo, baz = 10, 20"
                       ""
                       "bar = 20"))
  ;; FIXME: implement comma-indentation
  ;; (should-lua-indent '("foo,"
  ;;                      "   baz = 10, 20"
  ;;                      ""
  ;;                      "bar = 20"))
  (should-lua-indent '("foo, baz"
                       "   = 10, 20"
                       ""
                       "bar = 20"))
  (should-lua-indent '("foo, baz = "
                       "   10, 20"
                       ""
                       "bar = 20"))
  ;; FIXME: implement comma-indentation
  ;; (should-lua-indent '("foo, baz = 10,"
  ;;                      "   20"
  ;;                      ""
  ;;                      "bar = 20"))
  ;; (should-lua-indent '("foo, baz = "
  ;;                      "   10,"
  ;;                      "   20"
  ;;                      ""
  ;;                      "bar = 20"))
  (should-lua-indent '("local"
                       "   x = 5")))

(ert-deftest lua-indentation-test-issue33 ()
  (should-lua-indent
   '("a ="
     "   {"
     "   }"
     ""
     "b ="
     "   {"
     "   },"
     ""
     ""
     "a = {"
     "   table_elt_indented"
     "}"
     ""
     "a = a +"
     "   5 +"
     "   10"
     ""
     "this_should_be_unindented()"
     ""
     "-- here foobar should be indented as simple continuation statement"
     "a = a +"
     "   dosmth("
     "   ) +"
     "   foobar"
     ""
     "a ="
     "   do_smth("
     "      do_smth_arg"
     "   )"
     ""
     "b ="
     "   {"
     "      table_elt0_indented,"
     "      table_elt1_indented"
     "   }"
     ""
     "this_should_be_unindented_too ="
     "   {"
     "   }"
     ""
     "this_should_be_unindented_three = etc")))


(ert-deftest lua-indentation-dot-and-colon-continuation ()
  (should-lua-indent '("foo"
                       "   .bar:baz(xyz)"))
  (should-lua-indent '("foo."
                       "   bar:baz(xyz)"))
  (should-lua-indent '("foo.bar"
                       "   :baz(xyz)"))
  (should-lua-indent '("foo.bar:"
                       "   baz(xyz)"))
  (should-lua-indent '("foo.bar"
                       "   .baz"
                       "   .qux"
                       "   :quux(xyz)")))


(ert-deftest lua-indentation-binop-continuation ()
  (let ((binops '("+"  "-"  "*"  "/"  "^"  "%"  ".."
                 "<"  "<="  ">"  ">="  "=="  "~="
                 "and"  "or")))
    (cl-dolist (binop binops)
      (should-lua-indent `(,(concat "a = foo " binop)
                           "   bar"))
      (should-lua-indent `("a = foo "
                           ,(mapconcat 'identity '("   " " bar") binop))))))

(ert-deftest lua-indentation-return-continuation ()
  (should-lua-indent '("return"
                       "   123"))
  (should-lua-indent '("do"
                       "   return"
                       "      123"
                       "end"))
  (should-lua-indent '("do"
                       "   return"
                       "      x +"
                       "      y"
                       "end"))

  ;; make sure block-end tokens forbid continuation
  (should-lua-indent '("do"
                       "   return"
                       "end"
                       ""
                       "foo = bar"))
  (should-lua-indent '("if foo == bar then"
                       "   return"
                       "else"
                       "   foo = bar"
                       "end"))
  (should-lua-indent '("if foo > bar then"
                       "   return"
                       "elseif foo ~= bar then"
                       "   foo = bar"
                       "end"))
  (should-lua-indent '("repeat"
                       "   return"
                       "until foo == bar"
                       ""
                       "foo = bar")))


(ert-deftest lua-indentation-blocks ()
  ;; FIXME: test split block-intro indentations
  (should-lua-indent '("do"
                       "   a = a + 1"
                       "end"
                       ""
                       "a = 0"))

  (should-lua-indent '("while foo do"
                       "   a = a + 1"
                       "end"
                       ""
                       "a = 0"))

  (should-lua-indent '("repeat"
                       "   a = a + 1"
                       "until not foo"
                       ""
                       "a = 0"))

  (should-lua-indent '("for foo in pairs(bar) do"
                       "   a = a + 1"
                       "end"
                       ""
                       "a = 0"))

  (should-lua-indent '("for y = 0, 10 do"
                       "   a = a + 1"
                       "end"
                       ""
                       "a = 0")))

(ert-deftest lua-indentation-functioncall ()
  ;; FIXME: add
  ;; FIXME: test bare tablector variant
  ;; FIXME: test interaction with continuation
  )

(ert-deftest lua-indentation-conditional ()
  ;; 	 if exp then block {elseif exp then block} [else block] end
  ;; FIXME: add
  )

(ert-deftest lua-indentation-defun ()
  ;; 	 [local] function funcname funcbody
  ;; FIXME: add
  )

(ert-deftest lua-indentation-alignment ()
  ;; FIXME: add
  )

(ert-deftest lua-indentation-tablector ()
  ;; FIXME: add
  )

(ert-deftest lua-indentation-continuation-spans-over-empty-lines ()
  ;; FIXME: add
  ;; FIXME: check comment-only lines too
  )


(ert-deftest lua-indentation-keywords-with-special-characters ()
  (should-lua-indent '("do"
                       "   foobar = _do"
                       "end")))
