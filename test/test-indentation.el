(require 'cl-lib)
(require 'ert)
(require 'lua-font-lock-test-helpers
         ;; let's try a bit to help Emacs find the helpers, just in case
         (concat (file-name-directory (or load-file-name (buffer-file-name)
                                          default-directory))
                 "lua-font-lock-test-helpers.el"))

(ert-deftest lua-indentation-assignment ()
  (should-lua-indent "\
foo = 10

bar = 20")

  (should-lua-indent "\
foo
   = 10

bar = 20")

  (should-lua-indent "\
foo =
   10
bar = 20"))


;; (ert-deftest lua-indentation-assignment-with-commas ()
;;   (should-lua-indent "\
;; foo,
;;    baz = 10, 20

;; bar = 20")
;;    (should-lua-indent "\
;; foo, baz
;;    = 10, 20

;; bar = 20")

;;    (should-lua-indent "\
;; foo, baz = 10,
;;    20

;; bar = 20")

;;    (should-lua-indent "\
;; foo,
;;    baz =
;;    10, 20")

;;    (should-lua-indent "\
;; foo, baz =
;;    10,
;;    20

;; bar = 20")

;;    (should-lua-indent "\
;; local
;;    x = 5")

;;    (should-lua-indent "\
;; local
;;    x,
;;    y = 10, 20")

;;    (should-lua-indent "\
;; local
;;    x,
;;    y =
;;    10,
;;    20"))

(ert-deftest lua-indentation-test-issue33 ()
  (should-lua-indent "\
a =
   {
   }

b =
   {
   },


a = {
   table_elt_indented
}

a = a +
   5 +
   10

this_should_be_unindented()

-- here foobar should be indented as simple continuation statement
a = a +
   dosmth(
   ) +
   foobar

a =
   do_smth(
      do_smth_arg
   )

b =
   {
      table_elt0_indented,
      table_elt1_indented
   }

this_should_be_unindented_too =
   {
   }

this_should_be_unindented_three = etc"))


(ert-deftest lua-indentation-dot-and-colon-continuation ()
  (should-lua-indent "\
foo
   .bar:baz(xyz)")
  (should-lua-indent "\
foo.
   bar:baz(xyz)")
  (should-lua-indent "\
foo.bar
   :baz(xyz)")
  (should-lua-indent "\
foo.bar:
   baz(xyz)")
  (should-lua-indent "\
foo.bar
   .baz
   .qux
   :quux(xyz)"))


(ert-deftest lua-indentation-binop-continuation ()
  (let ((binops '("+"  "-"  "*"  "/"  "^"  "%"  ".."
                  "<"  "<="  ">"  ">="  "=="  "~="
                  "and"  "or")))
    (cl-dolist (binop binops)
      (should-lua-indent (replace-regexp-in-string "BINOP" binop "\
a = foo BINOP
   bar" 'fixedcase))
      (should-lua-indent (replace-regexp-in-string "BINOP" binop "\
a = foo
   BINOP bar" 'fixedcase)))))


(ert-deftest lua-indentation-return-continuation ()
  (should-lua-indent "\
return
   123")

  (should-lua-indent "\
do
   return
      123
end")

  (should-lua-indent "\
do
   return
      x +
      y
end")

  ;; make sure block-end tokens forbid continuation
  (should-lua-indent "\
do
   return
end

foo = bar")

  (should-lua-indent "\
if foo == bar then
   return
else
   foo = bar
end")

  (should-lua-indent "\
if foo == bar then
   return
elseif foo != bar then
   foo = bar
end")

  (should-lua-indent "\
repeat
   return
until foo == bar"))


(ert-deftest lua-indentation-do-block ()
  ;; FIXME: test split block-intro indentations
  (should-lua-indent "\
do
   a = a + 1
end

a = 0")

  (should-lua-indent "\
do a = a + 1 end

a = 0")

  (should-lua-indent "\
do a = a + 1
end

a = 0"))


(ert-deftest lua-indentation-while-block ()
  (should-lua-indent "\
while foo do
   a = a + 1
end

a = 0")

  (should-lua-indent "\
while foo
do
   a = a + 1
end

a = 0")

  (should-lua-indent "\
while
   foo
do
   a = a + 1
end

a = 0")

  (should-lua-indent "\
while foo do a = a + 1 end

a = 0")

  (should-lua-indent "\
while
   x +
   y > 0
do
   a = a + 1
end

a = 0"))


(ert-deftest lua-indentation-repeat-block ()
  (should-lua-indent "\
repeat
   a = a + 1
until foo

a = 0")

  (should-lua-indent "\
repeat
   a = a + 1
until
   foo

a = 0")

  (should-lua-indent "\
repeat
   a = a + 1
until
   not
   foo

a = 0")

  (should-lua-indent "\
repeat a = a + 1 until not foo

a = 0"))



(ert-deftest lua-indentation-for-block ()
  (should-lua-indent "\
for k, v in pairs(bar) do
   a = a + 1
end

a = 0")

  (should-lua-indent "\
for k, v in pairs(bar)
do
   a = a + 1
end

a = 0")

  (should-lua-indent "\
for k, v in pairs(bar) do a = a + 1 end

a = 0")

  (should-lua-indent "\
for y = 0, 10 do
   a = a + 1
end

a = 0")

  (should-lua-indent "\
for y = 0, 10
do
   a = a + 1
end

a = 0")

  (should-lua-indent "\
for y = 0, 10 do a = a + 1 end

a = 0"))


(ert-deftest lua-indentation-functioncall ()
  (should-lua-indent "\
foobar(
   a, b, c)")

  (should-lua-indent "\
foobar(
   a, b, c
)")

  (should-lua-indent "\
foobar(a,
       b,
       c)")

  (should-lua-indent "\
foobar{
   a, b, c
}"))


(ert-deftest lua-indentation-funcall-with-nested-table ()
  :expected-result :failed
    (should-lua-indent "\
foobar({
   a, b, c
})")

  (should-lua-indent "\
foobar(a, {
   b,
   c
})")

  (should-lua-indent "\
foobar(
   a,
   {
      b,
      c
   })")

  (should-lua-indent "\
foobar(a,
       {
          b,
          c
       })")

  (should-lua-indent "\
foobar(a,
       {
          b,
          c
       }
)")

  (should-lua-indent "\
foobar(
   {
      a,
      b
   },
   c, d
)"))


(ert-deftest lua-indentation-continuation-with-functioncall ()
  (should-lua-indent "\
x = foo(123,
        456)
   + bar(
      qux,
      quux)"))

(ert-deftest lua-indentation-conditional ()
  (should-lua-indent "\
if foo then
   a = a + 1
end

a = 0")

  (should-lua-indent "\
if foo then a = a + 1 end

a = 0")

  (should-lua-indent "\
if foo then
   a = a + 1
else
   a = a + 2
end

a = 0")


  (should-lua-indent "\
if foo then
   a = a + 1
elseif bar then
   a = a + 2
elseif baz then
   a = a + 3
end

a = 0")

  (should-lua-indent "\
if foo then a = a + 1 else
   a = a + 2
end"))

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
  (should-lua-indent "\
do
   foobar = _do
end"))


(ert-deftest lua-indentation-block-intro-continuation ()
  :expected-result :failed
  (should-lua-indent "\
while
   foo do
   a = a + 1
end

a = 0")

  (should-lua-indent "\
for k, v
   in pairs(bar) do a = a + 1 end

a = 0")

  (should-lua-indent "\
for k, v
   in pairs(bar) do a = a + 1 end

a = 0")

  )
