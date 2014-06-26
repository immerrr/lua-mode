(require 'cl-lib)
(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "test-helper.el") nil 'nomessage 'nosuffix)


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
foo123
   .bar:baz(xyz)")
  (should-lua-indent "\
foo123.
   bar:baz(xyz)")
  (should-lua-indent "\
foo123.bar
   :baz(xyz)")
  (should-lua-indent "\
foo123.bar:
   baz(xyz)")
  (should-lua-indent "\
foo123.bar
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


(ert-deftest lua-indentation-ellipsis ()
  (should-lua-indent "\
function x(...)
   a, b = 1, ...
   return b
end"))


(ert-deftest lua-indentation-unop-continuation ()
  :expected-result :failed
  (should-lua-indent "\
foo = bar
   -#some_str")

  (cl-dolist (unop '("-" "#" "not "))
    (should-lua-indent (replace-regexp-in-string "UNOP" unop  "\
foobar(qux,
       UNOPquux)" 'fixedcase))
    (should-lua-indent (replace-regexp-in-string "UNOP" unop "\
foobar(qux, xyzzy
          UNOPquux)" 'fixedcase))
    (should-lua-indent (replace-regexp-in-string "UNOP" unop "\
foobar(
   UNOPquux)" 'fixedcase))
    (should-lua-indent (replace-regexp-in-string "UNOP" unop "\
x = {qux,
     UNOPquux}" 'fixedcase))
    (should-lua-indent (replace-regexp-in-string "UNOP" unop "\
x = {qux;
     UNOPquux}" 'fixedcase))
    (should-lua-indent (replace-regexp-in-string "UNOP" unop "\
x = {qux, xyzzy
        UNOPquux}" 'fixedcase))
    (should-lua-indent (replace-regexp-in-string "UNOP" unop "\
x = {
   UNOPquux
}" 'fixedcase))))



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

(ert-deftest lua-indentation-labels ()
  (should-lua-indent "\
::foo::")

  (should-lua-indent "\
::foo::
::bar::

::baz::")

  ;; The following tests come from the LuaWiki goto page:
  ;; http://lua-users.org/wiki/GotoStatement
  (should-lua-indent "\
for z=1,10 do
   for y=1,10 do
      for x=1,10 do
         if x^2 + y^2 == z^2 then
            print('found a Pythagorean triple:', x, y, z)
            goto done
            goto done2
         end
      end
      ::done2::
   end
end

::done::")

  (should-lua-indent "\
-- 5.2.0-beta-rc2
for z=1,10 do
   for y=1,10 do
      for x=1,10 do
         if x^2 + y^2 == z^2 then
            print('found a Pythagorean triple:', x, y, z)
            print('now trying next z...')
            goto zcontinue
         end
      end
   end
   ::zcontinue::
end")

  (should-lua-indent "\
-- Lua 5.2.0-beta-rc2
for x=1,5 do ::redo::
   print(x .. ' + 1 = ?')
   local y = tonumber(io.read'*l')
   if y ~= x + 1 then goto redo end
end")

  (should-lua-indent "\
-- 5.2.0-beta-rc1
::a::
print 'A'
if math.random() < 0.3 then goto c end
::b::
print 'B'
if math.random() < 0.5 then goto a end
::c::
print 'C'
if math.random() < 0.1 then goto a else goto b end
")

  (should-lua-indent "\
-- 5.2.0-beta-rc2 - factorial with tail recursion simulated with goto's
-- (warning: there's no need to do this)
function fact_(n, ans)
   ::call::
   if n == 0 then
      return ans
   else
      n, ans = n - 1, ans * n
      goto call
   end
end
print(fact_(5, 1)) --> 120")

  (should-lua-indent "\
-- 5.2.0-beta-rc2
function f()
   if not g() then goto fail end
   if not h() then goto cleanup_g end
   if not i() then goto cleanup_h end
   do return true end    -- need do/end?

   ::cleanup_h::
   undo_h()
   ::cleanup_g::
   undo_g()
   ::fail::
   return false
end")

  (should-lua-indent "\
-- 5.2.0-beta-rc2
::redo:: 
for x=1,10 do
   for y=1,10 do
      if not f(x,y) then goto continue end
      if not g(x,y) then goto skip end
      if not h(x,y) then goto redo end
      ::continue::
   end
end ::skip::
print('foo')"))

(ert-deftest lua-indentation-indent-labels-inside-multiline-string ()
  :expected-result :failed
  ;; TODO: It seems that `should-lua-indent' tries to indent text
  ;; inside the "[[ ]]" multiline string?
  (should-lua-indent "\
local b, bt = [[
      if not x then goto a end
           f()
      goto b; ::a::
           g()
         ::b::
]]"))
