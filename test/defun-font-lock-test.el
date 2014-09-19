(require 'ert)
(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "test-helper.el") nil 'nomessage 'nosuffix)


(ert-deftest lua-font-lock-defuns ()
  (should-lua-font-lock-equal
   ;; Let's start with some basic stuff
   "function foo() end"
   '(("function" keyword "foo" function-name "end" keyword)))

  (should-lua-font-lock-equal
   ;; Check all defun variants, check embedded defuns
   "\
function foo()
  function bar() end
  local function baz() end
  qux = function() end
  local quux = function() end
end"
   '(("function" keyword "foo" function-name)
     ("function" keyword "bar" function-name "end" keyword)
     ("local" keyword "function" keyword "baz" function-name "end" keyword)
     ("qux" function-name "function" keyword "end" keyword)
     ("local" keyword "quux" function-name "function" keyword "end" keyword)
     ("end" keyword))))

(ert-deftest lua-font-lock-defuns-inside-table ()
  ;; Check defuns within table definition
  (should-lua-font-lock-equal
   "\
somefunc {
  function() end,
  foobar = function() end,
  [\"quxquux\"] = function() end
}"
   '(nil
     ("function" keyword "end" keyword)
     ("foobar" function-name "function" keyword "end" keyword)
     ("\"quxquux\"" string "function" keyword "end" keyword)
     nil)))

(ert-deftest lua-gh-issue59 ()
  (should-lua-font-lock-equal
   "\
local foo = function()
   ;
end
-- and
local function foo()
   ;
end"
   '(("local" keyword "foo" function-name "function" keyword)
     nil
     ("end" keyword)
     ("-- " comment-delimiter "and" comment)
     ("local" keyword "function" keyword "foo" function-name)
     nil
     ("end" keyword))))

(ert-deftest lua-funcnames-with-underscore ()
  (should-lua-font-lock-equal
   ;; Check all defun variants, check embedded defuns
   "\
function foo()
  function bar_bar() end
  local function baz_baz() end
  qux_qux = function() end
  local quux_quux = function() end
end"
   '(("function" keyword "foo" function-name)
     ("function" keyword "bar_bar" function-name "end" keyword)
     ("local" keyword "function" keyword "baz_baz" function-name "end" keyword)
     ("qux_qux" function-name "function" keyword "end" keyword)
     ("local" keyword "quux_quux" function-name "function" keyword "end" keyword)
     ("end" keyword)))  )

(ert-deftest lua-font-lock-labels ()
  (should-lua-font-lock-equal
    "\
goto foo
::foo::"
   '(("goto" keyword "foo" constant)
     ("::foo::" constant)))

  (should-lua-font-lock-equal
    "\
local foo = 'test' ::f12o::
goto f12o"
   '(("local" keyword "foo" variable-name "'test'" string "::f12o::" constant)
     ("goto" keyword "f12o" constant)))

  ;; With spaces after and before "::"
  (should-lua-font-lock-equal
    "\
goto foo
:: foo ::"
   '(("goto" keyword "foo" constant)
     (":: foo ::" constant)))

  ;; Don't font lock labels when substring "goto" appears as a suffix
  ;; of another variable
  (should-lua-font-lock-equal
    "\
JUNKgoto foo
:: foo ::"
   '(nil ;; don't font lock "foo"
     (":: foo ::" constant))))


(ert-deftest lua-font-lock-func-args ()
  (should-lua-font-lock-equal
   "\
function a(a1, a2)
end"
   '(("function" keyword "a" function-name
      "a1" variable-name "a2" variable-name)
     ("end" keyword)))
  (should-lua-font-lock-equal
   "\
local function b(b1, b2)
end"
   '(("local" keyword "function" keyword "b" function-name
      "b1" variable-name "b2" variable-name)
     ("end" keyword)))
  (should-lua-font-lock-equal
   "\
c = function(c1, c2)
end"
   '(("c" function-name "function" keyword
      "c1" variable-name "c2" variable-name)
     ("end" keyword)))
  (should-lua-font-lock-equal
   "\
local d = function(d1, d2)
end"
   '(("local" keyword "d" function-name "function" keyword
      "d1" variable-name "d2" variable-name)
     ("end" keyword)))

  (should-lua-font-lock-equal
   "\
function foo.bar._baz(e1, e2)
end"
   '(("function" keyword "foo.bar._baz" function-name
      "e1" variable-name "e2" variable-name)
     ("end" keyword))))


(ert-deftest lua-font-lock-func-args-multiple-lines ()
   (should-lua-font-lock-equal
    "\
function foo(
  a1, a2
  ,a3)
end"
    '(("function" keyword "foo" function-name)
      ("a1" variable-name "a2" variable-name)
      ("a3" variable-name)
      ("end" keyword)))
   (should-lua-font-lock-equal
    "\
function foo.bar.baz(
  b1, b2
  ,b3)
end"
    '(("function" keyword "foo.bar.baz" function-name)
      ("b1" variable-name "b2" variable-name)
      ("b3" variable-name)
      ("end" keyword))))


(ert-deftest lua-font-lock-func-args-with-comments ()
  (should-lua-font-lock-equal
   "\
function a(--[[foobarbaz]] a2, --qux
           a3)
end"
   '(("function" keyword "a" function-name
      "-" comment-delimiter "[[foobarbaz]" comment
      "a2" variable-name "--" comment-delimiter "qux" comment)
     ("a3" variable-name)
     ("end" keyword))))


(ert-deftest lua-font-lock-anonymous-func-args ()
  (should-lua-font-lock-equal
   "\
x = foobar(function(foo,
                    bar)
end)"
   '(("function" keyword "foo" variable-name)
     ("bar" variable-name)
     ("end" keyword))))
