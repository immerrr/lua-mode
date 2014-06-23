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
