(require 'ert)
(require 'lua-font-lock-test-helpers
         ;; let's try a bit to help Emacs find the helpers, just in case
         (concat (file-name-directory (or load-file-name (buffer-file-name)
                                          default-directory))
                 "lua-font-lock-test-helpers.el"))


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
