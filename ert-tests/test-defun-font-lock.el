(require 'ert)

(defun get-str-faces (str)
  "Find contiguous spans of non-default faces in STR.

E.g. for properly fontified Lua string \"local x = 100\" it should return
  '(\"local\" font-lock-keyword-face
    \"x\" font-lock-variable-name-face
    \"100\" font-lock-constant-face)
"
  (let ((pos 0)
        nextpos
        result prop)
    (while pos
      (setq nextpos (next-single-property-change pos 'face str)
            prop (get-text-property pos 'face str))
      (when prop
        (push (substring-no-properties str pos nextpos) result)
        (push prop result))
      (setq pos nextpos))
    (nreverse result)))

(defun lua-fontify-str (str)
  "Return string fontified according to lua-mode's rules"
  (with-temp-buffer
    (lua-mode)
    (insert str)
    (font-lock-fontify-buffer)
    (buffer-string)))

(defun lua-get-line-faces (str)
  "Find contiguous spans of non-default faces in each line of STR.

The result is a list of lists."
  (mapcar
   'get-str-faces
   (split-string (lua-fontify-str str) "\n" nil)))

(defun lua-mk-font-lock-faces (sym)
  "Decorate symbols with font-lock-%s-face recursively.

This is a mere typing/reading aid for lua-mode's font-lock tests."
  (or (cond
       ((symbolp sym) (intern-soft (format "font-lock-%s-face" (symbol-name sym))))
       ((listp sym) (mapcar 'lua-mk-font-lock-faces sym)))
      sym))

(defmacro should-lua-font-lock-equal (strs faces)
  `(should (equal (lua-get-line-faces ,strs)
                  (lua-mk-font-lock-faces ,faces))))

(progn
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

  (ert-run-tests-interactively t))

