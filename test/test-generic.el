;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc) -*-
(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "utils.el") nil 'nomessage 'nosuffix)

(require 'imenu)

(describe "lua-forward-sexp"
  (it "properly scans through curly braces"
    (with-lua-buffer
     (lua-insert-goto-<>
      '("local x = <>function() return {{}} end"
        ""
        "function foobar() end"))
     (lua-forward-sexp)
     (expect (looking-back (rx "x = function() return {{}} end")
                           (line-beginning-position)))))

  (it "scans through then .. end block"
    (with-lua-buffer
     (lua-insert-goto-<>
      '("if foo <>then"
        "  return bar"
        "--[[end here]] end"))
     (lua-forward-sexp)
     (expect (looking-back (rx "--[[end here]] end")
                           (line-beginning-position))))))


(describe "Check that beginning-of-defun works with "
  (it "handles differed function headers"
    (with-lua-buffer
     (lua-insert-goto-<>
      '("function foobar()"
        "<>"
        "end"))
     (beginning-of-defun)
     (expect (looking-at (rx "function foobar()"))))
    (with-lua-buffer
     (lua-insert-goto-<>
      '("local function foobar()"
        "<>"
        "end"))
     (beginning-of-defun)
     (expect (looking-at "local function foobar()")))
    (with-lua-buffer
     (lua-insert-goto-<>
      '("local foobar = function()"
        "<>"
        "end"))
     (beginning-of-defun)
     (expect (looking-at (rx "local foobar = function()"))))
    (with-lua-buffer
     (lua-insert-goto-<>
      '("foobar = function()"
        "<>"
        "end"))
     (beginning-of-defun)
     (expect (looking-at (rx "foobar = function()")))))

  (it "accepts dots and colons"
    (with-lua-buffer
     (lua-insert-goto-<>
      '("foo.bar = function (x,y,z)"
        "<>"
        "end"))
     (beginning-of-defun)
     (expect (looking-at (rx "foo.bar = function (x,y,z)"))))
    (with-lua-buffer
     (lua-insert-goto-<>
      '("function foo.bar:baz (x,y,z)"
        "<>"
        "end"))
     (beginning-of-defun)
     (expect (looking-at (rx "function foo.bar:baz (x,y,z)"))))))


(describe "lua-mode"
  (it "is derived from prog-mode"
    (with-lua-buffer
     (expect (derived-mode-p 'prog-mode)))))

(describe "imenu integration"
  (it "indexes functions"
    (with-lua-buffer
     (insert "\
function foo()
  function bar() end
  local function baz() end
  qux = function() end
  local quux = function() end
end
")
     (expect (mapcar 'car (funcall imenu-create-index-function))
             :to-equal '("foo" "bar" "baz" "qux" "quux"))))

  (it "indexes require statements"
    (with-lua-buffer
     (insert "\
foo = require (\"foo\")
local bar = require (\"bar\")
")
     (expect (mapcar (lambda (item) (cons (car item)
                                          (if (listp (cdr item))
                                              (mapcar 'car (cdr item))
                                            -1)))
                     (funcall imenu-create-index-function))
             :to-equal '(("Requires" . ("foo" "bar")))))))


(describe "lua-backward-up-list"
  (it "doesn't move point when no parent block open token exists"
    (expect "foo = bar<2><1>"
            :with-point-at "<1>"
            :after-executing (condition-case nil
                                 (lua-backward-up-list)
                               (scan-error nil))
            :to-end-up-at "<2>"))

  (it "doesn't move point when cannot skip intermediate close token"
    (expect "foo, bar = baz}, qux<2><1>"
            :with-point-at "<1>"
            :after-executing (condition-case nil
                                 (lua-backward-up-list)
                               (scan-error nil))
            :to-end-up-at "<2>"))

  (it "works for ("
    (expect "foo<2>(<1>"
            :with-point-at "<1>"
            :after-executing (lua-backward-up-list)
            :to-end-up-at "<2>"))

  (it "works for {"
    (expect "\
local foo = <2>{
   1,
   2,<1>
   3,
}"
            :with-point-at "<1>"
            :after-executing (lua-backward-up-list)
            :to-end-up-at "<2>"))

  (it "works for else"
    (expect "if foo then 123 <2>else 456<1> end"
            :with-point-at "<1>"
            :after-executing (lua-backward-up-list)
            :to-end-up-at "<2>"))

  (it "works for if/else/end"
    (expect "if foo then 123 <2>else 456<1> end"
            :with-point-at "<1>"
            :after-executing (lua-backward-up-list)
            :to-end-up-at "<2>"))

  (it "works for do blocks"
    (expect "while foo <2>do print(123) <1>end"
            :with-point-at "<1>"
            :after-executing (lua-backward-up-list)
            :to-end-up-at "<2>"))

  (it "works for while/do"
    (expect "<2>while foo <1>do print(123) end"
            :with-point-at "<1>"
            :after-executing (lua-backward-up-list)
            :to-end-up-at "<2>"))

  (it "skips complete while/do block"
    (expect "<2>do while true do print(123) end <1>end"
            :with-point-at "<1>"
            :after-executing (lua-backward-up-list)
            :to-end-up-at "<2>"))

  (it "skips complete repeat/until block"
    (expect "<2>do repeat print(123) until foo <1>end"
            :with-point-at "<1>"
            :after-executing (lua-backward-up-list)
            :to-end-up-at "<2>"))

  (it "skips complete if/elseif/else/end block"
    (expect "<2>do if foo then bar() elseif baz() then qux() else quux() end <1>end"
            :with-point-at "<1>"
            :after-executing (lua-backward-up-list)
            :to-end-up-at "<2>"))

  (it "skips from else to previous then"
    (expect "if foo <2>then bar() <1>else baz() end"
            :with-point-at "<1>"
            :after-executing (lua-backward-up-list)
            :to-end-up-at "<2>"))

  (it "skips from end to previous else"
    (expect "if foo then bar() <2>else baz() <1>end"
            :with-point-at "<1>"
            :after-executing (lua-backward-up-list)
            :to-end-up-at "<2>")))
