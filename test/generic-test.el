(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "test-helper.el") nil 'nomessage 'nosuffix)

(ert-deftest lua-forward-sexp-curly-braces ()
  (with-lua-buffer
   (lua-insert-goto-<>
    '("local x = <>function() return {{}} end"
      ""
      "function foobar() end"))
   (lua-forward-sexp)
   (should (looking-back "x = function() return {{}} end"))))

(ert-deftest lua-forward-sexp-if-then-block ()
  (with-lua-buffer
   (lua-insert-goto-<>
    '("if foo <>then"
      "  return bar"
      "--[[end here]] end"))
   (lua-forward-sexp)
   (should (looking-back (rx "--[[end here]] end")))))

(ert-deftest lua-beginning-of-defun-different-headers ()
  (with-lua-buffer
   (lua-insert-goto-<>
    '("function foobar()"
      "<>"
      "end"))
   (beginning-of-defun)
   (should (looking-at "function foobar()")))

  (with-lua-buffer
   (lua-insert-goto-<>
    '("local function foobar()"
      "<>"
      "end"))
   (beginning-of-defun)
   (should (looking-at "local function foobar()")))

  (with-lua-buffer
   (lua-insert-goto-<>
    '("local foobar = function()"
      "<>"
      "end"))
   (beginning-of-defun)
   (should (looking-at "local foobar = function()")))

  (with-lua-buffer
   (lua-insert-goto-<>
    '("foobar = function()"
      "<>"
      "end"))
   (beginning-of-defun)
   (should (looking-at "foobar = function()"))))

(ert-deftest lua-beginning-of-defun-accepts-dots-and-colons ()
   (with-lua-buffer
    (lua-insert-goto-<>
     '("foo.bar = function (x,y,z)"
       "<>"
       "end"))
    (beginning-of-defun)
    (should (looking-at "foo\\.bar = function (x,y,z)")))

   (with-lua-buffer
    (lua-insert-goto-<>
     '("function foo.bar:baz (x,y,z)"
       "<>"
       "end"))
    (beginning-of-defun)
    (should (looking-at "function foo.bar:baz (x,y,z)"))))
