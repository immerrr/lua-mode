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
