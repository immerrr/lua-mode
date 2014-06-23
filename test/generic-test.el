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

