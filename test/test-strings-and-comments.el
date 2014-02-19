(require 'ert)
(require 'lua-font-lock-test-helpers
         ;; let's try a bit to help Emacs find the helpers, just in case
         (concat (file-name-directory (or load-file-name (buffer-file-name)
                                          default-directory))
                 "lua-font-lock-test-helpers.el"))

(defmacro should= (lhs rhs)
  `(should (equal ,lhs ,rhs)))

(ert-deftest lua-M-j-works-for-simple-comment ()
  (should= (lua-buffer-strs
            (lua-insert-goto-<> '("-- foobar <>"))
            (execute-kbd-macro (kbd "M-j")))
           '("-- foobar"
             "-- "))

  (should= (lua-buffer-strs
            (lua-insert-goto-<> '("xyzzy -- foobar <>"))
            (execute-kbd-macro (kbd "M-j")))
           '("xyzzy -- foobar"
             "-- "))

  (should= (lua-buffer-strs
            (lua-insert-goto-<> '("xyz<> xyzzy -- foobar"))
            (execute-kbd-macro (kbd "M-j")))
           '("xyz"
             "xyzzy -- foobar")))


(ert-deftest lua-M-j-works-for-longer-comment ()
  (should= (lua-buffer-strs
            (lua-insert-goto-<> '("---- foobar <>"))
            (execute-kbd-macro (kbd "M-j")))
           '("---- foobar"
             "---- "))

  (should= (lua-buffer-strs
            (lua-insert-goto-<> '("xyzzy ---- foobar <>"))
            (execute-kbd-macro (kbd "M-j")))
           '("xyzzy ---- foobar"
             "---- ")))

(ert-deftest lua-M-j-handles-string-and-multiline-comments ()
  (should= (lua-buffer-strs
            (lua-insert-goto-<> '("\"-- \" .. foobar <>"))
            (execute-kbd-macro (kbd "M-j")))
           '("\"-- \" .. foobar"
             ""))

  (should= (lua-buffer-strs
            (lua-insert-goto-<> '("'-- ' .. foobar <>"))
            (execute-kbd-macro (kbd "M-j")))
           '("'-- ' .. foobar"
             ""))

  (should= (lua-buffer-strs
            (lua-insert-goto-<> '("[[-- ]] .. foobar <>"))
            (execute-kbd-macro (kbd "M-j")))
           '("[[-- ]] .. foobar"
             ""))

  (should= (lua-buffer-strs
            (lua-insert-goto-<> '("--[[-- ]] .. foobar <>"))
            (execute-kbd-macro (kbd "M-j")))
           '("--[[-- ]] .. foobar"
             ""))

  (should= (lua-buffer-strs
            (lua-insert-goto-<> '("---[[-- ]] .. foobar <>"))
            (execute-kbd-macro (kbd "M-j")))
           '("---[[-- ]] .. foobar"
             "---")))

(ert-deftest lua-M-j-works-if-comment-is-empty ()
  (should= (lua-buffer-strs
            (lua-insert-goto-<> '("-- <>"))
            (execute-kbd-macro (kbd "M-j")))
           '("--"
             "--"))

  ;; Let's make sure that whitespace is optional.
  (should= (lua-buffer-strs
            (lua-insert-goto-<> '("--<>"))
            (execute-kbd-macro (kbd "M-j")))
           '("--"
             "--")))
