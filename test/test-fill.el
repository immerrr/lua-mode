;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc) -*-
(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "utils.el") nil 'nomessage 'nosuffix)



(defun expect-filled-as (strs etalon)
  (expect
   (lua-buffer-strs
    (let ((fill-column 10))
      (lua-insert-goto-<>
       strs)
      (execute-kbd-macro (kbd "M-q"))))
   :to-equal
   etalon))



(describe "Test fill-paragraph"
  (it "filling single-line comment"
    (expect-filled-as '("<>-- foo bar baz qux")
                      '("-- foo bar"
                        "-- baz qux")))
  (it "filling comment after code"
    (expect-filled-as '("<>foo -- bar baz")
                      '("foo -- bar"
                        "    -- baz")))
  (xit "filling multiline comment"
    (expect-filled-as '("<>--[[ foo bar baz ]]")
                      '("--[[ foo bar"
                        "     baz ]]")))
  (it "does not spill comments into code (issue #25)"
    (expect-filled-as '("<>"
                        "-- foo bar baz qux"
                        "foo_func()")
                      '(""
                        "-- foo bar"
                        "-- baz qux"
                        "foo_func()"))))
