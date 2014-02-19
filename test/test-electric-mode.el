(require 'ert)
(require 's)

(defmacro with-lua-buffer (&rest body)
  `(with-temp-buffer
     (switch-to-buffer (current-buffer))
     (lua-mode)
     (font-lock-fontify-buffer)
     ,@body))


(ert-deftest test-electric-brace ()
  (with-lua-buffer
    (execute-kbd-macro (kbd "return SPC foo SPC { C-j"))
    (execute-kbd-macro (kbd "'baz' C-j"))
    (should (eq (current-indentation) lua-indent-level))

    (execute-kbd-macro (kbd "}"))
    (should (eq (current-indentation) 0))))


(ert-deftest test-electric-paren ()
   (with-lua-buffer
     (execute-kbd-macro (kbd "return SPC foo SPC ( C-j"))
     (execute-kbd-macro (kbd "'baz' C-j"))
     (should (eq (current-indentation) lua-indent-level))

     (execute-kbd-macro (kbd ")"))
     (should (eq (current-indentation) 0))))


(ert-deftest test-electric-end ()
   (with-lua-buffer
    (execute-kbd-macro (kbd "if SPC foo SPC then C-j"))
    (execute-kbd-macro (kbd "'baz' C-j"))
    (should (eq (current-indentation) lua-indent-level))

    (abbrev-mode 1)
    (execute-kbd-macro (kbd "end C-j"))
    (beginning-of-line 0)
    (should (eq (current-indentation) 0))))


(ert-deftest test-electric-else ()
  (with-lua-buffer
   (execute-kbd-macro (kbd "if SPC foo SPC then C-j"))
   (execute-kbd-macro (kbd "'baz' C-j"))
   (should (eq (current-indentation) lua-indent-level))

   (abbrev-mode 1)
   (execute-kbd-macro (kbd "else C-j"))
   (beginning-of-line 0)
   (should (eq (current-indentation) 0))))


(ert-deftest test-electric-elseif ()
  (with-lua-buffer
   (execute-kbd-macro (kbd "if SPC foo SPC then C-j"))
   (execute-kbd-macro (kbd "'baz' C-j"))
   (should (eq (current-indentation) lua-indent-level))

   (abbrev-mode 1)
   (execute-kbd-macro (kbd "elseif C-j"))
   (beginning-of-line 0)
   (should (eq (current-indentation) 0))))


(when (fboundp 'electric-pair-mode)
  (ert-deftest test-electric-pair-skip-self ()
    (let ((old-mode (if electric-pair-mode 1 0)))
      (unwind-protect
          (with-lua-buffer
           (set (make-local-variable 'electric-pair-skip-self) t)
           (set (make-local-variable 'lua-electric-flag) t)
           (electric-pair-mode 1)
           (execute-kbd-macro "(")
           (should (string= (buffer-string) "()"))
           (execute-kbd-macro ")")
           (should (string= (buffer-string) "()")))
        (electric-pair-mode old-mode)))))
