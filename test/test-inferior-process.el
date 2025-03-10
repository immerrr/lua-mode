;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc) ; lexical-binding:t -*-
(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "utils.el") nil 'nomessage 'nosuffix)
(require 'cl-lib)
(require 'comint)


(describe "Hiding process buffer does not switch current window"
  (it "when process is active"
    (with-lua-buffer
     (let ((cur-buf (current-buffer)))
       (expect (get-buffer-window cur-buf))
       (lua-start-process)
       (lua-hide-process-buffer)
       (expect (get-buffer-window cur-buf)))))

  (it "and does not signal when process is already killed"
    (with-lua-buffer
     (let ((cur-buf (current-buffer)))
       (lua-start-process)
       (lua-kill-process)
       (lua-hide-process-buffer)
       (expect (get-buffer-window cur-buf)))))

  (it "when process is not started"
    (with-lua-buffer
     (let ((cur-buf (current-buffer)))
       (expect lua-process-buffer :to-be nil)
       (lua-hide-process-buffer)
       (expect (get-buffer-window cur-buf))))))

(describe "Compilation minor mode"
  (it "sets comint-prompt-regexp in process buffer"
    (with-lua-buffer
     (lua-start-process)
     (with-current-buffer lua-process-buffer
       (expect "" :not :to-match comint-prompt-regexp)
       (expect "> " :to-match comint-prompt-regexp))
     (expect comint-prompt-regexp :to-equal "^"))
    (expect comint-prompt-regexp :to-equal "^")))

(describe "Splitting Lua code into literals of specified size"
  (it "splits strings into same-size Lua literals"
    (expect (lua--split-string-into-lua-literals "foobarbaz" 3)
            :to-equal '("'foo'" "'bar'" "'baz'")))

  (it "leaves last lua literal shorter if necessary"
    (expect (lua--split-string-into-lua-literals "foobarba" 3)
            :to-equal '("'foo'" "'bar'" "'ba'"))
    (expect (lua--split-string-into-lua-literals "foobarb" 3)
            :to-equal '("'foo'" "'bar'" "'b'"))
    (expect (lua--split-string-into-lua-literals "foobar" 3)
            :to-equal '("'foo'" "'bar'")))

  (it "escapes special characters and does not exceed max-length"
    (expect (lua--split-string-into-lua-literals "foo\nbar" 3)
            :to-equal '("'foo'" "'\\nb'" "'ar'")))

  (it "cuts literal short if it cannot fit a special character"
    (expect (lua--split-string-into-lua-literals "fo\nbar" 3)
            :to-equal '("'fo'" "'\\nb'" "'ar'"))))

(describe "Sending region chunked"
  (it "does that"
    (with-lua-buffer
     (insert "x = '|01234567890|1234567890|1234567890|'")
     (lua-start-process)
     (let ((lua-string-max-size 20))
       (lua-send-buffer))
     (lua-send-string "print(x)")
     (while (accept-process-output (lua-get-create-process) 0 200))
     (with-current-buffer lua-process-buffer
       (let* ((buf (buffer-substring-no-properties (point-min) (point-max)))
              (buf-lines (cdr (split-string buf "\n" nil))))
         (expect (nth 1 (nreverse buf-lines))
                 :to-equal "> |01234567890|1234567890|1234567890|"))))))



(require 'compile)
(if (fboundp 'compilation--loc->file-struct)
    (defun get-error-file (err-info)
      (caar (compilation--loc->file-struct
             (compilation--message->loc (compilation-next-error 0)))))
  (defun get-error-file (err-info)
    (caar (nth 2 (car err-info)))))


(describe "Fontification in compilation buffer"
  (xit "fontifies runtime error messages"
    (with-lua-buffer
     (insert "\
function bar()
   error(123)
end

function foo()
   bar()
end
")
     (rename-buffer "test-send-runtime-error.lua" 'unique)
     ;; By default non-nil compilation-message-face is appended to
     ;; compilation-error faces, let's simplify the checks.
     (let ((compilation-message-face nil))
       (lua-send-buffer)
       (lua-send-string "foo()")
       ;; Make sure to wait enough to get all the output from the subprocess.
       (while (accept-process-output lua-process 0 200))
       (with-current-buffer lua-process-buffer
         (expect
          (get-buffer-line-faces)
          :to-equal
          '(nil ;; motd line (not highlighted)
            nil ;; first prompt (also not highlighted)
            ("test-send-runtime-error.lua" compilation-error
             "2" compilation-line-number) ;; error message
            nil                           ;; stack traceback
            nil                           ;; in function error
            ("test-send-runtime-error.lua" compilation-error
             "2" compilation-line-number) ;; in 'bar'
            ("test-send-runtime-error.lua" compilation-error
             "6" compilation-line-number) ;; in 'foo'
            ("stdin" compilation-error "1" compilation-line-number)
            nil ;; in main chunk
            nil))))))

  (xit "fontifies syntax error messages"
    (with-lua-buffer
     (rename-buffer "test-send-syntax-error.lua")
     (insert "\
function () end
")
     ;; By default non-nil compilation-message-face is appended to
     ;; compilation-error faces, let's simplify the checks.
     (let ((compilation-message-face nil))
       (lua-send-buffer)
       (while (accept-process-output lua-process 0 200))
       (with-current-buffer lua-process-buffer
         (expect
          (get-buffer-line-faces)
          :to-equal
          '(nil ;; motd line, no highlight
            nil ;; first prompt, also no highlight
            (;; "stdin" is being highlighted here because compilation mode
             ;; thinks it is some sort of "make: ..." message.  This doesn't
             ;; happen in wildlife, because there's a default message face
             ;; (underline) that prevents this.  In tests this is turned off,
             ;; see `(compilation-message-face nil)' above, to simplify
             ;; font-lock face checks.
             "stdin" font-lock-function-name-face
             "test-send-syntax-error.lua" compilation-error
             "1" compilation-line-number)
            ;; stacktrace with misc info, no font-lock
            nil nil
            ("stdin" compilation-error "1" compilation-line-number)
            ("stdin" compilation-error "1" compilation-line-number)
            nil nil))))))


  ;; (it "does not ask for file on \"stdin:NN\" errors"
  ;;   (let ((fname (make-temp-file "lua_mode_test" nil ".lua"))
  ;;         buf)
  ;;     (unwind-protect
  ;;         (progn
  ;;           (save-current-buffer
  ;;             (setq buf (find-file fname))
  ;;             (insert "function () end")
  ;;             ;; Make sure the buffer can be killed cleanly
  ;;             (set-buffer-modified-p nil)
  ;;             (lua-send-buffer)
  ;;             (while (accept-process-output lua-process 0 200))
  ;;             (with-current-buffer lua-process-buffer
  ;;               (font-lock-fontify-buffer))
  ;;             (cl-letf
  ;;                 (((symbol-function 'read-file-name)
  ;;                   (lambda (&rest args)
  ;;                     (error "read-file-name must not be called"))))
  ;;               (expect (next-error) :to-be nil)
  ;;               (with-current-buffer lua-process-buffer
  ;;                 (expect fname :to-equal
  ;;                         (get-error-file (compilation-next-error 0))))

  ;;               (expect (next-error) :to-be nil)
  ;;               (with-current-buffer lua-process-buffer
  ;;                 (expect "stdin" :to-equal
  ;;                         (get-error-file (compilation-next-error 0)))))))
  ;;       (when buf
  ;;         (kill-buffer buf))
  ;;       (delete-file fname)
  ;;       (kill-buffer "*lua*"))))
  )

(describe "String escaping"
  (it "Escapes literal tabs"
    (expect (string=
	     (lua-make-lua-string "\
	-- comment indented with a tab")
	     "'\\t-- comment indented with a tab'"))))
