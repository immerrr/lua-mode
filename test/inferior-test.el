;; lua-mode tests for inferior process handling

(require 'cl-lib)

(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "test-helper.el") nil 'nomessage 'nosuffix)


(ert-deftest lua-hide-process-buffer-doesnt-switch-current-window ()
  (with-lua-buffer
   (let ((cur-buf (current-buffer)))
     (should (get-buffer-window cur-buf))
     (lua-start-process)
     (lua-hide-process-buffer)
     (should (get-buffer-window cur-buf)))))

(ert-deftest lua-hide-process-buffer-doesnt-signal-on-killed-process ()
  (with-lua-buffer
   (let ((cur-buf (current-buffer)))
     (lua-start-process)
     (lua-kill-process)
     (lua-hide-process-buffer)
     (should (get-buffer-window cur-buf)))))

(ert-deftest lua-hide-process-buffer-standalone ()
  (with-lua-buffer
    (let ((cur-buf (current-buffer)))
      ;; lua-process-buffer should be nil
      (lua-hide-process-buffer)
      (should (get-buffer-window cur-buf)))))

(ert-deftest lua-runtime-error-msg-is-fontified ()
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
       (should
        (equal
         ;; (buffer-string)
         (get-buffer-line-faces)
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
           nil)))))))


(ert-deftest lua-syntax-error-msg-is-fontified ()
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
       (should
        (equal
         (get-buffer-line-faces)
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
           nil nil)))))))


(require 'compile)
(if (fboundp 'compilation--loc->file-struct)
    (defun get-error-file (err-info)
      (caar (compilation--loc->file-struct
             (compilation--message->loc (compilation-next-error 0)))))
  (defun get-error-file (err-info)
    (caar (nth 2 (car err-info)))))

(ert-deftest lua-repl-doesnt-ask-for-stdin-file ()
  "Ensure REPL doesn't annoyingly ask to open file named \"stdin\"."
  (let ((fname (make-temp-file "lua_mode_test" nil ".lua"))
        buf)
    (unwind-protect
        (progn
          (save-current-buffer
            (setq buf (find-file fname))
            (insert "function () end")
            ;; Make sure the buffer can be killed cleanly
            (set-buffer-modified-p nil)
            (lua-send-buffer)
            (while (accept-process-output lua-process 0 200))
            (with-current-buffer lua-process-buffer
              (font-lock-fontify-buffer))
            (cl-letf
                (((symbol-function 'read-file-name)
                  (lambda (&rest args)
                    (error "read-file-name must not be called"))))
              (should (equal (next-error) nil))
              (with-current-buffer lua-process-buffer
                (should (equal
                         fname
                         (get-error-file (compilation-next-error 0)))))

              (should (equal (next-error) nil))
              (with-current-buffer lua-process-buffer
                (should (equal
                         "stdin"
                         (get-error-file (compilation-next-error 0))))))))
      (when buf
        (kill-buffer buf))
      (delete-file fname)
      (kill-buffer "*lua*"))))
