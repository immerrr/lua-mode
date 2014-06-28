;; lua-mode tests for inferior process handling

(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "test-helper.el") nil 'nomessage 'nosuffix)


(ert-deftest lua-hide-process-buffer-doesnt-switch-current-window ()
  :expected-result :failed

  (with-lua-buffer
   (let ((cur-buf (current-buffer)))
     (should (get-buffer-window cur-buf))

     (lua-hide-process-buffer)
     (should (get-buffer-window cur-buf)))))

(ert-deftest lua-hide-process-buffer-doesnt-signal-on-killed-process ()
  :expected-result :failed
  (with-lua-buffer
   (let ((cur-buf (current-buffer)))
     (lua-start-process)
     (lua-kill-process)

     (lua-hide-process-buffer)
     (should (get-buffer-window cur-buf)))))


(ert-deftest lua-runtime-error-msg-is-fontified ()
  :expected-result (if (eq 23 emacs-major-version) :failed :passed)
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
   ;; By default, basic face for all error messages is 'underline, this is pain
   ;; to filter out, let's change that to 'default.
   (let ((compilation-message-face 'default))
     (lua-send-buffer)
     (lua-send-string "foo()")
     ;; Make sure to wait enough to get all the output from the subprocess.
     (while (accept-process-output lua-process 0 200))
     (with-current-buffer lua-process-buffer
       (should
        (equal
         ;; (buffer-string)
         (get-buffer-line-faces)
         '(nil ;; motd line
           ("> " comint-highlight-prompt
            "test-send-runtime-error.lua" compilation-error
            "2" compilation-line-number) ;; error message
           nil ;; stack traceback
           nil ;; in function error
           ("test-send-runtime-error.lua" compilation-error
            "2" compilation-line-number) ;; in 'bar'
           ("test-send-runtime-error.lua" compilation-error
            "6" compilation-line-number) ;; in 'foo'
           nil ;; in main chunk
           nil
           nil)))))))


(ert-deftest lua-syntax-error-msg-is-fontified ()
  :expected-result (if (eq 23 emacs-major-version) :failed :passed)
  (with-lua-buffer
   (rename-buffer "test-send-syntax-error.lua")
   (insert "\
foo = 1
bar = 2

function () end
")
   (let ((compilation-message-face 'default))
     (lua-send-buffer)
     (while (accept-process-output lua-process 0 200))
     (with-current-buffer lua-process-buffer
       (should
        (equal
         (get-buffer-line-faces)
         '(nil
           ("> " comint-highlight-prompt
            "test-send-syntax-error.lua" compilation-error
            "4" compilation-line-number)
           ;; stacktrace with misc info, no font-lock
           nil nil nil nil nil nil)))))))
