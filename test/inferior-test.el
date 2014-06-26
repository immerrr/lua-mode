;; lua-mode tests for inferior process handling

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
         
