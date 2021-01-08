;;; test-funcname-at-point.el --- Test the repl functions

;;; Commentary:

;; Test functions to interact with the REPL.

;;; Code:

(describe "`lua-start-process'"
  (it "doesn't hang for an already-running process"
    ;; Acquire a *lua* repl buffer
    (save-current-buffer
      (call-interactively #'lua-start-process)
      ;; Input some text
      (with-current-buffer lua-process-buffer
        (insert "table.insert"))
      (switch-to-buffer (get-buffer-create "*scratch*"))
      ;; Try restarting the repl buffer
      (call-interactively #'lua-start-process)

      ;; `lua-start-process' shouldn't hang, and it should have switched back.
      (expect (current-buffer) :to-be lua-process-buffer))))

;;; test-process.el ends here
