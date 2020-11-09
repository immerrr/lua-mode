;;; test-funcname-at-point.el --- Test the repl functions

;;; Commentary:

;; Test functions to interact with the REPL.

;;; Code:

(describe "`lua-start-process'"
  (it "doesn't hang for an already-running process"
    ;; Acquire a *lua* repl buffer
    (lua-start-process)
    ;; Input some text
    (with-current-buffer lua-process-buffer
      (insert "table.insert"))
    ;; Try restarting the repl buffer
    (lua-start-process)
    ;; `lua-start-process' shouldn't hang
    (expect t)))

;;; test-process.el ends here
