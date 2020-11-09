;;; test-funcname-at-point.el --- Test `lua-funcname-at-point'

;;; Commentary:

;; Ensure that `lua-funcname-at-point' works correctly in all intended
;; circumstances.

;;; Code:

(describe "Test `lua-funcname-at-point'."
  (it "handles trailing periods"
    (with-temp-buffer
      (insert "table.insert.")
      (backward-char)
      (expect (lua-funcname-at-point) :to-equal "table.insert")))
  (it "handles point being in the middle"
    (with-temp-buffer
      (insert "table.")
      (save-excursion
        (insert "insert."))
      (expect (lua-funcname-at-point) :to-equal "table.insert")))
  (it "handles point being at the start of the buffer"
    (with-temp-buffer
      (save-excursion (insert "table.insert."))
      (expect (lua-funcname-at-point) :to-equal "table.insert")))
  (it "handles identifiers before point"
    (with-temp-buffer
      (insert "table.insert.")
      (expect (lua-funcname-at-point) :to-equal "table.insert"))))

;;; test-funcname-at-point.el ends here
