(require 'lua-mode)

(defun get-str-faces (str)
  "Find contiguous spans of non-default faces in STR.

E.g. for properly fontified Lua string \"local x = 100\" it should return
  '(\"local\" font-lock-keyword-face
    \"x\" font-lock-variable-name-face
    \"100\" font-lock-constant-face)
"
  (let ((pos 0)
        nextpos
        result prop)
    (while pos
      (setq nextpos (next-single-property-change pos 'face str)
            prop (get-text-property pos 'face str))
      (when prop
        (push (substring-no-properties str pos nextpos) result)
        (push prop result))
      (setq pos nextpos))
    (nreverse result)))

(defun lua-fontify-str (str)
  "Return string fontified according to lua-mode's rules"
  (with-temp-buffer
    (lua-mode)
    (insert str)
    (font-lock-fontify-buffer)
    (buffer-string)))

(defun lua-get-line-faces (str)
  "Find contiguous spans of non-default faces in each line of STR.

The result is a list of lists."
  (mapcar
   'get-str-faces
   (split-string (lua-fontify-str str) "\n" nil)))

(defun lua-mk-font-lock-faces (sym)
  "Decorate symbols with font-lock-%s-face recursively.

This is a mere typing/reading aid for lua-mode's font-lock tests."
  (or (cond
       ((symbolp sym) (intern-soft (format "font-lock-%s-face" (symbol-name sym))))
       ((listp sym) (mapcar 'lua-mk-font-lock-faces sym)))
      sym))

(defmacro should-lua-font-lock-equal (strs faces)
  `(should (equal (lua-get-line-faces ,strs)
                  (lua-mk-font-lock-faces ,faces))))

;; suppress fontification messages in emacs23 output
(setq font-lock-verbose nil)


(defun lua-join-lines (strs)
  (mapconcat (lambda (x) (concat x "\n")) strs ""))

(defmacro with-lua-buffer (&rest body)
  `(with-temp-buffer
     (switch-to-buffer (current-buffer))
     (lua-mode)
     (font-lock-fontify-buffer)
     ,@body))

(defun lua-get-indented-strs (strs)
  (butlast
   (split-string
    (with-lua-buffer
     (insert (replace-regexp-in-string "^\\s *" "" (lua-join-lines strs)))
     (indent-region (point-min) (point-max))
     (buffer-substring-no-properties
      (point-min) (point-max)))
    "\n" nil)))

(defun lua-insert-goto-<> (strs)
  "Insert sequence of strings and put point in place of \"<>\"."
  (insert (lua-join-lines strs))
  (goto-char (point-min))
  (re-search-forward "<>")
  (replace-match "")
  ;; Inserted text may contain multiline constructs which will only be
  ;; recognized after fontification.
  (font-lock-fontify-buffer))

(defmacro lua-buffer-strs (&rest body)
  `(butlast
    (split-string
     (with-lua-buffer
      (progn ,@body)
      (buffer-substring-no-properties (point-min) (point-max)))
     "\n" nil)))

(defmacro should-lua-indent (str)
  `(let ((strs (split-string ,str "\n"))
         (indent-tabs-mode nil))
     (should
      (equal strs (lua-get-indented-strs strs)))))


(provide 'lua-font-lock-test-helpers)
