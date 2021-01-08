;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc) -*-

(require 'lua-mode)
(require 'buttercup)


(defun to-be-fontified-as (text faces)
  (let ((expected-faces (lua-mk-font-lock-faces faces))
        (result-faces (lua-get-line-faces text))
        (lineno 1))
    (when (/= (length expected-faces) (length result-faces))
        (buttercup-fail "\
Fontification check failed for:
%S
  Text contains %d lines, face list contains %d lines"
                        text (length result-faces) (length expected-faces)))
    (while expected-faces
      (unless (equal (car expected-faces) (car result-faces))
        (buttercup-fail "\
Fontification check failed on line %d for:
%S
  Expected faces: %S
  Actual faces:   %S"
                        lineno text (car expected-faces) (car result-faces)))
      (setq expected-faces (cdr expected-faces)
            result-faces (cdr result-faces)
            lineno (1+ lineno)))
    (cons t "Fontification check passed")))


(buttercup-define-matcher :to-be-fontified-as (text faces)
  (to-be-fontified-as (funcall text) (funcall faces)))


(buttercup-define-matcher :to-precede (pos regexp)
  (save-excursion
    (goto-char (funcall pos))
    (let* ((precedes (looking-at (funcall regexp)))
           (substr-begin (min (point-max) (funcall pos)))
           (substr-end (min (point-max) (+ (funcall pos) 100)))
           (found-after (format "%S" (buffer-substring-no-properties
                                      substr-begin substr-end ))))
      (goto-char substr-end)
      (when (eobp) (setq found-after (concat found-after " (end-of-buffer)")))
      (cons precedes (format "Expected %s to see after point at %s: %S.  Found: %s"
                             (if precedes "NOT" "")
                             (funcall pos) (funcall regexp) found-after)))))



(defun get-str-faces (str)
  "Find contiguous spans of non-default faces in STR.

E.g. for properly fontified Lua string \"local x = 100\" it should return
  '(\"local\" font-lock-keyword-face
    \"x\" font-lock-variable-name-face
    \"100\" font-lock-constant-face)
"
  (let* ((pos 0)
         (prop (or (get-text-property pos 'face str)
                   (get-text-property pos 'font-lock-face str)))
         (nextpos 0)
         newprop
         result)
    (while nextpos
      (setq nextpos (next-property-change nextpos str))
      (setq newprop (when nextpos (or (get-text-property nextpos 'face str)
                                      (get-text-property nextpos 'font-lock-face str))))
      (when (not (equal prop newprop))
        (when (listp prop)
          (when (eq (car-safe (last prop)) 'default)
            (setq prop (butlast prop)))
          (when (= 1 (length prop))
            (setq prop (car prop)))
          (when (symbolp prop)
            (when (eq prop 'default)
              (setq prop nil))))
        (when prop
          (push (substring-no-properties str pos nextpos) result)
          (push prop result))
        (setq prop newprop
              pos nextpos)))
    (nreverse result)))

(defun lua-fontify-str (str)
  "Return string fontified according to lua-mode's rules"
  (with-temp-buffer
    (lua-mode)
    (insert str)
    (font-lock-fontify-buffer)
    (buffer-string)))

(defun get-buffer-line-faces ()
  (font-lock-fontify-buffer)
  (mapcar 'get-str-faces
          (split-string (buffer-string) "\n" nil)))


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
  (declare (debug (&rest form)))
  `(with-temp-buffer
     ;; font-lock is not activated if buffer name is temporary (starts with a
     ;; space) and if `noninteractive' is non-nil. Ensure tests that use
     ;; font-lock still work.
     (rename-buffer "temp-buffer.lua" t)
     (let (noninteractive)
       (lua-mode)
       (font-lock-mode 1))
     (set (make-local-variable 'lua-process) nil)
     (set (make-local-variable 'lua-process-buffer) nil)
     (pop-to-buffer (current-buffer))
     (unwind-protect
      (progn ,@body)
      (when (buffer-live-p lua-process-buffer)
        (lua-kill-process)))))

(defun lua-get-indented-strs (strs)
  (let ((indent-tabs-mode nil)
        (font-lock-verbose nil))
   (butlast
    (split-string
     (with-lua-buffer
      (let ((inhibit-message t))
        (insert (replace-regexp-in-string "^\\s *" "" (lua-join-lines strs)))
        (font-lock-fontify-buffer)
        (indent-region (point-min) (point-max))
        (buffer-substring-no-properties
         (point-min) (point-max))))
     "\n" nil))))

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

(defun lua--reindent-like (str)
  (let ((strs (split-string str "\n")))
    (equal strs (lua-get-indented-strs strs))))

(defun with-point-at-matcher (&rest args)
  (let* (lua-code
         origin-placeholder
         (origin-marker (make-marker))
         target-placeholder
         (target-marker (make-marker))
         body
         last-elt
         result
         message
         )
    (cl-dolist (elt args)
      (cond
       ((eq last-elt :lua-code)
        (setq lua-code (funcall elt)
              last-elt nil))
       ((eq last-elt :with-point-at)
        (setq origin-placeholder (funcall elt)
              last-elt nil))
       ((eq last-elt :to-end-up-at)
        (setq target-placeholder (funcall elt)
              last-elt nil))
       ((eq last-elt :after-executing)
        ;; No funcall here, funcall when the buffer is set up.
        (setq body elt
              last-elt nil))
       (t
        (setq last-elt (if (functionp elt) (funcall elt) elt)))))

    (with-lua-buffer
     (insert lua-code)

     (goto-char (point-min))
     (set-marker target-marker (search-forward target-placeholder))
     (replace-match "")

     (goto-char (point-min))
     (set-marker origin-marker (search-forward origin-placeholder))
     (replace-match "")

     (funcall body)

     (setq result (equal (point) (marker-position target-marker)))
     (setq message
           (if result
          (format "Expected point not to be here:\n\n%s|%s"
                  (buffer-substring-no-properties (point-min) (point))
                  (buffer-substring-no-properties (point) (point-max)))
        (format "Expected point to be here:\n============\n%s|%s\n============\n\nInstead it was here:\n============\n%s|%s\n============"
                (buffer-substring-no-properties (point-min) (marker-position target-marker))
                (buffer-substring-no-properties (marker-position target-marker) (point-max))
                (buffer-substring-no-properties (point-min) (point))
                (buffer-substring-no-properties (point) (point-max)))))
     (cons result message))))

(buttercup-define-matcher :with-point-at (&rest args)
  (apply #'with-point-at-matcher `(:lua-code ,(car args) :with-point-at ,@(cdr args))))

;;; Shortcut for with-point-at with <1> and <2> placeholders
(buttercup-define-matcher :to-move-point-from-1-to-2 (code-block lua-code)
  (with-point-at-matcher
   :lua-code lua-code
   :with-point-at (lambda () "<1>")
   :after-executing code-block
   :to-end-up-at (lambda () "<2>")))

(defun lua--string-trim (string &optional trim-left trim-right)
  ;; Backport of string-trim for Emacs 24 that doesn't have subr-x lib.
  (let ((sub-start 0) sub-end)
    (or trim-left (setq trim-left "[ \t\n\r]+"))
    (or trim-right (setq trim-right "[ \t\n\r]+"))
    (save-match-data
      (when (string-match (concat "\\`" trim-left) string)
        (setq sub-start (match-end 0)))
      (when (string-match (concat trim-right "\\'") string sub-start)
        (setq sub-end (match-beginning 0))))
    (if (or sub-start sub-end)
        (substring string sub-start sub-end)
      string)))


(buttercup-define-matcher :to-be-reindented-the-same-way (str)
  (let* ((lines (split-string (funcall str) "\n"))
         (indented-lines (lua-get-indented-strs lines)))
    (buttercup--test-expectation (equal lines indented-lines)
      :expect-match-phrase (format "Indentation check failed:\n=========\nExpected:\n---------\n%s\n---------\nActual:\n---------\n%s\n========="
                                   (lua--string-trim (mapconcat 'identity lines "\n"))
                                   (lua--string-trim (mapconcat 'identity indented-lines "\n")))
      :expect-mismatch-phrase (format "Expected `%S' to not be reindented like that"
                                      lines))))

(defmacro lua--parametrize-tests (variables param-values it description-form &rest body)
  `(progn
     ,@(cl-loop
        for params in param-values
        for let-bindings = (cl-loop for var in variables
                                    for param in params
                                    collect `(,var (quote ,param)))
        for description = (eval `(let ,let-bindings ,description-form))
        for test-body = `(let ,let-bindings ,@body)
        collect
        (macroexpand `(it ,description ,test-body)))))
