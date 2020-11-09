;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc) -*-
;; -*- lexical-binding: t -*-
(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "utils.el") nil 'nomessage 'nosuffix)

(require 'buttercup)
(require 'cl-lib)


(defun lua--get-indentation-test-sections (file-path)
  (with-temp-buffer
    (insert-file-contents-literally file-path)
    (hack-local-variables)
    (let (results
          section-name
          (begin (point-min))
          end
          cur-str
          (next-section-name "start"))
      (goto-char (point-min))
      (while next-section-name
        ;; Scan towards the next comment or end of file, save the comment as
        ;; the name for the section that comes AFTER the current one.
        (setq next-section-name
              (when (re-search-forward "^--\\(.*\\)" nil 'noerror) (lua--string-trim (match-string-no-properties 1))))
        ;; Record current section bounds and contents
        (setq end (if next-section-name (match-beginning 0) (point-max)))
        (setq cur-str (lua--string-trim (buffer-substring-no-properties begin end)))
        ;; Save current section to be returned
        (if (> (length cur-str) 0)
            (push (list (or section-name (format "section %d" (1+ (length results))))
                        cur-str
                        file-local-variables-alist)
                  results))
        ;; Transition to the next iteration of the loop.
        (setq section-name next-section-name)
        (setq begin (point)))
      (nreverse results))))

(defun lua--indentation-test-make-it-or-xit-clause (x)
  (let ((it-or-xit (if (string-match "XFAIL" (car x)) 'xit 'it)))
    (eval `(,it-or-xit ,(format "%s" (car x))
                       (let ((lua-code ,(cadr x))
                             ,@(mapcar (lambda (alist-cons)
                                         (list (car alist-cons) (cdr alist-cons)))
                                       ;; cl-caddr here is to support Emacs<26 that don't have caddr.
                                       (cl-caddr x)))
                         (expect lua-code :to-be-reindented-the-same-way))))))

(let* ((current-path (or load-file-name (buffer-file-name) default-directory))
       (indentation-tests-dir (concat (file-name-directory current-path) "indentation-tests"))
       (indentation-tests (directory-files indentation-tests-dir nil ".*\.lua$" 'nosort)))
  (mapcar (lambda (test-file)
            (let ((file-path (expand-file-name test-file indentation-tests-dir)))
              (describe (format "Indentation test `%s'" test-file)
                (mapcar #'lua--indentation-test-make-it-or-xit-clause
                        (lua--get-indentation-test-sections file-path)))))
          indentation-tests))

(describe "Continuation lines"
  (lua--parametrize-tests
   (binop)
   (("+") ("-") ("*") ("/") ("^") ("%") ("..")
    ("<") ("<=") (">") (">=") ("==") ("~=") ("and") ("or"))

   :it (format "are indented before/after binary operators: %s" binop)
   (expect (replace-regexp-in-string "BINOP" binop "\
a = foo BINOP
   bar" 'fixedcase)
           :to-be-reindented-the-same-way)
   (expect (replace-regexp-in-string "BINOP" binop "\
a = foo
   BINOP bar" 'fixedcase)
           :to-be-reindented-the-same-way))
) ;; (



;;    (xit "are indented before/after unary operators"
;;      (expect (lua--reindent-like "\
;; foo = bar
;;    -#some_str"))

;;      (cl-dolist (unop '("-" "#" "not "))
;;        (expect (lua--reindent-like (replace-regexp-in-string "<>" unop  "\
;; foobar(qux,
;;        <>quux)")))
;;        (expect (lua--reindent-like (replace-regexp-in-string "<>" unop "\
;; foobar(qux, xyzzy
;;           <>quux)")))
;;        (expect (lua--reindent-like (replace-regexp-in-string "<>" unop "\
;; foobar(
;;    <>quux)")))
;;        (expect (lua--reindent-like (replace-regexp-in-string "<>" unop "\
;; x = {qux,
;;      <>quux}")))
;;        (expect (lua--reindent-like (replace-regexp-in-string "<>" unop "\
;; x = {qux;
;;      <>quux}")))
;;        (expect (lua--reindent-like (replace-regexp-in-string "<>" unop "\
;; x = {qux, xyzzy
;;         <>quux}")))
;;        (expect (lua--reindent-like (replace-regexp-in-string "<>" unop "\
;; x = {
;;    <>quux
;; }"))))))



(describe "Function indentation"
  (it "indents function call arguments"
    (expect "\
foobar(
   a, b, c)" :to-be-reindented-the-same-way)
    (expect "\
foobar(
   a,
   b, c)" :to-be-reindented-the-same-way)

    (expect "\
foobar(
   a, b, c
)" :to-be-reindented-the-same-way)

    (expect "\
foobar(a,
       b,
       c)" :to-be-reindented-the-same-way)

    (expect "\
foobar{
   a, b, c
}" :to-be-reindented-the-same-way))

  (it "indent blocks with lua-indent-nested-block-content-align"
    (let ((lua-indent-nested-block-content-align nil))
      (expect "\
call_some_fn( something, {
      val = 5,
      another = 6,
} )" :to-be-reindented-the-same-way)
      (expect "\
local def = {
   some_very_long_name = { fn =
         function()
            return true
         end
   }
}" :to-be-reindented-the-same-way)
      ))

  (it "indent blocks with lua-indent-close-paren-align"
    (let ((lua-indent-close-paren-align nil))
      (expect "\
local foo = setmetatable( {
      a = 4,
      b = 5,
}, {
      __index = some_func,
} )" :to-be-reindented-the-same-way)
      ))

  (it "indents nested tables with alternative block indenting"
    (let ((lua-indent-nested-block-content-align nil)
	  (lua-indent-close-paren-align nil))
      (expect "\
foobar({
      a, b, c
})" :to-be-reindented-the-same-way)

      (expect "\
foobar(a, {
      b,
      c
})" :to-be-reindented-the-same-way)

      (expect "\
foobar(
   a,
   {
      b,
      c
})" :to-be-reindented-the-same-way)

      (expect "\
foobar(
   a,
   {
      b,
      c
   }
)" :to-be-reindented-the-same-way)

      (expect "\
foobar(a,
   {
      b,
      c
})" :to-be-reindented-the-same-way)

      (expect "\
foobar(a,
   {
      b,
      c
   }
)" :to-be-reindented-the-same-way)

      (expect "\
foobar(
   {
      a,
      b
   },
   c, d
)" :to-be-reindented-the-same-way)
      )))


(ert-deftest lua-indentation-keywords-with-special-characters ()
  (expect "\
do
   foobar = _do
end" :to-be-reindented-the-same-way))
