;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc) -*-
;; -*- lexical-binding: t -*-
(load (concat (file-name-directory (or load-file-name (buffer-file-name)
                                       default-directory))
              "utils.el") nil 'nomessage 'nosuffix)

(require 'buttercup)
(require 'subr-x)

(defun file-contents (path)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun string-trim-safe (str)
  (save-match-data (string-trim str)))

(defun indentation-test-sections (file-path)
  (with-temp-buffer
     (insert-file-contents-literally file-path)
     (let (results
           section-name
           (begin (point-min))
           end
           cur-str
           (next-section-name "start"))
       (goto-char (point-min))
       (while next-section-name
         (setq next-section-name
               (when (re-search-forward "^--\\(.*\\)" nil 'noerror) (string-trim-safe (match-string-no-properties 1))))
         (setq end (if next-section-name (match-beginning 0) (point-max)))
         (setq cur-str (string-trim-safe (buffer-substring-no-properties begin end)))
         (if (> (length cur-str) 0)
             (push (cons (or section-name (format "section %d" (1+ (length results))))
                         cur-str)
                   results))
         (setq section-name next-section-name)
         (setq begin (point)))
       (nreverse results))))

(defun make-indentation-it-or-xit-clause (x)
  (let ((it-or-xit (if (string-match "XFAIL" (car x)) 'xit 'it)))
    (eval `(,it-or-xit ,(format "%s" (car x))
                       (let ((lua-code ,(cdr x)))
                         (expect lua-code :to-be-reindented-the-same-way))))))

(let* ((current-path (or load-file-name (buffer-file-name) default-directory))
       (indentation-tests-dir (concat (file-name-directory current-path) "indentation-tests"))
       (indentation-tests (directory-files indentation-tests-dir nil ".*\.lua$" 'nosort)))
  (mapcar (lambda (test-file)
            (let ((file-path (expand-file-name test-file indentation-tests-dir)))
              (describe (format "Indentation test `%s'" test-file)
                (mapcar #'make-indentation-it-or-xit-clause
                        (indentation-test-sections file-path)))))
          indentation-tests))


(describe "Continuation lines"
  (it "are indented before/after binary operators"
    (let ((binops '("+"  "-"  "*"  "/"  "^"  "%"  ".."
                    "<"  "<="  ">"  ">="  "=="  "~="
                    "and"  "or")))
      (cl-dolist (binop binops)
        (lua--reindent-like (replace-regexp-in-string "BINOP" binop "\
a = foo BINOP
   bar" 'fixedcase))
        (lua--reindent-like (replace-regexp-in-string "BINOP" binop "\
a = foo
   BINOP bar" 'fixedcase)))))



  (xit "are indented before/after unary operators"
    (expect (lua--reindent-like "\
foo = bar
   -#some_str"))

    (cl-dolist (unop '("-" "#" "not "))
      (expect (lua--reindent-like (replace-regexp-in-string "<>" unop  "\
foobar(qux,
       <>quux)")))
      (expect (lua--reindent-like (replace-regexp-in-string "<>" unop "\
foobar(qux, xyzzy
          <>quux)")))
      (expect (lua--reindent-like (replace-regexp-in-string "<>" unop "\
foobar(
   <>quux)")))
      (expect (lua--reindent-like (replace-regexp-in-string "<>" unop "\
x = {qux,
     <>quux}")))
      (expect (lua--reindent-like (replace-regexp-in-string "<>" unop "\
x = {qux;
     <>quux}")))
      (expect (lua--reindent-like (replace-regexp-in-string "<>" unop "\
x = {qux, xyzzy
        <>quux}")))
      (expect (lua--reindent-like (replace-regexp-in-string "<>" unop "\
x = {
   <>quux
}"))))))



(describe "Block indentation"
  (it "works for do ... end blocks"
    ;; FIXME: test split block-intro indentations
    (expect (lua--reindent-like "\
do
   a = a + 1
end

a = 0"))

    (expect (lua--reindent-like "\
do a = a + 1 end

a = 0"))

    (expect (lua--reindent-like "\
do a = a + 1
end

a = 0")))


  (it "works for while ... do ... end blocks"
    (expect (lua--reindent-like "\
while foo do
   a = a + 1
end

a = 0"))

    (expect (lua--reindent-like "\
while foo
do
   a = a + 1
end

a = 0"))

    (expect (lua--reindent-like "\
while
   foo
do
   a = a + 1
end

a = 0"))

    (expect (lua--reindent-like "\
while foo do a = a + 1 end

a = 0"))

    (expect (lua--reindent-like "\
while
   x +
   y > 0
do
   a = a + 1
end

a = 0")))


  (it "works for repeat ... until blocks"
    (expect (lua--reindent-like "\
repeat
   a = a + 1
until foo

a = 0"))

    (expect (lua--reindent-like "\
repeat
   a = a + 1
until
   foo

a = 0"))

    (expect (lua--reindent-like "\
repeat
   a = a + 1
until
   not
   foo

a = 0"))

    (expect (lua--reindent-like "\
repeat a = a + 1 until not foo

a = 0")))



  (it "works for \"for ... do\" block "
    (expect (lua--reindent-like "\
for k, v in pairs(bar) do
   a = a + 1
end

a = 0"))

    (expect (lua--reindent-like "\
for k, v in pairs(bar)
do
   a = a + 1
end

a = 0"))

    (expect (lua--reindent-like "\
for k, v in pairs(bar) do a = a + 1 end

a = 0"))

    (expect (lua--reindent-like "\
for y = 0, 10 do
   a = a + 1
end

a = 0"))

    (expect (lua--reindent-like "\
for y = 0, 10
do
   a = a + 1
end

a = 0"))

    (expect (lua--reindent-like "\
for y = 0, 10 do a = a + 1 end

a = 0")))

  (it "works for conditionals"
    (expect (lua--reindent-like "\
if foo then
   a = a + 1
end

a = 0"))

    (expect (lua--reindent-like "\
if foo then a = a + 1 end

a = 0"))

    (expect (lua--reindent-like "\
if foo then
   a = a + 1
else
   a = a + 2
end

a = 0"))


    (expect (lua--reindent-like "\
if foo then
   a = a + 1
elseif bar then
   a = a + 2
elseif baz then
   a = a + 3
end

a = 0"))

    (expect (lua--reindent-like "\
if foo then a = a + 1 else
   a = a + 2
end"))))

(describe "Function indentation"
  (it "indents function call arguments"
    (expect (lua--reindent-like "\
foobar(
   a, b, c)"))
    (expect (lua--reindent-like "\
foobar(
   a,
   b, c)"))

    (expect (lua--reindent-like "\
foobar(
   a, b, c
)"))

    (expect (lua--reindent-like "\
foobar(a,
       b,
       c)"))

    (expect (lua--reindent-like "\
foobar{
   a, b, c
}")))

  (it "indent blocks with lua-indent-nested-block-content-align"
    (let ((lua-indent-nested-block-content-align nil))
      (expect (lua--reindent-like "\
call_some_fn( something, {
      val = 5,
      another = 6,
} )"))
      (expect (lua--reindent-like "\
local def = {
   some_very_long_name = { fn =
         function()
            return true
         end
   }
}"))
      ))

  (it "indent blocks with lua-indent-close-paren-align"
    (let ((lua-indent-close-paren-align nil))
      (expect (lua--reindent-like "\
local foo = setmetatable( {
      a = 4,
      b = 5,
}, {
      __index = some_func,
} )"))
      ))

  (it "indents nested tables with alternative block indenting"
    (let ((lua-indent-nested-block-content-align nil)
	  (lua-indent-close-paren-align nil))
      (expect (lua--reindent-like "\
foobar({
      a, b, c
})"))

      (expect (lua--reindent-like "\
foobar(a, {
      b,
      c
})"))

      (expect (lua--reindent-like "\
foobar(
   a,
   {
      b,
      c
})"))

      (expect (lua--reindent-like "\
foobar(
   a,
   {
      b,
      c
   }
)"))

      (expect (lua--reindent-like "\
foobar(a,
   {
      b,
      c
})"))

      (expect (lua--reindent-like "\
foobar(a,
   {
      b,
      c
   }
)"))

      (expect (lua--reindent-like "\
foobar(
   {
      a,
      b
   },
   c, d
)"))
      )))

(ert-deftest lua-indentation-defun ()
  ;; 	 [local] function funcname funcbody
  ;; FIXME: add
  )

(ert-deftest lua-indentation-alignment ()
  ;; FIXME: add
  )

(ert-deftest lua-indentation-tablector ()
  ;; FIXME: add
  )

(ert-deftest lua-indentation-continuation-spans-over-empty-lines ()
  ;; FIXME: add
  ;; FIXME: check comment-only lines too
  )


(ert-deftest lua-indentation-keywords-with-special-characters ()
  (expect (lua--reindent-like "\
do
   foobar = _do
end")))
