;;; lua-smie.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Stefan Monnier

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'smie)

(defconst lua-smie-grammar
  ;; Problems:
  ;; - The semi-colon between statements in a block is optional.  So they might
  ;;   have to be synthesized by the tokenizer (and they can be needed at lots
  ;;   and lots of places)!
  ;; - "do" is both opener and neither!
  ;; - "," separates vars in statement "a,b = e1,e2" but inversely
  ;;   separates fields in "{ a = e1, b = e2 }".
  ;; - "," and ";" have the same precedence when they separate fields in a
  ;;   table constructor, but not in assignment statements.
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((stat ("while" exp)
            ("repeat" block "until" exp)
            ("if" exp-then-block "end")
            ("if" exp-then-block "else" block "end")
            ("if" exp-then-block "elseif" exp-then-block "else" block "end")
            ("do" block "end")
            ("return" explist)
            ("for" assign)
            ("for" in-spec)
            ("function" args-and-block "end")
            ("local" assign)
            ("break")
            (exp)
            (assign))
      (args-and-block (exp))
      (exp-then-block (exp "then" block))
      (assign (varlist "=" explist))
      (in-spec (varlist "in" explist))
      (name-args-block (exp))
      (exp ("{" fieldlist "}")
           ("function" name-args-block "end")
           )         ;(Hash)Table constructor
      (id)
      (varlist (id) (varlist "," varlist))
      (explist (exp) (explist "," explist))
      ;; ",-tc" is the comma in table constructors.
      (fieldlist (field) (fieldlist ",-tc" fieldlist) (fieldlist ";" fieldlist))
      (field (exp) (id "=" exp))
      (block (stat) (block ";" block)))
    '((assoc ","))
    '((assoc ";" ",-tc")))))

(defun lua-smie-rules (kind token)
  ;; FIXME: The body of "function name (args) body end" is simply aligned
  ;; with name :-(
  (message "smie-rule: %s %s" kind token)
  (when (memq kind '(:after :before))
    (message "parent: " (smie-rule-parent-p "{")))
  (let ((result
	 (pcase (cons kind token)
	   (`(:list-intro . ,_) nil)
	   (`(:before . "=") smie-indent-basic)
	   (`(:after . "local") smie-indent-basic)
	   (`(:after . "then") (smie-rule-parent smie-indent-basic))
	   (`(:before . ,(or `"{" `"("))
	    (if (smie-rule-hanging-p) (smie-rule-parent)))
	   )))
    (message "result: %s" result)
    result))

(defun lua-smie-newline-is-semi-p ()
  (and (looking-at "[ \t]*\\(?:--\\|\n\\)")
       (save-excursion
         (skip-chars-backward " \t")
         ;; Only add implicit ; when needed.
         (let ((s (char-syntax (char-before))))
           (cond
            ((eq s ?w)
             (let ((prev (buffer-substring (point)
                                           (progn
                                             (skip-syntax-backward "w_")
                                             (point)))))
               (not (integerp (nth 2 (assoc prev smie-grammar))))))
            ((eq s ?\)))
            ((memq s '(?\" ?\|)) (not (nth 3 (syntax-ppss)))))))
       (save-excursion (let ((eol (line-end-position)))
                         (forward-comment (point-max))
                         (and (> (point) eol)
                              (eq (char-syntax (char-after)) ?w))))))

(defun lua-smie-forward-token ()
  (cond
   ((lua-smie-newline-is-semi-p)
    (skip-chars-forward " \t")
    (if (eolp) (forward-char 1) (forward-comment 1))
    ";")
   (t
    (let ((tok (smie-default-forward-token)))
      (message "token: %s" tok)
      tok))))

(defun lua-smie-backward-token ()
  (let ((pos (point)))
    (forward-comment (- pos))
    (cond
     ((and (lua-smie-newline-is-semi-p)
           (> pos (line-end-position)))
      ";")
     (t
      (let ((tok (smie-default-backward-token)))
        (message "token: %s" tok)
	tok)))))

(defun lua-syntax-propertize (start end)
  (goto-char start)
  (lua-syntax-propertize-end end)
  (funcall
   (syntax-propertize-rules
    ("\\(-\\)-\\[=*\\[" (1 (prog1 "!" (lua-syntax-propertize-end end))))
    ("\\(\\[\\)=*\\["   (1 (prog1 "|" (lua-syntax-propertize-end end)))))
   start end))

(defun lua-syntax-propertize-end (end)
  (let ((ppss (syntax-ppss)))
    (cond
     ((eq t (nth 3 ppss))               ;Long-bracket string.
      (if (search-forward (concat "]" (save-excursion
                                        (goto-char (1+ (nth 8 ppss)))
                                        (looking-at "=*")
                                        (match-string 0))
                                  "]")
                          end 'move)
          (put-text-property (1- (point)) (point)
                             'syntax-table (string-to-syntax "|"))))
     ((eq 'syntax-table (nth 7 ppss))   ;Long-bracket comment.
      (if (search-forward (concat "]" (save-excursion
                                        (goto-char (nth 8 ppss))
                                        (skip-chars-forward "-")
                                        (forward-char 1)
                                        (looking-at "=*")
                                        (match-string 0))
                                  "]")
                          end 'move)
          (put-text-property (1- (point)) (point)
                             'syntax-table (string-to-syntax "!")))))))

(defvar lua-font-lock-keywords
  ())

(defvar lua-smie-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    st))

;;;###autoload
(define-derived-mode lua-smie-mode prog-mode "Lua[SMIE]"
  "Simple major mode for LUA."
  (setq-local syntax-propertize-function #'lua-syntax-propertize)
  (setq-local font-lock-defaults '(lua-font-lock-keywords))
  (setq-local comment-start "-- ")
  (setq-local smie-indent-basic 3)
  (smie-setup lua-smie-grammar #'lua-smie-rules
              :backward-token #'lua-smie-backward-token
              :forward-token #'lua-smie-forward-token))

(provide 'lua-smie)
;;; lua-smie.el ends here
