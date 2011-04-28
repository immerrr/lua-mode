;;; lua-mode.el --- a major-mode for editing Lua scripts

;; Copyright (C) 1997, 2001, 2004, 2006, 2007, 2010, 2011 Free Software Foundation, Inc.

;; Author: 2011 immerrr <immerrr+lua@gmail.com>
;;         2010-2011 Reuben Thomas <rrt@sc3d.org>
;;         2006 Juergen Hoetzel <juergen@hoetzel.info>
;;         2004 various (support for Lua 5 and byte compilation)
;;         2001 Christian Vogler <cvogler@gradient.cis.upenn.edu>
;;         1997 Bret Mogilefsky <mogul-lua@gelatinous.com> starting from
;;              tcl-mode by Gregor Schmid <schmid@fb3-s7.math.tu-berlin.de>
;;              with tons of assistance from
;;              Paul Du Bois <pld-lua@gelatinous.com> and
;;              Aaron Smith <aaron-lua@gelatinous.com>.
;;
;; URL:         http://lua-mode.luaforge.net/
;; Version:     20110428
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;; Keywords: languages, processes, tools


;;; Commentary:

;; Thanks to Tobias Polzin <polzin@gmx.de> for function indenting
;; patch: Indent "(" like "{"

;; Thanks to Fabien <fleutot@gmail.com> for imenu patches.

;; Thanks to Simon Marshall <simonm@mail.esrin.esa.it> and Olivier
;; Andrieu <oandrieu@gmail.com> for font-lock patches.

;; Additional font-lock highlighting and indentation tweaks by
;; Adam D. Moss <adam@gimp.org>.

;; INSTALLATION:

;; To install, just copy this file into a directory on your load-path
;; (and byte-compile it). To set up Emacs to automatically edit files
;; ending in ".lua" or with a lua hash-bang line using lua-mode add
;; the following to your init file:
;;
;; (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;; (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;; (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Usage

;; Lua-mode supports c-mode style formatting and sending of
;; lines/regions/files to a Lua interpreter. An interpreter (see
;; variable `lua-default-application') will be started if you try to
;; send some code and none is running. You can use the process-buffer
;; (named after the application you chose) as if it were an
;; interactive shell. See the documentation for `comint.el' for
;; details.

;; Lua-mode works with Hide Show minor mode (see ``hs-minor-mode``).

;; Key-bindings

;; To see all the keybindings for Lua mode, look at `lua-setup-keymap'
;; or start `lua-mode' and type `\C-h m'.
;; The keybindings may seem strange, since I prefer to use them with
;; lua-prefix-key set to nil, but since those keybindings are already used
;; the default for `lua-prefix-key' is `\C-c', which is the conventional
;; prefix for major-mode commands.

;; You can customise the keybindings either by setting `lua-prefix-key'
;; or by putting the following in your .emacs
;;      (define-key lua-mode-map <your-key> <function>)
;; for all the functions you need.


;;; Code:
(require 'comint)

;; Local variables
(defgroup lua nil
  "Major mode for editing lua code."
  :prefix "lua-"
  :group 'languages)

(defcustom lua-indent-level 3
  "Amount by which Lua subexpressions are indented."
  :type 'integer
  :group 'lua)

(defcustom lua-comment-start "-- "
  "Default value of `comment-start'."
  :type 'string
  :group 'lua)

(defcustom lua-comment-start-skip "-- "
  "Default value of `comment-start-skip'."
  :type 'string
  :group 'lua)

(defcustom lua-default-application "lua"
  "Default application to run in lua subprocess."
  :type 'string
  :group 'lua)

(defcustom lua-default-command-switches (list "-i")
  "Command switches for `lua-default-application'.
Should be a list of strings."
  :type '(repeat string)
  :group 'lua)

(defcustom lua-always-show t
  "*Non-nil means display lua-process-buffer after sending a command."
  :type 'boolean
  :group 'lua)

(defcustom lua-search-url-prefix "http://www.lua.org/manual/5.1/manual.html#pdf-"
  "*URL at which to search for documentation on a word"
  :type 'string
  :group 'lua)

(defvar lua-process nil
  "The active Lua subprocess")

(defvar lua-process-buffer nil
  "Buffer used for communication with Lua subprocess")

(defun lua--customize-set-prefix-key (prefix-key-sym prefix-key-val)
  ;; FIXME: enable assertion, it requires 'cl and I'm not sure of its availability
  ;; (assert (eq prefix-key-sym 'lua-prefix-key))
  (set prefix-key-sym (if (and prefix-key-val (> (length prefix-key-val) 0))
                          ;; read-kbd-macro returns a string or a vector
                          ;; in both cases (elt x 0) is ok
                          (elt (read-kbd-macro prefix-key-val) 0)))
  (if (fboundp 'lua-prefix-key-update-bindings)
      (lua-prefix-key-update-bindings))
  (message "prefix key set to %S"  (single-key-description (eval prefix-key-sym))))

(defcustom lua-prefix-key "\C-c"
  "Prefix for all lua-mode commands."
  :type 'string
  :group 'lua
  :set 'lua--customize-set-prefix-key
  :get '(lambda (sym)
          (let ((val (eval sym))) (if val (single-key-description (eval sym)) ""))))

(defvar lua-mode-menu (make-sparse-keymap "Lua")
  "Keymap for lua-mode's menu.")

(defvar lua-prefix-mode-map
  (eval-when-compile
    (let ((result-map (make-sparse-keymap)))
      (mapc (lambda (key_defn)
              (define-key result-map (read-kbd-macro (car key_defn)) (cdr key_defn)))
            '(("C-l" . lua-send-buffer)
              ("C-f" . lua-search-documentation)
              ("C-;" . lua-mark-all-multiline-literals)))
      result-map))
  "Keymap that is used to define keys accessible by `lua-prefix-key'.

If the latter is nil, the keymap translates into `lua-mode-map' verbatim.")

(defvar lua-mode-map
  (let ((result-map (make-sparse-keymap))
        prefix-key)
    (mapc (lambda (key_defn)
            (define-key result-map (read-kbd-macro (car key_defn)) (cdr key_defn)))
          ;; here go all the default bindings
          ;; backquote enables evaluating certain symbols by comma
          `(("}" . lua-electric-match)
            ("]" . lua-electric-match)
            (")" . lua-electric-match)))
    (define-key result-map [menu-bar lua-mode] (cons "Lua" lua-mode-menu))

    ;; FIXME: see if the declared logic actually works
    ;; handle prefix-keyed bindings:
    ;; * if no prefix, set prefix-map as parent, i.e.
    ;;      if key is not defined look it up in prefix-map
    ;; * if prefix is set, bind the prefix-map to that key
    (if (boundp 'lua-prefix-key)
        (define-key result-map (vector lua-prefix-key) lua-prefix-mode-map)
      (set-keymap-parent result-map lua-prefix-mode-map))
    result-map)
  "Keymap used in lua-mode buffers.")

(defvar lua-electric-flag t
  "If t, electric actions (like automatic reindentation) will happen when an electric
 key like `{' is pressed")
(make-variable-buffer-local 'lua-electric-flag)

(defcustom lua-prompt-regexp "[^\n]*\\(>[\t ]+\\)+$"
  "Regexp which matches the Lua program's prompt."
  :type  'regexp
  :group 'lua)

(defcustom lua-traceback-line-re
  "^\\(?:[\t ]*\\|.*>[\t ]+\\)\\([^\n\t ]+\\):\\([0-9]+\\):"
  "Regular expression that describes tracebacks and errors."
  :type 'regexp
  :group 'lua)

(defcustom lua-jump-on-traceback t
  "*Jump to innermost traceback location in *lua* buffer.  When this
variable is non-nil and a traceback occurs when running Lua code in a
subprocess, jump immediately to the source code of the innermost
traceback location."
  :type 'boolean
  :group 'lua)

(defvar lua-mode-hook nil
  "Hooks called when Lua mode fires up.")

(defvar lua-region-start (make-marker)
  "Start of special region for Lua communication.")

(defvar lua-region-end (make-marker)
  "End of special region for Lua communication.")

(defvar lua-emacs-menu
  '(["Restart With Whole File" lua-restart-with-whole-file t]
    ["Kill Process" lua-kill-process t]
    ["Hide Process Buffer" lua-hide-process-buffer t]
    ["Show Process Buffer" lua-show-process-buffer t]
    ["Beginning Of Proc" lua-beginning-of-proc t]
    ["End Of Proc" lua-end-of-proc t]
    ["Set Lua-Region Start" lua-set-lua-region-start t]
    ["Set Lua-Region End" lua-set-lua-region-end t]
    ["Send Lua-Region" lua-send-lua-region t]
    ["Send Current Line" lua-send-current-line t]
    ["Send Region" lua-send-region t]
    ["Send Proc" lua-send-proc t]
    ["Send Buffer" lua-send-buffer t]
    ["Search Documentation" lua-search-documentation t])
  "Emacs menu for Lua mode.")

(defvar lua-font-lock-keywords
  (eval-when-compile
    (list
     ;; Handle variable names
     ;;  local blalba =
     ;;        ^^^^^^
     '("\\(local[ \t]+\\(\\sw+\\)[ \t]*=\\)"
       (2 font-lock-variable-name-face))

     ;; Function name declarations.
     '("^[ \t]*\\_<\\(\\(local[ \t]+\\)?function\\)\\_>[ \t]+\\(\\(\\sw:\\|\\sw\\.\\|\\sw_\\|\\sw\\)+\\)"
       (1 font-lock-keyword-face) (3 font-lock-function-name-face nil t))

     ;; Handle function names in assignments
     '("\\(\\(\\sw:\\|\\sw\\.\\|\\sw_\\|\\sw\\)+\\)[ \t]*=[ \t]*\\(function\\)\\_>"
       (1 font-lock-function-name-face nil t) (3 font-lock-keyword-face))

     ;; Keywords.
     (concat "\\_<"
             (regexp-opt '("and" "break" "do" "else" "elseif" "end" "false"
                           "for" "function" "if" "in" "local" "nil" "not"
                           "or" "repeat" "return" "then" "true" "until"
                           "while") t)
             "\\_>")

     "Default expressions to highlight in Lua mode.")))

(defvar lua-imenu-generic-expression
  '((nil "^[ \t]*\\(?:local[ \t]+\\)?function[ \t]+\\(\\(\\sw:\\|\\sw_\\|\\sw\\.\\|\\sw\\)+\\)" 1))
  "Imenu generic expression for lua-mode.  See `imenu-generic-expression'.")

(defvar lua-mode-abbrev-table nil
  "Abbreviation table used in lua-mode buffers.")

(defvar lua-sexp-alist '(("then" . "end")
                         ("function" . "end")
                         ("do" . "end")))

(define-abbrev-table 'lua-mode-abbrev-table
  '(
    ("end" "end" lua-indent-line 0)
    ("else" "else" lua-indent-line 0)
    ("elseif" "elseif" lua-indent-line 0)
    ))

(eval-and-compile
  (defalias 'lua-make-temp-file
    (if (fboundp 'make-temp-file)
        'make-temp-file
      (lambda (prefix &optional dir-flag) ;; Simple implementation
        (expand-file-name
         (make-temp-name prefix)
         (if (fboundp 'temp-directory)
             (temp-directory)
           temporary-file-directory))))))

;;;###autoload
(defun lua-mode ()
  "Major mode for editing Lua code.
The following keys are bound:
\\{lua-mode-map}
"
  (interactive)
  (let ((switches nil)
        s)
    (kill-all-local-variables)
    (setq major-mode 'lua-mode)
    (setq mode-name "Lua")
    (setq comint-prompt-regexp lua-prompt-regexp)
    (make-local-variable 'lua-default-command-switches)
    (set (make-local-variable 'beginning-of-defun-function)
         'lua-beginning-of-proc)
    (set (make-local-variable 'end-of-defun-function) 'lua-end-of-proc)
    (set (make-local-variable 'indent-line-function) 'lua-indent-line)
    (set (make-local-variable 'comment-start) lua-comment-start)
    (set (make-local-variable 'comment-start-skip) lua-comment-start-skip)
    (set (make-local-variable 'font-lock-defaults)
         '(lua-font-lock-keywords
           nil nil ((?_ . "w"))))
    (set (make-local-variable 'imenu-generic-expression)
         lua-imenu-generic-expression)
    (setq local-abbrev-table lua-mode-abbrev-table)
    (abbrev-mode 1)
    (make-local-variable 'lua-default-eval)
    (use-local-map lua-mode-map)
    (set-syntax-table (copy-syntax-table))
    (modify-syntax-entry ?+ ".")
    (modify-syntax-entry ?- ". 12")
    (modify-syntax-entry ?* ".")
    (modify-syntax-entry ?/ ".")
    (modify-syntax-entry ?^ ".")
    ;; This might be better as punctuation, as for C, but this way you
    ;; can treat table index as symbol.
    (modify-syntax-entry ?. "_")        ; e.g. `io.string'
    (modify-syntax-entry ?> ".")
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?= ".")
    (modify-syntax-entry ?~ ".")
    (modify-syntax-entry ?\n ">")
    (modify-syntax-entry ?\' "\"")
    (modify-syntax-entry ?\" "\"")
    ;; setup menu bar entry (XEmacs style)
    (if (and (featurep 'menubar)
             (boundp 'current-menubar)
             (fboundp 'set-buffer-menubar)
             (fboundp 'add-menu)
             (not (assoc "Lua" current-menubar)))
        (progn
          (set-buffer-menubar (copy-sequence current-menubar))
          (add-menu nil "Lua" lua-emacs-menu)))
    ;; Append Lua menu to popup menu for Emacs.
    (if (boundp 'mode-popup-menu)
        (setq mode-popup-menu
              (cons (concat mode-name " Mode Commands") lua-emacs-menu)))

    ;; hideshow setup
    (unless (assq 'lua-mode hs-special-modes-alist)
      (add-to-list 'hs-special-modes-alist
                   `(lua-mode
                     ,(regexp-opt (mapcar 'car lua-sexp-alist) 'words) ;start
                     ,(regexp-opt (mapcar 'cdr lua-sexp-alist) 'words) ;end
                     nil lua-forward-sexp)))

    (set (make-local-variable 'parse-sexp-lookup-properties) t)
    (lua-mark-all-multiline-literals)
    (run-hooks 'lua-mode-hook)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

(defun lua-electric-match (arg)
  "Insert character and adjust indentation."
  (interactive "P")
  (insert-char last-command-event (prefix-numeric-value arg))
  (if lua-electric-flag
      (lua-indent-line))
  (blink-matching-open))

;; private functions

(defun lua-prefix-key-update-bindings ()
  (let (old-cons)
    (if (eq lua-prefix-mode-map (keymap-parent lua-mode-map))
        ;; if prefix-map is a parent, delete the parent
        (set-keymap-parent lua-mode-map nil)
      ;; otherwise, look for it among children
      (if (setq old-cons (rassoc lua-prefix-mode-map lua-mode-map))
          (delq old-cons lua-mode-map)))

    (if (null lua-prefix-key)
        (set-keymap-parent lua-mode-map lua-prefix-mode-map)
      (define-key lua-mode-map (vector lua-prefix-key) lua-prefix-mode-map))))

(defun lua-set-prefix-key (new-key-str)
  "Changes `lua-prefix-key' properly and updates keymaps

This function replaces previous prefix-key binding with a new one."
  (interactive "sNew prefix key (empty string means no key): ")
  (lua--customize-set-prefix-key 'lua-prefix-key new-key-str)
  (lua-prefix-key-update-bindings))

(defun lua-string-p (&optional pos)
  "Returns true if the point is in a string."
  (elt (syntax-ppss pos) 3))

(defun lua-comment-p (&optional pos)
  "Returns true if the point is in a comment."
  (elt (syntax-ppss pos) 4))

(defun lua-comment-or-string-p (&optional pos)
  "Returns true if the point is in a comment or string."
  (let ((parse-result (syntax-ppss pos)))
    (or (elt parse-result 3) (elt parse-result 4))))

(defun lua-indent-line ()
  "Indent current line for Lua mode.
Return the amount the indentation changed by."
  (let (indent
        (case-fold-search nil)
        ;; save point as a distance to eob - it's invariant w.r.t indentation
        (pos (- (point-max) (point))))
    (back-to-indentation)
    (if (lua-string-p)  ;; don't indent if inside multiline string literal
        (goto-char (- (point-max) pos)) ;; just restore point position

      (setq indent (max 0 (- (lua-calculate-indentation nil)
                             (lua-calculate-unindentation))))
      (when (not (equal indent (current-column)))
        (delete-region (line-beginning-position) (point))
        (indent-to indent))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos)))
      indent)))

(defun lua-find-regexp (direction regexp &optional limit ignore-p)
  "Searches for a regular expression in the direction specified.
Direction is one of 'forward and 'backward.
By default, matches in comments and strings are ignored, but what to ignore is
configurable by specifying ignore-p. If the regexp is found, returns point
position, nil otherwise.
ignore-p returns true if the match at the current point position should be
ignored, nil otherwise."
  (let ((ignore-func (or ignore-p 'lua-comment-or-string-p))
        (search-func (if (eq direction 'forward)
                         're-search-forward 're-search-backward))
        (case-fold-search nil))
    (catch 'found
      (while (funcall search-func regexp limit t)
        (if (not (funcall ignore-func))
            (throw 'found (point)))))))

(defconst lua-block-regexp
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("do" "function" "repeat" "then"
                   "else" "elseif" "end" "until") t)
     "\\_>\\)\\|"
     (regexp-opt '("{" "(" "[" "]" ")" "}") t))))

(defconst lua-block-token-alist
  ;; The absence of "else" is deliberate. This construct in a way both
  ;; opens and closes a block. As a result, it is difficult to handle
  ;; cleanly. It is also ambiguous - if we are looking for the match
  ;; of "else", should we look backward for "then/elseif" or forward
  ;; for "end"?
  ;; Maybe later we will find a way to handle it.
  '(("do"       "\\_<end\\_>"                                 open)
    ("function" "\\_<end\\_>"                                 open)
    ("repeat"   "\\_<until\\_>"                               open)
    ("then"     "\\_<\\(e\\(lseif\\|nd\\)\\)\\_>"             open)
    ("{"        "}"                                           open)
    ("["        "]"                                           open)
    ("("        ")"                                           open)
    ("elseif"   "\\_<then\\_>"                                close)
    ("end"      "\\_<\\(do\\|function\\|then\\)\\_>"          close)
    ("until"    "\\_<repeat\\_>"                              close)
    ("}"        "{"                                           close)
    ("]"        "\\["                                         close)
    (")"        "("                                           close)))


(defconst lua-indentation-modifier-regexp
  ;; The absence of else is deliberate, since it does not modify the
  ;; indentation level per se. It only may cause the line, in which the
  ;; else is, to be shifted to the left.
  (concat
   "\\(\\_<"
   ;; n.b. "local function" is a bit of a hack, allowing only a single space
   (regexp-opt '("do" "local function" "function" "repeat" "then") t)
   "\\_>\\|"
   (regexp-opt '("{" "(" "["))
   "\\)\\|\\(\\_<"
   (regexp-opt '("elseif" "end" "until") t)
   "\\_>\\|"
   (regexp-opt '("]" ")" "}"))
   "\\)")
  )

(defun lua-backwards-to-block-begin-or-end ()
  "Move backwards to nearest block begin or end.  Returns nil if not successful."
  (interactive)
  (lua-find-regexp 'backward lua-block-regexp))

(defun lua-find-matching-token-word (token search-start)
  (let* ((token-info (assoc token lua-block-token-alist))
         (match (car (cdr token-info)))
         (match-type (car (cdr (cdr token-info))))
         (search-direction (if (eq match-type 'open) 'forward 'backward)))
    ;; if we are searching forward from the token at the current point
    ;; (i.e. for a closing token), need to step one character forward
    ;; first, or the regexp will match the opening token.
    (if (eq match-type 'open) (forward-char 1))
    (if search-start (goto-char search-start))
    (catch 'found
      (while (lua-find-regexp search-direction lua-indentation-modifier-regexp)
        ;; have we found a valid matching token?
        (let ((found-token (match-string 0))
              (found-pos (match-beginning 0)))
          (if (string-match match found-token)
              (throw 'found found-pos))
          ;; no - then there is a nested block. If we were looking for
          ;; a block begin token, found-token must be a block end
          ;; token; likewise, if we were looking for a block end token,
          ;; found-token must be a block begin token, otherwise there
          ;; is a grammatical error in the code.
          (if (not (and
                    (eq (car (cdr (cdr (assoc found-token lua-block-token-alist))))
                        match-type)
                    (lua-find-matching-token-word found-token nil)))
              (throw 'found nil)))))))

(defun lua-goto-matching-block-token (&optional search-start parse-start)
  "Find block begion/end token matching the one at the point.
This function moves the point to the token that matches the one
at the current point. Returns the point position of the first character of
the matching token if successful, nil otherwise."
  (if parse-start (goto-char parse-start))
  (let ((case-fold-search nil))
    (if (looking-at lua-indentation-modifier-regexp)
        (let ((position (lua-find-matching-token-word (match-string 0)
                                                      search-start)))
          (and position
               (goto-char position))))))

(defun lua-goto-matching-block (&optional noreport)
  "Go to the keyword balancing the one under the point.
If the point is on a keyword/brace that starts a block, go to the
matching keyword that ends the block, and vice versa."
  (interactive)
  ;; search backward to the beginning of the keyword if necessary
  (if (eq (char-syntax (following-char)) ?w)
      (re-search-backward "\\_<" nil t))
  (let ((position (lua-goto-matching-block-token)))
    (if (and (not position)
             (not noreport))
        (error "Not on a block control keyword or brace")
      position)))

(defun lua-forward-line-skip-blanks (&optional back)
  "Move 1 line forward (back if BACK is non-nil) skipping blank lines.

Moves point 1 line forward (or backward) skipping lines that contain
no Lua code besides comments. The point is put to the beginning of
the line.

Returns final value of point as integer or nil if operation failed."
  (catch 'found
    (while t
      (unless (eql (forward-line (if back -1 1)) 0)    ;; 0 means success
        (throw 'found nil))
      (unless (looking-at "\\s *\\(--.*\\)?$")       ;; blank lua line
        (throw 'found (point))))))

(eval-when-compile
  (defconst lua-operator-class
    "-+*/^.=<>~"))

(defconst lua-cont-eol-regexp
  "Regexp that matches the ending of a line that needs continuation

This regexp starts from eol and looks for a binary operator or an unclosed
block intro (i.e. 'for' without 'do' or 'if' without 'then') followed by
an optional whitespace till the end of the line."
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("and" "or" "not" "in" "for" "while"
                   "local" "function") t)
     "\\_>\\|"
     "\\(^\\|[^" lua-operator-class "]\\)"
     (regexp-opt '("+" "-" "*" "/" "^" ".." "==" "=" "<" ">" "<=" ">=" "~=") t)
     "\\)"
     "\\s *\\=")
    ))


(defconst lua-cont-bol-regexp
  "Regexp that matches a line that continues previous one

This regexp means, starting from point there is an optional whitespace followed
by Lua binary operator. Lua is very liberal when it comes to continuation line,
so we're safe to assume that every line that starts with a binop continues
previous one even though it looked like an end-of-statement."
  (eval-when-compile
    (concat
     "\\=\\s *"
     "\\(\\_<"
     (regexp-opt '("and" "or" "not") t)
     "\\_>\\|"
     (regexp-opt '("+" "-" "*" "/" "^" ".." "==" "=" "<" ">" "<=" ">=" "~=") t)
     "\\($\\|[^" lua-operator-class "]\\)"
     "\\)")

    ))

(defun lua-last-token-continues-p ()
  "Returns true if the last token on this line is a continuation token."
  (let ((line-begin (line-beginning-position))
        (line-end (line-end-position)))
    (save-excursion
      (end-of-line)
      ;; we need to check whether the line ends in a comment and
      ;; skip that one.
      (while (lua-find-regexp 'backward "-" line-begin 'lua-string-p)
        (if (looking-at "--")
            (setq line-end (point))))
      (goto-char line-end)
      (re-search-backward lua-cont-eol-regexp line-begin t))))

(defun lua-first-token-continues-p ()
  "Returns true if the first token on this line is a continuation token."
  (let ((line-end (line-end-position)))
    (save-excursion
      (beginning-of-line)
      (re-search-forward lua-cont-bol-regexp line-end t))))

(defun lua-is-continuing-statement-p (&optional parse-start)
  "Return non-nil if the line continues a statement.
More specifically, return the point in the line that is continued.
The criteria for a continuing statement are:

* the last token of the previous line is a continuing op,
  OR the first token of the current line is a continuing op

"
  (let ((prev-line nil))
    (save-excursion
      (if parse-start (goto-char parse-start))
      (save-excursion (setq prev-line (lua-forward-line-skip-blanks 'back)))
      (and prev-line
           (or (lua-first-token-continues-p)
               (and (goto-char prev-line)
                    ;; check last token of previous nonblank line
                    (lua-last-token-continues-p)))))))

(defun lua-make-indentation-info-pair (found-token found-pos)
  "This is a helper function to lua-calculate-indentation-info. Don't
use standalone."
  (cond
   ((string-equal found-token "function")
    ;; this is the location where we need to start searching for the
    ;; matching opening token, when we encounter the next closing token.
    ;; It is primarily an optimization to save some searching time.
    (cons 'absolute (+ (save-excursion (goto-char found-pos)
                                       (current-column))
                       lua-indent-level)))

   ((or (string-equal found-token "{")
        (string-equal found-token "("))
    (save-excursion
      ;; expression follows -> indent at start of next expression
      (if (and (not (search-forward-regexp "[[:space:]]--" (line-end-position) t))
               (search-forward-regexp "[^[:space:]]" (line-end-position) t))
          (cons 'absolute (1- (current-column)))
        (cons 'relative lua-indent-level))))

   ;; closing tokens follow
   ((string-equal found-token "end")
    (save-excursion
      (lua-goto-matching-block-token nil found-pos)
      (if (looking-at "\\_<function\\_>")
          (cons 'absolute
                (+ (current-indentation)
                   (lua-calculate-indentation-block-modifier
                    nil (point))))
        (cons 'relative (- lua-indent-level)))))

   ((or (string-equal found-token ")")
        (string-equal found-token "}"))
    (save-excursion
      (lua-goto-matching-block-token nil found-pos)
      (cons 'absolute
            (+ (current-indentation)
               (lua-calculate-indentation-block-modifier
                nil (point))))))

   ('other-indentation-modifier
    (cons 'relative (if (nth 2 (match-data))
                        ;; beginning of a block matched
                        lua-indent-level
                      ;; end of a block matched
                      (- lua-indent-level))))))


(defun lua-calculate-indentation-info (&optional parse-start parse-end)
  "For each block token on the line, computes how it affects the indentation.
The effect of each token can be either a shift relative to the current
indentation level, or indentation to some absolute column. This information
is collected in a list of indentation info pairs, which denote absolute
and relative each, and the shift/column to indent to."
  (let* ((line-end (line-end-position))
         (search-stop (if parse-end (min parse-end line-end) line-end))
         (indentation-info nil))
    (if parse-start (goto-char parse-start))
    (save-excursion
      (beginning-of-line)
      (while (lua-find-regexp 'forward lua-indentation-modifier-regexp
                              search-stop)
        (let ((found-token (match-string 0))
              (found-pos (match-beginning 0))
              (found-end (match-end 0))
              (data (match-data)))
          (setq indentation-info
                (cons (lua-make-indentation-info-pair found-token found-pos) indentation-info)))))
    indentation-info))

(defun lua-accumulate-indentation-info (info)
  "Accumulates the indentation information previously calculated by
lua-calculate-indentation-info. Returns either the relative indentation
shift, or the absolute column to indent to."
  (let ((info-list (reverse info))
        (type 'relative)
        (accu 0))
    (mapc (lambda (x)
            (setq accu (if (eq 'absolute (car x))
                           (progn (setq type 'absolute)
                                  (cdr x))
                         (+ accu (cdr x)))))
          info-list)
    (cons type accu)))

(defun lua-calculate-indentation-block-modifier (&optional parse-start
                                                           parse-end)
  "Return amount by which this line modifies the indentation.
Beginnings of blocks add lua-indent-level once each, and endings
of blocks subtract lua-indent-level once each. This function is used
to determine how the indentation of the following line relates to this
one."
  (if parse-start (goto-char parse-start))
  (let ((case-fold-search nil)
        (indentation-info (lua-accumulate-indentation-info
                           (lua-calculate-indentation-info nil parse-end))))
    (if (eq (car indentation-info) 'absolute)
        (- (cdr indentation-info)
           (current-indentation)
           ;; reduce indentation if this line also starts new continued statement
           ;; or next line cont. this line
           ;;This is for aesthetic reasons: the indentation should be
           ;;dosomething(d +
           ;;   e + f + g)
           ;;not
           ;;dosomething(d +
           ;;      e + f + g)"
           (save-excursion
             (or (and (lua-last-token-continues-p) lua-indent-level)
                 (and (lua-forward-line-skip-blanks) (lua-first-token-continues-p) lua-indent-level)
                 0)))
      (+ (lua-calculate-unindentation)
         (cdr indentation-info)
         (if (lua-is-continuing-statement-p) (- lua-indent-level) 0)))))

(defconst lua-unindentation-regexp
  ;; Compare the following situations:
  ;; {               {
  ;;    a,              a,
  ;;    b,              b,
  ;; }                  c}
  ;;
  ;; Basically, if close-token is prepended with some expression, such
  ;; expression  should still  be indented  and thus  any close-tokens
  ;; after an  expression doesn't  unindent the line.
  ;;
  ;; Whitespace and semicolons aren't an expression and may be skipped
  ;; (technically, multiline  comment --[[ ]] ending on  the same line
  ;; might also be skipped, but it's not handled properly right now).
  ;;
  ;; skip whitespaces and semicolons, closing keywords/parentheses
  (concat "[\\s ;]*"
          "\\(?1:\\_<" (regexp-opt '("else" "elseif" "until" "end")) "\\_>"
          "\\|" (regexp-opt '("]" "}" ")")) "\\)"))

(defun lua-calculate-unindentation (&optional parse-start)
  "Return amount, by which this line should be unindented.

Starting  from the  beginning of  the line,  look for  an  sequence of
block-closing tokens with  only whitespace/semicolons in between them.
For each of these tokens, shift  indentation to the left by the amount
specified in lua-indent-level.

If PARSE-START is  not nil, start from the beginning  of the line that
contains position PARSE-START."
  (let ((unindentation-accumulator 0)
        (case-fold-search nil)
        (block-token nil))
    (save-excursion
      (if parse-start (goto-char parse-start))
      (back-to-indentation)

      (while (and (looking-at lua-unindentation-regexp)
                  (not (lua-comment-or-string-p)))
        (let ((last-token (match-string 1)))
          (setq unindentation-accumulator (+ unindentation-accumulator
                                             lua-indent-level))
          (forward-char (length (match-string 0)))))
      unindentation-accumulator)))

(defun lua-calculate-indentation (&optional parse-start)
  "Return appropriate indentation for current line as Lua code.
In usual case returns an integer: the column to indent to."
  (let ((pos (point))
        shift-amt)
    (save-excursion
      (catch 'indent
        (if parse-start (setq pos (goto-char parse-start)))
        (beginning-of-line)

        ;; if bol is inside a string, suppress any indentation
        ;; or all of the whitespace will go into the literal
        (when (lua-string-p)
          (throw 'indent 0))

        (setq shift-amt (if (lua-is-continuing-statement-p) lua-indent-level 0))
        (if (bobp)          ; If we're at the beginning of the buffer, no change.
            (+ (current-indentation) shift-amt)
          ;; This code here searches backwards for a "block beginning/end"
          ;; It snarfs the indentation of that, plus whatever amount the
          ;; line was shifted left by, because of block end tokens. It
          ;; then adds the indentation modifier of that line to obtain the
          ;; final level of indentation.
          ;; Finally, if this line continues a statement from the
          ;; previous line, add another level of indentation.
          (if (lua-backwards-to-block-begin-or-end)
              ;; now we're at the line with block beginning or end.
              (max (+ (current-indentation)
                      (lua-calculate-indentation-block-modifier)
                      shift-amt)
                   0)
            ;; Failed to find a block begin/end.
            ;; Just use the previous line's indent.
            (goto-char pos)
            (beginning-of-line)
            (forward-line -1)
            (+ (current-indentation) shift-amt)))))))

(defun lua-beginning-of-proc (&optional arg)
  "Move backward to the beginning of a lua proc (or similar).
With argument, do it that many times.  Negative arg -N
means move forward to Nth following beginning of proc.
Returns t unless search stops due to beginning or end of buffer."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
        (ret t))
    (while (< arg 0)
      (if (re-search-forward "^function[ \t]" nil t)
          (setq arg (1+ arg)
                found t)
        (setq ret nil
              arg 0)))
    (if found
        (beginning-of-line))
    (if (> arg 0)
        (if (re-search-forward "^function[ \t]" nil t)
            (setq arg (1+ arg))
          (goto-char (point-max))))
    (while (> arg 0)
      (if (re-search-backward "^function[ \t]" nil t)
          (setq arg (1- arg))
        (setq ret nil
              arg 0)))
    ret))

(defun lua-end-of-proc (&optional arg)
  "Move forward to next end of lua proc (or similar).
With argument, do it that many times.  Negative argument -N means move
back to Nth preceding end of proc.

This function just searches for a `end' at the beginning of a line."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
        (ret t))
    (if (and (< arg 0)
             (not (bolp))
             (save-excursion
               (beginning-of-line)
               (eq (following-char) ?})))
        (forward-char -1))
    (while (> arg 0)
      (if (re-search-forward "^end" nil t)
          (setq arg (1- arg)
                found t)
        (setq ret nil
              arg 0)))
    (while (< arg 0)
      (if (re-search-backward "^end" nil t)
          (setq arg (1+ arg)
                found t)
        (setq ret nil
              arg 0)))
    (if found
        (progn
          (beginning-of-line)
          (forward-line)))
    ret))

(defun lua-start-process (&optional name program startfile &rest switches)
  "Start a lua process named NAME, running PROGRAM.
PROGRAM defaults to NAME, which defaults to `lua-default-application'.
When called interactively, switch to the process buffer."
  (interactive)
  (or switches
      (setq switches lua-default-command-switches))
  (setq name (or name lua-default-application))
  (setq program (or program name))
  (setq lua-process-buffer (apply 'make-comint name program startfile switches))
  (setq lua-process (get-buffer-process lua-process-buffer))
  ;; wait for prompt
  (with-current-buffer lua-process-buffer
    (while (not (lua-prompt-line))
      (accept-process-output (get-buffer-process (current-buffer)))
      (goto-char (point-max))))
  ;; when called interactively, switch to process buffer
  (if (called-interactively-p 'any)
      (switch-to-buffer lua-process-buffer)))

(defun lua-kill-process ()
  "Kill lua subprocess and its buffer."
  (interactive)
  (if lua-process-buffer
      (kill-buffer lua-process-buffer)))

(defun lua-set-lua-region-start (&optional arg)
  "Set start of region for use with `lua-send-lua-region'."
  (interactive)
  (set-marker lua-region-start (or arg (point))))

(defun lua-set-lua-region-end (&optional arg)
  "Set end of region for use with `lua-send-lua-region'."
  (interactive)
  (set-marker lua-region-end (or arg (point))))

(defun lua-send-current-line ()
  "Send current line to lua subprocess, found in `lua-process'.
If `lua-process' is nil or dead, start a new process first."
  (interactive)
  (lua-send-region (line-beginning-position) (line-end-position)))

(defun lua-send-region (start end)
  "Send region to lua subprocess."
  (interactive "r")
  ;; make temporary lua file
  (let ((tempfile (lua-make-temp-file "lua-"))
        (last-prompt nil)
        (prompt-found nil)
        (lua-stdin-line-offset (count-lines (point-min) start))
        (lua-stdin-buffer (current-buffer))
        current-prompt )
    (write-region start end tempfile)
    (or (and lua-process
             (comint-check-proc lua-process-buffer))
        (lua-start-process lua-default-application))
    ;; kill lua process without query
    (if (fboundp 'process-kill-without-query)
        (process-kill-without-query lua-process))
    ;; send dofile(tempfile)
    (with-current-buffer lua-process-buffer
      (goto-char (point-max))
      (setq last-prompt (point-max))
      (comint-simple-send (get-buffer-process (current-buffer))
                          (format "dofile(\"%s\")"
                                  (replace-regexp-in-string "\\\\" "\\\\\\\\" tempfile)))
      ;; wait for prompt
      (while (not prompt-found)
        (accept-process-output (get-buffer-process (current-buffer)))
        (goto-char (point-max))
        (setq prompt-found (and (lua-prompt-line) (< last-prompt (point-max)))))
      ;; remove temp. lua file
      (delete-file tempfile)
      (lua-postprocess-output-buffer lua-process-buffer last-prompt lua-stdin-line-offset)
      (if lua-always-show
          (display-buffer lua-process-buffer)))))

(defun lua-postprocess-output-buffer (buf start &optional lua-stdin-line-offset)
  "Highlight tracebacks found in buf. If an traceback occurred return
t, otherwise return nil.  BUF must exist."
  (let ((lua-stdin-line-offset (or lua-stdin-line-offset 0))
        line file bol err-p)
    (with-current-buffer buf
      (goto-char start)
      (beginning-of-line)
      (if (re-search-forward lua-traceback-line-re nil t)
          (setq file (match-string 1)
                line (string-to-number (match-string 2)))))
    (when (and lua-jump-on-traceback line)
      (beep)
      ;; FIXME: highlight
      (lua-jump-to-traceback file line lua-stdin-line-offset)
      (setq err-p t))
    err-p))

(defun lua-jump-to-traceback (file line lua-stdin-line-offset)
  "Jump to the Lua code in FILE at LINE."
  ;; sanity check: temporary-file-directory
  (if (string= (substring file 0 3)  "...")
      (message "Lua traceback output truncated: customize 'temporary-file-directory' or increase 'LUA_IDSIZE' in 'luaconf.h'.")
    (let ((buffer (cond ((or (string-equal file tempfile) (string-equal file "stdin"))
                         (setq line (+ line lua-stdin-line-offset))
                         lua-stdin-buffer)
                        (t (find-file-noselect file)))))
      (pop-to-buffer buffer)
      ;; Force Lua mode
      (if (not (eq major-mode 'lua-mode))
          (lua-mode))
      ;; FIXME: fix offset when executing region
      (goto-line line)
      (message "Jumping to error in file %s on line %d" file line))))

(defun lua-prompt-line ()
  (save-excursion
    (save-match-data
      (forward-line 0)
      (if (looking-at comint-prompt-regexp)
          (match-end 0)))))

(defun lua-send-lua-region ()
  "Send preset lua region to lua subprocess."
  (interactive)
  (or (and lua-region-start lua-region-end)
      (error "lua-region not set"))
  (or (and lua-process
           (comint-check-proc lua-process-buffer))
      (lua-start-process lua-default-application))
  (comint-simple-send lua-process
                      (buffer-substring lua-region-start lua-region-end)
                      )
  (if lua-always-show
      (display-buffer lua-process-buffer)))

(defun lua-send-proc ()
  "Send proc around point to lua subprocess."
  (interactive)
  (let (beg end)
    (save-excursion
      (lua-beginning-of-proc)
      (setq beg (point))
      (lua-end-of-proc)
      (setq end (point)))
    (or (and lua-process
             (comint-check-proc lua-process-buffer))
        (lua-start-process lua-default-application))
    (comint-simple-send lua-process
                        (buffer-substring beg end))
    (if lua-always-show
        (display-buffer lua-process-buffer))))

;; FIXME: This needs work... -Bret
(defun lua-send-buffer ()
  "Send whole buffer to lua subprocess."
  (interactive)
  (lua-send-region (point-min) (point-max)))

(defun lua-restart-with-whole-file ()
  "Restart lua subprocess and send whole file as input."
  (interactive)
  (lua-kill-process)
  (lua-start-process lua-default-application)
  (lua-send-buffer))

(defun lua-show-process-buffer ()
  "Make sure `lua-process-buffer' is being displayed."
  (interactive)
  (display-buffer lua-process-buffer))

(defun lua-hide-process-buffer ()
  "Delete all windows that display `lua-process-buffer'."
  (interactive)
  (delete-windows-on lua-process-buffer))

(defun lua-search-documentation ()
  "Search Lua documentation for the word at the point."
  (interactive)
  (browse-url (concat lua-search-url-prefix (current-word t))))

(defun lua-toggle-electric-state (&optional arg)
  "Toggle the electric indentation feature.
Optional numeric ARG, if supplied, turns on electric indentation when
positive, turns it off when negative, and just toggles it when zero or
left out."
  (interactive "P")
  (let ((num_arg (prefix-numeric-value arg)))
    (setq lua-electric-flag (cond ((or (null arg)
                                       (zerop num_arg)) (not lua-electric-flag))
                                  ((< num_arg 0) nil)
                                  ((> num_arg 0) t))))
  (message "%S" lua-electric-flag))

(defun lua-forward-sexp (&optional count)
  "Forward to block end"
  (interactive "p")
  (save-match-data
    (let* ((count (or count 1))
           (block-start (mapcar 'car lua-sexp-alist))
           (block-end (mapcar 'cdr lua-sexp-alist))
           (block-regex (regexp-opt (append  block-start block-end) 'words))
           current-exp
           )
      (while (> count 0)
        ;; skip whitespace
        (skip-chars-forward " \t\n")
        (if (looking-at (regexp-opt block-start 'words))
            (let ((keyword (match-string 1)))
              (lua-find-matching-token-word keyword nil))
          ;; If the current keyword is not a "begin" keyword, then just
          ;; perform the normal forward-sexp.
          (forward-sexp 1))
        (setq count (1- count))))))


;; menu bar

(define-key lua-mode-menu [restart-with-whole-file]
  '("Restart With Whole File" .  lua-restart-with-whole-file))
(define-key lua-mode-menu [kill-process]
  '("Kill Process" . lua-kill-process))

(define-key lua-mode-menu [hide-process-buffer]
  '("Hide Process Buffer" . lua-hide-process-buffer))
(define-key lua-mode-menu [show-process-buffer]
  '("Show Process Buffer" . lua-show-process-buffer))

(define-key lua-mode-menu [end-of-proc]
  '("End Of Proc" . lua-end-of-proc))
(define-key lua-mode-menu [beginning-of-proc]
  '("Beginning Of Proc" . lua-beginning-of-proc))

(define-key lua-mode-menu [send-lua-region]
  '("Send Lua-Region" . lua-send-lua-region))
(define-key lua-mode-menu [set-lua-region-end]
  '("Set Lua-Region End" . lua-set-lua-region-end))
(define-key lua-mode-menu [set-lua-region-start]
  '("Set Lua-Region Start" . lua-set-lua-region-start))

(define-key lua-mode-menu [send-current-line]
  '("Send Current Line" . lua-send-current-line))
(define-key lua-mode-menu [send-region]
  '("Send Region" . lua-send-region))
(define-key lua-mode-menu [send-proc]
  '("Send Proc" . lua-send-proc))
(define-key lua-mode-menu [send-buffer]
  '("Send Buffer" . lua-send-buffer))
(define-key lua-mode-menu [search-documentation]
  '("Search Documentation" . lua-search-documentation))

(defsubst lua-put-char-property (pos property value &optional object)
  (if value
      (put-text-property pos (1+ pos) property value object)
    (remove-text-properties pos (1+ pos) (list property nil))))

(defsubst lua-put-char-syntax-table (pos value &optional object)
  (lua-put-char-property pos 'syntax-table value object))

(defsubst lua-get-multiline-delim-syntax (type)
  (cond ((eq type 'string) '(15))
        ((eq type 'comment) '(14))
        (nil)))

(defun lua-mark-char-multiline-delim (pos type)
  "Mark character as a delimiter of Lua multiline construct

If TYPE is string, mark char  as string delimiter. If TYPE is comment,
mark char as comment delimiter.  Otherwise, remove the mark if any."
  (let ((old-modified-p (buffer-modified-p)))
    (unwind-protect
        (lua-put-char-syntax-table pos (lua-get-multiline-delim-syntax type))
      (set-buffer-modified-p old-modified-p))))

(defsubst lua-inside-multiline-p (&optional pos)
  (let ((status (syntax-ppss pos)))
    (or (eq (elt status 3) t)                ;; inside generic string
        (eq (elt status 7) 'syntax-table)))) ;; inside generic comment

(defun lua-get-multiline-start (&optional pos)
  (interactive)
  (when (lua-inside-multiline-p pos) ;; return string/comment start
    (elt (syntax-ppss pos) 8)))

(defun lua-unmark-multiline-literals (&optional begin end)
  "Clears all Lua multiline construct markers in region

If BEGIN is nil, start from `beginning-of-buffer'.
If END is nil, stop at `end-of-buffer'."
  (interactive)
  (let ((old-modified-p (buffer-modified-p)))
    (unwind-protect
        (remove-text-properties (or begin 1) (or end (buffer-size)) '(syntax-table ()))
      (set-buffer-modified-p old-modified-p)))
  (font-lock-fontify-buffer))

(defun lua-mark-multiline-region (begin end)
  (let ((type (if (eq ?- (char-after begin)) 'comment 'string)))
  (lua-mark-char-multiline-delim begin type)
  (when end
    (lua-mark-char-multiline-delim (1- end) type))))

(defun lua-mark-all-multiline-literals (&optional begin end)
  "Marks all Lua multiline constructs in region

If BEGIN is nil, start from `beginning-of-buffer'.
If END is nil, stop at `end-of-buffer'."
  (interactive)

  (if (and (called-interactively-p 'any) (use-region-p))
      (setq begin (region-beginning)
            end (region-end)))

  (lua-unmark-multiline-literals begin end)
  (save-excursion
    (goto-char (or begin 1))

    (while (and
            ;; must check  for point range,  because matching previous
            ;; multiline  end might  move  point beyond  end and  this
            ;; drives `re-search-forward' crazy
            (if end (< (point) end) t)
            ;; look for
            ;; 1. (optional) two or more dashes followed by
            ;; 2. lua multiline delimiter [[
            (re-search-forward "\\(?2:--\\)?\\[\\(?1:=*\\)\\[" end 'noerror))
      ;; match-start + 1 is considered instead of match-start, because
      ;; such  approach  handles  '---[[' situation  correctly:  Emacs
      ;; thinks 2nd dash (i.e.  match-start) is not yet a comment, but
      ;; the third one is, hence the +1.  In all the other situations,
      ;; '+1'  is safe  to use  because  it bears  the same  syntactic
      ;; properties, i.e.  if match-start is inside string-or-comment,
      ;; then '+1' is too and vice versa.
      ;;
      ;; PS. ping me if you find a situation in which this is not true
      (unless (lua-comment-or-string-p (1+ (match-beginning 0)))
        (let (ml-begin ml-end)
          (setq ml-begin (match-beginning 0))
          (when (re-search-forward (format "\\]%s\\]" (or (match-string 1) "")) nil 'noerror)
            (message "found match %s" (match-string 0))
            (setq ml-end (match-end 0)))
          (lua-mark-multiline-region ml-begin ml-end))))))

(provide 'lua-mode)


;;; lua-mode.el ends here
