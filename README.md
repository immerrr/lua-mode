# Lua mode

**lua-mode** is a major mode for editing Lua sources in Emacs.


If you have a problem or a suggestion about **lua-mode**, please, let me know about it via github's [Issue Tracker](https://github.com/immerrr/lua-mode/issues).

## INSTALLATION

### EL-GET INSTALLATION

[El-get](https://github.com/dimitri/el-get) is a package manager which greatly simplifies adding
modules to your Emacs and keeping them up-to-date. Once you have **el-get** set up, installing
**lua-mode** can be done with

    <M-x> el-get-install "lua-mode"

and updating is no more than

    <M-x> el-get-update "lua-mode"`
    
Please, consult with [el-get documentation](https://github.com/dimitri/el-get/blob/master/README.md) for further information.

### MANUAL INSTALLATION

To install, you need to make sure that `lua-mode.el` is on your load-path (and optionally byte-compile
it) and to set up Emacs to automatically enable **lua-mode** for `*.lua` files or ones that contain lua
hash-bang line (`#!/usr/bin/lua`). Putting this snippet to `.emacs` should be enough in most cases:
```lisp
    ;;;; This snippet enables lua-mode

    ;; This line is not necessary, if lua-mode.el is already on your load-path
    (add-to-list 'load-path "/path/to/directory/where/lua-mode-el/resides")

    (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
    (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
```

## FEATURES

- syntactic indentation & highlighting (including multiline literals/comments)
- evaluation of lines/regions/functions/files in Lua subprocess or direct interaction with its REPL
- documentation lookup (using online/offline reference manual, e.g. [string.find](http://www.lua.org/manual/5.1/manual.html#pdf-string.find))
- [imenu](http://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html) integration
- [HideShow](http://www.gnu.org/software/emacs/manual/html_node/emacs/Hideshow.html) integration

## CUSTOMIZATION

The following variables are available for customization (see more via `M-x customize-group lua`):

- `lua-indent-level` (default `3`): indentation offset in spaces
- `lua-default-application` (default `"lua"`): command to start up the interpreter
- `lua-default-command-switches` (default `"-i"`): arguments to pass to the interpreter on startup (make sure `-i` is there if you expect working with REPL)
- `lua-search-url-prefix` (default `"http://www.lua.org/manual/5.1/manual.html#pdf-"`): base URL for documentation lookup
- `lua-indent-string-contents` (default `nil`): set to `t` if you like to have contents of multiline strings to be indented like comments
