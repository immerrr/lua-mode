This directory contains files used to test lua-mode indentation engine. Test
input is stored in `*.lua` files which are compared to their respective etalon
files `*.lua.etalon`.

To run all tests, do
```
$ ./test_indentation.sh *.lua
```

Feel free to add your own tricky cases, if you don't see them here, no specific
guidelines here, just try to bundle them logically into several bigger files.

Additional lua-mode customization may be performed on a file basis with the use
of [emacs file variables].

[emacs file variables]: http://www.gnu.org/software/emacs/manual/html_node/emacs/File-Variables.html
