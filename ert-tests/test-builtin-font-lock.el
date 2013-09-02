(require 'ert)
(require 'lua-font-lock-test-helpers
         ;; let's try a bit to help Emacs find the helpers, just in case
         (concat (file-name-directory (or load-file-name (buffer-file-name)
                                          default-directory))
                 "lua-font-lock-test-helpers.el"))


(ert-deftest lua-font-lock-builtin-constants()
  (should-lua-font-lock-equal
   "a = { nil, true, false}"
   '(("nil" constant "true" constant "false" constant)))

  (should-lua-font-lock-equal
   "a = { foo.true, foo:false }"
   '(;; This case won't work while '.' has symbol syntax
     ;; ("true" constant "false" constant)
     ("false" constant))))
