=============================
 Lua-mode
=============================

:Author: Jürgen Hötzel
:Contact: juergen@hoetzel.info

.. contents::

INSTALLATION
------------
To install, just drop ``lua-mode.el`` into a directory on your ``load-path`` (and optionally 
byte-compile it).  

To set up Emacs to automatically edit files ending in .lua using Lua-mode add the following to your ``~/.emacs`` file (GNU
Emacs) or ``~/.xemacs/init.el`` file (XEmacs)::

(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

If you want colorization, turn on ``global-font-lock`` or add this::

(add-hook 'lua-mode-hook 'turn-on-font-lock)