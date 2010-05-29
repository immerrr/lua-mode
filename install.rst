=============================
 Lua-mode
=============================

:Maintainer: Reuben Thomas
:Contact: rrt@sc3d.org

.. contents::

INSTALLATION
------------
To install, just drop ``lua-mode.el`` into a directory on your ``load-path`` (and optionally 
byte-compile it).  

To set up Emacs to automatically edit files ending in .lua using Lua-mode add the following to your ``~/.emacs`` file (GNU
Emacs) or ``~/.xemacs/init.el`` file (XEmacs)::

(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

lua-mode works with Hide Show minor mode (see ``hs-minor-mode``).

DOWNLOAD
------------
Lua-mode is available from the project_ page at LuaForge.

.. _project: http://luaforge.net/projects/lua-mode/