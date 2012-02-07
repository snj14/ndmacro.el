
概要
====

- emacs用に作られたdmacro.elに連番機能を加えたxyzzyのndmacro.lをemacsで使えるようにしたもの。
- dmacro.el + 連番機能

設定
====

 (require 'ndmacro)
 (global-set-key (kbd "C-t") 'ndmacro)

使い方
======

 hoge C-m hoge C-m C-t C-t C-t ....

 10 C-m 11 C-m C-t C-t C-t ....


License
=======

- MIT License
- Copyright (c) 2012 snj14

