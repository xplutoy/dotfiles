;;; dump.el --- dump-init.                           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  yangxue

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
                                        ;
;; borrow from "Painless Transition to Portable Dumper"
;; @see https://archive.casouri.cc/note/2020/painless-transition-to-portable-dumper/index.html

;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'init-utils)

(require 'package)
(package-initialize)
(setq yx/dumped-load-path load-path
      yx/dumped t)

;; 一些需要在包加载之前设置的参数
(setq evil-disable-insert-state-bindings t)

(dolist (package
         '(elec-pair
           savehist
           winner
           flyspell
           flymake
           eglot
           use-package
           ;; evil
           evil
           evil-owl
           evil-mc
           evil-escape
           evil-surround
           evil-matchit
           evil-visualstar
           vimish-fold
           evil-vimish-fold
           ;; org
           org
           org-journal
           org-transclusion
           ;; org-super-agenda (dont dump this package)
           olivetti
           denote
           consult-notes
           ;; minibuffer
           vertico
           orderless
           embark
           consult
           corfu
           cape
           ;; keymaps
           hydra
           major-mode-hydra
           general
           ;; misc
           elfeed
           posframe
           shackle
           gcmh
           ace-window
           avy
           ibuffer-vc
           indent-guide
           golden-ratio
           winum
           ef-themes
           minions
           zoxide
           move-text
           which-key
           aggressive-indent
           rainbow-delimiters
           buffer-move
           ;; programming
           yasnippet
           yasnippet-snippets
           ))
  (require package))
;; pre-load themes
(load-theme 'ef-duo-light t t)
;; dump image
(dump-emacs-portable (expand-file-name "emacs.pdmp" user-emacs-directory))

(provide 'dump)
;;; dump.el ends here
