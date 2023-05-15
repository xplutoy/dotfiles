;;; init.el --- emacs init.el. -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:
;; elpa-init
(require 'package)
(setq package-archives
      '(("melpa"         . "https://melpa.org/packages/")
        ("melpa-stable"  . "https://stable.melpa.org/packages/")
        ("gnu"           . "https://elpa.gnu.org/packages/")
        ("nongnu"        . "https://elpa.nongnu.org/nongnu/")))
(setq package-quickstart t)
(package-activate-all)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'init-ui)
(require 'init-utils)
(require 'init-basic)
(require 'init-keymaps)
(require 'init-completion)
(require 'init-evil)
(require 'init-misc)
(require 'init-notes)
(require 'init-mail)
(require 'init-elfeed)
(require 'init-ide)

;;; init.el ends here
