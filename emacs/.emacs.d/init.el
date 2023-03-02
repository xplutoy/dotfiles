;;; init.el --- emacs init.el. -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'init-utils)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; elpa-init
(require 'package)
(setq package-archives
      '(("melpa"         . "https://melpa.org/packages/")
        ("melpa-stable"  . "https://stable.melpa.org/packages/")
        ("gnu"           . "https://elpa.gnu.org/packages/")
        ("nongnu"        . "https://elpa.nongnu.org/nongnu/")))
(setq package-quickstart t)

(yx/dumped-if
    (progn
      (setq load-path yx/dumped-load-path)
      (transient-mark-mode 1)
      (global-font-lock-mode 1)
      (add-hook #'after-init-hook
                #'(lambda ()
                    (save-excursion
                      (switch-to-buffer "*scratch*")
                      (lisp-interaction-mode)))))
  (package-activate-all))

(require 'init-ui)
(require 'init-basic)
(require 'init-keymaps)
(require 'init-completion)
(require 'init-chinese)
(require 'init-evil)
(require 'init-misc)
(require 'init-notes)
(require 'init-mail)
(require 'init-elfeed)
(require 'init-ide)
(require 'init-lang)

;;; init.el ends here
