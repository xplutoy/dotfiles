;;; init.el --- emacs init.el. -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:
(defvar yx/org-root         "~/privacy")
(defvar yx/gpg-sign-key     "67B86CB8A5630C51!")
(defvar yx/gpg-encrypt-key  "8B1F9B207AF00BCF!")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; elpa-init
(require 'package)
(setq package-archives
      '(("melpa"         . "https://melpa.org/packages/")
        ("melpa-stable"  . "https://stable.melpa.org/packages/")
        ("gnu"           . "https://elpa.gnu.org/packages/")
        ("nongnu"        . "https://elpa.nongnu.org/nongnu/")))
(setq package-quickstart t)
(package-activate-all)

(require 'init-base)
(require 'init-comp)
(require 'init-evil)
(require 'init-misc)
(require 'init-note)
(require 'init-lang)

;;; init.el ends here
