;;; init.el --- emacs init.el. -*- coding: utf-8; lexical-binding: t; -*-
(defvar yx/org-root         "~/privacy")
(defvar yx/gpg-sign-key     "67B86CB8A5630C51!")
(defvar yx/gpg-encrypt-key  "8B1F9B207AF00BCF!")

(set-language-environment "UTF-8")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(setq
 package-quickstart t
 package-archives
 '(("melpa"         . "https://melpa.org/packages/")
   ("melpa-stable"  . "https://stable.melpa.org/packages/")
   ("gnu"           . "https://elpa.gnu.org/packages/")
   ("nongnu"        . "https://elpa.nongnu.org/nongnu/")))
(package-activate-all)

(require 'init-ui)
(require 'init-basic)
(require 'init-compl)
(require 'init-evil)
(require 'init-misc)
(require 'init-note)
(require 'init-mail)
(require 'init-lang)
(require 'init-eshell)
(require 'init-elfeed)
;;; init.el ends here
