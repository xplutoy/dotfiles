;;; init.el --- emacs init.el. -*- coding: utf-8; lexical-binding: t; -*-
(defvar yx/doc-dir          "~/yxdocs/")
(defvar yx/etc-dir          "~/.emacs.d/etc/")
(defvar yx/var-dir          "~/.emacs.d/.cache/")
(defvar yx/gpg-sign-key     "67B86CB8A5630C51!")
(defvar yx/gpg-encrypt-key  "8B1F9B207AF00BCF!")

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(setq custom-file (expand-file-name "custom.el" yx/etc-dir))
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

(benchmark-init/activate)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

(require 'init-ui)
(require 'init-basic)
(require 'init-compl)
(require 'init-evil)
(require 'init-hydra)
(require 'init-misc)
(require 'init-window)
(require 'init-note)
(require 'init-mail)
(require 'init-lang)
(require 'init-python)
(require 'init-eshell)
(require 'init-elfeed)
;;; init.el ends here
