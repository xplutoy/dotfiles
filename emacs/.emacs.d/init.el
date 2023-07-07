;;; init.el --- emacs init.el. -*- coding: utf-8; lexical-binding: t; -*-
(setq user-full-name     "yangxue")
(setq user-mail-address  "yangxue.cs@foxmail.com")

(defvar yx/doc-dir          "~/yxdocs/")
(defvar yx/var-dir          "~/.emacs.d/.cache/")
(defvar yx/etc-dir          "~/.emacs.d/.cache/etc/")
(defvar yx/gpg-sign-key     "67B86CB8A5630C51!")
(defvar yx/gpg-encrypt-key  "8B1F9B207AF00BCF!")

(defconst -is-mac (eq system-type 'darwin))
(defconst -is-win (eq system-type 'windows-nt))
(defconst -is-linux (eq system-type 'gnu/linux))

(defconst -os-default-opener
  (cond (-is-mac "open")
        (-is-win "start")
        (t       "xdg-open"))
  )

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(setq custom-file (expand-file-name "custom.el" yx/etc-dir))
(load custom-file 'noerror)
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(setq
 package-archives
 '(("melpa"         . "https://melpa.org/packages/")
   ("melpa-stable"  . "https://stable.melpa.org/packages/")
   ("gnu"           . "https://elpa.gnu.org/packages/")
   ("nongnu"        . "https://elpa.nongnu.org/nongnu/"))
 package-quickstart t
 package-user-dir (expand-file-name "elpa" yx/var-dir)
 package-gnupghome-dir package-user-dir)
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
(require 'init-tex)
(require 'init-mail)
(require 'init-lang)
(require 'init-eshell)
(require 'init-erc)
(require 'init-elfeed)


;;; init.el ends here
