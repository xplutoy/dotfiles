;;; -*- coding: utf-8; lexical-binding: t; -*-
(defvar yx/default-font "Cascadia Code PL")
(defvar yx/fixed-pitch-font "JetBrains Mono NL")
(defvar yx/fixed-pitch-serif-font "Latin Modern Mono")
(defvar yx/variable-pitch-font "Latin Modern Roman")

(defun yx/setup-fonts ()
  (set-face-attribute 'default nil :family yx/default-font :height 160)
  (set-face-attribute 'fixed-pitch nil :family yx/fixed-pitch-font)
  (set-face-attribute 'fixed-pitch-serif nil :family yx/fixed-pitch-serif-font)
  (set-face-attribute 'variable-pitch nil :family yx/variable-pitch-font)
  (setq face-font-rescale-alist '(("LXGW WenKai Mono" . 1.05))) ;; 1.05 magic number
  (set-fontset-font t '(#x4e00 . #x9fff) "LXGW WenKai Mono")
  (when -is-mac
    (set-fontset-font
     t 'symbol
     (font-spec :family "Apple Color Emoji") nil 'prepend))
  )

(if (daemonp)
    (add-hook
     'after-make-frame-functions
     (lambda (frame)
       (with-selected-frame frame (yx/setup-fonts))))
  (yx/setup-fonts))

(add-hook
 'prog-mode-hook
 (lambda ()
   (setq buffer-face-mode-face 'fixed-pitch)
   (buffer-face-mode))
 )

(use-package ef-themes
  :init
  (setq
   ef-themes-to-toggle '(ef-light ef-night)
   ef-themes-headings
   '((0 . (variable-pitch 1.3))
     (1 . (regular 1.25))
     (2 . (variable-pitch 1.15))
     (3 . (1.1))
     (4 . (1.05))
     (agenda-date . (variable-pitch 1.25))
     (agenda-structure . (variable-pitch 1.25))
     (t . (t))))
  (ef-themes-select 'ef-light)
  )

(use-package theme-changer
  :after ef-themes
  :defer 10
  :config
  (change-theme 'ef-light 'ef-night)
  )

(use-package minions
  :hook (after-init . minions-mode)
  )

(setq column-number-mode t)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq
   doom-modeline-icon t
   doom-modeline-height 1
   doom-modeline-modal-icon t
   doom-modeline-project-detection 'project)
  )

(use-package nerd-icons
  :config
  (setq
   nerd-icons-scale-factor 1.0
   nerd-icons-font-famil "Hack nerd font"))
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


(provide 'init-ui)
