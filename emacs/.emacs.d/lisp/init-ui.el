;;; -*- coding: utf-8; lexical-binding: t; -*-
(defun font-installed-p (font-name)
  (find-font (font-spec :name font-name)))
(defun yx/setup-fonts ()
  (set-face-attribute 'default nil
                      :family "Inconsolata Nerd Font Mono" :height 160)
  (set-face-attribute 'fixed-pitch nil
                      :family "Incosolata Nerd Font Mono" :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family "Source Sans Pro" :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil
                      :family "Latin Modern Mono" :height 1.0)
  (setq face-font-rescale-alist '(("LXGW WenKai Mono" . 1.05))) ;; 1.05 magic number
  (set-fontset-font t '(#x4e00 . #x9fff) "LXGW WenKai Mono")
  (set-fontset-font t 'symbol "Symbols Nerd Font Mono" nil 'append)
  (set-fontset-font t 'emoji "Noto Color Emoji" nil 'append)
  )

(if (daemonp)
    (add-hook
     'after-make-frame-functions
     (lambda (frame)
       (with-selected-frame frame (yx/setup-fonts))))
  (yx/setup-fonts))


(use-package minions
  :hook (after-init . minions-mode)
  )

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq
   doom-modeline-icon t
   doom-modeline-height 1
   doom-modeline-modal-icon t
   doom-modeline-project-detection 'project)
  )

(use-package nerd-icons)
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package ef-themes
  :init
  (setq
   ef-themes-to-toggle '(ef-light ef-night)
   ef-themes-headings
   '((1 . (variable-pitch 1.3))
     (2 . (regular 1.2))
     (3 . (1.1))
     (agenda-date . (1.3))
     (agenda-structure . (variable-pitch 1.3))
     (t . (t))))
  (ef-themes-select 'ef-light))


(provide 'init-ui)
