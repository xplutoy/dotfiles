;;; init-mini.el --- minimal configurations without external plugins.	-*- lexical-binding: t no-byte-compile: t -*-
(prefer-coding-system 'utf-8)
;; better defaults
(setq use-short-answers t)
(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq delete-by-moving-to-trash t)
(setq make-backup-files nil)
(setq create-lockfiles  nil)
(setq auto-save-default nil)
(setq set-mark-command-repeat-pop t)
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(setq ring-bell-function 'ignore)

(setq-default
 tab-width 2
 indent-tabs-mode nil)

;; ui
(fringe-mode 4)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; font
(set-face-attribute 'default nil
                    :family "Inconsolata" :height 140)

(load-theme 'modus-vivendi t)
;; (load-theme 'wombat t)

;; basic mode
(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

(add-hook #'prog-mode-hook
          #'(lambda ()
              show-trailing-whitespace t
              (electric-pair-mode 1)
              (display-line-numbers-mode 1)))

;; Completion
(when (fboundp 'fido-mode)
  (progn
    (fido-mode 1)
    (fido-vertical-mode 1)

    (defun fido-recentf-open ()
      "Use `completing-read' to find a recent file."
      (interactive)
      (find-file (completing-read "Find recent file: " recentf-list)))
    (keymap-global-set "C-x C-r" 'fido-recentf-open)))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/elpa/meow-1.4.2/")
(require 'init-meow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init-mini.el ends here
