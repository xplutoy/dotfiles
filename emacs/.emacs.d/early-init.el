;;; -*- coding: utf-8; lexical-binding: t; -*-
(setq
 tool-bar-mode nil
 menu-bar-mode nil
 scroll-bar-mode nil
 default-frame-alist
 '((tool-bar-lines . 0)
   (menu-bar-lines . 0)
   (alpha-background . 75)
   (undecorated-round . t)
   (vertical-scroll-bars . nil)))

(setq
 use-dialog-box nil
 use-file-dialog nil
 inhibit-default-init t
 inhibit-splash-screen t
 frame-resize-pixelwise t
 inhibit-startup-message t
 package-enable-at-startup nil
 frame-inhibit-implied-resize t
 inhibit-compacting-font-caches t)

(let ((old-file-name-handler-alist file-name-handler-alist))
  (setq-default file-name-handler-alist nil)
  (defun yx/restore-file-name-handler-alist ()
    (setq file-name-handler-alist
          (delete-dups
           (append file-name-handler-alist
                   old-file-name-handler-alist))
          inhibit-trace nil))
  (add-hook #'emacs-startup-hook
            #'yx/restore-file-name-handler-alist))

(setq gc-cons-threshold (* 512 1024 1024)
      gc-cons-percentage 0.6)

(defun font-installed-p (font-name)
  (find-font (font-spec :name font-name)))
(defun yx/setup-fonts ()
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family "Inconsolata Nerd Font Mono" :height 150)
    (set-face-attribute 'fixed-pitch nil :family "Incosolata Nerd Font Mono" :height 1.0)
    (set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 1.0)
    (set-face-attribute 'fixed-pitch-serif nil :family "Latin Modern Mono" :height 1.0)
    (setq face-font-rescale-alist '(("LXGW WenKai Mono" . 1.05))) ;; 1.05 magic number
    (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family "LXGW WenKai Mono"))
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'symbol (font-spec :family font)))
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (set-fontset-font t 'emoji (font-spec :family font)))
    ))
(add-hook 'window-setup-hook #'yx/setup-fonts)
;; (load-theme 'modus-operandi)

;;; early-init.el ends here
