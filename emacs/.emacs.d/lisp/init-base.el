;;; init-base --- basic settings. -*- coding: utf-8; lexical-binding: t; -*-
;;; code:
(setq-default
 tab-width 2
 abbrev-mode t
 truncate-lines t
 indent-tabs-mode nil
 tab-always-indent 'complete)

(setq
 use-short-answers  t
 ring-bell-function 'ignore
 help-window-select t
 help-window-keep-selected t
 create-lockfiles nil
 make-backup-files nil
 flyspell-issue-message-flag nil
 display-line-numbers-type 'relative
 compilation-scroll-output 'first-error
 winner-boring-buffers-regexp "^\\*"
 backward-delete-char-untabify-method 'hungry
 uniquify-buffer-name-style 'post-forward-angle-brackets
 sentence-end-double-space nil
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 recentf-exclude '("\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$")
 )

;; use-package
(setq
 use-package-always-ensure      t
 use-package-always-defer       t
 use-package-expand-minimally   t
 )

;; xref
(setq
 xref-search-program
 (cond
  ((executable-find "ugrep")
   'ugrep)
  ((executable-find "rg")
   'ripgrep))
 xref-show-xrefs-function 'xref-show-definitions-completing-read
 xref-show-definitions-function 'xref-show-definitions-completing-read
 )

;; ibuffer
(setq
 ibuffer-expert t
 ibuffer-display-summary nil
 ibuffer-show-empty-filter-groups nil
 ibuffer-never-show-predicates '("^\\*"))

;; epa
(setq
 epa-pinentry-mode 'loopback
 epa-file-select-keys yx/gpg-encrypt-key)
(add-to-list 'auth-sources "~/.emacs.d/authinfo.gpg")

;; os specific settings stay here
(when (eq system-type 'darwin)
  (setq
   mac-command-modifier       'super
   ns-use-thin-smoothing      t
   insert-directory-program   "gls"))

;; text-mode
(defun yx/text-mode-setup ()
  (setq-local
   word-wrap t
   word-wrap-by-category t
   truncate-lines nil)
  (setq show-trailing-whitespace t)
  (auto-fill-mode      1)
  (visual-line-mode    1)
  (variable-pitch-mode 1))
(add-hook #'text-mode #'yx/text-mode-setup)

;; prog-mode
(defun yx/prog-mode-setup ()
  (setq show-trailing-whitespace t)
  (flymake-mode               1)
  (display-line-numbers-mode  1)
  (hs-minor-mode              1))
(add-hook #'prog-mode-hook #'yx/prog-mode-setup)
(add-hook #'conf-mode-hook #'yx/prog-mode-setup)

(add-hook
 #'after-init-hook
 #'(lambda ()
     (server-mode 1)
     (winner-mode 1)
     (repeat-mode 1)
     (recentf-mode 1)
     (savehist-mode 1)
     (menu-bar-mode -1)
     (save-place-mode 1)
     (blink-cursor-mode -1)
     (electric-pair-mode 1)
     (global-so-long-mode 1)
     (global-hl-line-mode 1)
     (auto-compression-mode 1)
     (delete-selection-mode 1)
     (auto-save-visited-mode 1)
     (global-auto-revert-mode 1)
     (pixel-scroll-precision-mode 1)
     (minibuffer-depth-indicate-mode 1)
     )
 )

(defun yx/eshell-toggle ()
  (interactive)
  (if (equal major-mode 'eshell-mode)
      (delete-window)
    (eshell)))

(defun font-installed-p (font-name)
  (find-font (font-spec :name font-name)))
(defun yx/setup-fonts ()
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family "Inconsolata" :height 140)
    (set-face-attribute 'fixed-pitch nil :family "Inconsolata" :height 1.0)
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

;; (:dark ef-cherie ef-winter ef-dark ef-autumn) (:light ef-duo-light)
(use-package ef-themes
  :init
  (setq
   ef-themes-mixed-fonts t
   ef-themes-to-toggle '(ef-duo-light ef-winter)
   ef-themes-headings '((1 . (variable-pitch 1.3)) (2 . (regular 1.2)) (3 . (1.1)) (agenda-date . (1.3)) (agenda-structure . (variable-pitch 1.3)) (t . (t))))
  (ef-themes-select 'ef-duo-light)
  )

(provide 'init-base)
;;; init-base.el ends here
