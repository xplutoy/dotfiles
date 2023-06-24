;;; init-base --- basic settings. -*- coding: utf-8; lexical-binding: t; -*-
(setq-default
 tab-width 2
 abbrev-mode t
 truncate-lines t
 indent-tabs-mode nil
 require-final-newline t
 show-trailing-whitespace t
 tab-always-indent 'complete)

(setq
 use-short-answers  t
 help-window-select t
 isearch-lazy-count t
 create-lockfiles nil
 make-backup-files nil
 confirm-kill-processes nil
 find-file-visit-truename t
 ring-bell-function 'ignore
 delete-by-moving-to-trash t
 help-window-keep-selected t
 display-line-numbers-width-start t
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
 auth-sources '("~/.emacs.d/authinfo.gpg")
 epa-file-select-keys yx/gpg-encrypt-key)

;; mouse
(setq
 mouse-yank-at-point t
 mouse-drag-and-drop-region-cross-program t
 )

;; flyspell
(setq
 ispell-program-name "hunspell"
 ispell-local-dictionary "en_US"
 ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))
 ispell-hunspell-dictionary-alist ispell-local-dictionary-alist
 )

;; dired
(setq
 dired-mouse-drag-files t
 dired-omit-files "^\\..*$"
 dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
(add-hook 'dired-mode-hook 'dired-omit-mode)

;; os specific settings stay here
(when (eq system-type 'darwin)
  (setq
   mac-command-modifier       'super
   ns-use-thin-smoothing      t
   ns-use-native-fullscreen   nil
   insert-directory-program   "gls"))

(add-hook
 #'text-mode
 (lambda ()
   (setq-local
    word-wrap t
    word-wrap-by-category t
    truncate-lines nil)
   (flyspell-mode 1)
   (auto-fill-mode 1)
   (visual-line-mode 1)
   (variable-pitch-mode 1))
 )

(add-hook
 #'prog-mode-hook
 (lambda ()
   (hs-minor-mode 1)
   (elide-head-mode 1)
   (flyspell-prog-mode)
   (display-line-numbers-mode 1)))

(add-hook
 #'after-init-hook
 (lambda ()
   (server-mode 1)
   (winner-mode 1)
   (repeat-mode 1)
   (recentf-mode 1)
   (savehist-mode 1)
   (save-place-mode 1)
   (blink-cursor-mode -1)
   (electric-pair-mode 1)
   (global-so-long-mode 1)
   ;; (global-hl-line-mode 1)
   (auto-compression-mode 1)
   (delete-selection-mode 1)
   (auto-save-visited-mode 1)
   (global-auto-revert-mode 1)
   (pixel-scroll-precision-mode 1)
   (minibuffer-depth-indicate-mode 1))
 )

(defun yx/eshell-toggle ()
  (interactive)
  (if (equal major-mode 'eshell-mode)
      (delete-window)
    (eshell)))

(provide 'init-base)
;;; init-base.el ends here
