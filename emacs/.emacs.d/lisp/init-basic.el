;;; -*- coding: utf-8; lexical-binding: t; -*-
(setq-default
 tab-width 2
 abbrev-mode t
 fill-column 120
 truncate-lines t
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent 'complete)

(setq
 use-short-answers  t
 system-time-locale "C"
 auto-revert-verbose nil
 confirm-kill-processes nil
 find-file-visit-truename t
 ring-bell-function 'ignore
 initial-scratch-message ""
 delete-by-moving-to-trash t
 fast-but-imprecise-scrolling t
 compilation-scroll-output 'first-error
 backward-delete-char-untabify-method 'hungry
 sentence-end-double-space nil
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"

 )

;; use-package
(setq
 use-package-always-ensure      t
 use-package-always-defer       t
 use-package-expand-minimally   t
 )

;; auto save
(setq
 auto-save-no-message t
 auto-save-visited-interval 30
 )

;; eldoc
(setq
 eldoc-echo-area-use-multiline-p nil)

;; uniquify
(setq
 uniquify-strip-common-suffix t
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*"
 uniquify-buffer-name-style 'post-forward-angle-brackets
 )

;; line number
(setq
 display-line-numbers-type 'visual
 display-line-numbers-width-start t
 )

;; recentf
(setq
 recentf-auto-cleanup 'never
 recentf-max-saved-items 200
 recentf-exclude '("\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"))
(add-hook 'after-init-hook 'recentf-mode)

;; whitespace
(setq
 whitespace-line-column nil
 show-trailing-whitespace nil)

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

;; completion
(setq
 completions-detailed t
 completion-ignore-case t
 completions-format 'horizontal
 completions-header-format nil
 completions-max-height 30
 completion-auto-help 'visible
 completion-cycle-threshold 3
 completion-show-help nil
 completion-show-inline-help nil
 completion-auto-select 'second-tab
 )

(setq
 hippie-expand-max-buffers 10
 hippie-expand-try-functions-list
 '(try-complete-file-name
   try-complete-file-name-partially
   try-expand-dabbrev
   try-expand-dabbrev-from-kill
   try-expand-dabbrev-all-buffers
   )
 )

;; isearch
(setq
 isearch-lazy-count t
 isearch-allow-motion t
 apropos-sort-by-scores t
 lazy-highlight-no-delay-length 3
 )

;; epa
(setq
 epa-pinentry-mode 'loopback
 auth-sources
 (list (expand-file-name "authinfo.gpg" yx/etc-dir))
 epa-file-select-keys yx/gpg-encrypt-key)

;; mouse
(setq
 mouse-yank-at-point t
 mouse-wheel-tilt-scroll t
 mouse-drag-mode-line-buffer t
 mouse-drag-and-drop-region-cross-program t
 )

;; eww
(setq
 eww-auto-rename-buffer 'title
 eww-search-prefix "https://www.bing.com/?q="
 browse-url-browser-function 'eww-browse-url
 )

;; flyspell
(setq
 ispell-program-name "hunspell"
 ispell-local-dictionary "en_US"
 ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))
 ispell-hunspell-dictionary-alist ispell-local-dictionary-alist
 )

;; font-lock
(setq
 jit-lock-defer-time 0.05
 jit-lock-stealth-time 1.0
 jit-lock-stealth-nice 0.2)
(setq-default jit-lock-contextually t)

;; savehist
(setq
 history-delete-duplicates t
 savehist-additional-variables
 '(mark-ring
   global-mark-ring
   search-ring
   regexp-search-ring
   extended-command-history
   ))
(add-hook 'after-init-hook 'savehist-mode)

;; emacs session
(setq
 desktop-save 'if-exists
 desktop-restore-eager 5
 desktop-auto-save-timeout   60
 desktop-load-locked-desktop nil)


(setq
 calendar-latitude 30.67
 calendar-longitude 104.07
 calendar-mode-line-format nil
 calendar-mark-diary-entries-flag t
 )

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
   (auto-fill-mode 1)
   (visual-line-mode 1)
   (goto-address-mode 1)
   (variable-pitch-mode 1))
 )

(add-hook
 #'prog-mode-hook
 (lambda ()
   (setq-local
    whitespace-style
    '(face trailing lines-char space-before-tab space-after-tab))
   (hs-minor-mode 1)
   (whitespace-mode 1)
   (flyspell-prog-mode)
   (electric-pair-mode 1)
   (display-line-numbers-mode 1)
   (electric-indent-local-mode 1)
   ))

(add-hook
 #'after-init-hook
 (lambda ()
   (repeat-mode 1)
   (save-place-mode 1)
   (blink-cursor-mode -1)
   (auto-compression-mode 1)
   (delete-selection-mode 1)
   (auto-save-visited-mode 1)
   (global-auto-revert-mode 1)
   (pixel-scroll-precision-mode 1)
   )
 )

(provide 'init-basic)
