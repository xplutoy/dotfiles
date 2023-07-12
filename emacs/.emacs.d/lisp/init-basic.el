;;; -*- coding: utf-8; lexical-binding: t; -*-
(setq-default
 tab-width 2
 abbrev-mode t
 fill-column 88
 truncate-lines t
 indent-tabs-mode nil
 require-final-newline t
 bidi-display-reordering nil
 tab-always-indent 'complete)

(setq
 track-eol t
 visible-bell nil
 view-read-only t
 use-short-answers t
 backup-by-copying t
 line-move-visual nil
 hl-line-sticky-flag nil
 auto-revert-verbose nil
 require-final-newline t
 confirm-kill-processes nil
 find-file-visit-truename t
 initial-scratch-message ""
 delete-by-moving-to-trash t
 set-mark-command-repeat-pop t
 show-paren-context-when-offscreen t
 compilation-scroll-output 'first-error
 backward-delete-char-untabify-method 'hungry
 sentence-end-double-space nil
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

(setq system-time-locale "C")
(format-time-string "%Y-%m-%d %a")

;; use-package
(setq
 use-package-always-ensure t
 use-package-always-defer t
 use-package-expand-minimally t)

;; auto save
(setq
 auto-save-no-message t
 auto-save-visited-interval 30)

;; eldoc
(setq eldoc-echo-area-use-multiline-p nil)

;; uniquify
(setq
 uniquify-strip-common-suffix t
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*"
 uniquify-buffer-name-style 'post-forward-angle-brackets)

;; line number
(setq
 display-line-numbers-type 'visual
 display-line-numbers-width-start t)

;; recentf
(setq
 recentf-max-saved-items 50
 recentf-auto-cleanup 'never
 recentf-exclude
 '("COMMIT_MSG"
   "COMMIT_EDITMSG"
   "/Downloads/"
   "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
   ".*\\.cache.*"
   "^/.*"
   "^/ssh:"
   "/elpa/"
   file-remote-p))
(add-hook 'after-init-hook 'recentf-mode)


;; whitespace
(setq
 whitespace-line-column nil
 show-trailing-whitespace nil
 whitespace-action '(auto-clean))

;; xref
(setq
 xref-search-program
 (cond
  ((executable-find "ugrep")
   'ugrep)
  ((executable-find "rg")
   'ripgrep))
 xref-show-xrefs-function 'xref-show-definitions-completing-read
 xref-show-definitions-function 'xref-show-definitions-completing-read)

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
 completion-auto-select 'second-tab)

(setq
 hippie-expand-max-buffers 10
 hippie-expand-try-functions-list
 '(try-complete-file-name
   try-complete-file-name-partially
   try-expand-dabbrev
   try-expand-dabbrev-from-kill
   try-expand-dabbrev-all-buffers))

;; isearch
(setq
 isearch-lazy-count t
 isearch-allow-motion t
 apropos-sort-by-scores t
 lazy-highlight-no-delay-length 3)

;; epa
(setq
 epa-pinentry-mode 'loopback
 auth-sources (list (expand-file-name "authinfo.gpg" user-emacs-directory))
 epa-file-select-keys yx/gpg-encrypt-key)

;; mouse
(setq
 mouse-yank-at-point t
 mouse-wheel-tilt-scroll t
 mouse-wheel-follow-mouse t
 mouse-drag-mode-line-buffer t
 mouse-wheel-progressive-speed nil
 mouse-drag-and-drop-region-cross-program t
 mouse-wheel-scroll-amount-horizontal 2
 mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))

;; scroll
(setq
 scroll-step 1
 scroll-margin 1
 scroll-conservatively 101
 fast-but-imprecise-scrolling t
 scroll-preserve-screen-position 'always)

;; eww
(setq
 eww-auto-rename-buffer 'title
 eww-search-prefix "https://www.bing.com/?q="
 browse-url-browser-function 'eww-browse-url)

;; flyspell
(setq
 ;; ispell-program-name "enchant-2" ;; lag
 ispell-program-name "hunspell"
 ispell-local-dictionary "en_US"
 ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))
 ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)

;; font-lock
(setq
 jit-lock-defer-time 0.25
 jit-lock-chunk-size 4096
 jit-lock-stealth-time 0.5)
(setq-default jit-lock-contextually t)

;; savehist
(setq
 history-delete-duplicates t
 savehist-additional-variables '(mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history))
(add-hook 'after-init-hook 'savehist-mode)

;; emacs session
(setq
 desktop-save t
 desktop-restore-eager 5
 desktop-auto-save-timeout 60
 desktop-load-locked-desktop 'check-pid)
;; (add-hook 'emacs-startup-hook 'desktop-read)
;; (add-hook 'emacs-startup-hook 'desktop-save-mode)

;; midnight-mode
(setq
 midnight-period 36000
 clean-buffer-list-delay-general 1
 clean-buffer-list-kill-regexps '(".*")
 clean-buffer-list-kill-never-buffer-names '("*eshell*" "*scratch*" "*server*" "*vterm*" "*Group*"))
(add-hook 'after-init-hook 'midnight-mode)
(with-eval-after-load 'midnight
  (midnight-delay-set 'midnight-delay 60))

;; time-stamp
(add-hook 'before-save-hook 'time-stamp)

;; calendar
(setq
 calendar-latitude 30.67
 calendar-longitude 104.07
 calendar-mode-line-format nil
 calendar-mark-diary-entries-flag t)

;; os specific settings stay here
(when (eq system-type 'darwin)
  (setq
   mac-command-modifier 'super
   ns-use-thin-smoothing t
   ns-use-native-fullscreen nil
   insert-directory-program "gls"))

(add-hook
 #'text-mode
 (lambda ()
   (setq-local
    word-wrap t
    word-wrap-by-category t
    truncate-lines nil)
   (visual-line-mode 1)
   (goto-address-mode 1)))

(add-hook
 #'after-init-hook
 (lambda ()
   (repeat-mode 1)
   (ffap-bindings)
   (save-place-mode 1)
   (blink-cursor-mode -1)
   (auto-compression-mode 1)
   (delete-selection-mode 1)
   (auto-save-visited-mode 1)
   (global-auto-revert-mode 1)
   (windmove-default-keybindings)
   (pixel-scroll-precision-mode 1)))

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; never kill scratch
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

(provide 'init-basic)
