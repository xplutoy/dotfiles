;;; init-basic --- basic settings. -*- coding: utf-8; lexical-binding: t; -*-
;;; code:
(setq-default
 fill-column 100
 tab-width 2
 cursor-type 'box
 abbrev-mode t
 truncate-lines t
 case-fold-search t
 indent-tabs-mode nil
 tab-always-indent 'complete
 show-trailing-whitespace nil
 cursor-in-non-selected-windows nil)

(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)

(setq
 track-eol t
 visible-bell t
 view-read-only t
 use-dialog-box nil
 echo-keystrokes 0.2
 use-short-answers  t
 auto-window-vscroll nil
 initial-scratch-message ""
 ;; system-time-locale "zh_CN"
 ring-bell-function 'ignore
 delete-by-moving-to-trash ON-MAC
 enable-recursive-minibuffers t
 read-buffer-completion-ignore-case t
 line-move-visual nil
 word-wrap-by-category t
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil
 set-mark-command-repeat-pop t
 kmacro-execute-before-append nil
 vc-follow-symlinks t
 find-file-visit-truename t
 help-window-select t
 help-window-keep-selected t
 switch-to-buffer-obey-display-actions t
 switch-to-buffer-in-dedicated-window 'pop
 kill-do-not-save-duplicates t
 backward-delete-char-untabify-method 'hungry
 require-final-newline t
 highlight-nonselected-windows nil
 confirm-kill-processes nil
 create-lockfiles nil
 make-backup-files nil
 windmove-wrap-around t
 hippie-expand-max-buffers 10
 compilation-scroll-output 'first-error
 winner-boring-buffers-regexp "^\\*"
 syntax-wholeline-max 120
 )

;; use-package
(setq
 use-package-verbose            t
 use-package-always-ensure      t
 use-package-always-defer       t
 use-package-expand-minimally   t
 use-package-compute-statistics t
 )

;; native-comp
(when (native-comp-available-p)
  (setq
   package-native-compile t
   native-comp-deferred-compilation t
   native-comp-async-report-warnings-errors nil
   ))

;; abbrev
(setq
 abbrev-suggest 't
 save-abbrevs 'silently
 abbrev-suggest-hint-threshold 1
 )

;; auto save
(setq
 auto-save-default t
 auto-save-no-message t
 delete-auto-save-files t
 auto-save-visited-interval 30
 kill-buffer-delete-auto-save-files 1
 )

;; line-number
(setq
 display-line-numbers-type 'relative
 display-line-numbers-width-start t
 )

;; mouse
(setq
 mouse-yank-at-point t
 mouse-wheel-tilt-scroll t
 mouse-drag-mode-line-buffer t
 mouse-drag-and-drop-region-cross-program t
 )

;; paran
(setq
 show-paren-when-point-inside-paren t
 show-paren-when-point-in-periphery t
 show-paren-context-when-offscreen  t
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

;; uniquify
(setq
 uniquify-strip-common-suffix t
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*"
 uniquify-buffer-name-style 'post-forward-angle-brackets
 )

;; autorevert
(setq
 global-auto-revert-non-file-buffers t
 auto-revert-check-vc-info t
 auto-revert-verbose nil
 )

;; recentf
(setq
 recentf-auto-cleanup 'never
 recentf-max-saved-items 500
 recentf-exclude '("\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$")
 )

;; ibuffer
(setq
 ibuffer-expert t
 ibuffer-display-summary nil
 ibuffer-show-empty-filter-groups nil
 ibuffer-never-show-predicates '("^\\*"))
(add-hook
 #'ibuffer-mode-hook
 #'(lambda ()
     (ibuffer-do-sort-by-recency)
     (ibuffer-auto-mode 1)))


(setq
 resize-mini-windows t
 completion-ignore-case t
 completions-format 'one-column
 completion-show-inline-help nil
 completions-detailed t
 completions-header-format nil
 completions-max-height 20
 completion-auto-help 'visible
 completion-cycle-threshold 3 ;;or
 completion-show-help nil
 completion-auto-select 'second-tab
 read-file-name-completion-ignore-case t
 max-mini-window-height 0.33
 )

;; savehist
(setq
 history-length 1000
 history-delete-duplicates t
 savehist-save-minibuffer-history t
 savehist-additional-variables
 '(mark-ring
   global-mark-ring
   search-ring
   regexp-search-ring
   extended-command-history
   )
 )

;; isearch
(setq
 isearch-lazy-count t
 lazy-highlight-no-delay-length 3
 isearch-allow-motion t
 apropos-sort-by-scores t
 )

;; browse-url
(setq-default
 shr-inhibit-images t
 shr-use-fonts nil)
(setq
 browse-url-browser-function 'eww-browse-url
 eww-auto-rename-buffer 'title
 eww-search-prefix "https://www.bing.com/?q="
 )

;; emacs session
(setq
 desktop-save t
 desktop-restore-eager 5
 desktop-auto-save-timeout   60
 desktop-load-locked-desktop nil)

;; tabbar
(setq
 tab-bar-tab-hints t
 tab-bar-new-tab-choice 'window
 tab-bar-select-tab-modifiers '(super))

;; calendar chendu (30.67 104.07)
(setq
 calendar-latitude 30.67
 calendar-longitude 104.07
 calendar-mode-line-format nil
 calendar-mark-diary-entries-flag t
 diary-file (expand-file-name "diary" yx/org-root))

;; dired
(setq
 dired-dwim-target t
 dired-mouse-drag-files t
 dired-recursive-copies 'always
 dired-recursive-deletes 'top
 dired-kill-when-opening-new-dired-buffer t
 dired-listing-switches "-aBhl  --group-directories-first")
(put 'dired-find-alternate-file 'disabled nil)

;; ispell
(setq ispell-dictionary "en_US"
      flyspell-issue-message-flag nil)
(when (executable-find "aspell")
  (setq ispell-list-command "--list"
        ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

;; epa
(setq
 epa-file-encrypt-to yx/gpg-encrypt-key
 epa-file-select-keys nil
 epa-pinentry-mode 'loopback
 epa-file-cache-passphrase-for-symmetric-encryption t
 )
(add-to-list 'auth-sources (concat user-emacs-directory "authinfo.gpg"))

;; auto-insert
(setq
 auto-insert-query nil
 auto-insert-directory (locate-user-emacs-file "templates"))

;; os specific settings stay here
(cond
 (ON-MAC ;; MACOS
  (setq
   mac-command-modifier       'super   ;; s: super(Command/Win)
   mac-control-modifier       'control ;; C: Ctrl
   mac-option-modifier        'meta    ;; M: Meta (Option/Alt)
   mac-right-command-modifier 'super   ;; ns-function-modifier
   mac-right-option-modifier  'none    ;; Leave Option to macOS
   mac-right-control-modifier 'control ;; C: Ctrl
   ns-function-modifier       'hyper   ;; ns-function-modifier
   ns-use-thin-smoothing      t
   insert-directory-program   "gls"
   browse-url-generic-program "open"
   ))
 (ON-WINDOWS
  ;;
  )
 (ON-LINUX
  ;;
  )
 )

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
  (elide-head-mode            1)
  (subword-mode               1)
  (flymake-mode               1)
  (display-line-numbers-mode  1)
  (electric-indent-local-mode 1)
  (goto-address-prog-mode     1)
  (bug-reference-prog-mode    1)
  (hs-minor-mode              1))
(add-hook #'prog-mode-hook #'yx/prog-mode-setup)
(add-hook #'conf-mode-hook #'yx/prog-mode-setup)

(add-hook
 #'after-init-hook
 #'(lambda ()
     (repeat-mode 1)
     (recentf-mode 1)
     (menu-bar-mode -1)                 ; only has effect after early-init
     )
 )

(add-hook
 #'before-save-hook
 #'(lambda ()
     (delete-trailing-whitespace)
     (whitespace-cleanup)))

;; delay load global mirror mode
(run-with-idle-timer
 1
 nil
 #'(lambda ()
     (unless (display-graphic-p)
       (xterm-mouse-mode 1))
     (find-function-setup-keys)
     (windmove-default-keybindings)
     (server-mode 1)
     (winner-mode 1)
     (savehist-mode 1)
     (save-place-mode 1)
     ;; (show-paren-mode 1)
     (auto-insert-mode 1)
     (desktop-save-mode -1)
     (blink-cursor-mode -1)
     (electric-pair-mode 1)
     ;; (which-function-mode 1)
     (global-so-long-mode 1)
     (global-hl-line-mode 1)
     (tab-bar-history-mode 1)
     (delete-selection-mode 1)
     (global-superword-mode 1)
     (file-name-shadow-mode 1)
     (auto-save-visited-mode 1)
     (global-auto-revert-mode 1)
     (pixel-scroll-precision-mode 1)
     (minibuffer-depth-indicate-mode 1)
     (auto-compression-mode 1)))

(provide 'init-basic)
;;; init-basic.el ends here
