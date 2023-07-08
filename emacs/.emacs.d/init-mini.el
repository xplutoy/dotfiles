;;; -*- lexical-binding: t no-byte-compile: t -*-

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000)))

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(setq
 use-dialog-box nil
 use-file-dialog nil
 use-short-answers t
 inhibit-splash-screen t
 inhibit-startup-screen t
 initial-scratch-message ""
 ring-bell-function 'ignore
 confirm-kill-processes nil
 frame-inhibit-implied-resize t
 )

(defun display-startup-echo-area-message ()
  (message nil))
(fset 'display-startup-echo-area-message 'ignore)

(setq user-emacs-directory "~/.emacs.d/.cache/emacs_minimal/")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'package)
(setq
 package-enable-at-startup nil
 package-user-dir (expand-file-name "elpa" user-emacs-directory)
 )
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

(defvar yx/default-font "JetBrains Mono NL")
(set-face-attribute 'default nil :family yx/default-font :height 150)

(load-theme 'modus-vivendi :no-confirm)

(setq-default
 tab-width 2
 fill-column 88
 cursor-type 'bar
 truncate-lines t
 indent-tabs-mode nil
 tab-always-indent 'complete
 )

(setq
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 )

(setq
 vc-follow-symlinks t
 mouse-yank-at-point t
 require-final-newline t
 eldoc-echo-area-use-multiline-p nil
 )

(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq
 display-line-numbers-type 'visual
 display-line-numbers-width-start t)
(add-hook 'prog-mode 'display-line-numbers-mode)

(setq
 recentf-max-saved-items 50
 recentf-auto-cleanup 'never
 recentf-exclude
 '("COMMIT_MSG" "COMMIT_EDITMSG" "/Downloads/"
   "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
   ".*\\.cache.*" "^/.*" "^/ssh:" "/elpa/"
   file-remote-p))
(add-hook 'emacs-startup-hook 'recentf-mode)

(setq
 winner-dont-bind-my-keys t
 winner-boring-buffers-regexp "^\\*")
(add-hook 'emacs-startup-hook 'winner-mode)

(setq
 dired-dwim-target t
 dired-omit-files "^\\..*$"
 dired-recursive-copies 'always
 dired-kill-when-opening-new-dired-buffer t
 dired-listing-switches
 "-l --almost-all --human-readable --group-directories-first --no-group"
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
 competion-styles '(basic partial-completion flex)
 )

(setq
 icomplete-vertical-prospects-height 7)
(add-hook
 'emacs-startup-hook
 (lambda () (fido-vertical-mode 1)))

(setq
 mouse-wheel-follow-mouse t
 mouse-wheel-progressive-speed nil
 mouse-wheel-scroll-amount-horizontal 2
 mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
 )

(setq
 scroll-step 1
 scroll-margin 1
 scroll-conservatively 101
 fast-but-imprecise-scrolling t
 scroll-preserve-screen-position  'always
 )

(add-hook
 #'emacs-startup-hook
 (lambda ()
   (repeat-mode 1)
   (save-place-mode 1)
   (blink-cursor-mode -1)
   (delete-selection-mode 1)
   (auto-save-visited-mode 1)
   (global-auto-revert-mode 1)
   (windmove-default-keybindings)
   (pixel-scroll-precision-mode 1)
   )
 )

(add-hook
 #'prog-mode-hook
 (lambda ()
   (hl-line-mode 1)
   (hs-minor-mode 1)
   (show-paren-mode 1)
   (electric-pair-mode 1)
   (which-function-mode 1)
   (electric-indent-local-mode 1)
   (setq-local
    whitespace-style
    '(face trailing lines-char space-before-tab space-after-tab)
    show-trailing-whitespace t)
   (whitespace-mode 1)
   (local-set-key (kbd "RET") 'newline-and-indent)
   (add-hook 'before-save-hook 'delete-trailing-whitespace)
   )
 )

(unless (package-installed-p 'meow)
    (package-install 'meow))

(require 'meow)
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   ;; windows
   '("o" . delete-other-windows)
   '("=" . split-window-right)
   '("-" . split-window-below)
   ;; high frequency
   '("e" . "C-x C-e")
   '("<SPC>" . "C-x C-s")
   '(";" . comment-dwim)
   '("k" . kill-this-buffer)
   '("p" . project-find-file)
   '("b" . switch-to-buffer)
   '("f" . find-file)
   '("i" . imenu)
   '("F" . toggle-frame-maximized)
   '("r" . recentf-open)
   )
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("N" . meow-pop-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("V" . meow-kmacro-matches)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-kmacro-lines)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("'" . repeat)
   '("\\" . quoted-insert)
   '("<escape>" . ignore)))

(when window-system
  (setq meow-replace-state-name-list
        '((normal . "🅝")
          (beacon . "🅑")
          (insert . "🅘")
          (motion . "🅜")
          (keypad . "🅚")))
  )
(setq
 meow-esc-delay 0.001
 meow-select-on-change t
 meow-cursor-type-normal 'box
 meow-cursor-type-insert '(bar . 4)
 meow-keypad-describe-delay 0.5
 meow-keypad-leader-dispatch "C-c"
 meow-expand-hint-remove-delay 2.0)
(meow-setup)
(meow-setup-indicator)
(meow-setup-line-number)
(unless (bound-and-true-p meow-global-mode)
  (meow-global-mode 1))
(meow-esc-mode 1)
