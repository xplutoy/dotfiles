;;; init-misc.el --- misc plugin configure. -*- coding: utf-8; lexical-binding: t; -*-
(use-package gcmh
  :defer 1
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (gcmh-mode 1))

(use-package ibuffer-vc
  :init
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package which-key
  :defer 1
  :init
  (setq which-key-idle-delay 1.0
        which-key-idle-secondary-delay 0.05)
  :config
  (which-key-setup-minibuffer)
  (which-key-mode 1)
  )

(use-package buffer-move)
(use-package ace-window
  :init
  (setq aw-background nil
        aw-scope 'frame
        aw-dispatch-always nil
        ;; aw-ignored-buffers
        aw-minibuffer-flag nil)
  :bind ("M-o" . ace-window))

(use-package winum
  :defer 1
  :init
  (setq winum-scope 'frame-local
        winum-format " [%s] "
        winum-auto-assign-0-to-minibuffer t
        winum-ignored-buffers-regexp '("^\\*Help.*")
        )
  :config
  (winum-set-keymap-prefix nil)
  (winum-mode 1)
  )

(use-package avy
  :init
  (setq avy-style 'at
        avy-timeout-seconds 0.8)
  )

(use-package posframe)

(use-package keyfreq
  :defer 5
  :init
  (setq keyfreq-excluded-commands
        '(backward-char
          dap-tooltip-mouse-motion
          delete-backward-char
          execute-extended-command
          forward-char
          keyboard-quit
          kill-buffer
          left-char
          minibuffer-keyboard-quit
          mouse-drag-region
          mouse-set-point
          move-beginning-of-line
          move-end-of-line
          next-line
          org-delete-backward-char
          org-end-of-line
          org-return
          org-self-insert-command
          pixel-scroll-precision
          previous-line
          previous-line
          right-char
          right-word
          save-buffer
          selectrum-next-candidate
          selectrum-select-current-candidate
          self-insert-command
          yank))
  :custom
  (keyfreq-file "~/.emacs.d/keyfreq")
  (keyfreq-file-lock "~/.emacs.d/keyfreq.lock")
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  )

(use-package crux
  :config (progn (crux-with-region-or-buffer indent-region)
                 (crux-reopen-as-root-mode))
  :bind (("C-c C-o"                      . crux-open-with)
         ("S-<return>"                   . crux-smart-open-line)
         ("C-S-<return>"                 . smart-open-line-above)
         ("C-c C-k"                      . crux-kill-other-buffers)
         )
  )

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark))
  )

(use-package minions
  :hook (after-init . minions-mode)
  )

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-hide-dotfiles
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map ("." . dired-hide-dotfiles-mode)))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(use-package dired-narrow
  :bind (:map dired-mode-map
              ([remap dired-do-man] . dired-narrow))
  )

(use-package eshell-z
  :after eshell)

(use-package ialign)

(use-package golden-ratio
  :defer 1
  :init
  (setq golden-ratio-auto-scale t
        golden-ratio-max-width nil)
  (setq golden-ratio-exclude-modes
        '(gnus-mode
          gnus-summary-mode
          gnus-article-mode
          which-key-mode
          vundo-mode
          calendar-mode
          imenu-list-major-mode))
  :config
  (dolist (re '("^\\*Ilist"
                "^\\*Org"
                "^\\*Agenda"
                "^nnrs"))
    (add-to-list 'golden-ratio-exclude-buffer-regexp re))
  (golden-ratio-mode 1)
  )

(use-package shackle
  :defer 1
  :init
  (setq shackle-default-size 0.33
        shackle-select-reused-windows t
        shackle-inhibit-window-quit-on-same-windows t)
  :config
  (setq
   shackle-rules
   '((("\\*Ibuffer\\*"
       "\\*Help\\*"
       "\\*helpful .*"
       "\\*es?hell\\*"
       "\\*info\\*"
       "\\*[Wo]*Man.*\\*"
       "\\*SDCV\\*"
       "\\*Dictionary\\*"
       "\\*Flymake .*"
       "\\*vterm\\*"
       "^CAPTURE-"
       dired-mode
       occur-mode
       vterm-mode
       color-rg-mode)
      :regexp t :select t :popup t :align below)
     (("\\*Go-Translate\\*"
       "\\*Warnings\\*"
       "\\*Messages\\*"
       "\\*evil-registers\\*"
       "\\*evil-owl\\*"
       "^\\*Compile"
       "\\*Agenda Commands\\*"
       "^\\*Org Note"
       "^\\*Org Select"
       "\\*Capture\\*"
       "\\*Shell Command Output\\*")
      :regexp t :select nil :popup t :align below)
     (("^magit")
      :regexp t :select t :same t)
     ))
  (shackle-mode 1)
  )

(use-package bug-hunter)

(use-package vim-empty-lines-mode
  :hook (prog-mode . vim-empty-lines-mode)
  )

(use-package vundo)

(use-package imenu-list
  :init
  (setq imenu-list-size 0.25
        imenu-list-position 'right
        imenu-list-auto-resize nil
        imenu-list-focus-after-activation t)
  )

(use-package zoxide)

(use-package esup
  :init
  (setq esup-depth 0)
  )

(use-package move-text
  :defer 1
  :config
  (move-text-default-bindings)
  )

(use-package holymotion
  :ensure nil
  :load-path "site-lisp/holymotion"
  :hook ((prog-mode conf-mode) . holymotion-mode)
  :bind (:map holymotion-mode-map
              ([remap next-line]          . holymotion-next-line)
              ([remap previous-line]      . holymotion-previous-line)
              ([remap forward-word]       . holymotion-forward-word)
              ([remap backward-word]      . holymotion-backward-word)
              ([remap forward-sentence]   . holymotion-forward-sentence)
              ([remap backward-sentence]  . holymotion-backward-sentence)
              ([remap beginning-of-defun] . holymotion-backward-beginning-of-defun)
              ([remap end-of-defun]       . holymotion-forward-end-of-defun)
              )
  :config
  (holymotion-make-motion
   holymotion-forward-end-of-defun #'end-of-defun)
  )

(provide 'init-misc)
;;; init-misc.el ends here
