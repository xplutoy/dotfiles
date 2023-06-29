;;; -*- coding: utf-8; lexical-binding: t; -*-
(defun yx/toggole-eshell ()
  (interactive)
  (if (equal major-mode 'eshell-mode)
      (quit-window)
    (eshell))
  )

(defun eshell/gst (&rest args)
  (magit-status (pop args) nil)
  (eshell/echo))

(use-package eshell
  :init
  (setq eshell-hist-ignoredups t
        eshell-error-if-no-glob t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-scroll-to-bottom-on-input 'all
        eshell-destroy-buffer-when-process-dies t
        )
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "C-l") 'eshell/clear)
              (setq eshell-visual-commands
                    '("vim" "ssh" "tail" "top" "htop" "tmux" "less" "more")
                    eshell-visual-subcommands
                    '(("git" "log" "diff" "show")))

              (eshell/alias "dd"   "dired $1")
              (eshell/alias "ff"   "find-file $1")
              (eshell/alias "fo"   "find-file-other-windows $1")
              (eshell/alias "gs"   "git status")
              (eshell/alias "gd"   "magit-diff-unstaged")
              (eshell/alias "gds"  "magit-diff-staged")

              (eshell/alias "ll"   "ls -AlohG --color=always")
              )
            )
  )
(use-package eshell-git-prompt-yx
  :load-path "site-lisp/eshell-git-prompt-yx"
  :autoload eshell-git-prompt-multiline
  :init
  (setq eshell-prompt-function 'eshell-git-prompt-multiline)
  )

(use-package pcmpl-args :defer 1)

(use-package vterm
  :bind (:map vterm-mode-map
              ("C-y" . vterm-yank)
              ("M-y" . vterm-yank-pop)
              ("C-k" . vterm-send-C-k-and-kill))
  :config
  (setq vterm-always-compile-module t)
  (defun vterm-send-C-k-and-kill ()
    (kill-ring-save (point) (vterm-end-of-line))
    (vterm-send-key "k" nil nil t)
    )
  )
(use-package vterm-toggle
  :custom
  (vterm-toggle-hide-method 'reset-window-configration)
  (vterm-toggle-cd-auto-create-buffer nil))

(use-package eat
  :defer 1
  :load-path "site-lisp/emacs-eat"
  :hook ((eshell-load . eat-eshell-mode)
         (eshell-load . eat-eshell-visual-command-mode))
  :init
  (setq
   eat-kill-buffer-on-exit t
   eat-enable-yank-to-terminal t)
  )

(provide 'init-eshell)
