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
  :commands eshell-git-prompt-multiline
  :init
  (setq eshell-prompt-function 'eshell-git-prompt-multiline)
  )

(provide 'init-eshell)
