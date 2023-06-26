;;; -*- coding: utf-8; lexical-binding: t; -*-
(defun yx/toggole-eshell ()
  (interactive)
  (if (equal major-mode 'eshell-mode)
      (quit-window)
    (eshell 123))
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

              (add-to-list 'eshell-visual-commands "ssh")
              (add-to-list 'eshell-visual-commands "tail")
              (add-to-list 'eshell-visual-commands "top")

              (eshell/alias "dd"   "dired $1")
              (eshell/alias "ff"   "find-file $1")
              (eshell/alias "ee"   "find-file-other-windows $1")
              (eshell/alias "gs"   "git status")
              (eshell/alias "gd"   "magit-diff-unstaged")
              (eshell/alias "gds"  "magit-diff-staged")

              (eshell/alias "ll" (concat ls "-AlohG --color=always"))
              )
            )
  )

(provide 'init-eshell)
