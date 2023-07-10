;;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package gcmh
  :defer 1
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (gcmh-mode 1))

(when (featurep 'xwidget-internal)
  (use-package xwidget
    :ensure nil
    :bind (:map xwidget-webkit-mode-map
                ("W" . xwidget-webkit-fit-width))
    )
  )

(use-package crux-yx
  :defer 1
  :load-path "site-lisp/crux-yx"
  :config
  (add-hook 'eww-mode-hook
            (lambda ()
              (setq-local imenu-create-index-function 'unpackaged/imenu-eww-headings))))

(use-package benchmark-init
  :disabled
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package which-key
  :defer 1
  :init
  (setq
   which-key-idle-delay 1.5
   which-key-show-early-on-C-h t
   which-key-show-remaining-keys t
   which-key-idle-secondary-delay 0.05)
  :config
  (which-key-setup-minibuffer)
  (which-key-mode 1)
  )

(use-package avy
  :init
  (setq avy-style 'at
        avy-timeout-seconds 0.8)
  )

(use-package posframe)

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark))
  )

(use-package move-text
  :defer 1
  :config
  (move-text-default-bindings)
  )

(use-package smart-hungry-delete
  :bind (:map prog-mode-map
              ([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
              ([remap delete-backward-char] . smart-hungry-delete-backward-char)
              ([remap delete-char] . smart-hungry-delete-backward-char))
  :init (smart-hungry-delete-add-default-hooks)
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

(use-package sis
  :defer 1
  :hook
  ((org-mode . sis-set-other))
  :init
  ;; C-s/r 默认优先使用英文
  (setq
   sis-respect-go-english-triggers
   '(isearch-forward isearch-backward)
   sis-respect-restore-triggers
   '(isearch-exit isearch-abort))
  (setq
   sis-inline-with-other t
   sis-other-cursor-color "#FF2121"
   sis-auto-refresh-seconds nil
   sis-inline-tighten-tail-rule 'all)
  :config
  (sis-ism-lazyman-config
   "com.apple.keylayout.US"
   "com.sogou.inputmethod.sogou.pinyin")
  (sis-global-inline-mode 1)
  (sis-global-context-mode 1)
  (sis-global-respect-mode 1)
  (sis-global-cursor-color-mode 1)
  (add-hook 'org-mode-hook
            (lambda () (sis-context-mode -1))) ; bug
  )

(use-package no-littering
  :demand
  :init
  (setq no-littering-var-directory yx/var-dir
        no-littering-etc-directory yx/etc-dir
        )
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-var-directory))
    )
  :config
  (no-littering-theme-backups)
  )

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("s-;" . flyspell-correct-wrapper))
  )
(use-package jinx
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "en_US")
  )

(use-package keyfreq
  :defer 1
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package pdf-tools)
(use-package emacsql-sqlite-builtin)

;; dict
(use-package osx-dictionary
  :if -is-mac
  :defer 1)

(provide 'init-misc)
;;; init-misc.el ends here
