;; -*- coding: utf-8; lexical-binding: t; -*-
;; python
(add-hook
 #'python-mode-hook
 #'(lambda()
     (setq-local tab-width 4)
     (setq imenu-create-index-function #'python-imenu-create-flat-index)
     (semantic-mode 1)
     ))
(use-package py-isort
  :hook (before-save . py-isort-before-save))
(use-package pyvenv)
(use-package pyvenv-auto
  :hook (python-mode . pyvenv-auto-mode))

;; c/c++
(add-hook
 #'c-mode-common-hook
 #'(lambda ()
     (c-set-style "linux"))
 )


;; yaml
(use-package yaml-mode)

;; vim-script
(use-package vimrc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.?vim\\(rc\\)?\\'" . vimrc-mode))
  )

(provide 'init-lang)
