;;; -*- coding: utf-8; lexical-binding: t; -*-
(add-hook
 'python-ts-mode-hook
 (lambda()
   (setq-local tab-width 4)
   (setq imenu-create-index-function 'python-imenu-create-flat-index)
   (semantic-mode 1)
   ))

(use-package pyvenv)
(use-package pyvenv-auto
  :hook (python-ts-mode . pyvenv-auto-run))

(use-package code-cells
  )

;; =======
(provide 'init-python)
