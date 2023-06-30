;;; -*- coding: utf-8; lexical-binding: t; -*-
(add-hook
 'python-ts-mode-hook
 (lambda()
   (setq-local
    tab-width 2
    python-indent-offset 4
    imenu-create-index-function 'python-imenu-create-flat-index
    )
   (semantic-mode 1)
   ))

(use-package pyvenv)
(use-package pyvenv-auto
  :hook (python-ts-mode . pyvenv-auto-run))

(use-package code-cells
  :hook (python-ts-mode . code-cells-mode-maybe)
  :custom
  code-cells-eval-region-commands
  '((python-ts-mode . python-shell-send-region)
    (jupyter-repl-interaction-mode . jupyter-eval-region))
  code-cells-convert-ipynb-style
  '(("pandoc" "--to" "ipynb" "--from" "org")
    ("pandoc" "--to" "org" "--from" "ipynb")
    org-mode)
  :bind (:map code-cells-mode-map
              ("C-c C-r" . code-cells-eval)
              ("M-p"     . code-cells-backward-cell)
              ("M-n"     . code-cells-forward-cell))

  )

;; =======
(provide 'init-python)
