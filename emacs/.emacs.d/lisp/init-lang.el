;;; -*- coding: utf-8; lexical-binding: t; -*-
(add-hook
 #'prog-mode-hook
 (lambda ()
   (hl-line-mode 1)
   (hs-minor-mode 1)
   (show-paren-mode 1)
   (flyspell-prog-mode)
   (electric-pair-mode 1)
   (which-function-mode 1)
   (display-line-numbers-mode 1)
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

;; ediff
(setq
 ediff-keep-variants nil
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-horizontally)

(with-eval-after-load 'hideshow
  (keymap-set hs-minor-mode-map "C-<return>" 'hs-toggle-hiding))

(use-package eglot
  :ensure nil
  :init
  (setq
   eglot-autoshutdown t
   eglot-extend-to-xref t)
  :hook ((c-mode
          c-ts-mode
          R-mode
          python-mode
          python-ts-mode
          julia-mode
          julia-ts-mode
          LaTeX-mode) . eglot-ensure)
  )

(use-package treesit
  :ensure nil
  :init
  (setq
   major-mode-remap-alist
   '((c-mode . c-ts-mode)
     (julia-mode . julia-ts-mode)
     (python-mode . python-ts-mode))
   treesit-extra-load-path (list (no-littering-expand-var-file-name "tree-sitter")))
  )

(use-package reformatter
  :defer 1
  :config
  ;; python
  (reformatter-define python-isort
    :program "isort"
    :args '("--stdout" "--atomic" "-"))
  (add-hook 'python-ts-mode-hook 'python-isort-on-save-mode)
  (reformatter-define python-black
    :program "black"
    :args '("-"))
  (add-hook 'python-ts-mode-hook 'python-black-on-save-mode)
  )

(use-package tempel
  :bind  (("M-=" . tempel-complete)
          ("M-*" . tempel-insert))
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons 'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  )
(use-package tempel-collection)

;; project
(with-eval-after-load "project"
  (keymap-set project-prefix-map "m" 'magit))

;; diff-hl
(use-package diff-hl
  :defer 2
  :config
  (setq diff-hl-disable-on-remote t)
  (global-diff-hl-mode 1)
  :hook (dired-mode . diff-hl-dired-mode)
  )

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;; aggressive-indent
(use-package aggressive-indent
  :hook
  (((emacs-lisp-mode python-mode julia-mode) . aggressive-indent-mode)
   (find-file . (lambda ()
                  (if (> (buffer-size) (* 3000 80)) (aggressive-indent-mode -1))))))

;; indent-guide
(use-package indent-guide
  :init
  (setq indent-guide-recursive nil)
  :hook (prog-mode . indent-guide-mode)
  )

;; symbol-overlay
(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  )

;; magit
(use-package magit)

(use-package color-rg
  :defer 2
  :load-path "site-lisp/color-rg"
  :init
  (setq color-rg-search-no-ignore-file nil
        color-rg-mac-load-path-from-shell nil
        )
  )

(use-package combobulate
  :ensure nil
  :after treesit
  :load-path "site-lisp/combobulate"
  :hook ((python-ts-mode c-ts-mode julia-ts-mode) . combobulate-mode)
  )

(use-package devdocs)

;; lisp
(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

(use-package code-cells
  :hook ((python-ts-mode julia-ts-mode) . code-cells-mode-maybe)
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

;; python
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

(setq
 python-shell-dedicated t
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "-i --simple-prompt")

(use-package pyvenv)
(use-package pyvenv-auto
  :hook (python-ts-mode . pyvenv-auto-run))
(use-package poetry)

;; Julia
(use-package julia-mode)
(use-package julia-ts-mode
  :mode "\\.jl$")

(use-package eglot-jl
  :init
  (with-eval-after-load 'eglot
    (eglot-jl-init))
  )
(use-package julia-snail
  :custom
  (julia-snail-terminal-type :eat)
  :hook (julia-ts-mode . julia-snail-mode)
  )

;; yaml
(use-package yaml-mode)

;; sh-script
(add-hook
 'sh-mode-hook
 (lambda()
   (setq sh-indentation 2
         sh-basic-offset 2)
   (electric-indent-mode -1)
   (ansi-color-for-comint-mode-on)
   (compilation-shell-minor-mode 1)
   )
 )

;; vim-script
(use-package vimrc-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.?vim\\(rc\\)?\\'" . vimrc-mode))
  )

;; maxima
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode))

(provide 'init-lang)
