;;; -*- coding: utf-8; lexical-binding: t; -*-
(add-hook
 #'before-save-hook
 #'(lambda ()
     (delete-trailing-whitespace)
     (whitespace-cleanup)))

(use-package eglot
  :ensure nil
  :hook ((c-mode
          c-ts-mode
          R-mode
          python-mode
          python-ts-mode
          julia-mode
          julia-ts-mode) . eglot-ensure)
  )

(use-package treesit
  :ensure nil
  :init
  (setq
   treesit-extra-load-path (list (no-littering-expand-var-file-name "tree-sitter"))
   treesit-language-source-alist
   '( (c      . ("https://github.com/tree-sitter/tree-sitter-c"))
      (julia  . ("https://github.com/tree-sitter/tree-sitter-julia"))
      (python . ("https://github.com/tree-sitter/tree-sitter-python")))
   )
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (julia-mode . julia-ts-mode)
          (python-mode . python-ts-mode)))
  )

(use-package reformatter)

(use-package yasnippet-snippets)
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

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
(add-hook
 'emacs-lisp-mode
 (lambda ()
   (prettify-symbols-mode)
   ))

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
