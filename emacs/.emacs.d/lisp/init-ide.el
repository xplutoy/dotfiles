;;; init-ide.el --- plugin about ide. -*- coding: utf-8; lexical-binding: t; -*-
(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c++-ts-mode c-mode c-ts-mode) "clangd"))
  (add-to-list 'eglot-server-programs '(python-mode "pyls"))
  :hook ((c-mode c-ts-mode c++-mode c++-ts-mode python-mode sh-mode) . eglot-ensure)
  )

(use-package treesit
  :ensure nil
  :init
  (setq treesit-language-source-alist
        '((bash   . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c      . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp    . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (lua    . ("https://github.com/Azganoth/tree-sitter-lua"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (rust   . ("https://github.com/tree-sitter/tree-sitter-rust"))))
  (setq major-mode-remap-alist
        '((c-mode      . c-ts-mode)
          (c++-mode    . c++-ts-mode)
          (sh-mode     . bash-ts-mode)
          (python-mode . python-ts-mode))))

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
  (global-diff-hl-mode 1)
  :hook (dired-mode . diff-hl-dired-mode)
  )

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;; aggressive-indent
(use-package aggressive-indent
  :hook (((emacs-lisp-mode c-mode python-mode) . aggressive-indent-mode)
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1))))))

;; indent-guide
(use-package indent-guide
  :init
  (setq indent-guide-recursive nil)
  :hook (prog-mode . indent-guide-mode)
  )

;; magit
(use-package magit)

(use-package symbol-overlay
  :bind (("M-i" . symbol-overlay-put)
         ("M-I" . symbol-overlay-remove-all)
         :map symbol-overlay-map
         ("C-g"  . symbol-overlay-remove-all))
  :hook (prog-mode . symbol-overlay-mode)
  )

(use-package color-rg
  :defer 2
  :load-path "site-lisp/color-rg"
  :init
  (setq color-rg-search-no-ignore-file nil
        color-rg-mac-load-path-from-shell nil)
  )

(use-package combobulate
  :ensure nil
  :after treesit
  :load-path "site-lisp/combobulate"
  :hook ((python-ts-mode c++-ts-mode c-ts-mode bash-ts-mode) . combobulate-mode)
  )

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

(provide 'init-ide)
;;; init-ide.el ends here
