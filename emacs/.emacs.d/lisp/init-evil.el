;;; init-evli.el --- evil configure -*- coding: utf-8; lexical-binding: t; -*-
(use-package evil
  :hook (after-init . evil-mode)
  :custom
  (evil-undo-system 'undo-redo)
  :init
  (setq evil-default-state 'emacs
        evil-search-module 'evil-search
        evil-respect-visual-line-mode t
        evil-want-C-u-scroll nil
        evil-want-C-w-delete t
        evil-want-Y-yank-to-eol t
        evil-move-cursor-back t
        evil-move-beyond-eol t
        evil-want-integration t
        evil-want-keybinding nil
        evil-motion-state-modes nil
        evil-disable-insert-state-bindings t)

  :config
  ;; mode with normal states
  (defvar yx-initial-evil-state-setup
    '((conf-mode . normal)
      (prog-mode . normal))
    "Default evil state per major mode.")
  (dolist (p yx-initial-evil-state-setup)
    (evil-set-initial-state (car p) (cdr p)))

  (general-def 'normal org-mode-map
    [tab]   'org-cycle
    [S-tab] 'org-shifttab)
  (general-def 'motion 'org-mode-map
    "{" 'org-backward-paragraph
    "}" 'org-forward-paragraph
    "(" 'org-backward-sentence
    ")" 'org-forward-sentence)
  :general
  (:states 'normal
           "C-p" 'previous-line
           "C-n" 'next-line
           "C-a" 'crux-move-beginning-of-line
           "C-e" 'end-of-line
           "C-y" 'yank
           "M-." 'xref-find-definitions)
  (:states 'insert
           "C-a" 'crux-move-beginning-of-line)
  (evil-ex-completion-map "C-a" 'crux-move-beginning-of-line
                          "C-b" 'backward-char
                          "M-n" 'next-complete-history-element
                          "M-p" 'previous-complete-history-element)
  )

(use-package evil-surround
  :defer 1
  :config
  (global-evil-surround-mode 1)
  )

(use-package evil-escape
  :defer 1
  :init
  (setq-default
   evil-escape-delay 0.15
   evil-escape-key-sequence "jk")
  (setq evil-escape-unordered-key-sequence t)
  :config
  (evil-escape-mode 1)
  )

(use-package evil-matchit
  :defer 1
  :config
  (global-evil-matchit-mode 1)
  )

(use-package evil-visualstar
  :defer 1
  :init
  (setq evil-visualstar/persistent t)
  :config
  (global-evil-visualstar-mode 1)
  )

(use-package vimish-fold)
(use-package evil-vimish-fold
  :defer 1
  :config
  (global-evil-vimish-fold-mode 1)
  )

(use-package evil-owl
  :defer 1
  :init
  (setq evil-owl-idle-delay 0.2)
  :config
  (evil-owl-mode 1)
  )

(provide 'init-evil)
;;; init-evil.el ends here
