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

(use-package evil-mc
  :init
  (defvar evil-mc-key-map (make-sparse-keymap))
  :config
  (defhydra yx/hydra-evil-mc ( :color pink
                               :hint nil
                               :pre (evil-mc-pause-cursors))
    "
 _M_ all match          _m_ here           _u_ undo
 _n_ next match         _J_ next line      _s_ suspend
 _p_ prev match         _K_ previous line  _r_ resume
 _N_ skip & next match  _H_ first cursor   _q_ quit
 _P_ skip & prev match  _L_ last cursor    _U_ undo all
    "
    ("m" evil-mc-make-cursor-here)
    ("M" evil-mc-make-all-cursors)
    ("n" evil-mc-make-and-goto-next-match)
    ("p" evil-mc-make-and-goto-prev-match)
    ("N" evil-mc-skip-and-goto-next-match)
    ("P" evil-mc-skip-and-goto-prev-match)
    ("J" evil-mc-make-cursor-move-next-line)
    ("K" evil-mc-make-cursor-move-prev-line)
    ("H" evil-mc-make-and-goto-first-cursor)
    ("L" evil-mc-make-and-goto-last-cursor)
    ("u" evil-mc-undo-last-added-cursor)
    ("U" evil-mc-undo-all-cursors)
    ("s" evil-mc-pause-cursors)
    ("r" evil-mc-resume-cursors :exit t)
    ("q" evil-mc-undo-all-cursors :exit t)
    )

  (evil-define-key '(normal visual) 'global
    (kbd "M") 'yx/hydra-evil-mc/body)

  (evil-define-key 'visual evil-mc-key-map
    "A" 'evil-mc-make-cursor-in-visual-selection-end
    "I" 'evil-mc-make-cursor-in-visual-selection-beg)
  )

(use-package evil-owl
  :init
  (setq evil-owl-idle-delay 0.2)
  :config
  (evil-owl-mode 1)
  )

(provide 'init-evil)
;;; init-evil.el ends here
