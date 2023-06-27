;;; init-evli.el --- evil configure -*- coding: utf-8; lexical-binding: t; -*-
(use-package general
  :config
  (general-evil-setup t)
  (general-unbind flyspell-mode-map "C-," "C-." "C-;")
  (general-define-key
   ;; beter defaults
   [remap zap-to-char]            'zap-up-to-char
   [remap list-buffers]           'ibuffer
   [remap dabbrev-expand]         'hippie-expand
   [remap undo]                   'undo-only
   [remap kill-buffer]            'kill-current-buffer
   [remap toggle-input-method]    'sis-switch
   [remap comment-dwim]           'evil-commentary-line
   [remap tab-to-tab-stop]        'consult-imenu-multi
   "s-<return>" 'toggle-frame-maximized
   "C-'"     'vterm-toggle-cd
   "C-;"   'yx/toggole-eshell
   "C-."   'embark-act
   "C-,"   'embark-dwim
   "C-h B" 'embark-bindings
   "C-c a" 'org-agenda
   "C-c c" 'org-capture
   "C-c b" 'org-switchb
   "C-c l" 'org-store-link
   "C-c t" 'org-show-todo-tree
   "C-c j" 'org-journal-new-entry
   "C-c g" 'consult-ripgrep
   )
  (general-create-definer yx-space-leader-def
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :states '(normal visual insert emacs))
  (yx-space-leader-def :keymaps 'prog-mode-map
    "d" '(:ignore t :which-key "code docs")
    "de" 'eldoc
    "dd" 'devdocs-lookup
    "g" '(:ignore t :which-key "code navigate")
    "gf" 'find-function
    "gv" 'find-variable
    "gs" 'consult-eglot-symbols
    "gd" 'xref-find-definitions
    "gr" 'xref-find-references
    "go" 'xref-find-definitions-other-window
    "e" '(:ignore t :which-key "code lint and refactor")
    "ef" 'eglot-format
    "er" 'eglot-rename
    "eh" 'eglot-help-at-point
    "en" 'flymake-goto-next-error
    "ep" 'flymake-goto-prev-error
    "eb" 'flymake-show-buffer-diagnostics
    "h" '(:ignore t :which-key "code hilight")
    "hh" 'symbol-overlay-put
    "hc" 'symbol-overlay-remove-all
    )
  (yx-space-leader-def
    "SPC" 'execute-extended-command-for-buffer
    "0" 'winum-select-window-0-or-10
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9
    ;; The `j' prefix is for jumping(in buffer), joining and splitting.
    "j" '(:ignore t :which-key "jump in buffer")
    "j;"  'evil-goto-last-change
    "jj"  'evil-avy-goto-char-timer
    "jw"  'evil-avy-goto-word-or-subword-1
    "jl"  'consult-line
    "jL"  'consult-line-multi
    "ji"  'consult-imenu
    "jo"  'consult-outline
    "jm"  'consult-mark
    "jM"  'consult-global-mark
    "v" '(:ignore t :which-key "magit")
    "vs"  'magit-status
    "vb"  'magit-blame
    "vd"  'magit-diff-buffer-file
    "vv"  'magit-dispatch
    "vf" '(:ignore t :which-key "file")
    "vfc" 'magit-file-checkout
    "vfd" 'magit-file-delete
    "vfr" 'magit-file-rename
    "vfs" 'magit-stage-file
    "vfu" 'magit-unstage-file
    "w" '(:ignore t :which-key "windows")
    "wu"  'winner-undo
    "wr"  'winner-redo
    "w0"  'delete-window
    "w1"  'delete-other-windows
    "w2"  'split-window-vertically
    "w3"  'split-window-horizontally
    "s" '(:ignore t :which-key "search")
    "sg"  'color-rg-search-symbol
    "sh"  'color-rg-search-input
    "sj"  'color-rg-search-symbol-in-project
    "sk"  'color-rg-search-input-in-project
    "s,"  'color-rg-search-symbol-in-current-file
    "s."  'color-rg-search-input-in-current-file
    "t" '(:ignore t :which-key "toggole")
    "tgr" 'golden-ratio-mode
    "tsl" 'so-long-minor-mode
    "thi" 'hi-lock-mode
    "thl" 'hl-line-mode
    "p" '(:ignore t :which-key "project")
    "pp"  'project-switch-project
    "pb"  'project-switch-to-buffer
    "pd"  'project-find-dir
    "pf"  'project-find-file
    "p!"  'project-shell-command
    "p&"  'project-async-shell-command
    "pe"  'project-eshell
    "pr"  'project-query-replace-regexp
    "pg"  'project-find-regexp
    "px"  'project-execute-extended-command
    "r" '(:ignore t :which-key "register & bookmark")
    "rj"  'jump-to-register
    "ri"  'insert-register
    "rS"  'copy-to-register
    "rs"  'consult-register-store
    "rl"  'consult-register-load
    "rr"  'consult-register
    "rb"  'consult-bookmark
    ))

(use-package evil
  :hook (after-init . evil-mode)
  :custom
  (evil-undo-system 'undo-redo)
  :init
  (setq evil-default-state 'emacs
        evil-want-fine-undo t
        evil-want-C-u-scroll t
        evil-want-C-w-delete t
        evil-want-integration t
        evil-want-keybinding nil
        evil-want-Y-yank-to-eol t
        evil-motion-state-modes nil
        evil-magic 'very-magic
        evil-search-module 'evil-search
        evil-respect-visual-line-mode t
        evil-ex-search-vim-style-regexp t
        evil-disable-insert-state-bindings t)

  :config
  (defvar yx-initial-evil-state-setup
    '((conf-mode . normal)
      (text-mode . normal)
      (prog-mode . normal)
      (color-rg-mode . emacs))
    "Default evil state per major mode.")
  (dolist (p yx-initial-evil-state-setup)
    (evil-set-initial-state (car p) (cdr p)))

  (general-def 'normal org-mode-map
    [tab]   'org-cycle
    [S-tab] 'org-shifttab)
  :general
  (:states 'normal
           "C-." nil  ;; unbind evil-repeat-pop
           ;; more emacs style
           "C-p" 'previous-line
           "C-n" 'next-line
           "C-a" 'move-beginning-of-line
           "C-e" 'end-of-line
           "C-y" 'yank
           "M-." 'xref-find-definitions
           "gD"  'devdocs-lookup
           )
  (:states 'insert
           "C-a" 'move-beginning-of-line)
  (evil-ex-completion-map "C-a" 'move-beginning-of-line
                          "C-b" 'backward-char
                          "M-n" 'next-complete-history-element
                          "M-p" 'previous-complete-history-element)
  )

(use-package evil-commentary
  :hook (prog-mode . evil-commentary-mode)
  )

(use-package vimish-fold)
(use-package evil-vimish-fold
  :hook (prog-mode . evil-vimish-fold-mode)
  :init
  (setq evil-vimish-fold-mode-lighter " ₱")
  )

(use-package evil-owl
  :defer 1
  :config
  (evil-owl-mode 1)
  )

(provide 'init-evil)
