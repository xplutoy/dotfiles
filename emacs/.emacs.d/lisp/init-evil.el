;;; init-evli.el --- evil configure -*- coding: utf-8; lexical-binding: t; -*-
(use-package general
  :config
  (general-evil-setup t)
  (general-define-key
   ;; beter defaults
   [remap zap-to-char]            'zap-up-to-char
   [remap list-buffers]           'ibuffer
   [remap dabbrev-expand]         'hippie-expand
   [remap undo]                   'undo-only
   [remap kill-buffer]            'kill-current-buffer
   [remap toggle-input-method]    'sis-switch
   [remap comment-dwim]           'evil-commentary-line
   "s-<return>" 'toggle-frame-maximized
   "C-,"   'yx/eshell-toggle
   "C-c a" 'org-agenda
   "C-c c" 'org-capture
   "C-c b" 'org-switchb
   "C-c l" 'org-store-link
   "C-c j" 'org-journal-new-entry
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
    "e" '(:ignore t :which-key "code lint")
    "en" 'flymake-goto-next-error
    "ep" 'flymake-goto-prev-error
    "eb" 'flymake-diagnostic-buffer
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
    "jj"  'evil-avy-goto-char-timer
    "jw"  'evil-avy-goto-word-or-subword-1
    "jl"  'consult-line
    "jL"  'consult-line-multi
    "ji"  'consult-imenu
    "jo"  'consult-outline
    "jm"  'consult-mark
    "jM"  'consult-global-mark
    ;; the `g' pregix is for goto
    "gg"  'consult-ripgrep
    "g;"  'evil-goto-last-change

    "w" '(:ignore t :which-key "windows")
    "wu"  'winner-undo
    "wr"  'winner-redo
    "w0"  'delete-window
    "w1"  'delete-other-windows
    "w2"  'split-window-vertically
    "w3"  'split-window-horizontally
    "s" '(:ignore t :which-key "search")
    "ss"  'color-rg-search-symbol
    "sp"  'color-rg-search-input-in-project
    "sb"  'color-rg-search-input-in-current-file
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
        evil-want-C-u-scroll nil
        evil-want-Y-yank-to-eol t
        evil-want-integration t
        evil-want-keybinding nil
        evil-motion-state-modes nil
        evil-search-module 'evil-search
        evil-respect-visual-line-mode t
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
  :defer 1
  :config
  (global-evil-vimish-fold-mode 1)
  )

(use-package evil-owl
  :defer 1
  :config
  (evil-owl-mode 1)
  )

(provide 'init-evil)
;;; init-evil.el ends here
