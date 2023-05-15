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
   [remap comment-dwim]           'yx/comment-or-uncomment
   "<f1> f" 'toggle-frame-maximized
   "C-,"   'yx/eshell-toggle
   "C-c a" 'org-agenda
   "C-c c" 'org-capture
   "C-c b" 'org-switchb
   "C-c l" 'org-store-link
   )
  (general-create-definer yx-space-leader-def
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :states '(normal visual insert emacs))
  (yx-space-leader-def
    ;; 0 - 9 for window select
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
    "fp"  'project-find-file
    ;; The `j' prefix is for jumping(in buffer), joining and splitting.
    "jj"  'evil-avy-goto-char-timer
    "jw"  'evil-avy-goto-word-or-subword-1
    "jc"  'goto-last-change
    "jd"  'dired-jump
    "ji"  'consult-imenu
    "jo"  'consult-outline
    "jm"  'consult-mark
    "jM"  'consult-global-mark
    ;; the `g' pregix is for goto
    "gg"  'consult-ripgrep
    "gl"  'consult-line
    "gL"  'consult-line-multi
    "gn"  'consult-notes
    "gf" 'find-function
    "gv" 'find-variable
    "gd" 'xref-find-definitions
    "gr" 'xref-find-references
    "go" 'xref-find-definitions-other-window
    "en" 'flymake-goto-next-error
    "ep" 'flymake-goto-prev-error
    "eb" 'flymake-diagnostic-buffer
    "hs" 'symbol-overlay-put
    ;; The `w' prefix is for windows
    "wu"  'winner-undo
    "wr"  'winner-redo
    "w-"  'split-window-below
    "ws"  'split-window-below
    "w/"  'split-window-right
    "wv"  'split-window-right
    "x0"  'delete-window
    "x1"  'delete-other-windows
    "x2"  'split-window-vertically
    "x3"  'split-window-horizontally
    ;; the `b' prefix is for buffer
    "bk"  'kill-buffer-and-window
    "bn"  'next-buffer
    "bp"  'previous-buffer
    ;; the `s' prefix is for search
    "ss"  'color-rg-search-symbol
    "sp"  'color-rg-search-input-in-project
    "sb"  'color-rg-search-input-in-current-file
    "sN"  'consult-notes-search-in-all-notes
    ;; the `S' prefix is for flyspell
    "Sb"  'flyspell-buffer
    "Sn"  'evil-next-flyspell-error
    "Sp"  'evil-prev-flyspell-error
    "Sc"  'flyspell-correct-word-before-point
    ;; the `p' prefix is for project
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
    ;; `r' is for register and bookmark
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
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-want-integration t
        evil-want-keybinding nil
        evil-motion-state-modes nil
        evil-disable-insert-state-bindings t)

  :config
  (defvar yx-initial-evil-state-setup
    '((conf-mode . normal)
      (prog-mode . normal))
    "Default evil state per major mode.")
  (dolist (p yx-initial-evil-state-setup)
    (evil-set-initial-state (car p) (cdr p)))

  (general-def 'normal org-mode-map
    [tab]   'org-cycle
    [S-tab] 'org-shifttab)
  :general
  (:states 'normal
           "C-p" 'previous-line
           "C-n" 'next-line
           "C-a" 'move-beginning-of-line
           "C-e" 'end-of-line
           "C-y" 'yank
           "M-." 'xref-find-definitions)
  (:states 'insert
           "C-a" 'move-beginning-of-line)
  (evil-ex-completion-map "C-a" 'move-beginning-of-line
                          "C-b" 'backward-char
                          "M-n" 'next-complete-history-element
                          "M-p" 'previous-complete-history-element)
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
