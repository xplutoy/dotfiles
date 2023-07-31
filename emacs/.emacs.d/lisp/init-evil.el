;;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package general
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)
  (general-create-definer yx-space-leader-def
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (general-create-definer yx-comma-leader-def
    :states '(normal visual insert emacs)
    :major-modes t
    :prefix ","
    :non-normal-prefix "M-,"
    )
  )

(use-package key-chord
  :init
  (key-chord-mode 1)
  (key-chord-define-global "zz"     'zoom)
  (key-chord-define-global "ss"     'scratch-buffer)
  (key-chord-define-global "jj"     'avy-goto-char-timer)
  (key-chord-define-global "jl"     'avy-goto-line)
  (key-chord-define-global "jk"     'evil-emacs-state)
  (key-chord-define-global "gh"     'consult-ripgrep)
  (key-chord-define-global "hh"     'color-rg-search-symbol)
  (key-chord-define-global "ff"     'find-file)
  (key-chord-define-global "jb"     'consult-buffer)
  (key-chord-define-global "df"     'osx-dictionary-search-word-at-point)
  )

(general-unbind flyspell-mode-map "C-," "C-." "C-;")

(general-define-key
 ;; beter defaults
 "C->" 'scroll-right
 "C-<" 'scroll-left
 "C-{" 'shrink-window-horizontally
 "C-}" 'enlarge-window-horizontally
 "C-^" 'enlarge-window

 "H-z" 'zoom

 [remap what-cursor-position]   'zoom                   ;C-x =
 [remap describe-bindings]      'embark-bindings        ;C-h b
 [remap list-buffers]           'ibuffer                ;C-x C-b
 [remap dabbrev-expand]         'hippie-expand          ;M-/
 [remap undo]                   'undo-only              ;C-/
 [remap kill-buffer]            'kill-buffer-and-window ;C-x k
 [remap toggle-input-method]    'sis-switch             ;C-\\
 [remap comment-dwim]           'evil-commentary-line   ;M-;
 [remap tab-to-tab-stop]        'consult-imenu-multi    ;M-i
 [remap zap-to-char]            'vg-quick-zap-to-char   ;M-z
 [remap customize]              'winner-undo            ;s-,
 "s-."                          'winner-redo
 "s-<return>" 'toggle-frame-maximized
 "s-;"        'flyspell-correct-wrapper
 "s-'"        'flyspell-correct-next
 "s-r"        'consult-recent-file
 "s-b"        'ibuffer
 "s-o"        'ace-window
 "C-'"        'vterm-toggle-cd
 "C-;"        'yx/toggole-eshell
 "C-."        'embark-act
 "C-,"        'embark-dwim

 "M-g ;"      'goto-last-change

 "C-c v"      'magit-file-dispatch

 "C-c d"      'osx-dictionary-search-pointer
 "C-c D"      'osx-dictionary-search-input

 "C-c g"      'consult-ripgrep
 "C-c f"      'consult-find

 "C-c u u"    'browse-url-at-point  ;g x in evil

 "C-c a"      'org-agenda
 "C-c c"      'org-capture
 "C-c b"      'org-switchb
 "C-c l"      'org-store-link
 "C-c t"      'org-show-todo-tree

 "C-c n l"    'org-roam-buffer-toggle
 "C-c n n"    'org-roam-node-find
 "C-c n i"    'org-roam-node-insert
 "C-c n c"    'org-roam-capture
 "C-c n j"    'org-roam-dailies-capture-today
 "C-c n r"    'consult-org-roam-search
 "C-c n b"    'consult-org-roam-backlinks
 "C-c n f"    'consult-org-roam-forward-links
 "C-c n t"    'org-transclusion-add
 "C-c n T"    'org-transclusion-add-all


 "C-h M"      'which-key-show-major-mode
 "C-h B"      'embark-bindings-at-point
 "C-h C-f"    'find-function
 "C-h C-k"    'find-function-on-key
 "C-h C-v"    'find-variable
 "C-h C-l"    'find-library
 "C-h C-i"    'info-display-manual
 )

(yx-space-leader-def
  "SPC" 'execute-extended-command-for-buffer
  "!" 'shell-command
  "/" 'consult-ripgrep
  "u" 'universal-argument

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
  "f" '(:ignore t :which-key "file/buffer")
  "ff"  'find-file
  "ft"  'find-file-other-tab
  "fo"  'find-file-other-window
  "fO"  'crux-open-with
  "fr"  'rename-visited-file
  "fF"  'crux-sudo-edit
  "fE"  'crux-reopen-as-root
  "fk"  'kill-current-buffer
  "fK"  'crux-kill-other-buffers
  "fD"  'crux-delete-file-and-buffer
  "j" '(:ignore t :which-key "jump(in buffer)")
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
  "vp"  'magit-pull-branch
  "vP"  'magit-push-current
  "vr"  'magit-rebase
  "vv"  'magit-dispatch
  "vl" '(:ignore t :which-key "log")
  "vlc" 'magit-log-current
  "vlf" 'magit-log-buffer-file
  "vf" '(:ignore t :which-key "file")
  "vff" 'magit-file-dispatch
  "vfc" 'magit-file-checkout
  "vfd" 'magit-file-delete
  "vfr" 'magit-file-rename
  "vfs" 'magit-stage-file
  "vfu" 'magit-unstage-file
  "w" '(:ignore t :which-key "windows")
  "w,"  'winner-undo
  "w."  'winner-redo
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
  "T" '(:ignore t :which-key "Taggol")
  "Tsl" 'so-long-minor-mode
  "Thi" 'hi-lock-mode
  "Thl" 'hl-line-mode
  "t" '(:ignore t :which-key "tabbar && tabline")
  "tt"  'tab-bar-mode
  "tn"  'tab-bar-new
  "tc"  'tab-bar-close
  "tl"  'tab-line-mode
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
  )

(yx-comma-leader-def prog-mode-map
  "d" '(:ignore t :which-key "code docs")
  "de" 'eldoc
  "dd" 'devdocs-lookup
  "g" '(:ignore t :which-key "code navigate")
  "gg" 'consult-xref
  "gf" 'consult-flymake
  "gs" 'consult-eglot-symbols
  "gd" 'xref-find-definitions
  "gr" 'xref-find-references
  "go" 'xref-find-definitions-other-window
  "e" '(:ignore t :which-key "code action")
  "ef" 'eglot-format
  "er" 'eglot-rename
  "eh" 'eglot-help-at-point
  "ea" 'eglot-code-actions
  "en" 'flymake-goto-next-erroer
  "ep" 'flymake-goto-prev-error
  "eb" 'flymake-show-buffer-diagnostics
  "eB" 'flymake-show-project-diagnostics
  "h" '(:ignore t :which-key "code hilight")
  "hh" 'symbol-overlay-put
  "hc" 'symbol-overlay-remove-all
  "ht" 'hl-todo-occur
  )

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq
   evil-move-beyond-eol t
   evil-default-state 'emacs
   evil-shift-width tab-width
   evil-want-fine-undo t
   evil-want-C-u-scroll t
   evil-want-C-w-delete nil
   evil-want-integration t
   evil-want-keybinding nil
   evil-want-Y-yank-to-eol t
   evil-undo-system 'undo-redo
   evil-magic 'very-magic
   evil-search-module 'evil-search
   evil-respect-visual-line-mode t
   evil-ex-search-vim-style-regexp t
   evil-disable-insert-state-bindings t
   )
  :config
  (defvar yx-initial-evil-state-setup
    '((conf-mode . normal)
      (text-mode . normal)
      (prog-mode . normal)
      (org-mode  . insert)
      (color-rg-mode . emacs))
    "Default evil state per major mode.")
  (dolist (p yx-initial-evil-state-setup)
    (evil-set-initial-state (car p) (cdr p)))

  (general-def 'normal org-mode-map
    [tab]   'org-cycle
    [S-tab] 'org-shifttab)
  :general
  (:states '(normal visual)
           "C-." nil  ;; unbind evil-repeat-pop
           ;; more emacs style
           "C-p" 'previous-line
           "C-n" 'next-line
           "C-a" 'move-beginning-of-line
           "C-e" 'end-of-line
           "C-y" 'yank
           "C-w" 'kill-region
           "M-." 'xref-find-definitions
           "gD"  'devdocs-lookup
           )
  (:states 'insert
           "C-a" 'move-beginning-of-line
           "C-y" 'yank
           "C-k" 'kill-line
           "C-w" 'kill-region
           "C-g" 'evil-normal-state)
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
  :custom
  (evil-vimish-fold-mode-lighter " ♇")
  )

(use-package evil-owl
  :defer 1
  :config
  (evil-owl-mode 1)
  )

(provide 'init-evil)
