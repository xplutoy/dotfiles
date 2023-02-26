;;; init-keymaps.el --- evil configure -*- coding: utf-8; lexical-binding: t; -*-
;;; code:
(use-package hydra)
(use-package major-mode-hydra
  :after hydra
  :config
  (pretty-hydra-define yx/hydra-smerge
    (:hint nil :color pink :quit-key "q" :title "hydra-smerge")
    ("Move"
     (("n" smerge-next "next")
      ("p" smerge-prev "previous"))
     "Keep"
     (("RET" smerge-keep-current "current")
      ("a" smerge-keep-all "all")
      ("b" smerge-keep-base "base")
      ("l" smerge-keep-lower "lower")
      ("u" smerge-keep-upper "upper"))
     "Diff"
     (("<" smerge-diff-base-upper "upper/base")
      ("=" smerge-diff-upper-lower "upper/lower")
      (">" smerge-diff-base-lower "base/lower")
      ("R" smerge-refine "redefine")
      ("E" smerge-ediff "ediff"))
     "Other"
     (("C" smerge-combine-with-next "combine")
      ("r" smerge-resolve "resolve")
      ("k" smerge-kill-current "kill current"))))
  )
(use-package general
  :config
  (general-evil-setup t)
  (general-define-key
   ;; beter defaults
   [remap zap-to-char]            'zap-up-to-char
   [remap list-buffers]           'ibuffer
   [remap move-beginning-of-line] 'crux-move-beginning-of-line
   [remap kill-whole-line]        'crux-kill-whole-line ;; C-S-<backspace>
   [remap dabbrev-expand]         'hippie-expand
   [remap undo]                   'undo-only
   ;; global keybindings used requently
   "C-;"   'embark-act
   "C-`"   'vterm-toggle-cd
   "M-`"   'yx/eshell-toggle
   "C-c a" 'org-agenda
   "C-c c" 'org-capture
   "C-c b" 'org-switchb
   "C-c l" 'org-store-link
   )
  ;; for programimg
  (general-create-definer yx-comma-leader-def
    :prefix ";"
    :non-normal-prefix "s-;"
    :states '(normal visual insert emacs))
  (yx-comma-leader-def
    "M"  'yx/hydra-smerge/body

    "//" 'crux-indent-defun
    ;; the `e' prefix is for error
    "en" 'flymake-goto-next-error
    "ep" 'flymake-goto-prev-error
    "eb" 'flymake-diagnostic-buffer
    ;; the `h' prefix is for highlight
    "hs" 'symbol-overlay-put
    ;; `g' prefix is for jump or goto
    "gf" 'find-function
    "gv" 'find-variable
    "gd" 'xref-find-definitions
    "gr" 'xref-find-references
    "go" 'xref-find-definitions-other-window
    )
  ;; ohter than programing
  (general-create-definer yx-space-leader-def
    :prefix "SPC"
    :non-normal-prefix "s-SPC"
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
    ;; hydra
    "M" 'yx/hydra-evil-mc/body
    "N" 'yx/hydra-denote/body
    ;; The `f' prefix is for file
    "ff"  'find-file
    "fz"  'zoxide-find-file
    "fp"  'project-find-file
    "fr"  'consult-recent-file
    "fo"  'find-file-other-window
    "fD"  'crux-delete-file-and-buffer
    "fR"  'crux-rename-buffer-and-file
    "fE"  'crux-sudo-edit
    "fO"  'crux-open-with
    "fc"  'crux-copy-file-preserve-attributes
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
    ;; The `w' prefix is for windows
    "w="  'balance-windows-area
    "wd"  'ace-delete-window
    "wD"  'ace-delete-other-windows
    "wh"  'evil-window-left
    "wH"  'evil-window-move-far-left
    "wl"  'evil-window-right
    "wL"  'evil-window-move-far-right
    "wj"  'evil-window-down
    "wJ"  'evil-window-move-very-bottom
    "wk"  'evil-window-up
    "wK"  'evil-window-move-very-top
    "wo"  'other-window
    "ww"  'ace-window
    "wS"  'ace-swap-window
    "wm"  'ace-maximize-window
    "wu"  'winner-undo
    "wU"  'winner-redo
    "w-"  'split-window-below
    "ws"  'split-window-below
    "w/"  'split-window-right
    "wv"  'split-window-right
    "x0"  'delete-window
    "x1"  'delete-other-windows
    "x2"  'split-window-vertically
    "x3"  'split-window-horizontally
    "xo"  'ace-window
    ;; the `b' prefix is for buffer
    "bk"  'kill-buffer
    "bK"  'kill-buffer-and-window
    "b1"  'crux-kill-other-buffers
    "bn"  'next-buffer
    "bp"  'previous-buffer
    ;; the `s' prefix is for search
    "ss"  'color-rg-search-symbol
    "sp"  'color-rg-search-input-in-project
    "sb"  'color-rg-search-input-in-current-file
    "sN"  'consult-notes-search-in-all-notes
    ;; the `S' prefix is for flyspell
    "Sb"  'flyspell-buffer
    "Sr"  'flyspell-region
    "Sn"  'evil-next-flyspell-error
    "Sp"  'evil-prev-flyspell-error
    "Sc"  'flyspell-correct-word-before-point
    ;; the `p' prefix is for project
    "pp"  'project-switch-project
    "pb"  'project-switch-to-buffer
    "pB"  'project-list-buffers
    "pd"  'project-find-dir
    "pD"  'project-dired
    "pf"  'project-find-file
    "pk"  'project-kill-buffers
    "p!"  'project-shell-command
    "p&"  'project-async-shell-command
    "pe"  'project-eshell
    "pc"  'project-compile
    "pr"  'project-query-replace-regexp
    "pv"  'project-vc-dir
    "pg"  'project-find-regexp
    "px"  'project-execute-extended-command
    ;; `x' is for text or execute
    "xa"  'align-regexp
    "xu"  'downcase-region
    "xU"  'upcase-region
    "xm"  'yx/hydra-evil-mc/body
    ;; `r' is for register and bookmark
    "rj"  'jump-to-register
    "ri"  'insert-register
    "rS"  'copy-to-register
    "rs"  'consult-register-store
    "rl"  'consult-register-load
    "rr"  'consult-register
    "rb"  'consult-bookmark
    ;; `t' is for toggle
    "tH"  'holymotion-mode
    "tS"  'flyspell-mode
    "tI"  'imenu-list-smart-toggle
    "tG"  'golden-ratio-mode

    ;; misc
    "ds" 'sdcv-search-input
    "dt" 'sdcv-search-pointer+
    ))

(provide 'init-keymaps)
;;; init-keymaps.el ends here
