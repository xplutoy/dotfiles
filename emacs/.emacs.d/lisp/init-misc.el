;;; init-misc.el --- misc plugin configure. -*- coding: utf-8; lexical-binding: t; -*-
(use-package gcmh
  :defer 1
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (gcmh-mode 1))

(use-package ibuffer-vc
  :init
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package which-key
  :defer 1
  :init
  (setq which-key-idle-delay 1.0
        which-key-idle-secondary-delay 0.05)
  :config
  (which-key-setup-minibuffer)
  (which-key-mode 1)
  )

(use-package winum
  :defer 1
  :init
  (setq winum-scope 'frame-local
        winum-format " [%s] "
        winum-auto-assign-0-to-minibuffer t
        winum-ignored-buffers-regexp '("^\\*Help.*")
        )
  :config
  (winum-set-keymap-prefix nil)
  (winum-mode 1)
  )

(use-package avy
  :init
  (setq avy-style 'at
        avy-timeout-seconds 0.8)
  )

(use-package posframe)

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark))
  )

(use-package minions
  :hook (after-init . minions-mode)
  )

(use-package golden-ratio
  :init
  (setq golden-ratio-auto-scale t
        golden-ratio-max-width nil)
  (setq golden-ratio-exclude-modes
        '(gnus-mode
          gnus-summary-mode
          gnus-article-mode
          which-key-mode
          vundo-mode
          calendar-mode
          imenu-list-major-mode))
  :config
  (dolist (re '("^\\*Ilist"
                "^\\*Org"
                "^\\*Agenda"
                "^\\*eshell"
                "^nnrs"))
    (add-to-list 'golden-ratio-exclude-buffer-regexp re))
  )

(use-package popper
  :defer 1
  :bind (("C-`" . popper-toggle-latest)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (setq popper-group-function #'popper-group-by-directory)
  (setq popper-reference-buffers
        '("\\*Ibuffer\\*"
          "\\*evil-registers\\*"
          "\\*evil-owl\\*"
          help-mode
          occur-mode
          dired-mode
          compilation-mode
          ))
  (popper-mode 1)
  (popper-echo-mode 1)
  )

(use-package shackle
  :defer 1
  :init
  (setq shackle-default-size 0.33
        shackle-select-reused-windows t
        shackle-inhibit-window-quit-on-same-windows t)
  :config
  (setq
   shackle-rules
   '((("\\*Ibuffer\\*"
       "\\*Help\\*"
       "\\*es?hell.*"
       "\\*info\\*"
       "\\*[Wo]*Man.*\\*"
       "\\*Dictionary\\*"
       "\\*Flymake .*"
       "^CAPTURE-"
       dired-mode
       occur-mode
       color-rg-mode)
      :regexp t :select t :popup t :align below)
     (("\\*Warnings\\*"
       "\\*Messages\\*"
       "\\*evil-registers\\*"
       "\\*evil-owl\\*"
       "^\\*Compile"
       "\\*Agenda Commands\\*"
       "^\\*Org Note"
       "^\\*Org Select"
       "\\*Capture\\*"
       "\\*Shell Command Output\\*")
      :regexp t :select nil :popup t :align below)
     (("^magit")
      :regexp t :select t :same t)
     ))
  (shackle-mode 1)
  )

(use-package move-text
  :defer 1
  :config
  (move-text-default-bindings)
  )

(use-package holymotion
  :ensure nil
  :load-path "site-lisp/holymotion"
  :hook ((prog-mode conf-mode) . holymotion-mode)
  :bind (:map holymotion-mode-map
              ([remap next-line]          . holymotion-next-line)
              ([remap previous-line]      . holymotion-previous-line)
              ([remap forward-word]       . holymotion-forward-word)
              ([remap backward-word]      . holymotion-backward-word)
              ([remap forward-sentence]   . holymotion-forward-sentence)
              ([remap backward-sentence]  . holymotion-backward-sentence)
              ([remap beginning-of-defun] . holymotion-backward-beginning-of-defun)
              ([remap end-of-defun]       . holymotion-forward-end-of-defun)
              )
  :config
  (holymotion-make-motion
   holymotion-forward-end-of-defun #'end-of-defun)
  )

(use-package sis
  :defer 1
  :hook
  ((org-capture-mode . sis-set-other))
  :init
  ;; C-s/r 默认优先使用英文
  (setq
   sis-respect-go-english-triggers
   '(isearch-forward isearch-backward)
   sis-respect-restore-triggers
   '(isearch-exit isearch-abort))
  (setq
   sis-inline-with-other t
   sis-other-cursor-color "#FF2121"
   sis-auto-refresh-seconds nil
   sis-inline-tighten-tail-rule 'all)
  :config
  (sis-ism-lazyman-config
   "com.apple.keylayout.US"
   "com.sogou.inputmethod.sogou.pinyin")
  (sis-global-inline-mode 1)
  (sis-global-context-mode 1)
  (sis-global-respect-mode 1)
  (sis-global-cursor-color-mode 1)
  )

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq
   doom-modeline-icon t
   doom-modeline-height 1
   doom-modeline-modal-icon t
   doom-modeline-project-detection 'project)
  )

(use-package diredfl
  :hook (dired-mode . diredfl-mode)
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t)
  )

(use-package nerd-icons)
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package dirvish
  :hook (after-init . dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~" "Home")
     ("d" "~/Downloads/" "Downloads")
     ("w" "~/workspace/" "Workspace")))
  :config
  (setq dirvish-attributes '(file-size collapse vc-state subtree-state)
        dirvish-use-header-line nil
        dirvish-default-layout '(0 0.3 0.7)
        dirvish-preview-dispatchers nil)
  :bind
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)
   )
  )

(use-package ef-themes
  :init
  (setq
   ef-themes-mixed-fonts t
   ef-themes-to-toggle '(ef-light ef-night)
   ef-themes-headings '((1 . (variable-pitch 1.3)) (2 . (regular 1.2)) (3 . (1.1)) (agenda-date . (1.3)) (agenda-structure . (variable-pitch 1.3)) (t . (t))))
  (ef-themes-select 'ef-light))

(provide 'init-misc)
;;; init-misc.el ends here
