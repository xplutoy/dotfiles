;;; -*- coding: utf-8; lexical-binding: t; -*-
(setq
 winner-dont-bind-my-keys t
 winner-boring-buffers-regexp "^\\*")
(add-hook 'after-init-hook 'winner-mode)

(setq
 help-window-select t
 help-window-keep-selected t)

(setq
 switch-to-buffer-obey-display-actions t
 switch-to-buffer-in-dedicated-window nil
 switch-to-buffer-preserve-window-point t
 display-buffer-base-action
 '((display-buffer-same-window
    display-buffer-reuse-window
    display-buffer-in-previous-window
    display-buffer-reuse-mode-window
    display-buffer-below-selected
    display-buffer-pop-up-window)
   (reusable-frames . t)
   (window-min-width . 30)
   (window-height . 15)
   )
 )

;; tabbar
(setq
 tab-bar-tab-hints t
 tab-bar-select-tab-modifiers '(super))


;; ibuffer
(setq
 ibuffer-expert t
 ibuffer-display-summary nil
 ibuffer-show-empty-filter-groups nil
 ibuffer-never-show-predicates '("^\\*"))
(add-hook
 'ibuffer-mode-hook
 (lambda ()
   (ibuffer-do-sort-by-recency)
   (ibuffer-auto-mode 1)))

(use-package ibuffer-vc
  :init
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-mouse-drag-files t)
  (dired-omit-files "^\\..*$")
  (dired-recursive-copies 'always)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  (dired-guess-shell-alist-user `(("\\.jpe?g\\'" "open")
                                  ("\\.mp4\\'"   "iina")))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-omit-mode)
              (dired-hide-details-mode)))
  )

(use-package diredfl
  :hook (dired-mode . diredfl-mode)
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t)
  )

(use-package dired-narrow
  :bind (:map dired-mode-map
              ([remap dired-do-man] . dired-narrow-fuzzy)))
(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))


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

(use-package popper
  :defer 1
  :bind (("C-`" . popper-toggle-latest)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (setq
   popper-display-control 'user
   popper-group-function #'popper-group-by-directory)
  (setq popper-reference-buffers
        '("\\*Ibuffer\\*"
          "\\*evil-registers\\*"
          "\\*evil-owl\\*"
          "\\*shell.*\\*$" shell-mode
          "\\*eshell.*\\*$" eshell-mode
          "\\*term.*\\*$" term-mode
          "\\*julia\\*$"
          "\\*color-rg\\*$"
          help-mode
          occur-mode
          compilation-mode
          ))
  (popper-mode 1)
  (popper-echo-mode 1)
  )

(use-package shackle
  :defer 1
  :custom
  (shackle-default-size 0.4)
  (shackle-select-reused-windows t)
  (shackle-default-alignment 'below)
  (shackle-inhibit-window-quit-on-same-windows nil)
  :config
  (setq
   shackle-rules
   '((("\\*Ibuffer\\*"
       "\\*Help\\*"
       "\\*info\\*"
       "\\*[Wo]*Man.*\\*"
       "\\*Dictionary\\*"
       "\\*Flymake .*"
       "^CAPTURE-"
       "^\\*julia.*")
      :regexp t :select t :popup t :align t)
     (("\\*Warnings\\*"
       "\\*Messages\\*"
       "\\*evil-registers\\*"
       "\\*evil-owl\\*"
       "^\\*Compile"
       "\\*Agenda Commands\\*"
       "^\\*Org Note"
       "^\\*Org Select"
       "\\*Capture\\*"
       "^\\*org-roam\\*"
       "\\*Shell Command Output\\*")
      :regexp t :nonselect t :popup t :align t)
     ((dired-mode color-rg-mode)
      :select t :popup t)
     (("^magit" magit-mode vterm-mode)
      :regexp t :select t :same t :inhibit-window-quit t)
     )
   )
  (shackle-mode 1)
  )

(provide 'init-window)
