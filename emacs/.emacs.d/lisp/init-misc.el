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
  :defer 1
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
                "^nnrs"))
    (add-to-list 'golden-ratio-exclude-buffer-regexp re))
  (golden-ratio-mode 1)
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
          "^\\*eshell.*\\*$" eshell-mode
          help-mode
          occur-mode
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
       "\\*es?hell\\*"
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

(use-package pyim
  :init
  (setq default-input-method "pyim"
        pyim-default-scheme 'quanpin
        pyim-cloudim        'baidu
        pyim-indicator-list '(pyim-indicator-with-modeline))
  (setq-default
   pyim-punctuation-translate-p '(auto)
   pyim-english-input-switch-functions
   '(pyim-probe-program-mode
     pyim-probe-auto-english
     pyim-probe-evil-normal-mode
     pyim-probe-org-speed-commands)
   pyim-punctuation-half-width-functions
   '(pyim-probe-punctuation-line-beginning
     pyim-probe-punctuation-after-punctuation))
  :config
  (use-package pyim-basedict
    :init (pyim-basedict-enable))
  )

(use-package elfeed
  :init
  (setq elfeed-feeds
        '(
          ("https://ruder.io/rss/index.rss" ai nlp)
          ("https://www.inference.vc/rss" ai)
          ("https://emacsredux.com/atom.xml" emacs)
          ("https://sachachua.com/blog/category/emacs/feed/" emacs)
          ("https://manateelazycat.github.io/feed.xml" emacs)
          ("https://linuxhandbook.com/rss/" linux)
          ("http://www.ruanyifeng.com/blog/atom.xml" 技术 软件))
        )
  )

(provide 'init-misc)
;;; init-misc.el ends here
