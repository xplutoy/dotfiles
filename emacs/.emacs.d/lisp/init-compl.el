;;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package vertico
  :demand t
  :config
  (setq vertico-resize nil)
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  (vertico-indexed-mode 1)
  :bind ( :map vertico-map
          ("C-q" . vertico-quick-insert)
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  )

(use-package vertico-prescient
  :after vertico
  :defer 1
  :config
  (vertico-prescient-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)))

(use-package embark
  :init
  (setq embark-selection-indicator nil))
(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (setq consult-ripgrep-args
        (concat consult-ripgrep-args " --hidden"))
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any))

  :bind (([remap goto-line]                     . consult-goto-line)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop)
         ([remap apropos]                       . consult-apropos)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap imenu]                         . consult-imenu)
         ("s-r" . consult-recent-file)
         ("C-x p b" . consult-project-buffer)
         ("M-#"     . consult-register-load)
         ("M-'"     . consult-register-store)
         ("C-M-#"   . consult-register)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  )

;; consult-*** ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))
(use-package consult-project-extra
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

(use-package consult-eglot
  :bind (("M-s s" . consult-eglot-symbols))
  )

;; corfu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package corfu
  :init
  (setq corfu-auto t
        corfu-preselect 'valid
        corfu-echo-documentation nil)
  :config
  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (corfu-indexed-mode 1)
  (corfu-popupinfo-mode 1)
  :hook ((text-mode prog-mode) . corfu-mode)
  :bind (:map corfu-map
              ("TAB"   . corfu-next)
              ("S-TAB" . corfu-previous)
              ("C-q"   . corfu-quick-insert)
              ("SPC"   . corfu-insert-separator))
  )

(use-package corfu-prescient
  :after corfu
  :defer 1
  :config
  (corfu-prescient-mode 1))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode +1))

;; cape
(use-package cape
  :after corfu
  :defer 1
  :init
  (setq cape-dabbrev-min-length 2
        completion-at-point-functions
        '(cape-ispell cape-dabbrev cape-file cape-symbol cape-abbrev))
  )

(provide 'init-compl)
