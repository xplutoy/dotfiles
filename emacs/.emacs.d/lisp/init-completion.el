;;; init-completion.el --- minibuffer enhancement. -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:
(use-package vertico
  :demand t
  :init
  (setq vertico-cycle t
        vertico-scroll-margin 1
        vertico-resize t)
  :config
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  (vertico-indexed-mode 1)
  :bind ( :map vertico-map
          ("M-q" . vertico-quick-insert) ;;vertico-quick
          ("C-q" . vertico-quick-exit)
          ("RET" . vertico-directory-enter) ;;vertico-directory
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  )

;; orderless ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package orderless
  :init
  (setq completion-styles             '(basic orderless)
        completion-category-defaults  nil)
  (setq completion-category-overrides
        '((file (styles . (basic partial-completion orderless)))
          (imenu (styles . (basic substring orderless)))
          (kill-ring (styles . (basic substring orderless)))
          (project-file (styles . (basic substring partial-completion orderless)))))
  )


;; consult ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  :config
  (setq consult-narrow-key "<")
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  :bind (([remap goto-line]                     . consult-goto-line)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop)
         ([remap apropos]                       . consult-apropos)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap imenu]                         . consult-imenu)
         ([remap find-file-read-only]           . consult-recent-file)
         ("C-x p b" . consult-project-buffer)
         ("M-#"     . consult-register-load)
         ("M-'"     . consult-register-store)
         ("C-M-#"   . consult-register)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
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
  (setq corfu-auto t  ;; set befor global-corfu-mode
        corfu-cycle t
        corfu-quit-at-boundary nil
        corfu-quit-no-match 'separator
        corfu-auto-delay 0
        corfu-auto-prefix 1
        corfu-preselect 'prompt
        corfu-echo-documentation nil)
  :config
  (global-corfu-mode 1)
  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (corfu-indexed-mode 1)
  (corfu-popupinfo-mode 1)
  :bind (:map corfu-map
              ("TAB"   . corfu-next)
              ("S-TAB" . corfu-previous)
              ("SPC"   . corfu-insert-separator)
              ("M-q"   . corfu-quick-complete)
              ("C-q"   . corfu-quick-insert))
  )

(use-package corfu-terminal
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode +1))

;; cape
(use-package cape
  :defer 1
  :init
  (setq cape-dabbrev-min-length 2)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  )

(provide 'init-completion)
;;; init-completion.el ends here
