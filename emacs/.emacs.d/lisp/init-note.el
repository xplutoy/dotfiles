;;; -*- lexical-binding: t no-byte-compile: t -*-
(with-eval-after-load 'org
  (define-keymap
    :keymap org-mode-map
    "C-c <up>"   'org-priority-up
    "C-c <down>" 'org-priority-down
    "C-c y"      'org-download-yank
    "C-c Y"      'org-download-screenshot
    "C-c B"      'org-cite-insert
    )
  )

(use-package org
  :ensure nil
  :defer 1
  :custom
  (org-directory yx/doc-dir)
  (org-ellipsis "...")
  (org-tags-column 0)
  (org-num-max-level 2)
  (org-log-into-drawer t)
  ;; (org-return-follows-link t)
  (org-crypt-key yx/gpg-encrypt-key)
  (org-hide-emphasis-markers t)
  (org-use-sub-superscripts '{})
  (org-image-actual-width '(600))
  (org-special-ctrl-k t)
  (org-special-ctrl-a/e t)
  (org-fontify-quote-and-verse-blocks t)
  (org-agenda-window-setup 'current-window)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (org-src-tab-acts-natively t)
  (org-src-fontify-natively t)
  (org-src-window-setup 'split-window-right)
  (org-src-ask-before-returning-to-edit-buffer nil)

  (org-modules '(org-habit org-tempo))
  (org-habit-graph-column 60)

  (org-default-notes-file
   (expand-file-name "inbox.org" org-directory))
  (org-capture-templates
   '(("t" "task" entry (file+headline org-default-notes-file "Tasks")
      "* TODO [#B] %^{Title}\n%?")
     ("h" "habit" entry (file+headline org-default-notes-file "Habits")
      "* NEXT [#B] %^{Title}\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n%?"))
   )

  ;; org-cite
  (org-cite-global-bibliography
   (list (expand-file-name "references.bib" yx/doc-dir)))

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t) (python . t) (R . t) (julia . t)))
  (org-crypt-use-before-save-magic)
  (dolist (ele '(("sh" . "src shell")
                 ("el" . "src emacs-lisp")
                 ("py" . "src python")
                 ("r"  . "src R")
                 ("jl" . "src julia")))
    (add-to-list 'org-structure-template-alist ele))
  )

(use-package valign
  :hook (org-mode . valign-mode)
  )

(use-package olivetti
  :hook (org-mode . olivetti-mode)
  :init
  (setq
   olivetti-body-width 78)
  )

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-delay 1)
  (org-appear-autolinks t)
  )

(use-package org-download
  :after org
  :custom
  (org-download-screenshot-method "screencapture -i %s")
  )

(use-package xeft
  :defer 2
  :init
  (setq
   xeft-recursive 'follow-symlinks
   xeft-ignore-extension '("gpg" "asc" "bib")
   xeft-title-function 'file-name-nondirectory
   xeft-database (no-littering-expand-var-file-name "xeft")
   xeft-directory (expand-file-name "yx-slip-notes" yx/doc-dir))
  )

;; org-roam
(use-package org-roam
  :after org
  :init
  (setq org-roam-directory
        (expand-file-name "yx-slip-notes" yx/doc-dir)
        org-roam-database-connector 'sqlite-builtin
        )
  :config
  (org-roam-db-autosync-enable)
  (add-hook 'org-roam-capture-new-node-hook
            (lambda () (org-roam-tag-add '("draft"))))
  )
(use-package org-roam-ui
  :after org-roam
  :init
  (when (featurep 'xwidget-internal)
    (setq org-roam-ui-browser-function 'xwidget-webkit-browse-url)
    )
  )

;; citar
(use-package citar
  :after org
  :config
  (setq
   org-cite-insert-processor 'citar
   org-cite-follow-processor 'citar
   org-cite-activate-processor 'citar
   citar-bibliography org-cite-global-bibliography
   citar-notes-paths
   (list (expand-file-name "research" org-roam-directory)))
  :hook
  (org-mode . citar-capf-setup)
  )
(use-package citar-embark
  :after citar embark
  :demand t
  :custom
  (citar-at-point-function 'embark-act)
  :config (citar-embark-mode)
  )
(use-package citar-org-roam
  :after (citar org-roam)
  :demand t
  :config (citar-org-roam-mode)
  )


;; ========== end ==========
(provide 'init-note)
