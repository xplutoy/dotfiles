;;; -*- lexical-binding: t no-byte-compile: t -*-
(setq
 org-directory yx/org-root
 org-crypt-key yx/gpg-encrypt-key
 org-default-notes-file (expand-file-name "inbox.org" org-directory))

(setq
 org-ellipsis "..."
 org-tags-column 0
 org-num-max-level 2
 org-log-into-drawer t
 org-return-follows-link t
 org-hide-emphasis-markers t
 org-use-sub-superscripts '{}
 org-image-actual-width '(600)
 org-special-ctrl-k t
 org-special-ctrl-a/e t
 org-src-fontify-natively t
 org-src-tab-acts-natively t
 org-fontify-quote-and-verse-blocks t
 org-agenda-window-setup 'current-window
 org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
 )

(setq
 org-modules '(org-habit org-tempo)
 org-habit-graph-column 60)




(setq
 org-capture-templates
 '(("t" "task" entry (file+headline org-default-notes-file "Tasks") "* TODO [#B] %^{Title}\n%?")
   ("h" "habit" entry (file+headline org-default-notes-file "Habits") "* NEXT [#B] %^{Title}\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n%?"))
 )

(use-package org
  :ensure nil
  :defer 2
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

(use-package org-journal
  :after org
  :init
  (setq
   org-journal-time-format ""
   org-journal-file-format "%Y_%m_%d.org"
   org-journal-date-format "#+TITLE: Journal Entry- %e %b %Y (%A)"
   org-journal-encrypt-journal t
   org-journal-find-file 'find-file
   org-journal-dir "~/privacy/journal/"
   org-journal-enable-agenda-integration t
   )
  )

(use-package valign
  :hook (org-mode . valign-mode)
  )

(provide 'init-note)
