;;; init-org.el --- notes setting. -*- lexical-binding: t no-byte-compile: t -*-
;;; code:
(setq
 org-directory yx/org-root
 org-default-notes-file (expand-file-name "inbox.org" org-directory)
 )

(setq
 org-tag-alist '(("crypt" . ?c))
 org-todo-keywords
 '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
   (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq
 org-capture-templates
 '(("t" "任务" entry (file+headline org-default-notes-file "任务") "* TODO [#B] %^{Title}\n%?")
   ("h" "习惯" entry (file+headline org-default-notes-file "习惯")
    "* NEXT [#B] %^{Title}\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n%?")
   ))

(setq
 org-refile-targets
 '((nil :maxlevel . 2)
   (org-agenda-files :maxlevel . 2))
 org-refile-use-outline-path t
 org-refile-allow-creating-parent-nodes 'confirm
 )

(setq
 org-agenda-span 'day
 org-agenda-files '("inbox.org")
 org-agenda-compact-blocks t
 org-agenda-todo-list-sublevels t
 org-agenda-window-setup 'current-window)
(setq
 org-agenda-use-time-grid t
 org-agenda-time-grid '((daily today require-timed)
                        (300 600 900 1200 1500 1800 2100 2400)
                        "......"
                        "-----------------------------------------------------"))
(setq
 org-agenda-custom-commands
 '(("f" "要紧事"
    ((tags-todo "-STYLE=\"habit\"+PRIORITY=\"A\"|+urgent|+important")))
   ("d" "截止事"
    ((alltodo "" ((org-agenda-overriding-header "")
                  (org-super-agenda-groups
                   '((:deadline today)
                     (:deadline future)
                     (:deadline past)
                     (:discard (:anything t))))))))
   )
 )

(setq
 org-log-done 'time
 org-log-repeat 'time
 org-log-refile 'time
 org-log-redeadline 'note
 org-log-reschedule 'note
 org-log-into-drawer t)

(setq
 org-ellipsis " ▾"
 org-startup-folded 'content
 org-hide-emphasis-markers t
 org-use-sub-superscripts '{}
 org-fontify-quote-and-verse-blocks t
 org-footnote-auto-adjust t
 org-startup-with-inline-images t
 org-image-actual-width '(600)
 )

;; misc
(setq
 org-modules nil                        ; performance
 org-id-method 'uuid
 org-special-ctrl-k t
 org-special-ctrl-a/e t
 org-use-speed-commands t
 org-reverse-note-order t
 org-yank-adjusted-subtrees t
 org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
 )

;; crypt
(setq
 org-crypt-key yx/gpg-encrypt-key
 org-crypt-disable-auto-save t
 org-tags-exclude-from-inheritance '("crypt")
 )

;; @see https://www.n16f.net/blog/org-mode-headline-tips/
(setq
 org-goto-max-level 2
 org-goto-interface 'outline-path-completion
 org-outline-path-complete-in-steps nil
 org-insert-heading-respect-content t)

(setq
 org-html-doctype "html5"
 org-html-html5-fancy t
 org-html-validation-link nil
 org-export-with-smart-quotes t
 org-export-with-sub-superscripts '{})

(use-package org
  :ensure nil
  :config
  (org-crypt-use-before-save-magic)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t) (R . t) (org . t) (lua . t) (shell . t) (python . t) (emacs-lisp . t)))
  )

(use-package org-super-agenda
  :hook (org-mode . org-super-agenda-mode)
  :init
  (setq org-super-agenda-groups '((:auto-outline-path t)))
  )

(use-package org-journal
  :ensure t
  :defer 1
  :init
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir
        org-journal-file-format "%F.org"
        org-journal-file-type 'monthly
        org-journal-find-file 'find-file)
  )

(use-package valign
  :after org
  :hook (org-mode . valign-mode)
  )

(use-package olivetti
  :init
  (setq olivetti-style 'fancy)
  )

(provide 'init-org)
;;; init-notes.el ends here
