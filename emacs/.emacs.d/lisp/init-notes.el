;;; init-notes.el --- notes setting. -*- lexical-binding: t no-byte-compile: t -*-
;;; code:
(setq
 org-directory yx/org-root
 org-default-notes-file (expand-file-name "inbox.org" org-directory)
 )

(setq
 org-todo-keywords
 '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
   (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq
 org-tags-column 0
 org-tag-alist '(("crypt" . ?c) ("urgent" . ?w)("important" . ?i)))

(defvar yx/org-journal-file (expand-file-name "journal.org" org-directory))
(defvar yx/org-anniversary-file (expand-file-name "anniversary.org" org-directory))
(defvar yx/org-solid-note-dir (expand-file-name "solid_notes" org-directory))
(defvar yx/org-fleeting-note-dir (expand-file-name "fleeting_notes" org-directory))

(setq
 org-capture-templates
 '(("t" "任务" entry (file+headline org-default-notes-file "任务") "* TODO [#B] %^{Title}\n%?")
   ("p" "项目" entry (file+headline org-default-notes-file "项目") "* TODO [#B] %^{Title} :project:\n%?")
   ("h" "习惯" entry (file+headline org-default-notes-file "习惯")
    "* NEXT [#B] %^{Title}\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n%?")
   ("j" "日记" entry (file+datetree yx/org-journal-file) "* %?" :tree-type week)
   ("a" "纪念" plain (file yx/org-anniversary-file) "%% (org-anniversary %(format-time-string \"%Y %m %d\")\) %?")
   ))

(setq
 org-refile-targets
 '((nil :maxlevel . 2)
   (org-agenda-files :maxlevel . 2)))

(setq
 org-agenda-span 'day
 org-agenda-files '("inbox.org" "anniversary.org")
 org-agenda-sticky t
 org-agenda-include-diary t
 org-agenda-compact-blocks t
 org-agenda-inhibit-startup t
 org-agenda-include-deadlines t
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
 '(("z" "今日事"
    ((agenda "" ((org-deadline-warning-days 7)
                 (org-super-agenda-groups
                  '((:name "Today" :time-grid t :date today :scheduled today)))))))
   ("f" "要紧事"
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
 org-refile-use-outline-path t
 org-refile-allow-creating-parent-nodes 'confirm)

(setq
 org-ellipsis " ▾"
 ;; org-hidden-keywords t
 org-startup-indented nil
 org-adapt-indentation nil
 org-startup-folded 'content
 org-hide-emphasis-markers t
 org-use-sub-superscripts '{}
 org-fontify-quote-and-verse-blocks t
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
 org-deadline-warning-days 7
 org-yank-adjusted-subtrees t
 org-enforce-todo-dependencies t
 org-edit-src-content-indentation 0
 ;; org-enforce-todo-checkbox-dependencies t
 org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
 )

;; crypt
(setq
 org-crypt-key yx/gpg-encrypt-key
 org-crypt-disable-auto-save t
 org-tags-exclude-from-inheritance '("crypt" "project")
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
 org-export-with-section-numbers t
 org-export-htmlize-output-type 'inline-css ; default:inline-css
 org-export-with-smart-quotes t
 org-export-with-sub-superscripts '{})

(use-package org
  :ensure nil
  :config
  (org-crypt-use-before-save-magic)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (R . t)
     (org . t)
     (lua . t)
     (shell . t)
     (python . t)
     (emacs-lisp . t)))
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
  (setq org-journal-dir yx/org-fleeting-note-dir
        org-journal-file-format "%F.org"
        org-journal-file-type 'month
        org-journal-find-file 'find-file)
  (defun org-journal-find-location ()
    (org-journal-new-entry t)
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    (goto-char (point-max)))
  (add-to-list 'org-capture-templates
               '("f" "碎记" plain (function org-journal-find-location)
                 "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"))
  )

(use-package valign
  :after org
  :hook (org-mode . valign-mode)
  )

(use-package denote
  :defer 1
  :init
  (setq denote-directory yx/org-solid-note-dir
        denote-prompts '(title keywords)
        denote-known-keywords nil
        denote-infer-keywords t
        denote-backlinks-show-context t)
  :config
  (defun yx/denote-public ()
    "Create public notes in yx-notes dir."
    (interactive)
    (denote
     (denote--title-prompt)
     (denote--keywords-prompt)
     'org
     (expand-file-name "yx-notes" denote-directory))
    )
  (require 'denote-org-dblock)
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
  (pretty-hydra-define yx/hydra-denote
    (:color blue :title "hydra-denote" :quit-key "q")
    ("create"
     (("c" denote)
      ("t" denote-templates)
      ("n" yx/denote-public))
     "link"
     (("i" denote-link)
      ("I" denote-link-or-create)
      ("l" denote-link-after-creating)
      ("L" denote-link-add-missing-links))
     "rename"
     (("a" denote-add-front-matter)
      ("r" denote-rename-file-using-front-matter)
      ("R" denote-rename-file))
     "navigate"
     (("f" denote-link-find-file)
      ("b" denote-link-find-backlink))
     ))
  )

(use-package consult-notes
  :after denote
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :config
  (setq consult-notes-use-rg t
        consult-notes-org-headings-files `(,org-default-notes-file))
  (consult-notes-org-headings-mode 1)
  (consult-notes-denote-mode 1)
  )

(use-package olivetti
  :init
  (setq olivetti-style 'fancy)
  )

(provide 'init-notes)
;;; init-notes.el ends here
