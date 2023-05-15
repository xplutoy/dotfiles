;;; init-note.el --- notes setting. -*- lexical-binding: t no-byte-compile: t -*-
;;; code:
(setq
 org-directory yx/org-root
 org-default-notes-file (expand-file-name "inbox.org" org-directory)
 org-capture-templates
 '(("t" "任务" entry (file+headline org-default-notes-file "任务") "* TODO [#B] %^{Title}\n%?")
   ("h" "习惯" entry (file+headline org-default-notes-file "习惯")
    "* NEXT [#B] %^{Title}\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n%?")
   ))

(setq
 org-agenda-span 'day
 org-agenda-files '("inbox.org")
 org-agenda-window-setup 'current-window)

(setq
 org-ellipsis ""
 org-tags-column 0
 org-startup-folded 'content
 org-pretty-entities t
 org-startup-indented t
 org-hide-emphasis-markers t
 ;; org-use-sub-superscripts '{}
 org-fontify-quote-and-verse-blocks t
 org-image-actual-width '(600)
 org-special-ctrl-k t
 org-special-ctrl-a/e t
 org-crypt-key yx/gpg-encrypt-key
 org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
 )

(org-crypt-use-before-save-magic)

(use-package org-journal
  :defer 2
  :init
  (setq org-journal-prefix-key "C-c j ")
  (setq org-journal-dir "~/privacy/fleeting_notes"
        org-journal-file-format "%F.org"
        org-journal-file-type 'monthly
        org-journal-find-file 'find-file)
  )

(use-package valign
  :hook (org-mode . valign-mode)
  )

(provide 'init-note)
;;; init-notes.el ends here
