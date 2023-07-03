;;; -*- lexical-binding: t no-byte-compile: t -*-
(setq user-full-name     "yangxue")
(setq user-mail-address  "yangxue.cs@foxmail.com")

(setq
 mail-envelope-from 'header
 mail-specify-envelope-from t
 mail-user-agent 'gnus-user-agent
 message-send-mail-function 'message-send-mail-with-sendmail)

(setq mml-default-sign-method "pgpmime")
(setq mml-secure-openpgp-sign-with-sender t)

(use-package gnus
  :commands (gnus)
  :ensure nil
  :config
  (setq
   mail-sources
   '((imap
       :server "imap.qq.com"
       :port 993
       :user "yangxue.cs@foxmail.com"
       :mailbox "INBOX"
       :fetchflag "\\Seen"
       :stream tls
       :dontexpunge t)
     ))
  (setq
   gnus-select-method '(nnnil "")
   gnus-secondary-select-methods
   '((nnml "qqmail")
     (nnimap "outlook"
             (nnimap-address "outlook.office365.com")
             (nnimap-stream ssl)
             (nnimap-server-port 993)
             (nnimap-expunge 'nerver)
             (nnimap-search-engine imap)
             (nnimap-split-methods default))
     )
   )
  (setq
   nnmail-split-methods 'nnmail-split-fancy
   nnmail-split-fancy
   '(|
     (: nnmail-split-fancy-with-parent)
     "INBOX.misc")
   )

  (setq
   gnus-asynchronous t
   gnus-use-header-prefetch t
   gnus-use-cache t
   gnus-use-scoring nil
   gnus-suppress-duplicates t
   gnus-novice-user nil
   gnus-expert-user t
   gnus-interactive-exit 'quiet
   gnus-inhibit-startup-message t)
  (setq
   gnus-save-newsrc-file nil
   gnus-read-newsrc-file nil
   gnus-save-killed-list nil
   gnus-search-use-parsed-queries t
   gnus-article-browse-delete-temp t
   gnus-check-new-newsgroups 'ask-server
   gnus-mime-display-multipart-related-as-mixed t
   gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies)
  (setq
   gnus-cache-remove-articles '(read)
   gnus-cacheable-groups "^\\(nntp\\|nnimap\\)"
   gnus-cache-enter-articles '(ticked dormant unread))
  (setq nnrss-ignore-article-fields '(description guid pubData dc:creator link))
  )

(use-package gnus-group
  :ensure nil
  :hook (gnus-select-group . gnus-group-set-timestamp)
  :config
  (setq
   gnus-sum-thread-tree-root            "┌ "
   gnus-sum-thread-tree-false-root      "◌ "
   gnus-sum-thread-tree-single-indent   "◎ "
   gnus-sum-thread-tree-vertical        "│"
   gnus-sum-thread-tree-indent          "  "
   gnus-sum-thread-tree-leaf-with-other "├─►"
   gnus-sum-thread-tree-single-leaf     "╰─►"
   gnus-summary-line-format "%U%R %3d %[%-23,23f%] %B %s\n")
  (setq
   gnus-summary-make-false-root 'adopt
   gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
   gnus-simplify-subject-functions '(gnus-simplify-subject-re gnus-simplify-whitespace))
  (setq
   gnus-fetch-old-headers 2
   gnus-build-sparse-threads 'some
   gnus-fetch-old-ephemeral-headers 2)
  (setq
   gnus-show-threads t
   gnus-thread-indent-level 2
   gnus-thread-hide-subtree nil
   gnus-thread-sort-functions
   '(gnus-thread-sort-by-subject
     gnus-thread-sort-by-most-recent-number))
  (setq
   gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
   gnus-subthread-sort-functions '(gnus-thread-sort-by-date))
  (setq
   gnus-view-pseudos 'automatic
   gnus-view-pseudos-separately t
   gnus-view-pseudo-asynchronously t)
  (setq
   gnus-auto-select-first nil
   gnus-auto-select-next nil
   gnus-paging-select-next nil)
  )

(use-package gnus-group
  :ensure nil
  :hook (gnus-group-mode . gnus-topic-mode)
  :config
  (require 'gnus-topic)
  ;;          indentation ------------.
  ;;  #      process mark ----------. |
  ;;                level --------. | |
  ;;           subscribed ------. | | |
  ;;  %          new mail ----. | | | |
  ;;  *   marked articles --. | | | | |
  ;;                        | | | | | |  Ticked    New     Unread  open-status Group
  (setq gnus-group-line-format "%M%m%S%L%p%P %1(%7i%) %3(%7U%) %3(%7y%) %4(%B%-45G%) %d\n")
  (setq gnus-group-sort-function '(gnus-group-sort-by-level gnus-group-sort-by-alphabet)))

;; ==== end =========
(provide 'init-mail)
