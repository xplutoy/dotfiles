;;; -*- lexical-binding: t no-byte-compile: t -*-
;; (setq gnus-verbose 10)
(setq
 gnus-asynchronous t
 gnus-large-newsgroup nil
 gnus-save-newsrc-file nil
 gnus-read-newsrc-file nil
 gnus-save-killed-list nil
 gnus-read-active-file nil
 gnus-auto-select-first nil
 gnus-check-new-newsgroups nil
 gnus-inhibit-startup-message t
 gnus-always-read-dribble-file t)

(setq
 gnus-select-method '(nnnil "")
 gnus-secondary-select-methods
 '(
   (nntp "news.gwene.org")
   (nnml "qqmail")
   (nnimap "outlook"
           (nnimap-address "outlook.office365.com")
           (nnimap-server-port 993)
           (nnimap-inbox "INBOX")
           (nnimap-stream ssl)
           (nnimap-expunge 'never)
           (nnimap-split-methods default)
           (nnimap-search-engine imap))
   )
 )

(setq
 nntp-connection-timeout 5
 nntp-maximum-request 1)

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
 nnmail-treat-duplicates 'delete
 nnmail-split-fancy-match-partial-words t
 nnmail-cache-accepted-message-ids t
 nnmail-message-id-cache-length 5000
 nnmail-split-methods 'nnmail-split-fancy
 nnmail-use-long-file-names t)
(setq
 nnmail-split-fancy
 '(|
   (any "help-gnu-emacs@.*" "INBOX.help-gnu-emacs")
   (any "emacs-orgmode@.*" "INBOX.emacs-orgmode")
   (any "lua-l@.*" "INBOX.lua-l")
   (any "arch-general@.*" "INBOX.arch-general")
   (any "python-list@.*" "INBOX.python-list")
   (any ".*@haskell.org" "INBOX.haskell-list")
   (any ".*@redditmail.com" "INBOX.reddit")
   (: nnmail-split-fancy-with-parent)
   "INBOX.misc"
   ))

(setq
 gnus-user-date-format-alist '((t . "%m-%d %H:%M"))
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n"
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]"
 gnus-summary-display-arrow nil
 gnus-summary-gather-subject-limit 'fuzzy
 )

(setq
 gnus-fetch-old-headers t
 gnus-thread-hide-subtree t
 gnus-thread-ignore-subject t
 gnus-thread-sort-functions '(gnus-thread-sort-by-subject
                              gnus-thread-sort-by-most-recent-number))

(setq
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-single-indent ""
 gnus-sum-thread-tree-indent "    "
 gnus-sum-thread-tree-vertical "|   "
 gnus-sum-thread-tree-leaf-with-other "+---"
 gnus-sum-thread-tree-single-leaf "+---")

(setq
 gnus-suppress-duplicates t
 gnus-save-duplicate-list t
 gnus-duplicate-list-length 5000)

;; @see https://mail.gnu.org/archive/html/info-gnus-english/2009-01/msg00053.html
(setq nnrss-ignore-article-fields '(description guid pubData dc:creator link))

(setq
 mm-sign-option nil
 mm-verify-option 'always)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(gnus-demon-add-handler 'gnus-demon-scan-news 120 5)
