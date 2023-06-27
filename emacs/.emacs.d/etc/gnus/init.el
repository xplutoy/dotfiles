;;; -*- lexical-binding: t no-byte-compile: t -*-
(setq
 gnus-select-method '(nnnil "")
 gnus-inhibit-startup-message t
 gnus-secondary-select-methods
 '((nnml "qqmail")
   (nnimap "outlook"
           (nnimap-address "outlook.office365.com")
           (nnimap-stream ssl)
           (nnimap-server-port 993)
           (nnimap-search-engine imap)
           (nnimap-split-methods default))
   )
 )

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
 nnmail-split-methods 'nnmail-split-fancy
 nnmail-split-fancy
 '(|
   (: nnmail-split-fancy-with-parent)
   "INBOX.misc")
 )

(setq
 gnus-thread-hide-subtree t
 gnus-thread-ignore-subject t
 gnus-thread-sort-functions
 '(gnus-thread-sort-by-subject
   gnus-thread-sort-by-most-recent-number))

(setq
 gnus-user-date-format-alist '((t . "%m-%d %H:%M"))
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n"
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]"
 gnus-summary-display-arrow nil
 gnus-summary-gather-subject-limit 'fuzzy
 )

(setq
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-single-indent ""
 gnus-sum-thread-tree-indent "    "
 gnus-sum-thread-tree-vertical "|   "
 gnus-sum-thread-tree-leaf-with-other "+---"
 gnus-sum-thread-tree-single-leaf "+---")

;; @see https://mail.gnu.org/archive/html/info-gnus-english/2009-01/msg00053.html
(setq nnrss-ignore-article-fields '(description guid pubData dc:creator link))
