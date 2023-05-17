;;; -*- lexical-binding: t no-byte-compile: t -*-

(setq
 gnus-select-method '(nnnil "")
 gnus-inhibit-startup-message t
 gnus-secondary-select-methods
 '(
   ;; (nntp "news.gwene.org")
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
   ; (any "lua-l@.*" "INBOX.lua-l")
   (: nnmail-split-fancy-with-parent)
   "INBOX.misc")
  )
