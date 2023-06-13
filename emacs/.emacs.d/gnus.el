;;; -*- lexical-binding: t no-byte-compile: t -*-
(setq
 gnus-select-method '(nnnil "")
 gnus-inhibit-startup-message t
 gnus-secondary-select-methods
 '((nnml "qqmail")
   (nnimap "gmail"
           (nnimap-address "imap.gmail.com")
           (nnimap-stream ssl)
           (nnimap-server-port 993)
           (nnimap-search-engine imap)
           (nnimap-split-methods default))
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
   ; (any "lua-l@.*" "INBOX.lua-l")
   (: nnmail-split-fancy-with-parent)
   "INBOX.misc")
  )
