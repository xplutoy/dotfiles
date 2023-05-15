;;; -*- lexical-binding: t no-byte-compile: t -*-
(setq user-full-name     "yangxue")
(setq user-mail-address  "yangxue.cs@foxmail.com")

(setq rmail-preserve-inbox t
      rmail-primary-inbox-list nil)

(setq mail-yank-prefix   "> ")
(setq mail-user-agent    'gnus-user-agent)

(setq message-directory "~/.mail")
(setq message-send-mail-function 'message-send-mail-with-sendmail
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

(setq send-mail-function 'smtpmail-send-it
      smtpmail-queue-mail t
      smtpmail-smtp-user user-mail-address
      smtpmail-smtp-server "smtp.qq.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)

(setq mml-default-sign-method "pgpmime")
(setq mml-secure-openpgp-sign-with-sender t)
(setq mml-secure-openpgp-encrypt-to-self t)

;; gnus
(setq gnus-inhibit-startup-message t
      gnus-directory (expand-file-name ".gnus.d" user-emacs-directory)
      gnus-init-file (expand-file-name "gnus.el" user-emacs-directory)
      gnus-startup-file (expand-file-name "newsrc" user-emacs-directory))

(provide 'init-mail)
