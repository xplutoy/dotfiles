;;; -*- lexical-binding: t no-byte-compile: t -*-
(setq user-full-name     "yangxue")
(setq user-mail-address  "yangxue.cs@foxmail.com")

(setq gnus-inhibit-startup-message t
      gnus-directory "~/.cache/gnus.d"
      gnus-init-file "~/.emacs.d/gnus.el")

(setq message-directory "~/.mail"
      mail-envelope-from 'header
      mail-user-agent    'gnus-user-agent
      message-send-mail-function 'message-send-mail-with-sendmail
      mail-specify-envelope-from t)

(setq send-mail-function 'smtpmail-send-it
      smtpmail-queue-mail t
      smtpmail-smtp-user user-mail-address
      smtpmail-smtp-server "smtp.qq.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)

(setq mml-default-sign-method "pgpmime")
(setq mml-secure-openpgp-sign-with-sender t)

(provide 'init-mail)
