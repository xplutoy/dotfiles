;;; -*- lexical-binding: t no-byte-compile: t -*-
(setq user-full-name     "yangxue")
(setq user-mail-address  "yangxue.cs@foxmail.com")

(setq
 gnus-use-dribble-file nil
 gnus-save-newsrc-file nil
 gnus-read-newsrc-file nil
 gnus-read-active-file nil
 gnus-inhibit-startup-message t)

(setq
 mail-envelope-from 'header
 mail-specify-envelope-from t
 mail-user-agent 'gnus-user-agent
 message-send-mail-function 'message-send-mail-with-sendmail)

(setq mml-default-sign-method "pgpmime")
(setq mml-secure-openpgp-sign-with-sender t)

(provide 'init-mail)
