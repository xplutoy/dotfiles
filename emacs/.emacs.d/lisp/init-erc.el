;;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package erc
  :ensure nil
  :config
  (setq
   erc-nick "xplutoyz"
   erc-join-buffer 'bury
   erc-server-auto-reconnect t
   erc-kill-buffer-on-part t
   erc-kill-queries-on-quit t
   erc-kill-server-buffer-on-quit t
   erc-fill-prefix "        ↳ "
   erc-prompt-for-password nil
   erc-prompt-for-nickserv-password nil
   erc-track-position-in-mode-line t
   erc-autojoin-mode t
   erc-autojoin-channels-alist '(("#emacs" "#emacs.tw"))
   )
  (add-to-list 'erc-modules 'services)
  (erc-update-modules)
  :bind (:map erc-mode-map
              ("<return>" . nil)
              ("C-c C-r" . erc-server-reconnect)
              ("C-<return>" . erc-send-current-line))
  )

;; ==== end ====
(provide 'init-erc)
