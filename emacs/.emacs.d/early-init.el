;;; -*- coding: utf-8; lexical-binding: t; -*-
(setq
 tool-bar-mode nil
 menu-bar-mode nil
 scroll-bar-mode nil
 default-frame-alist
 '((tool-bar-lines . 0)
   (menu-bar-lines . 0)
   (alpha-background . 75)
   (undecorated-round . t)
   (vertical-scroll-bars . nil)))

(setq
 use-dialog-box nil
 use-file-dialog nil
 inhibit-default-init t
 inhibit-splash-screen t
 frame-resize-pixelwise t
 inhibit-startup-message t
 package-enable-at-startup nil
 frame-inhibit-implied-resize t
 inhibit-compacting-font-caches t)

(let ((old-file-name-handler-alist file-name-handler-alist))
  (setq-default file-name-handler-alist nil)
  (defun yx/restore-file-name-handler-alist ()
    (setq file-name-handler-alist
          (delete-dups
           (append file-name-handler-alist
                   old-file-name-handler-alist))
          inhibit-trace nil))
  (add-hook #'emacs-startup-hook
            #'yx/restore-file-name-handler-alist))

(setq gc-cons-threshold (* 512 1024 1024)
      gc-cons-percentage 0.6)
;;; early-init.el ends here
