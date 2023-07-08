;;; -*- coding: utf-8; lexical-binding: t; -*-
(setq
 tool-bar-mode nil
 menu-bar-mode nil
 use-dialog-box nil
 scroll-bar-mode nil
 use-file-dialog nil
 inhibit-splash-screen t
 frame-resize-pixelwise t
 inhibit-startup-message t
 )

(setq
 inhibit-default-init t
 package-enable-at-startup nil
 frame-inhibit-implied-resize t
 read-process-output-max (* 1024 1024)
 inhibit-compacting-font-caches t
 native-comp-async-report-warnings-errors nil
 )

(setq
 default-frame-alist
 '((width . 90)
   (height . 36)
   (tool-bar-lines . 0)
   (menu-bar-lines . 0)
   (alpha-background . 75)
   (undecorated-round . t)
   (vertical-scroll-bars . nil)))

(setq initial-frame-alist '((alpha . 0.97)))

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

(setq
 gc-cons-percentage 0.6
 gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda()
            (setq
             gc-cons-percentage 0.1
             gc-cons-threshold (* 16 1024 1024))))


(defun display-startup-echo-area-message ()
  (message nil))
;;; early-init.el ends here
