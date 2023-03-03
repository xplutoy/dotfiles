;; -*- coding: utf-8; lexical-binding: t; -*-
;; no menu bar, toolbar, scroll bar
(dolist (var '(default-frame-alist initial-frame-alist))
  (add-to-list var '(left-fringe . 4))
  (add-to-list var '(right-fringe . 4))
  (add-to-list var '(tool-bar-lines . 0))
  (add-to-list var '(menu-bar-lines . 0))
  (add-to-list var '(vertical-scroll-bars . nil))
  (add-to-list var '(horizontal-scroll-bars . nil))
  (add-to-list var '(undecorated-round . t))
  (add-to-list var '(fullscreen . maximized))
  )

(setq inhibit-splash-screen          t
      inhibit-startup-message        t
      initial-major-mode             'fundamental-mode
      inhibit-default-init           t
      inhibit-compacting-font-caches t
      frame-resize-pixelwise         t
      frame-inhibit-implied-resize   t
      fast-but-imprecise-scrolling   t
      redisplay-skip-fontification-on-input t
      use-file-dialog                nil
      use-dialog-box                 nil
      load-prefer-newer              nil
      package-enable-at-startup      nil
      package--init-file-ensured     t
      read-process-output-max        (* 1024 1024)
      ffap-machine-p-known           'reject)

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

;; To suppress flashing at startup
(setq inhibit-message t
      inhibit-redisplay t)
(add-hook #'window-setup-hook
          #'(lambda ()
              (setq inhibit-message nil
                    inhibit-redisplay nil)
              (redisplay)))

(setq gc-cons-threshold (* 512 1024 1024)
      gc-cons-percentage 0.6)
;;; early-init.el ends here
