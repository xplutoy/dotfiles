;;; init-utils.el --- const variable global variable and useful function.  -*- coding: utf-8; lexical-binding: t; -*-
;;; code:
(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defvar yx/org-root         "~/privacy")
(defvar yx/gpg-sign-key     "67B86CB8A5630C51!")
(defvar yx/gpg-encrypt-key  "8B1F9B207AF00BCF!")

(defun yx/eshell-toggle ()
  "eshell toggle"
  (interactive)
  (if (equal major-mode 'eshell-mode)
      (delete-window)
    (eshell)))

(defun yx/comment-or-uncomment ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region
       (region-beginning)(region-end))
    (comment-or-uncomment-region
     (line-beginning-position)(line-end-position))))

(provide 'init-utils)
 ;;; init-utils.el ends here
