;;; init-utils.el --- const variable global variable and useful function.  -*- coding: utf-8; lexical-binding: t; -*-
;;; code:
;; const
(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defvar yx/org-root "~/personal/org/")
(defvar yx/gpg-sign-key "67B86CB8A5630C51!")
(defvar yx/gpg-encrypt-key "8B1F9B207AF00BCF!")
(defvar yx/share-data-path "~/personal/local/share/")

;; funtions
(defun yx/eshell-toggle ()
  "eshell toggle"
  (interactive)
  (if (equal major-mode 'eshell-mode)
      (delete-window)
    (eshell)))

(defun yx/org-insert-screenshot (file-name)
  "Save screenshot to FILE-NAME and insert an Org link at point.

This calls the `import' from ImageMagick to take the screenshot,
and `optipng' to reduce the file size if the program is present."
  (interactive "FSave to file: ")
  (let ((file (expand-file-name file-name)))
    (make-directory (file-name-directory file) 'parents)
    (unless (= 0 (call-process "import" nil nil nil file))
      (user-error "`import' failed to create screenshot %s" file))
    (insert
     (format "[[file:%s]]"
             (file-relative-name file
                                 (file-name-directory buffer-file-name))))
    (when (eq major-mode 'org-mode)
      (org-redisplay-inline-images))))

(defmacro yx/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message " Execution time: %.06f" (float-time (time-since time)))))

(provide 'init-utils)
 ;;; init-utils.el ends here
