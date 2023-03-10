;;; init-utils.el --- const variable global variable and useful function.  -*- coding: utf-8; lexical-binding: t; -*-
;;; code:
(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defvar yx/org-root         "~/personal/org/")
(defvar yx/gpg-sign-key     "67B86CB8A5630C51!")
(defvar yx/gpg-encrypt-key  "8B1F9B207AF00BCF!")
(defvar yx/share-data-path  "~/personal/local/share/")

(defvar yx/dumped nil)
(defvar yx/dumped-load-path nil)

(defmacro yx/dumped-if (then &rest else)
  "Evaluate THEN if running with a dump file, else evaluate ELSE."
  (declare (indent 1))
  `(if yx/dumped
       ,then
     ,@else))

(defmacro yx/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message " Execution time: %.06f" (float-time (time-since time)))))

(defun yx/dump-emacs ()
  (interactive)
  (make-process
   :name "dump"
   :buffer "*dump process*"
   :command (list "emacs" "--batch" "-q" "-l"
                  (expand-file-name "dump.el" user-emacs-directory)))
  )

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
