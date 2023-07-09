;;; -*- lexical-binding: t no-byte-compile: t -*-
(use-package tex
  :ensure auctex
  :mode (("\\.tex\\'" . LaTeX-mode))
  :config
  (setq-default
   Tex-master nil
   TeX-engine 'xetex)

  (setq
   TeX-auto-save t
   TeX-parse-self t
   TeX-view-program-selection
   '((output-pdf "pdf-tools"))
   TeX-view-program-list
   '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (setq reftex-plug-into-AUCTeX t)

  (mapc (lambda (mode)
          (add-hook 'LaTeX-mode-hook mode))
        (list
         'flyspell-mode
         'TeX-PDF-mode
         'turn-on-reftex
         'LaTeX-math-mode
         'TeX-source-correlate-mode))

  (add-hook 'TeX-after-comilation-finished-functions
            'TeX-revert-document-buffer)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(latex-mode "texlab"))))

(use-package cdlatex
  :config
  (add-hook 'org-mode-hook 'turn-on-cdlatex)
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  )

;; ==== end ====
(provide 'init-tex)
