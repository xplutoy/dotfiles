;;; init-ui.el --- ui setttings. -*- lexical-binding: t no-byte-compile: t -*-
;;; code:
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun yx/setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family "Inconsolata" :height 140)
    (set-face-attribute 'fixed-pitch nil :family "Inconsolata" :height 1.0)
    (set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 1.0)
    (set-face-attribute 'fixed-pitch-serif nil :family "Latin Modern Mono" :height 1.0)
    (setq face-font-rescale-alist '(("LXGW WenKai Mono" . 1.05))) ;; 1.05 magic number
    (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family "LXGW WenKai Mono"))
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'symbol (font-spec :family font)))
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (set-fontset-font t 'emoji (font-spec :family font)))
    ))
(add-hook 'window-setup-hook #'yx/setup-fonts)
(add-hook 'server-after-make-frame-hook #'yx/setup-fonts)

;; theme
;; 1, monokai-theme
;; 2, ef-themes (:dark ef-cherie ef-winter ef-dark ef-autumn) (:light ef-duo-light)
(use-package ef-themes
  :init
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t
        ef-themes-disable-other-themes t
        ef-themes-region '(intense no-extend neutral)
        ef-themes-headings '((0 . (variable-pitch 1.5))
                             (1 . (variable-pitch 1.3))
                             (2 . (variable-pitch 1.1))
                             (agenda-date . (1.3))
                             (agenda-structure . (variable-pitch light 1.3))
                             (t . (variable-pitch))))
  ;; (mapc #'disable-theme custom-enabled-themes)
  (setq ef-themes-to-toggle '(ef-duo-light ef-winter))

  :config
  (ef-themes-select 'ef-duo-light)
  )

(provide 'init-ui)
;;; init-ui.el ends here
