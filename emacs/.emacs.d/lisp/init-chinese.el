;;; init-chinese.el --- chinese relative settings    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  yangxue

;; Author: yangxue <yangxue.cs@foxmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(setq default-input-method "pyim") ; rime or pyim
(add-hook 'text-mode-hook 'toggle-input-method)

(use-package rime
  :disabled t
  :when ON-MAC
  :init
  (setq rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "C-v" "M-v" "<delete>")
        rime-librime-root "~/.emacs.d/librime/dist"
        rime-user-data-dir "~/Library/Rime"
        rime-cursor "˰"
        rime-commit1-forall t
        rime-show-candidate 'posframe
        rime-posframe-style 'simple
        rime-show-preedit 'inline
        rime-posframe-fixed-position t
        rime-inline-ascii-trigger 'shift-r
        rime-inline-ascii-holder ?x)
  (setq rime-posframe-properties
        '( :background-color "#333333"
           :foreground-color "#dcdccc"
           :internal-border-width 3)
        rime-disable-predicates
        '(rime-predicate-hydra-p
          rime-predicate-evil-mode-p
          rime-predicate-ace-window-p
          rime-predicate-prog-in-code-p
          rime-predicate-auto-english-p
          rime-predicate-org-in-src-block-p
          rime-predicate-current-uppercase-letter-p))
  :bind (
         :map rime-mode-map
         ("s-," . rime-force-enable)
         :map rime-active-mode-map
         ("s-." . rime-inline-ascii))
  )

(use-package pyim
  :init
  (setq default-input-method "pyim"
        pyim-page-length 5
        pyim-page-tooltip 'posframe
        pyim-default-scheme 'quanpin
        pyim-indicator-list '(pyim-indicator-with-modeline)
        )
  (setq-default pyim-punctuation-translate-p '(auto))
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-program-mode
                  pyim-probe-auto-english
                  pyim-probe-evil-normal-mode
                  pyim-probe-org-speed-commands))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  :config
  (use-package pyim-basedict
    :init
    (pyim-basedict-enable))
  :bind (([remap forward-word] . pyim-forward-word)
         ([remap backward-word] . pyim-backward-word))
  )

;; sdcv
(use-package sdcv
  :ensure nil
  :defer 5
  :load-path "site-lisp/sdcv"
  :init
  (setq sdcv-dictionary-simple-list (list "朗道英汉字典5.0")
        sdcv-dictionary-complete-list (list "朗道英汉字典5.0")
        sdcv-dictionary-data-dir "/Users/yx/.local/share/stardict/dic")
  )

(provide 'init-chinese)
;;; init-chinese.el ends here
