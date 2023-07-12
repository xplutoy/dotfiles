;;; -*- coding: utf-8; lexical-binding: t; -*-
(setq
 elfeed-feeds
 '(("https://sachachua.com/blog/category/emacs/feed/" emacs)
   ("https://planet.emacslife.com/atom.xml" emacs)
   ("https://blog.aaronbieber.com/posts/index.xml" emacs)
   (" https://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-and-content.xml" emacs)
   ("http://www.ruanyifeng.com/blog/atom.xml" tech)
   ("https://www.juliabloggers.com/feed/" julia)
   ("https://feeds.feedburner.com/RBloggers" r-lang)
   ("https://rustcc.cn/rss" rust)
   ("https://planetpython.org/rss20.xml" python)
   ("https://python-bloggers.com/feed/" python)
   ("https://spaces.ac.cn/feed" AI webkit)
   ("https://ruder.io/rss/index.rss" AI)
   ("https://www.inference.vc/rss" AI)
   ("https://safjan.com/feeds/all.rss.xml" AI)
   ("https://lilianweng.github.io/index.xml" AI webkit)
   ("https://www.bmpi.dev/index.xml" misc)
   ("https://www.xianmin.org/index.xml" misc)
   )
 )

(defun elfeed-eww-browse ()
  "Wrapper to open eww and mark elfeed as read"
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (eww-browse-url link))))

(use-package elfeed
  :config
  (bind-keys
   :map elfeed-show-mode-map
   ("B" . elfeed-eww-browse)
   ("q" . (lambda ()
            "Switch to *elfeed-search* buffer."
            (interactive)
            (switch-to-buffer "*elfeed-search*"))))
  )

(use-package elfeed-webkit
  :after elfeed
  :demand t
  :config
  (setq elfeed-webkit-auto-enable-tags '(webkit))
  (elfeed-webkit-auto-toggle-by-tag)
  :bind (:map elfeed-show-mode-map
              ("W" . elfeed-webkit-toggle))
  )

(provide 'init-elfeed)
