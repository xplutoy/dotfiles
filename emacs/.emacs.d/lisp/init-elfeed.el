;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package elfeed
  :init
  (setq elfeed-db-directory (expand-file-name ".elfeed" user-emacs-directory))
  (setq-default elfeed-search-filter "@1-months-ago +unread")
  (setq elfeed-feeds
        '(
          ("https://ruder.io/rss/index.rss" ai nlp)
          ("https://www.inference.vc/rss" ai)
          ("https://emacsredux.com/atom.xml" emacs)
          ("https://sachachua.com/blog/category/emacs/feed/" emacs)
          ("https://manateelazycat.github.io/feed.xml" emacs)
          ("https://linuxhandbook.com/rss/" linux)
          ("http://www.ruanyifeng.com/blog/atom.xml" 技术 软件))
        )
  )

(provide 'init-elfeed)
