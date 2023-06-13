(use-package elfeed
  :init
  (setq elfeed-feeds
        '(("https://ruder.io/rss/index.rss" ai nlp)
          ("https://www.inference.vc/rss" ai)
          ("https://emacsredux.com/atom.xml" emacs)
          ("https://sachachua.com/blog/category/emacs/feed/" emacs)
          ("https://manateelazycat.github.io/feed.xml" emacs)
          ("http://www.ruanyifeng.com/blog/atom.xml" 技术 软件))
        )
  )

(provide 'init-feed)
