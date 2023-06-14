(use-package elfeed
  :init
  (setq elfeed-feeds
        '(;; 工具、技术
          ("https://sachachua.com/blog/category/emacs/feed/" emacs)
          ("https://manateelazycat.github.io/feed.xml" emacs)
          ("http://www.ruanyifeng.com/blog/atom.xml" TECH)
          ;; 数据、R、Python、Julia等
          ;; 科研、数学、物理
          ("https://ruder.io/rss/index.rss" AI)
          ("https://www.inference.vc/rss" AI)
          ("https://lilianweng.github.io/index.xml" AI)
          )
        )
  )

(provide 'init-feed)
