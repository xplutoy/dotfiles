(use-package elfeed
  :init
  (setq elfeed-feeds
        '(
          ;; 工具、技术
          ("https://sachachua.com/blog/category/emacs/feed/" emacs)
          ("https://manateelazycat.github.io/feed.xml" emacs)
          ("http://www.ruanyifeng.com/blog/atom.xml" 技术 软件)
          
          ;; 数据、R、Python、Julia等
          
          ;; 科研、数学、物理
          ("https://ruder.io/rss/index.rss" ai nlp)
          ("https://www.inference.vc/rss" ai)
          ("https://lilianweng.github.io/index.xml" AI)
          )
        )
  )

(provide 'init-feed)
