;;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package elfeed
  ;; -*- coding: utf-8; lexical-binding: t; -*-
  :init
  (setq elfeed-feeds
        '(;; 工具、技术
          ("https://sachachua.com/blog/category/emacs/feed/" emacs)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("http://www.ruanyifeng.com/blog/atom.xml" tech)
          ;; 数据、R、Python、Julia等
          ("https://www.juliabloggers.com/feed/" julia)
          ("https://feeds.feedburner.com/RBloggers" r-lang)
          ;; 科研、数学、物理
          ("https://spaces.ac.cn/feed" AI)
          ("https://ruder.io/rss/index.rss" AI)
          ("https://www.inference.vc/rss" AI)
          ("https://lilianweng.github.io/index.xml" AI)
          )
        )
  )

(provide 'init-feed)