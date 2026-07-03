;; Performance
(setq gc-cons-threshold (* 50 1000 1000))

;; avoid startup flash
(setq default-frame-alist
      '((background-color . "#050810")
        (foreground-color . "#68b8cc")
        (cursor-color . "#00e5ff")))
