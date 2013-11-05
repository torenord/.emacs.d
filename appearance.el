;; Don't waste pixels
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Clean
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

;; Misc
(setq visible-bell t)
(setq truncate-partial-width-windows nil)
(setq redisplay-dont-pause t)
(blink-cursor-mode 0)
(show-paren-mode 0)
(column-number-mode 1)

(provide 'appearance)
