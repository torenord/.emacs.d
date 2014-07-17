(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

(setq visible-bell t)
(setq truncate-partial-width-windows nil)
(setq redisplay-dont-pause t)
(setq line-number-mode t)
(setq column-number-mode t)

(when (and on-mac (fboundp 'font-family-list))
  (set-face-attribute 'default nil :height 180)
  (when (member "Inconsolata" (font-family-list))
    (set-face-attribute 'default nil :family "Inconsolata")))

(set-face-attribute 'show-paren-match nil :background "#888888")
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(provide 'appearance)
