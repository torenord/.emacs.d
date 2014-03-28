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
  (set-face-attribute 'default nil :height 150)
  (when (member "Inconsolata" (font-family-list))
    (set-face-attribute 'default nil :family "Inconsolata")))

(defun apply-molokai ()
  (require 'color-theme)
  (require 'color-theme-molokai)
  (color-theme-molokai)
  (custom-set-faces
   '(fringe ((t (:background "#000"))))
   '(show-paren-match ((t (:background "#888"))))
   '(mode-line ((t (:foreground "#fff" :background "#000" :box nil))))
   '(mode-line-inactive ((t (:foreground "#fff" :background "#000" :box nil))))))

(defun apply-github ()
  (require 'color-theme)
  (require 'color-theme-github)
  (color-theme-github)

  (custom-set-faces
   '(fringe ((t (:background "#fff"))))
   '(show-paren-match ((t (:background "#888"))))
   '(mode-line ((t (:foreground "#fff" :background "#000" :box nil))))
   '(mode-line-inactive ((t (:foreground "#fff" :background "#000" :box nil))))))

(setq is-molokai t)
(setq is-github nil)

(defun switch-theme ()
  (interactive)
  (setq is-molokai (not is-molokai))
  (setq is-github (not is-github))
  (cond (is-molokai (apply-github) (apply-github))
        (is-github (apply-molokai) (apply-molokai))))

(provide 'appearance)
