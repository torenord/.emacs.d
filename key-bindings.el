(global-set-key (kbd "C-c C-m") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-x m") 'smex)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "C-æ") 'mc/mark-next-like-this)
(global-set-key (kbd "C-Æ") 'mc/mark-all-like-this)

(global-set-key (kbd "<F8>") 'switch-theme)
(global-set-key (kbd "C-.") 'hippie-expand)

(global-set-key (kbd "M-,") 'goto-init-el)
(global-set-key (kbd "C-z") 'shell)
(global-set-key (kbd "C-x C-z") 'shell)

(global-set-key (kbd "M-<RET>") 'toggle-fullscreen)
(global-set-key (kbd "C-c C-e") 'eval-and-replace)
(global-set-key (kbd "<C-M-up>") 'move-text-up)
(global-set-key (kbd "<C-M-down>") 'move-text-down)

(global-set-key (kbd "\C-x\g") 'magit-status)
(global-set-key (kbd "C-M-<return>") 'toggle-window-split)

(global-set-key (kbd "C-c C-d") 'torenord/insert-date)
(global-set-key (kbd "<f9>") 'torenord/compile)

(global-set-key (kbd "C-M-<backspace>") 'torenord/rotate-windows)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

(global-set-key (kbd "C-x C-j") 'kill-all-buffers)

(provide 'key-bindings)
