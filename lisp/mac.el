(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

(define-key key-translation-map (kbd "s-8") (kbd "["))
(define-key key-translation-map (kbd "s-(") (kbd "{"))
(define-key key-translation-map (kbd "s-9") (kbd "]"))
(define-key key-translation-map (kbd "s-)") (kbd "}"))
(define-key key-translation-map (kbd "s-7") (kbd "|"))
(define-key key-translation-map (kbd "s-/") (kbd "\\"))
(define-key key-translation-map (kbd "M-s-7") (kbd "M-|"))

(global-set-key (quote [M-f10]) (quote ns-toggle-fullscreen))

(global-unset-key (kbd "C-z"))

(add-to-list 'ido-ignore-files "\\.DS_Store")

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(provide 'mac)
