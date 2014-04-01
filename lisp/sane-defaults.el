(fset 'yes-or-no-p 'y-or-n-p)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(delete-selection-mode t)
(transient-mark-mode t)
(show-paren-mode t)

(setq save-interprogram-paste-before-kill t)
(setq scroll-conservatively 1)
(setq-default indent-tabs-mode nil)
(setq-default next-line-add-newlines nil)
(setq-default require-final-newline nil)

(provide 'sane-defaults)
