(defun torenord/shell-clear ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(setq-default comint-prompt-read-only t)

(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-l") 'torenord/shell-clear)))

(add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines t)))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(provide 'setup-shell)
