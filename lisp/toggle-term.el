;;; toggle-term.el -- Toggle and switch between terminal buffers -*- lexical-binding: t -*-

;;; Commentary:

;; TODO: For M-<left>, if in *terminal<N>*, goto *terminal<N+1>*. For
;; M-<right>, goto *terminal<N-1>*.

;;; Code:

(require 'multi-term)

(defvar toggle-term/last-term 1)

(defun toggle-term/multi-term-switch (N)
  "Switch to *terminal<N>*."
  (setq toggle-term/last-term N)
  (let ((buffer-name (format "*terminal<%d>*" N)))
    (cond ((get-buffer buffer-name)
           (switch-to-buffer buffer-name))
          (t
           (set-process-query-on-exit-flag (get-buffer-process (multi-term)) nil)
           (rename-buffer buffer-name)))))

(defun toggle-term/goto-non-terminal-buffer ()
  "Switch to last non-terminal buffer."
  (interactive)
  (let* ((r "^\\*terminal<[1-9][0-9]*>\\*$")
         (terminal-buffer-p (lambda (b) (string-match-p r (buffer-name b))))
         (non-terminals (cl-remove-if terminal-buffer-p (buffer-list))))
    (when non-terminals
      (switch-to-buffer (car non-terminals)))))

(defun toggle-term/multi-term-toggle ()
  "Toggle between terminal and non-terminal buffers."
  (interactive)
  (if (string-match-p "^\\*terminal<[1-9][0-9]*>\\*$" (buffer-name))
      (toggle-term/goto-non-terminal-buffer)
    (toggle-term/multi-term-switch toggle-term/last-term)))

(dolist (n (number-sequence 1 9))
  (global-set-key (kbd (concat "M-" (int-to-string n)))
                  (lambda () (interactive) (toggle-term/multi-term-switch n))))

(global-set-key (kbd "C-z") 'toggle-term/multi-term-toggle)
(global-set-key (kbd "M-<left>") 'multi-term-prev)
(global-set-key (kbd "M-<right>") 'multi-term-next)

(provide 'toggle-term)

;;; toggle-term.el ends here
