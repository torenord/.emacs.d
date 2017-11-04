;; Turn off garbage collection during startup. Turn back on when
;; startup is complete, but set new threshold to 100MB.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 100000000)))

;; Disable modes
(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; Enable modes
(column-number-mode 1)
(delete-selection-mode 1)

;; Sane defaults
(fset 'display-startup-echo-area-message 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

(setq custom-file (make-temp-file ""))
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq load-prefer-newer t)
(setq mouse-yank-at-point t)
(setq ring-bell-function 'ignore)
(setq save-interprogram-paste-before-kill t)
(setq scroll-conservatively 1000)
(setq sentence-end-double-space nil)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default next-line-add-newlines nil)
(setq-default require-final-newline nil)

;; Enable disabled commands
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Key-bindings
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-c m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

;; Kill all buffers except for internal ones
(global-set-key (kbd "<f12>") 'desktop-clear)

;; Jump to next window
(global-set-key (kbd "M-'") 'next-multiframe-window)

;;; Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(use-package browse-kill-ring+)

(use-package company
  :diminish company-mode
  :config
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete)

  (global-company-mode t))

(use-package dired-details
  :config
  (if (executable-find "gls")
      (progn
        (setq dired-use-ls-dired t)
        (setq insert-directory-program (executable-find "gls")))
    (progn
      (setq ls-lisp-use-insert-directory-program nil)
      (require 'ls-lisp)))

  (setq-default dired-details-hidden-string "--- ")
  (dired-details-install))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("M-æ" . er/expand-region))

(use-package git-gutter-fringe
  :if window-system
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode 1))

(use-package ivy
  :config
  (ivy-mode 1)

  (use-package counsel
    :config
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-m") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate))

  (use-package swiper
    :config
    (global-set-key (kbd "C-c s") 'swiper)))

(use-package kotlin-mode
  :config
  (setq kotlin-tab-width 4))

(use-package ledger-mode :mode "\\.ledger\\'")

(use-package macrostep :bind ("C-c e m" . macrostep-expand))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows)))

(use-package markdown-mode :defer)

(use-package maude-mode
  :mode ("\\.fm\\'"
         "\\.rtmaude\\'"))

(use-package multi-term
  :config
  (require 'toggle-term))

(use-package multiple-cursors
  :bind (("M-ø" . mc/mark-next-like-this)
         ("M-Ø" . mc/mark-all-like-this)))

(use-package org
  :defer
  :config
  (let* ((package--builtins '())
         (missing (remove-if 'package-installed-p '(org))))
    (when missing
      (package-refresh-contents)
      (mapc 'package-install missing)))

  (use-package org-ref))

(use-package pdf-tools
  :if window-system
  :mode "\\.pdf\\'"
  :config
  (add-hook 'pdf-tools-enabled-hook
            (lambda ()
              (setq buffer-face-mode-face `(:background "#eeeeee"))
              (buffer-face-mode 1)))

  (pdf-tools-install))

(use-package server
  :defer 0.1
  :if window-system
  :config (unless (server-running-p) (server-start)))

(use-package shell
  :disabled
  :bind (("C-z" . toggle-shell)
         ("C-x C-z" . toggle-shell))
  :config
  (setq dirtrack-list '("λ \\([^ ]+\\) " 1))
  (add-hook 'shell-mode-hook 'dirtrack-mode)

  (add-to-list 'display-buffer-alist
               '("^\\*shell\\*$" . (display-buffer-same-window)))

  (defun torenord/shell-clear ()
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))

  (setq-default comint-prompt-read-only t)

  (add-hook 'shell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-l") 'torenord/shell-clear)))

  (add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines -1)))
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

  (defun comint-delchar-or-eof-or-kill-buffer (arg)
    (interactive "p")
    (if (null (get-buffer-process (current-buffer)))
        (kill-buffer)
      (comint-delchar-or-maybe-eof arg)))

  (add-hook 'shell-mode-hook
            (lambda ()
              (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

  (defun my-comint-init ()
    (setq comint-process-echoes t))
  (add-hook 'comint-mode-hook 'my-comint-init)

  (defun toggle-shell ()
    "Jump to shell or back."
    (interactive)
    (if (string= (buffer-name) "*shell*")
        (switch-to-prev-buffer)
      (progn
        (shell)
        (set-process-query-on-exit-flag (get-process "shell") nil)))))

(use-package try :defer)

(use-package undo-tree
  :defer 0.1
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package web-mode
  :mode ("\\.php\\'"
         "\\.html\\'"
         "\\.js\\'"
         "\\.css\\'"))

(use-package which-key
  :defer 0.1
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode 1))

;;; --- Various ---

(setq show-paren-delay 0)
(show-paren-mode 1)

(defun tidy ()
  "Ident, untabify and unwhitespacify current buffer, or region if active."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (indent-region beg end)
    (whitespace-cleanup)
    (untabify beg (if (< end (point-max)) end (point-max)))))
(global-set-key (kbd "<C-tab>") 'tidy)

;; From https://github.com/larstvei/dot-emacs
(defun duplicate-thing (comment)
  "Duplicates the current line, or the region if active. If an
argument is given, the duplicated region will be commented out."
  (interactive "P")
  (save-excursion
    (let ((start (if (region-active-p) (region-beginning) (point-at-bol)))
          (end   (if (region-active-p) (region-end) (point-at-eol))))
      (goto-char end)
      (unless (region-active-p)
        (newline))
      (insert (buffer-substring start end))
      (when comment (comment-region start end)))))
(global-set-key (kbd "C-c d") 'duplicate-thing)

(defadvice eval-last-sexp (around replace-sexp (arg) activate)
  "Replace sexp when called with a prefix argument."
  (if arg
      (let ((pos (point)))
        ad-do-it
        (goto-char pos)
        (backward-kill-sexp)
        (forward-sexp))
    ad-do-it))

(defadvice load-theme
    (before disable-before-load (theme &optional no-confirm no-enable) activate)
  (mapc 'disable-theme custom-enabled-themes))

;;; --- OS specifics ---

;; Mac OS X
(when (eq system-type 'darwin)
  (when window-system
    (menu-bar-mode 1)

    ;; Fix comma on Norwegian Apple USB Keyboard keypad
    (define-key function-key-map (kbd "<kp-decimal>") (kbd ","))

    (when (equal window-system 'ns)
      (setq ns-alternate-modifier 'none)
      (setq ns-command-modifier 'meta)
      (setq ns-function-modifier 'hyper)))

  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash/emacs"))

;;; --- Apperance ---

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;;; Private ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((private-file (concat user-emacs-directory "private.el")))
  (when (file-exists-p private-file)
    (load-file private-file)))
