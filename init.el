(defconst emacs-start-time (current-time))

;;; Sane defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn off garbage collection during startup. Turn back on when
;; startup is complete, but set new threshold to 100MB.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 100000000)))

;; Warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Disable modes
(blink-cursor-mode -1)
(menu-bar-mode -1)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(tooltip-mode -1)

;; Enable modes
(column-number-mode 1)
(delete-selection-mode 1)

;; Sanity
(setq custom-file (make-temp-file ""))
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)

(setq load-prefer-newer t)
(setq mouse-yank-at-point t)
(setq save-interprogram-paste-before-kill t)
(setq scroll-conservatively 1000)
(setq uniquify-buffer-name-style 'forward)

(fset 'display-startup-echo-area-message 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

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
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;;; Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(use-package anaconda-mode
  :defer
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (add-hook 'python-mode-hook
            (lambda ()
              (use-package company-anaconda
                :ensure
                :config
                (add-to-list 'company-backends 'company-anaconda)))))

(use-package autodisass-java-bytecode)

(use-package company
  :diminish company-mode
  :config
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete)

  (global-company-mode t))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (unless window-system (diff-hl-margin-mode))
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package diminish)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package gradle-mode
  :defer 1
  :config
  (use-package groovy-mode))

(use-package ivy
  :defer 0.1
  :diminish ivy-mode
  :config
  (setq ivy-do-completion-in-region nil)
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
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "M-y") 'counsel-yank-pop))

  (use-package swiper
    :config
    (global-set-key (kbd "C-c s") 'swiper)))

(use-package kotlin-mode :defer :config (setq kotlin-tab-width 4))

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

(use-package maude-mode :defer)

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-s-c C-s-c" . mc/edit-lines)))

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

(use-package yasnippet
  :defer 1
  :diminish yas-minor-mode
  :config
  (setq yas-verbosity 2)
  (yas-global-mode 1)
  (use-package yasnippet-snippets))

;;; Various ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(diminish 'eldoc-mode)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-c m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-l") 'recenter)

(setq show-paren-delay 0)
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(setq sentence-end-double-space nil)
(setq tab-always-indent 'complete)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

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

;;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; OS specifics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (setq trash-directory "~/.Trash/emacs")

  (if (executable-find "gls")
      (progn
        (setq dired-use-ls-dired t)
        (setq insert-directory-program (executable-find "gls")))
    (progn
      (setq ls-lisp-use-insert-directory-program nil)
      (require 'ls-lisp))))

;;; Private ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((private-file (concat user-emacs-directory "private.el")))
  (when (file-exists-p private-file)
    (load-file private-file)))

;;; Post initialization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
