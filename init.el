;;; --- Setup ---

;; Disable modes
(dolist (mode
         '(menu-bar-mode
           blink-cursor-mode
           scroll-bar-mode
           tool-bar-mode
           tooltip-mode))
  (if (fboundp mode) (funcall mode -1)))

;; Enable modes
(dolist (mode
         '(delete-selection-mode
           column-number-mode
           winner-mode))
  (if (fboundp mode) (funcall mode 1)))

;; Sane defaults
(eval-after-load "startup" '(fset 'display-startup-echo-area-message 'ignore))

(setq custom-file (make-temp-file ""))
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq load-prefer-newer t)
(setq ring-bell-function 'ignore)
(setq save-interprogram-paste-before-kill t)
(setq scroll-conservatively 1000)
(setq truncate-partial-width-windows nil)

(setq-default indent-tabs-mode nil)
(setq-default next-line-add-newlines nil)
(setq-default require-final-newline nil)

(fset 'yes-or-no-p 'y-or-n-p)

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

;; Locale
(set-locale-environment "no_NO.UTF-8")

;;; --- Packages ---

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package calendar
  :config
  ;; http://www.emacswiki.org/emacs/calendarweeknumbers
  (copy-face font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil :height 0.7)
  (setq calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'calendar-iso-week-face))
  (setq calendar-week-start-day 1))

(use-package company
  :diminish company-mode
  :config
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete)

  (global-company-mode t))

(use-package company-jedi
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook))

(use-package dired
  :ensure nil
  :config
  (add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "å") 'dired-up-directory))))

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

(use-package dired-narrow
  :bind ("C-c f" . dired-narrow))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("M-æ" . er/expand-region))

(use-package focus)

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :if (window-system)
  :preface
  (global-git-gutter-mode 1)
  (dolist (p '((git-gutter:added    . "#0c0")
               (git-gutter:deleted  . "#c00")
               (git-gutter:modified . "#c0c")))
    (set-face-foreground (car p) (cdr p))
    (set-face-background (car p) (cdr p))))

(use-package haskell-mode)

(use-package swiper
  :disabled
  :config
  (ivy-mode)
  (setq ivy-wrap t)
  (setq ivy-height 20)
  (global-set-key (kbd "C-x C-m") 'execute-extended-command))

(use-package helm
  :diminish helm-mode
  :demand
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("C-c h g" . helm-google-suggest)
         ("C-c h o" . helm-occur)
         ("C-c h" . helm-command-prefix)
         ("M-i" . helm-swoop))
  :config
  (require 'helm-config)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)

  (setq helm-ff-skip-boring-files t)
  (setq helm-mode-fuzzy-match t)
  (setq helm-move-to-line-cycle-in-source t)

  (helm-mode 1)
  (helm-adaptive-mode 1)
  (helm-push-mark-mode 1))

(use-package helm-swoop)

(use-package ledger-mode
  :mode "\\.ledger\\'")

(use-package lua-mode)

(use-package macrostep
  :bind ("C-c x" . macrostep-expand))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-c m" . magit-status))
  :config
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows)))

(use-package markdown-mode)

(use-package maude-mode
  :mode "\\.fm\\'")

(use-package move-text)
(use-package multi-term)

(use-package multiple-cursors
  :bind (("M-ø" . mc/mark-next-like-this)
         ("M-Ø" . mc/mark-all-like-this)))

(use-package org-mode
  :ensure nil
  :init
  (setq org-agenda-default-appointment-duration 120
        org-agenda-skip-deadline-if-done nil
        org-agenda-skip-scheduled-if-done nil
        org-agenda-start-on-weekday nil
        org-deadline-warning-days 10
        org-default-notes-file "~/org/notes.org"
        org-habit-show-all-today nil
        org-habit-show-done-always-green t
        org-habit-show-habits-only-for-today t
        org-startup-indented t)
  :bind ("C-c c" . org-capture))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-pomodoro)

(use-package paredit
  :diminish "()"
  :config
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))

(use-package pdf-tools
  :if (window-system)
  :mode "\\.pdf\\'"
  :config
  (add-hook 'pdf-tools-enabled-hook 'auto-revert-mode)
  (pdf-tools-install))

(use-package php-mode)

(use-package server
  :if window-system
  :config
  (unless (server-running-p)
    (server-start)))

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

(use-package try)

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package web-mode
  :mode ("\\.php\\'"
         "\\.html\\'"
         "\\.js\\'"
         "\\.css\\'"))

(use-package yasnippet
  :config
  (yas-global-mode))

;;; --- Various ---

(defun tidy ()
  "Ident, untabify and unwhitespacify current buffer, or region if active."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (indent-region beg end)
    (whitespace-cleanup)
    (untabify beg (if (< end (point-max)) end (point-max)))))

(defun goto-init-el ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

;; From org-trim in org.el.
(defun torenord/trim (s)
  "Remove whitespace at beginning and end of string."
  (if (string-match "\\`[ \t\n\r]+" s) (setq s (replace-match "" t t s)))
  (if (string-match "[ \t\n\r]+\\'" s) (setq s (replace-match "" t t s)))
  s)

(defun torenord/insert-date ()
  "Insert current date at point."
  (interactive)
  (insert (torenord/trim (format-time-string "%e. %B %Y"))))

;; from https://github.com/larstvei/dot-emacs
(defun jump-to-symbol-internal (&optional backwardp)
  "Jumps to the next symbol near the point if such a symbol
exists. If BACKWARDP is non-nil it jumps backward."
  (let* ((point (point))
         (bounds (find-tag-default-bounds))
         (beg (car bounds)) (end (cdr bounds))
         (str (isearch-symbol-regexp (find-tag-default)))
         (search (if backwardp 'search-backward-regexp
                   'search-forward-regexp)))
    (goto-char (if backwardp beg end))
    (funcall search str nil t)
    (cond ((<= beg (point) end) (goto-char point))
          (backwardp (forward-char (- point beg)))
          (t  (backward-char (- end point))))))

;; from https://github.com/larstvei/dot-emacs
(defun jump-to-previous-like-this ()
  "Jumps to the previous occurrence of the symbol at point."
  (interactive)
  (jump-to-symbol-internal t))

;; from https://github.com/larstvei/dot-emacs
(defun jump-to-next-like-this ()
  "Jumps to the next occurrence of the symbol at point."
  (interactive)
  (jump-to-symbol-internal))

;; from https://github.com/larstvei/dot-emacs
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

(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR. If
  you are deleting forward, the CHAR is replaced and the point is
  put before CHAR"
  (insert char)
  (if (< 0 arg) (forward-char -1)))

(defadvice eval-last-sexp (around replace-sexp (arg) activate)
  "Replace sexp when called with a prefix argument."
  (if arg
      (let ((pos (point)))
        ad-do-it
        (goto-char pos)
        (backward-kill-sexp)
        (forward-sexp))
    ad-do-it))

;;; --- OS specifics ---

;; GNU/Linux
(when (equal system-type 'gnu/linux)
  (set-face-attribute 'default nil
                      :height 115
                      :family "Liberation Mono"))

;; Mac OS X
(when (equal system-type 'darwin)
  (setq ns-alternate-modifier 'none)
  (setq ns-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)

  ;; Fix comma on Apple USB Keyboard
  (define-key function-key-map (kbd "<kp-decimal>") (kbd ","))

  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash/emacs")

  (when window-system
    (menu-bar-mode 1)

    (when (fboundp 'font-family-list)
      (set-face-attribute 'default nil :height 150)
      (when (member "Source Code Pro" (font-family-list))
        (set-face-attribute 'default nil :family "Source Code Pro")))))

;; Windows
(when (equal system-type 'windows-nt)
  (set-face-attribute 'default nil :height 115)
  (set-face-attribute 'default nil :family "Lucida Console"))

;;; --- Apperance ---

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))

  (defadvice load-theme
      (before disable-before-load (theme &optional no-confirm no-enable) activate)
    (mapc 'disable-theme custom-enabled-themes))

  (defun setup-adwaita ()
    (load-theme 'adwaita t)
    (set-cursor-color "black")
    (set-background-color "#f3e7da")
    (set-face-attribute 'show-paren-match nil :background "#b3d283")
    (set-face-attribute 'mode-line nil :box nil))

  (setup-adwaita))

;;; --- Keybindings ---

(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

;; Jump by paragraph
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Goto init.el
(define-key custom-bindings-map (kbd "M-,") 'goto-init-el)

;; Go fullscreen
(define-key custom-bindings-map (kbd "M-<f11>") 'toggle-frame-fullscreen)

(define-key custom-bindings-map (kbd "C-c e") 'mc/edit-lines)

(define-key custom-bindings-map (kbd "C-c d") 'duplicate-thing)
(define-key custom-bindings-map (kbd "C-c q") 'torenord/insert-date)

(define-key custom-bindings-map (kbd "<M-S-up>") 'move-text-up)
(define-key custom-bindings-map (kbd "<M-S-down>") 'move-text-down)

(define-key custom-bindings-map (kbd "C-,") 'jump-to-previous-like-this)
(define-key custom-bindings-map (kbd "C-.") 'jump-to-next-like-this)

(define-key custom-bindings-map (kbd "<C-tab>") 'tidy)
(define-key custom-bindings-map (kbd "C-x C-j") 'desktop-clear)
(define-key custom-bindings-map (kbd "C-x k") 'kill-this-buffer)

(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings."
  t nil custom-bindings-map)

;;; --- Private ---

;; Load private.el if it exists (from https://github.com/larstvei/dot-emacs)
(add-hook
 'after-init-hook
 (lambda ()
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file)))))
