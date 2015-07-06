(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq user-full-name "Tore Norderud"
      user-mail-address "torenord@uio.no")

(setq on-linux (equal system-type 'gnu/linux))
(setq on-mac (equal system-type 'darwin))

;; ### Modes ###

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
           column-number-mode))
  (if (fboundp mode) (funcall mode 1)))

;; ### Sanity ###

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq scroll-conservatively 1)
(setq visible-bell t)
(setq save-interprogram-paste-before-kill t)
(setq truncate-partial-width-windows nil)
(setq load-prefer-newer t)
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

(setq-default indent-tabs-mode nil)
(setq-default next-line-add-newlines nil)
(setq-default require-final-newline nil)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'erase-buffer 'disabled nil)

(unless (boundp 'user-emacs-directory)
  (setq user-emacs-directory "~/.emacs.d/"))

;; from better-defaults.el
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; --- utf-8 ---

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; --- locale ---

(set-locale-environment "no_NO.UTF-8")

;; ### Packages ###

(when (require 'package nil 'noerror)
  (setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("marmalade" . "https://marmalade-repo.org/packages/")
          ("melpa" . "http://melpa.milkbox.net/packages/")
          ("org" . "http://orgmode.org/elpa/")))

  (package-initialize)

  (when (not package-archive-contents)
    (package-refresh-contents))

  (dolist (p
           '(ace-jump-mode
             clojure-mode
             company
             dired-details
             evil
             exec-path-from-shell
             expand-region
             flyspell
             geiser
             git-gutter-fringe
             gitconfig-mode
             gitignore-mode
             haskell-mode
             ido-vertical-mode
             jedi
             js2-mode
             leuven-theme
             macrostep
             magit
             markdown-mode
             maude-mode
             molokai-theme
             move-text
             multi-term
             multiple-cursors
             nginx-mode
             olivetti
             org
             org-bullets
             paredit
             pdf-tools
             php-mode
             smex
             try
             undo-tree
             use-package
             web-mode))
    (when (not (package-installed-p p))
      (package-install p)
      (delete-other-windows))))

(or (require 'use-package nil 'noerror)
    (defmacro use-package (&rest args)
      `nil))

(use-package ido
  :init
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil)

  :config
  (ido-mode 1)

  (when on-mac
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

(use-package org-mode
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

  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  :bind ("C-c c" . org-capture))

(use-package haskell-mode
  :defer
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  :config
  (eval-after-load "haskell-mode"
    '(progn
       (define-key haskell-mode-map (kbd "C-x C-d") nil)
       (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
       (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
       (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
       (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
       (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
       (define-key haskell-mode-map (kbd "C-c M-.") nil)
       (define-key haskell-mode-map (kbd "C-c C-d") nil)))
  (eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
  (eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile)))

(use-package lisp-mode
  :defer
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode))

(use-package python
  :defer
  :init
  (require 'jedi)
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package calendar
  :defer
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

(use-package macrostep
  :bind ("C-c e m" . macrostep-expand))

(use-package nginx-mode
  :mode ("/etc/nginx/sites-*/*" . nginx-mode))

(use-package ace-jump-mode
  :bind ("M-z" . ace-jump-mode)
  :config
  (setq ace-jump-mode-submode-list
        '(ace-jump-char-mode
          ace-jump-word-mode
          ace-jump-line-mode)))

(use-package web-mode
  :mode ("\\.php\\'"
         "\\.html\\'"
         "\\.js\\'"
         "\\.css\\'"))

(use-package dired
  :config
  (add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "å") 'dired-up-directory))))

(use-package dired-details
  :defer 1
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

(use-package magit
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows)))

(use-package exec-path-from-shell
  :defer 1
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package server
  :defer 1
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

(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-x m") 'execute-extended-command)

(use-package smex
  :bind (("M-x" . smex)
         ("C-c C-m" . smex)
         ("C-x C-m" . smex)
         ("C-x m" . smex))
  :config
  (defadvice smex (around space-inserts-hyphen activate compile)
    (let ((ido-cannot-complete-command
           `(lambda ()
              (interactive)
              (if (string= " " (this-command-keys))
                  (insert ?-)
                (funcall ,ido-cannot-complete-command)))))
      ad-do-it)))

(use-package paren
  :defer 1
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1))

(use-package multiple-cursors
  :bind (("C-æ" . mc/mark-next-like-this)
         ("C-Æ" . mc/mark-all-like-this)))

(use-package company
  :config
  (global-company-mode t))

(use-package paredit
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(use-package git-gutter-fringe
  :preface
  (global-git-gutter-mode 1)
  (set-face-foreground 'git-gutter:modified "#c0c")
  (set-face-foreground 'git-gutter:added "#0c0")
  (set-face-foreground 'git-gutter:deleted "#c00")
  (set-face-background 'git-gutter:modified "#c0c")
  (set-face-background 'git-gutter:added "#0c0")
  (set-face-background 'git-gutter:deleted "#c00")
  (custom-set-variables
   '(git-gutter:update-interval 1))
  :if (window-system))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; ### Keybindings ###

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-M-n") 'forward-sexp)
(global-set-key (kbd "C-M-p") 'backward-sexp)
(global-set-key (kbd "C-.") 'hippie-expand)
(global-set-key (kbd "M-,") 'goto-init-el)
(global-set-key (kbd "M-<RET>") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c C-e") 'eval-and-replace)
(global-set-key (kbd "<C-M-S-up>") 'move-text-up)
(global-set-key (kbd "<C-M-S-down>") 'move-text-down)
(global-set-key (kbd "C-M-<return>") 'toggle-window-split)
(global-set-key (kbd "C-M-<backspace>") 'torenord/rotate-windows)
(global-set-key (kbd "C-c C-d") 'torenord/insert-date)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-x C-j") 'kill-all-buffers)
(global-set-key (kbd "C-c SPC") 'er/expand-region)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "<C-tab>") 'tidy)

;; ### Various ###

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

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))))

(defun kill-all-buffers ()
  "Kill all buffers, only leaving *scratch*."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun torenord/rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "you can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numwindows (count-windows))
         (while  (< i numwindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numwindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

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

(eval-after-load "geiser" '(setq geiser-active-implementations '(guile)))

;; ### Linux ###

(when on-linux
  (setq x-super-keysym 'meta)
  (set-face-attribute 'default nil
                      :height 90
                      :family "Liberation Mono"))

;; ### Mac ###

(when on-mac
  (setq ns-alternate-modifier 'none)
  (setq ns-command-modifier 'meta)

  (define-key function-key-map (kbd "<kp-decimal>") (kbd ","))

  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash/emacs")

  (when window-system
    (menu-bar-mode 1)

    (when (fboundp 'font-family-list)
      (set-face-attribute 'default nil :height 135)
      (when (member "Monaco" (font-family-list))
        (set-face-attribute 'default nil :family "Monaco")))))

;; ### Apperance ###

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))

  ((lambda (theme)
     (cond ((equal theme 'molokai)
            (progn
              (load-theme 'molokai t)
              (set-cursor-color "#fff")
              (set-face-attribute 'show-paren-match nil :background "#aaa")
              (set-face-attribute 'mode-line nil :background "#888" :box nil)
              (set-face-attribute 'mode-line-inactive nil :background "#000" :box nil)
              (set-face-attribute 'region nil :background "#888")))
           ((equal theme 'leuven)
            (progn
              (load-theme 'leuven t)
              (set-face-attribute 'region nil :background "#b3d2f3")
              (set-face-attribute 'show-paren-match nil :background "#b3d2f3")
              (set-cursor-color "black")))))
   'leuven))

;; ------------------------------------------------

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       emacs-start-time))))
               (message "Loading %s...done (%.3fs)"
                        ,load-file-name elapsed)))
          t)
