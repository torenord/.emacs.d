;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;; Set up appearance early
(require 'appearance)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(ido-mode 1)
(delete-selection-mode 1)
(transient-mark-mode 1)

(setq ido-enable-flex-matching t)
(setq save-interprogram-paste-before-kill t)
(setq scroll-conservatively 1)
(setq-default comint-prompt-read-only t)
(setq-default indent-tabs-mode nil)
(setq-default next-line-add-newlines nil)
(setq-default require-final-newline nil)

;(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines 1)))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(set-face-attribute 'default nil :height 150)
(fset 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; ---------------------------------------------------------

(when (require 'package nil 'noerror)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize)

  (when (not package-archive-contents)
    (package-refresh-contents))

  (dolist (p
           '(better-defaults
             exec-path-from-shell
             git-commit-mode
             gitconfig-mode
             gitignore-mode
             ido-ubiquitous
             magit
             monokai-theme
             multiple-cursors
             org
             php-mode))
    (when (not (package-installed-p p))
      (package-install p)
      (delete-other-windows))))

;; ---------------------------------------------------------

(when (display-graphic-p)
  (if (fboundp 'menu-bar-mode) (menu-bar-mode 1)))

(set-face-attribute 'default nil :height 150)
(set-face-attribute 'default nil :family "Inconsolata")

(require 'color-theme)
(require 'color-theme-molokai)
(color-theme-molokai)

(custom-set-faces
 '(show-paren-match ((t (:background "#888"))))
 '(mode-line ((t (:foreground "#fff" :background "#000" :box nil))))
 '(mode-line-inactive ((t (:foreground "#fff" :background "#000" :box nil)))))

(set-frame-parameter nil 'fullscreen 'fullheight)

;; ---------------------------------------------------------

(require 'key-bindings)

(global-set-key (kbd "C-.") 'hippie-expand)

(defun goto-init-el ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(global-set-key "\M-," 'goto-init-el)
(global-set-key "\C-x\C-z" 'shell)

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(global-set-key (kbd "M-<RET>") 'toggle-fullscreen)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))))
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key (kbd "<C-M-up>") 'move-text-up)
(global-set-key (kbd "<C-M-down>") 'move-text-down)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (global-set-key (kbd "C-z") nil))

(global-set-key (kbd "\C-x\g") 'magit-status)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
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

(global-set-key (kbd "C-M-<return>") 'toggle-window-split)

(defun kill-all-buffers ()
  "Kill all buffers, only leaving *scratch*."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
	  (buffer-list))
  (delete-other-windows))
(global-set-key "\C-x\C-j" 'kill-all-buffers)

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

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

(global-set-key (kbd "C-M-<backspace>") 'rotate-windows)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

(defun my-clear ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun my-shell-hook ()
  (local-set-key (kbd "\C-l") 'my-clear))
(add-hook 'shell-mode-hook 'my-shell-hook)

;; http://xahlee.blogspot.no/2011/09/emacs-lisp-function-to-trim-string.html
(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun insert-date ()
  "Insert current date at point."
  (interactive)
  (insert (trim-string (format-time-string "%e. %B %Y"))))
(global-set-key "\C-c\C-d" 'insert-date)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))
(when is-mac (require 'mac))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))
