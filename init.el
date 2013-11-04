(setq user-full-name "Tore Norderud")
(setq user-mail-address "tore.norderud@gmail.com")

(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq ns-function-modifier 'hyper)

(setq bell-volume 0)
(setq comint-prompt-read-only t)
(setq ido-enable-flex-matching t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq save-interprogram-paste-before-kill t)
(setq scroll-conservatively 1)
(setq sound-alist nil)
(setq visible-bell t)
(setq-default comint-prompt-read-only t)
(setq-default indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq-default next-line-add-newlines nil)
(setq-default require-final-newline nil)
(setq initial-scratch-message nil)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines 1)))

(set-face-attribute 'default nil :height 150)
(fset 'yes-or-no-p 'y-or-n-p)

;; ---------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p
	 '(better-defaults
	   exec-path-from-shell
	   git-commit-mode
	   ido-ubiquitous
	   magit
	   monokai-theme
	   org
	   php-mode))
  (when (not (package-installed-p p))
    (package-install p)
    (delete-other-windows)))

;; ---------------------------------------------------------

(when (display-graphic-p)
  (if (fboundp 'menu-bar-mode) (menu-bar-mode t))
  (require 'monokai-theme))

(custom-set-faces
 '(mode-line ((t (:foreground "#fff" :background "#444" :box nil))))
 '(mode-line-inactive ((t (:foreground "#ccc" :background "#222" :box nil)))))

(blink-cursor-mode 0)
(ido-mode 1)
(show-paren-mode 0)
(column-number-mode)
(show-paren-mode)
(transient-mark-mode 1)
(global-hl-line-mode 0)
(git-gutter-mode 1)
(delete-selection-mode 1)

;; ---------------------------------------------------------

(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\m" 'execute-extended-command)

(global-set-key "\M-n" 'forward-paragraph)
(global-set-key "\M-p" 'backward-paragraph)

(global-set-key "\M-," (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key "\C-x\C-z" 'shell)

(define-key key-translation-map (kbd "s-8") (kbd "["))
(define-key key-translation-map (kbd "s-(") (kbd "{"))
(define-key key-translation-map (kbd "s-9") (kbd "]"))
(define-key key-translation-map (kbd "s-)") (kbd "}"))
(define-key key-translation-map (kbd "s-7") (kbd "|"))
(define-key key-translation-map (kbd "s-/") (kbd "\\"))
(define-key key-translation-map (kbd "M-s-7") (kbd "M-|"))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(global-set-key (kbd "M-<RET>") 'toggle-fullscreen)

(global-set-key "\C-ca" 'org-agenda)

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
