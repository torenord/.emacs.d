(setq user-full-name "Tore Norderud"
      user-mail-address "tore.norderud@gmail.com")

(setq on-mac (equal system-type 'darwin))
(setq on-gnu-linux (equal system-type 'gnu/linux))

(when (not (boundp 'user-emacs-directory))
  (when (or on-mac on-gnu-linux) (setq user-emacs-directory "~/.emacs.d/")))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'sane-defaults)
(require 'appearance)
(require 'key-bindings)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; ---------------------------------------------------------

(when (require 'package nil 'noerror)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize)

  (when (not package-archive-contents)
    (package-refresh-contents))

  (dolist (p
           '(better-defaults
             color-theme
             color-theme-molokai
             dired-details
             exec-path-from-shell
             git-commit-mode
             gitconfig-mode
             gitignore-mode
             ido-vertical-mode
             flx
             flx-ido
             ido-ubiquitous
             ido-at-point
             smex
             magit
             multiple-cursors
             maude-mode
             org
             php-mode))
    (when (not (package-installed-p p))
      (package-install p)
      (delete-other-windows))))

;; ---------------------------------------------------------

(defun goto-init-el ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))))

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

(defun kill-all-buffers ()
  "Kill all buffers, only leaving *scratch*."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
	  (buffer-list))
  (delete-other-windows))

(defun torenord/rotate-windows ()
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

 ;; from org-trim in org.el
(defun torenord/trim (s)
  "Remove whitespace at beginning and end of string."
  (if (string-match "\\`[ \t\n\r]+" s) (setq s (replace-match "" nil nil s)))
  (if (string-match "[ \t\n\r]+\\'" s) (setq s (replace-match "" nil nil s)))
  s)

(defun torenord/insert-date ()
  "Insert current date at point."
  (interactive)
  (insert (torenord/trim (format-time-string "%e. %B %Y"))))

(defun torenord/compile ()
  (interactive)
  (save-window-excursion
    (compile "make -k")))

(require 'setup-dired)
(require 'setup-shell)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (blink-cursor-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode t))

(apply-molokai)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10)

;; Try out flx-ido for better flex matching between words
(require 'flx-ido)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; flx-ido looks better with ido-vertical-mode
(require 'ido-vertical-mode)
(ido-vertical-mode)

(defun sd/ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match))

(defun my/setup-ido ()
  ;; Go straight home
  (define-key ido-file-completion-map
    (kbd "~")
    (lambda ()
      (interactive)
      (cond
       ((looking-back "~/") (insert "projects/"))
       ((looking-back "/") (insert "~/"))
       (:else (call-interactively 'self-insert-command)))))

  ;; Use C-w to go back up a dir to better match normal usage of C-w
  ;; - insert current file name with C-x C-w instead.
  (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
  (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)

  (define-key ido-file-dir-completion-map (kbd "C-w") 'ido-delete-backward-updir)
  (define-key ido-file-dir-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name))

(add-hook 'ido-setup-hook 'my/setup-ido)

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

;; Ido at point (C-,)
(require 'ido-at-point)
(ido-at-point-mode)

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-vertical-mode t)

(when on-mac (require 'mac))
