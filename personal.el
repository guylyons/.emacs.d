;; Personel.el
;; Custom settings and keybindings

;; swiper settings taken from oremacs.com
(custom-set-faces
 '(swiper-minibuffer-match-face-1
   ((t :background "#dddddd")))
 '(swiper-minibuffer-match-face-2
   ((t :background "#bbbbbb" :weight bold)))
 '(swiper-minibuffer-match-face-3
   ((t :background "#bbbbff" :weight bold)))
 '(swiper-minibuffer-match-face-4
   ((t :background "#ffbbff" :weight bold))))

;; Helm
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1)
(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
	helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))
(setq helm-M-x-fuzzy-match t
      helm-recentf-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(setq redisplay-dont-pause t)

;; Skewer
(defun skewer-dev ()
  (interactive)
  (split-window-below)
  (run-skewer)
  (skewer-repl)
  )

;; Multiple Curosrs
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; emacs title bar formatting
(setq frame-title-format '("" "[ %b ] %m-mode emacs " emacs-version))

;; locate ispell program for spell checking
(setq ispell-program-name "/usr/local/bin/ispell")

;; dired will only ask once for deletion
(setq dired-recursive-deletes 'always)

;; js highlight level
(setq js2-highlight-level 3)

;; Emacs backup settings
(defun make-backup-file-name (FILE)
  (let ((dirname (concat "~/.backups/emacs/"
			 (format-time-string "%y/%m/%d/"))))
    (if (not (file-exists-p dirname))
	(make-directory dirname t))
    (concat dirname (file-name-nondirectory FILE))))

;; remap for meta key in mac
(setq mac-command-modifier 'meta)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'lisp-interaction-mode)

(require 'dired-k)
(define-key dired-mode-map (kbd "K") 'dired-k)
(define-key dired-mode-map (kbd "g") 'dired-k)

(add-hook 'dired-initial-position-hook 'dired-k)

(setq ring-bell-function 'ignore)

