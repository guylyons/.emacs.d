;; Personel.el
;; Custom settings and keybindings

(global-set-key (kbd "C-j") 'emmet-expand-line)
;; make Projectile global
(projectile-global-mode)

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

;; helm configuration
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
	helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))
(setq helm-M-x-fuzzy-match t
      helm-recentf-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(setq redisplay-dont-pause t)

;; my personal skewer start up function
(defun skewer-dev ()
  (interactive)
  (split-window-below)
  (run-skewer)
  (skewer-repl)
  )

;; multiple curosrs
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; yasnippet
(require 'yasnippet)
;;(yas-global-mode 1)

;; emacs title bar formatting
(setq frame-title-format '("" "[ %b ] %m-mode emacs " emacs-version))

;; locate ispell program for spell checking
(setq ispell-program-name "/usr/local/bin/ispell")

;; dired will only ask once for deletion
(setq dired-recursive-deletes 'always)

;; js highlight level
(setq js2-highlight-level 2)

;; Emacs backup settings
(defun make-backup-file-name (FILE)
  (let ((dirname (concat "~/.backups/emacs/"
			 (format-time-string "%y/%m/%d/"))))
    (if (not (file-exists-p dirname))
	(make-directory dirname t))
    (concat dirname (file-name-nondirectory FILE))))

;; My personal key bindings
(global-set-key (kbd "C-x p") 'helm-projectile)
(global-set-key (kbd "C-c >") 'calendar)
(global-set-key (kbd "C-x w r") 'writeroom-mode)

;; remap for meta key in mac
(setq mac-command-modifier 'meta)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; keybinding for quick access to org notes.org
(global-set-key (kbd "C-c n")
		(lambda () (interactive) (find-file "~/org/notes.org")))
(set-register ?o (cons 'file "~/org/notes.org"))

;; journal-mode hooks
(add-hook 'org-journal-mode-hook 'visual-fill-column-mode)
(add-hook 'org-journal-mode-hook 'writegood-mode)

(autopair-global-mode)

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'lisp-interaction-mode)

(require 'dired-k)
(define-key dired-mode-map (kbd "K") 'dired-k)

;; You can use dired-k alternative to revert-buffer
(define-key dired-mode-map (kbd "g") 'dired-k)

;; always execute dired-k when dired buffer is opened
(add-hook 'dired-initial-position-hook 'dired-k)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq ring-bell-function 'ignore)

