;; Personel.el
;; Custom settings and keybindings

;; Helm
(require 'helm-config)
(helm-mode 1)

(setq helm-M-x-fuzzy-match t
      helm-recentf-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(setq redisplay-dont-pause t)

;; Multiple Curosrs
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; emacs title bar formatting
(setq frame-title-format '("" "[ %b ] %m-mode emacs " emacs-version))

;; locate ispell program for spell checking
(setq ispell-program-name "/usr/local/bin/ispell")

;; dired will only ask once for deletion
(setq dired-recursive-deletes 'always)

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

