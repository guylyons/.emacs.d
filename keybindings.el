;; keybindings.el

(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-S-F") 'helm-find-files)

(global-set-key (kbd "C-S-D") 'helm-projectile-find-file-dwim)

(global-set-key (kbd "C-c s") 'set-mark-command)

(global-set-key (kbd "C-c n")
                (lambda () (interactive) (find-file "~/org/notes.org")))
(set-register ?o (cons 'file "~/org/notes.org"))
(add-hook 'org-mode-hook 'org-password-manager-key-bindings)

(global-set-key (kbd "C-c p") 'helm-projectile)
(global-set-key (kbd "C-c >") 'calendar)
(define-key scss-mode-map (kbd "C-c s") 'helm-css-scss)

(global-set-key (kbd "C-c g") 'xah-lookup-google)

(global-set-key (kbd "C-t") 'swiper)
(global-set-key (kbd "C-c C-p") 'list-packages)
(global-set-key (kbd "C-c C-e") 'erc)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-6") 'magit-stage)
(global-set-key (kbd "C-S-S") 'save-buffer)

;; helm keybindings
;;(global-set-key (kbd "C-1") 'helm-find-files)
;;(global-set-key (kbd "C-2") 'helm-mini)
(global-set-key (kbd "<f3>") 'helm-projectile)
(global-set-key (kbd "<f5>") 'helm-projectile-ack)
(global-set-key (kbd "<f4>") 'helm-projectile-find-file)
(global-set-key (kbd "<f6>") 'helm-projectile-ag)
;;(global-set-key (kbd "C-`") 'kill-buffer-and-window)
(global-set-key (kbd "C-!") 'eshell)

;; ivy keybindings
(global-set-key (kbd "C-x C-f") 'counsel-find-file) (global-set-key (kbd "<f1> f") 'counsel-describe-function) (global-set-key (kbd "<f1> v") 'counsel-describe-variable) (global-set-key (kbd "<f1> l") 'counsel-find-library) (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol) (global-set-key (kbd "<f2> u") 'counsel-unicode-char)

(global-set-key (kbd "C-c g") 'counsel-git) (global-set-key (kbd "C-c j") 'counsel-git-grep) (global-set-key (kbd "C-c k") 'counsel-ag) (global-set-key (kbd "C-x l") 'counsel-locate) (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

(global-set-key (kbd "C-c C-r") 'ivy-resume)


(global-set-key [C-tab] 'previous-buffer)
(global-set-key [M-tab] 'next-buffer)
(global-set-key (kbd "C-0") 'other-window)

(define-key web-mode-map (kbd "C-S-E") 'emmet-expand-line)

(global-set-key (kbd "C-S-P") 'helm-projectile)
(global-set-key (kbd "C-S-W") 'helm-projectile-switch-project)
(global-set-key (kbd "C-S-A") 'mark-whole-buffer)

(global-set-key [C-M-tab] [alt-tab])
(setq mac-command-modifier 'control)

(drag-stuff-define-keys)

;; window management
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C--") 'split-window-right)
(global-set-key (kbd "C-=") 'split-window-below)
(global-set-key (kbd "C-S-O") 'delete-other-windows)

(global-set-key (kbd "C-S-W") 'whitespace-cleanup)

(define-key xah-fly-key-map (kbd "a") 'helm-buffers-list)
(define-key xah-fly-leader-key-map (kbd "b") 'helm-buffers-list)
(define-key xah-fly-leader-key-map (kbd "RET") 'helm-M-x)
(define-key xah-fly-leader-key-map (kbd "f") 'helm-find-files)
(define-key xah-fly-leader-key-map (kbd "p") 'helm-projectile-find-file-dwim)
(define-key xah-fly-leader-key-map (kbd "]") 'magit-status)

(add-hook 'after-save-hook 'xah-fly-command-mode-activate)

(global-set-key (kbd "C-`") 'xah-fly-command-mode-activate)

(defun hl-line-mode-on () (global-hl-line-mode 1))
(defun hl-line-mode-off () (global-hl-line-mode 0))

(add-hook 'xah-fly-command-mode-activate-hook 'hl-line-mode-on)
(add-hook 'xah-fly-insert-mode-activate-hook  'hl-line-mode-off)
