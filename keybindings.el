;; keybindings.el
;; ------------------------------
;; Author: gl
;; Email: guylyons@protonmail.com
;; ------------------------------
;;


(global-set-key (kbd "C-x T") 'sane-term-create)
(global-set-key (kbd "C-x t") 'sane-term)

(global-set-key (kbd "<f2>") 'calculator)
(global-set-key (kbd "C-t") 'swiper)
(global-set-key (kbd "C-c C-p") 'list-packages)
(global-set-key (kbd "C-c C-e") 'erc)

(global-set-key (kbd "C-1") 'helm-find-files)
(global-set-key (kbd "C-2") 'helm-mini)
(global-set-key (kbd "C-3") 'helm-projectile)
(global-set-key (kbd "C-4") 'helm-projectile-find-file)
(global-set-key (kbd "C-8") 'helm-projectile-ack)
(global-set-key (kbd "C-9") 'helm-projectile-ag)
(global-set-key (kbd "C-`") 'kill-buffer-and-window)
(global-set-key [C-tab] 'previous-buffer)
(global-set-key [M-tab] 'next-buffer)
(global-set-key (kbd "C-0") 'other-window)

(global-set-key [C-M-tab] [alt-tab])

(setq mac-command-modifier 'control)
