;; keybindings.el
;; ------------------------------
;; Author: gl
;; Email: guylyons@protonmail.com
;; ------------------------------
;;


(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-S-F") 'helm-find-files)

(global-set-key (kbd "M-SPC") 'set-mark-command)

(global-set-key (kbd "C-c n")
		(lambda () (interactive) (find-file "~/org/notes.org")))
(set-register ?o (cons 'file "~/org/notes.org"))
(add-hook 'org-mode-hook 'org-password-manager-key-bindings)

(global-set-key (kbd "C-x T") 'sane-term-create)
(global-set-key (kbd "C-x t") 'sane-term)

(global-set-key (kbd "C-x p") 'helm-projectile)
(global-set-key (kbd "C-c >") 'calendar)
(global-set-key (kbd "C-x w r") 'writeroom-mode)
(global-set-key (kbd "C-c s") 'helm-css-scss)
(global-set-key (kbd "C-c h") 'helm-projectile)

(global-set-key (kbd "C-c g") 'xah-lookup-google)

(global-set-key (kbd "<f2>") 'calculator)
(global-set-key (kbd "<f3>") 'sane-term)

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

(global-set-key (kbd "C-S-E") 'emmet-expand-line)
(global-set-key (kbd "C-S-P") 'helm-projectile)
(global-set-key (kbd "C-S-S") 'save-buffer)

(global-set-key [C-M-tab] [alt-tab])
(setq mac-command-modifier 'control)

(drag-stuff-define-keys)

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))
