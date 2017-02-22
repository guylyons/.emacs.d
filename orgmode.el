;; Org Mode stuff

(add-hook 'org-journal-mode-hook 'org-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Key bindings
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)

(org-clock-persistence-insinuate)
(require 'org)
(setq org-agenda-files (list "~/org/" "~/org/journal/"))
(setq org-agenda-include-diary t)
(setq org-clock-persist 'history)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-journal-dir "~/org/journal/")
(setq org-journal-file-format "%Y-%m-%d.org")
(setq org-log-done t)
(setq org-todo-keywords
      '(
	(sequence "TODO" "STARTED" "|" "DONE" "DELEGATED" "POSTPONED" "CANCELED")
	(sequence "CLONED" "TEST SENT" "|" "SCHEDULED")
	(sequence "SENT" "APPROVED" "|" "PAID")
	))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#ff39a3" :weight bold))
	("STARTED" . "#E35DBF")
	("CANCELED" . (:foreground "white" :background "#4d4d4d" :weight bold))
	("DELEGATED" . "pink")
	("POSTPONED" . "#008080")))
;; keybindings for password manager
(add-hook 'org-mode-hook 'org-password-manager-key-bindings)
