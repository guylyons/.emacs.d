;; orgmode.el
;; ------------------------------
;; Author: gl
;; Email: guylyons@protonmail.com
;; ------------------------------
;;

(add-hook 'org-journal-mode-hook 'org-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

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
	(sequence "TODO" "DOING" "|" "DONE" "POSTPONED" "CANCELED")
	(sequence "SENT" "APPROVED" "|" "PAID")
	))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#ff39a3" :weight bold))
	("STARTED" . "#E35DBF")
	("CANCELED" . (:foreground "white" :background "#4d4d4d" :weight bold))
	("DELEGATED" . "pink")
	("POSTPONED" . "#008080")))

(add-hook 'org-journal-mode-hook 'visual-fill-column-mode)
(add-hook 'org-journal-mode-hook 'writegood-mode)


