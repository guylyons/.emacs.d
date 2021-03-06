;; orgmode.el

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

;; babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (haskell . nil)
   (latex . t)
   (ledger . t)         ;this is the important one for this tutorial
   (ocaml . nil)
   (octave . t)
   (python . t)
   (ruby . t)
   (shell . t)
   (screen . nil)
   (sql . nil)
   (sqlite . t)))

;; allow coding indentation in src blocks
(setq org-src-tab-acts-natively t)
