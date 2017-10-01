;; init.el
;; ------------------------------
;; Author: gl
;; Email: guylyons@protonmail.com
;; ------------------------------
;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; (add-to-list 'load-path "~/.emacs.d/elpa/async-20150203.2127")
(add-to-list 'load-path "~/.emacs.d/lisp/helm/")

;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;; configure path
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

;; Ruby
(setq ruby-indent-level 2)

(add-hook 'term-mode-hook (lambda()
                (yas-minor-mode -1)))

;; Snippets
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(yas-global-mode t)

;; highlight symbol mode
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(projectile-global-mode)
(drag-stuff-global-mode)
(autopair-global-mode)
(ivy-mode 1)
(nyan-mode 1)
(add-hook 'after-init-hook 'global-company-mode)

(require 'company)
(require 'company-web-html)
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(global-set-key (kbd "C-c /") 'company-files)

(add-hook 'web-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-web-html))
                          (company-mode t)))

;; directory tracking for sane-term
(set-variable 'dirtrack-list '("^.*[^ ]+:\\(.*\\)]" 1 nil))
(dirtrack-mode 1)

;; load custom config
(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "personal.el")
(load-user-file "javascript.el")
(load-user-file "python.el")
(load-user-file "keybindings.el")
(load-user-file "orgmode.el")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Git
(global-set-key (kbd "C-x g") 'magit-status)
;; magit prevent startup message
(setq magit-last-seen-setup-instructions "1.4.0")

(require 'dired+)
(require 'sane-term)

(require 'dired-k)
(define-key dired-mode-map (kbd "K") 'dired-k)

;; You can use dired-k alternative to revert-buffer
(define-key dired-mode-map (kbd "g") 'dired-k)

;; always execute dired-k when dired buffer is opened
(add-hook 'dired-initial-position-hook 'dired-k)

(add-hook 'dired-after-readin-hook #'dired-k-no-revert)

;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; c-mode
(require 'cc-mode)
(define-key c-mode-base-map (kbd "C-x c") 'compile)
(add-hook 'c-mode-hook 'linum-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

;; Flyspell
(add-hook 'erc-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'gnus 'flyspell-mode)

;; CSS
(add-hook 'scss-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'scss-mode-hook 'drag-stuff-mode)

;; Emmet
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

(add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.
(setq emmet-move-cursor-between-quotes t) ;; default nil
(setq emmet-move-cursor-after-expanding nil) ;; default

;; Web-mode
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'drag-stuff-mode)q
(add-hook 'web-mode-hook 'linum-mode)
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))

(autoload 'scss-mode "scss-mode")
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 4)
  )
(with-eval-after-load 'scss-mode
  (define-key scss-mode-map (kbd "C-c u") 'helm-css-scss))

(defvar css-indent-offset 2)

(setq debug-on-error t)

(require 'drag-stuff)
(require 'flycheck)
(require 'less-css-mode)
(require 'web-mode)
(require 'autopair)

(add-to-list 'default-frame-alist '(font . "Hack-14" ))
(set-face-attribute 'default t :font "Hack-14")

;; no backups
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#d6d6d6"))
 '(cursor-type (quote bar))
 '(custom-safe-themes
   (quote
    ("125fd2180e880802ae98b85f282b17f0aa8fa6cb9fc4f33d7fb19a38c40acef0" "3b5ce826b9c9f455b7c4c8bff22c020779383a12f2f57bf2eb25139244bb7290" "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "65d9573b64ec94844f95e6055fe7a82451215f551c45275ca5b78653d505bc42" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4e4d9f6e1f5b50805478c5630be80cce40bee4e640077e1a6a7c78490765b03f" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(fci-rule-color "#37474f")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(hl-sexp-background-color "#1c1f26")
 '(nrepl-message-colors
   (quote
    ("#183691" "#969896" "#a71d5d" "#969896" "#0086b3" "#795da3" "#a71d5d" "#969896")))
 '(pdf-view-midnight-colors (quote ("#969896" . "#f8eec7")))
 '(save-place t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
