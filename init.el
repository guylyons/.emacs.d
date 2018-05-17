;; init.el
;; ------------------------------
;; Author: gl
;; Email: guylyons@protonmail.com
;; ------------------------------
;;

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/helm/")

;; Indendation settings
(setq-default indent-tabs-mode nil)

;; Path settings
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

;; xah fly keys for my rsi
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)

(require 'which-key) (which-key-mode)

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; no backups
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Snippets
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(add-hook 'term-mode-hook (lambda()
                (yas-minor-mode -1)))
(yas-global-mode t)

;; some fan favorites
(projectile-global-mode)
(drag-stuff-global-mode)
;;(autopair-global-mode)
(global-hl-line-mode)

(ivy-mode 1)
(ivy-set-occur 'swiper 'swiper-occur)


(add-hook 'after-init-hook 'global-company-mode)
(global-subword-mode 1)

(require 'drag-stuff)
(require 'flycheck)
(require 'less-css-mode)
(require 'web-mode)
(require 'autopair)
(require 'dired+)

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

(require 'sane-term)
;; directory tracking for sane-term
(set-variable 'dirtrack-list '("^.*[^ ]+:\\(.*\\)]" 1 nil))
(dirtrack-mode 1)

;; load custom config
(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;; emacs includes
(load-user-file "personal.el")
(load-user-file "keybindings.el")
(load-user-file "javascript.el")
(load-user-file "python.el")
(load-user-file "orgmode.el")
(load-user-file "ruby.el")
(load-user-file "go.el")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Git
(global-set-key (kbd "C-x g") 'magit-status)
;; magit prevent startup message
(setq magit-last-seen-setup-instructions "1.4.0")

;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; C
(require 'cc-mode)
(define-key c-mode-base-map (kbd "C-x c") 'compile)
(add-hook 'c-mode-hook 'linum-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

;; Spelling
(add-hook 'erc-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'gnus 'flyspell-mode)

;; erc
(setq erc-nick "cstls")
(add-hook 'erc-mode-hook 'visual-line-mode)

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
(add-hook 'web-mode-hook 'drag-stuff-mode)
(add-hook 'web-mode-hook 'linum-mode)

;; mime type associations
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))

;; css
(autoload 'scss-mode "scss-mode")
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 4))

(with-eval-after-load 'scss-mode
  (define-key scss-mode-map (kbd "C-c u") 'helm-css-scss))

(setq css-indent-offset 2)

;; C / Java indentation
(setq c-default-style "linux"
      c-basic-offset 4)


;; fonts
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14" ))
(set-face-attribute 'default t :font "Source Code Pro-14")

;; save customize-variable
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
