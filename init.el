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

(add-to-list 'load-path "~/.emacs.d/elpa/async-20150203.2127")
(add-to-list 'load-path "~/.emacs.d/lisp/helm/")

;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;; Default Path: ~/.emacs.d
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Python
(elpy-enable)
(require 'ob-python)
(require 're-builder)

(setq reb-re-syntax 'string)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")

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

;; Global Modes
(projectile-global-mode)
(ivy-mode 1)
(global-company-mode 1)
(global-hl-line-mode 1)
(syntactic-close 1)
(autopair-global-mode)

;; directory tracking for sane-term and the like
(set-variable 'dirtrack-list '("^.*[^ ]+:\\(.*\\)]" 1 nil))
(dirtrack-mode 1)

;; load custom config
(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "personal.el")
(load-user-file "javascript.el")
(load-user-file "keybindings.el")
(load-user-file "orgmode.el")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'simple-httpd)

;; set root folder for httpd server
(setq httpd-root "~/Sites")

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
;; magit prevent startup message
(setq magit-last-seen-setup-instructions "1.4.0")

;; C
(require 'cc-mode)
(define-key c-mode-base-map (kbd "C-x c") 'compile)

;; File management and terminals
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(add-to-list 'ac-modes 'web-mode)

(require 'dired+)
(require 'sane-term)

;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'c-mode-hook 'linum-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

(add-hook 'erc-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(add-hook 'scss-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'scss-mode-hook 'drag-stuff-mode)
(add-hook 'scss-mode-hook 'truncate-lines)

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'auto-complete)

(defun add-emmet-expand-to-smart-tab-completions ()
  (add-to-list 'smart-tab-completion-functions-alist
               (cons major-mode #'emmet-expand-line)))

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'add-emmet-expand-to-smart-tab-completions)
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'css-mode-hook 'add-emmet-expand-to-smart-tab-completions)

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'ac-emmet-html-setup)
(add-hook 'web-mode-hook 'my-web-mode-hook)
(add-hook 'web-mode-hook 'drag-stuff-mode)
(add-hook 'web-mode-hook 'toggle-truncate-lines)
(add-hook 'web-mode-hook 'linum-mode)
(add-hook 'web-mode-hook 'company-mode)
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

(require 'drag-stuff)
(require 'emmet-mode)
(require 'flycheck)
(require 'less-css-mode)
(require 'web-mode)
(require 'autopair)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(swiper-minibuffer-match-face-1 ((t :background "#dddddd")))
 '(swiper-minibuffer-match-face-2 ((t :background "#bbbbbb" :weight bold)))
 '(swiper-minibuffer-match-face-3 ((t :background "#bbbbff" :weight bold)))
 '(swiper-minibuffer-match-face-4 ((t :background "#ffbbff" :weight bold))))

(add-to-list 'default-frame-alist '(font . "Source Code Pro-14" ))
(set-face-attribute 'default t :font "Source Code Pro-14")
(sml/setup)
(setq sml/theme 'respectful)
(load-theme 'solarized-light t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (vue-mode major-mode-icons zenburn-theme yari xah-lookup xah-fly-keys writeroom-mode writegood-mode web-mode web-beautify url-shortener unicode-fonts twittering-mode tramp-term thesaurus textmate tern-auto-complete tabbar-ruler syntactic-close synonyms swoop swiper-helm svg sublime-themes stem ssh sqlup-mode sos solarized-theme smex smartparens smart-tabs-mode smart-tab smart-mode-line-powerline-theme smart-forward smart-compile slime-theme slime slack shell-pop scss-mode sane-term rvm ruby-refactor robe rinari replace-pairs replace+ regex-tool ranger rainbow-delimiters python-x python-mode pytest projectile-speedbar projectile-rails pastelmac-theme password-vault paper-theme pandoc-mode osx-lib osx-dictionary org-seek org-random-todo org-password-manager org-journal org-ac oceanic-theme ob-ipython nyan-mode nodejs-repl neotree names muse multi-term move-text meacupla-theme material-theme markdown-preview-mode markdown-mode+ majapahit-theme magit lorem-ipsum linum-relative leuven-theme less-css-mode kooten-theme json-mode js-comint jedi jdee indent-tools indent-guide impatient-mode hlinum hl-todo highlight-symbol highlight-parentheses highlight-indent-guides hideshow-org helm-robe helm-projectile helm-package helm-mode-manager helm-git helm-fuzzy-find helm-fuzzier helm-flyspell helm-flymake helm-flx helm-emmet helm-dictionary helm-dash helm-css-scss helm-anything helm-ag helm-ack handlebars-sgml-mode handlebars-mode hackernews grunt golden-ratio go-eldoc go gmail-message-mode git-gutter git-gutter+ git fuzzy flymake-ruby flymake-jslint figlet fancy-narrow exec-path-from-shell etable eslint-fix eshell-prompt-extras ergoemacs-mode erc-crypt encourage-mode emoji-fontset emoji-cheat-sheet-plus elpy elisp-lint dumb-jump drag-stuff dracula-theme discover-js2-refactor dired-ranger dired-k dired+ diff-hl dictionary deft dash-at-point darkburn-theme csv-mode company-jedi company-flx color-theme-sanityinc-tomorrow color-theme coffee-mode chess calfw cabledolphin bongo blgrep blackboard-theme bitly avy autopair aurora-theme atom-one-dark-theme atom-dark-theme apache-mode anzu anaphora anaconda-mode adoc-mode)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
