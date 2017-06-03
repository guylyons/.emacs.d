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
(ivy-mode 1)
(autopair-global-mode)

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
(add-hook 'scss-mode-hook 'truncate-lines)

;; Emmet
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; Web-mode
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'ac-emmet-html-setup)
(add-hook 'web-mode-hook 'drag-stuff-mode)
(add-hook 'web-mode-hook 'toggle-truncate-lines)
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

(add-to-list 'default-frame-alist '(font . "Source Code Pro-14" ))
(set-face-attribute 'default t :font "Source Code Pro-14")

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
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("2f427a54ff2c7ae9e61fde650037c1590e35922fefafa47c162a94e4d6d422a6" "14e6b27c801eece344f7b11337076d6e56ff444588c5f51c5f14854befc6c3b1" "7ed277f12c9fd279f258a34cc618d203243dd56e7f37feac892bb26ad7a1cad2" "2f2d9b9827bcfd4dab08df6e0f5cbf347b65c8fc112d947cf696234b80261ebf" "54dd7c04a3fcebbf69ba8f9a9f116699e7247b6fdbd46a917914364426634747" "332b53f3331d7280557d79b3398e376fc290c299ac1c4870d51b9019c2831362" "18a393331877d6332e859d36ec6abf755f560437c2b14f4513a0745ff0d420a5" "8ccfc8f0da78d622fd390e84fdc955e404478b4a6c97a421fcea2b914bdea5ff" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "a74c967cabd9218c477507ba82eee52cb5683078e2a39c8ef90c051a692aea80" "ee0e801a44255007eb4ad1d9aebc923f237ffcf5101ad35f131e1a4b062ff6f4" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "47744f6c8133824bdd104acc4280dbed4b34b85faa05ac2600f716b0226fb3f6" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(fci-rule-color "#383838")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (counsel use-package hledger-mode zenburn-theme yari xah-lookup xah-fly-keys writeroom-mode writegood-mode web-mode web-beautify vue-mode url-shortener unicode-fonts twittering-mode tramp-term thesaurus textmate tern-auto-complete tabbar-ruler syntactic-close synonyms swoop swiper-helm svg sublime-themes stem ssh sqlup-mode sos solarized-theme smex smartparens smart-mode-line-powerline-theme smart-forward smart-compile slime-theme slime slack shell-pop scss-mode sane-term rvm ruby-refactor robe rinari restclient replace-pairs replace+ regex-tool ranger rainbow-delimiters python-x python-mode pytest projectile-speedbar projectile-rails pastelmac-theme password-vault paper-theme pandoc-mode osx-lib osx-dictionary org-seek org-random-todo org-password-manager org-journal org-ac oceanic-theme ob-ipython nyan-mode nodejs-repl neotree names muse multi-term move-text meacupla-theme material-theme markdown-preview-mode markdown-mode+ majapahit-theme magit lorem-ipsum linum-relative leuven-theme less-css-mode ledger-mode kooten-theme json-mode js-comint jedi jdee indent-tools indent-guide impatient-mode hlinum hl-todo highlight-symbol highlight-parentheses highlight-indent-guides hideshow-org helm-robe helm-projectile helm-package helm-mode-manager helm-git helm-fuzzy-find helm-fuzzier helm-flyspell helm-flymake helm-flx helm-emmet helm-dictionary helm-dash helm-css-scss helm-anything helm-ag helm-ack handlebars-sgml-mode handlebars-mode hackernews grunt golden-ratio go-eldoc go gmail-message-mode git-gutter git-gutter+ git fuzzy flymake-ruby figlet fancy-narrow exec-path-from-shell etable eslint-fix eshell-prompt-extras ergoemacs-mode erc-crypt encourage-mode emoji-fontset emoji-cheat-sheet-plus elpy elisp-lint editorconfig dumb-jump drag-stuff dracula-theme discover-js2-refactor dired-ranger dired-k dired+ diff-hl dictionary deft dash-at-point darkburn-theme csv-mode company-php company-jedi company-flx color-theme-sanityinc-tomorrow color-theme coffee-mode chess calfw cabledolphin bongo blgrep blackboard-theme bitly avy autopair aurora-theme atom-one-dark-theme atom-dark-theme apache-mode anzu anaphora anaconda-mode adoc-mode)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(swiper-minibuffer-match-face-1 ((t :background "#dddddd")))
 '(swiper-minibuffer-match-face-2 ((t :background "#bbbbbb" :weight bold)))
 '(swiper-minibuffer-match-face-3 ((t :background "#bbbbff" :weight bold)))
 '(swiper-minibuffer-match-face-4 ((t :background "#ffbbff" :weight bold))))
