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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#212121" "#B71C1C" "#558b2f" "#FFA000" "#2196f3" "#4527A0" "#00796b" "#FAFAFA"))
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-mode t nil (cua-base))
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("51897d0e185a9d350a124afac8d5e95cda53e737f3b33befc44ab02f2b03dab1" "14f0fbf6f7851bfa60bf1f30347003e2348bf7a1005570fd758133c87dafe08f" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "7485c473334e95534b8aaa238bf6d4b7a12777a6bd3cc14d78b627d806e745c9" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "01ac390c8aa5476078be3769f3c72a9e1f5820c9d9a8e8fcde21d0ff0bbeeec1" "4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "f81a9aabc6a70441e4a742dfd6d10b2bae1088830dc7aba9c9922f4b1bd2ba50" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(erc-autojoin-mode t)
 '(erc-button-mode t)
 '(erc-fill-mode t)
 '(erc-irccontrols-mode t)
 '(erc-list-mode t)
 '(erc-match-mode t)
 '(erc-menu-mode t)
 '(erc-move-to-prompt-mode t)
 '(erc-netsplit-mode t)
 '(erc-networks-mode t)
 '(erc-noncommands-mode t)
 '(erc-pcomplete-mode t)
 '(erc-readonly-mode t)
 '(erc-ring-mode t)
 '(erc-stamp-mode t)
 '(erc-track-mode t)
 '(erc-track-position-in-mode-line t t)
 '(erc-user-full-name "adslkjfad;slkjflkj, asdf?")
 '(exec-path-from-shell-check-startup-files nil)
 '(js-indent-level 2)
 '(linum-format "%3d ")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (eslint-fix xah-lookup indent-tools org-seek projectile-speedbar hl-todo syntactic-close git-gutter+ solarized-theme color-theme-sanityinc-tomorrow move-text apache-mode yari xah-fly-keys writeroom-mode writegood-mode web-mode web-beautify url-shortener unicode-fonts twittering-mode tramp-term thesaurus textmate tern-auto-complete tabbar-ruler synonyms swoop svg sublime-themes stem ssh sqlup-mode sos smex smartparens smart-tabs-mode smart-tab smart-mode-line-powerline-theme smart-forward smart-compile slime-theme slime slack shell-pop scss-mode sane-term rvm ruby-refactor robe rinari replace-pairs replace+ regex-tool ranger rainbow-delimiters python-x python-mode pytest projectile-rails pastelmac-theme password-vault paper-theme pandoc-mode osx-lib osx-dictionary org-random-todo org-password-manager org-journal org-ac oceanic-theme ob-ipython nyan-mode nodejs-repl neotree names muse multi-term meacupla-theme material-theme markdown-preview-mode markdown-mode+ majapahit-theme magit lorem-ipsum linum-relative leuven-theme less-css-mode kooten-theme json-mode js-comint jedi jdee indent-guide impatient-mode hlinum highlight-symbol highlight-parentheses highlight-indent-guides hideshow-org helm-robe helm-projectile helm-package helm-mode-manager helm-git helm-fuzzy-find helm-fuzzier helm-flyspell helm-flymake helm-flx helm-dictionary helm-dash helm-css-scss helm-anything helm-ag helm-ack handlebars-sgml-mode handlebars-mode hackernews grunt golden-ratio go-eldoc go gmail-message-mode git-gutter git fuzzy flymake-ruby flymake-jslint figlet fancy-narrow exec-path-from-shell etable eshell-prompt-extras ergoemacs-mode erc-crypt encourage-mode elpy elisp-lint dumb-jump drag-stuff discover-js2-refactor dired-ranger dired-k dired+ diff-hl dictionary deft dash-at-point darkburn-theme csv-mode company-jedi company-flx color-theme coffee-mode chess calfw cabledolphin bongo blgrep bitly avy autopair aurora-theme atom-one-dark-theme atom-dark-theme anzu anaphora anaconda-mode ag adoc-mode)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(save-place t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#B71C1C")
     (40 . "#FF5722")
     (60 . "#FFA000")
     (80 . "#558b2f")
     (100 . "#00796b")
     (120 . "#2196f3")
     (140 . "#4527A0")
     (160 . "#B71C1C")
     (180 . "#FF5722")
     (200 . "#FFA000")
     (220 . "#558b2f")
     (240 . "#00796b")
     (260 . "#2196f3")
     (280 . "#4527A0")
     (300 . "#B71C1C")
     (320 . "#FF5722")
     (340 . "#FFA000")
     (360 . "#558b2f"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))

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
