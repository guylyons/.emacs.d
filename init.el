;; init.el
;; ------------------------------
;; Author: gl
;; Email: guylyons@protonmail.com
;; ------------------------------
;;

;; melpa goodness
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
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
(autopair-global-mode)
(global-hl-line-mode)
(ivy-mode 1)
(add-hook 'after-init-hook 'global-company-mode)

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

;; css
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

;; fonts
(add-to-list 'default-frame-alist '(font . "Hack-14" ))
(set-face-attribute 'default t :font "Hack-14")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#d6d6d6"))
 '(cua-mode t nil (cua-base))
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "3a5f04a517096b08b08ef39db6d12bd55c04ed3d43b344cf8bd855bde6d3a1ae" "e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "125fd2180e880802ae98b85f282b17f0aa8fa6cb9fc4f33d7fb19a38c40acef0" "3b5ce826b9c9f455b7c4c8bff22c020779383a12f2f57bf2eb25139244bb7290" "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "65d9573b64ec94844f95e6055fe7a82451215f551c45275ca5b78653d505bc42" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4e4d9f6e1f5b50805478c5630be80cce40bee4e640077e1a6a7c78490765b03f" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(display-battery-mode t)
 '(emmet-move-cursor-after-expanding t)
 '(fci-rule-color "#37474f")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(fringe-mode 0 nil (fringe))
 '(hl-sexp-background-color "#1c1f26")
 '(indicate-empty-lines t)
 '(linum-format "%d ")
 '(neo-show-hidden-files nil)
 '(nrepl-message-colors
   (quote
    ("#183691" "#969896" "#a71d5d" "#969896" "#0086b3" "#795da3" "#a71d5d" "#969896")))
 '(package-selected-packages
   (quote
    (emms calfw-ical calfw-cal calfw-org zenburn-theme yari xah-lookup xah-fly-keys writeroom-mode writegood-mode web-mode web-beautify vue-mode use-package url-shortener unicode-fonts twittering-mode treemacs-projectile tramp-term thesaurus textmate tern-auto-complete term+ tabbar-ruler syntactic-close synonyms swoop swiper-helm svg sublime-themes stem ssh sqlup-mode sos solarized-theme smex smartparens smart-mode-line-powerline-theme smart-forward smart-compile slime-theme slime shell-pop scss-mode sane-term rvm ruby-refactor robe rjsx-mode rinari restclient-test replace-pairs replace+ regex-tool rebecca-theme ranger rainbow-mode rainbow-delimiters python-x python-mode pytest projectile-speedbar projectile-rails prettier-js pastelmac-theme password-vault pass paper-theme pandoc-mode osx-lib osx-dictionary org-seek org-random-todo org-password-manager org-journal org-ac oceanic-theme ob-ipython oauth2 nyan-mode nodejs-repl nlinum-hl neotree names muse multi-term move-text meacupla-theme material-theme markdown-preview-mode markdown-mode+ majapahit-theme lorem-ipsum linum-relative leuven-theme less-css-mode ledger-mode kooten-theme json-mode js-comint jedi jdee indent-tools indent-guide impatient-mode hlinum hledger-mode hl-todo hl-line+ highlight-symbol highlight-parentheses highlight-indent-guides highlight-blocks hideshow-org helm-robe helm-projectile helm-package helm-mode-manager helm-git helm-fuzzy-find helm-fuzzier helm-flyspell helm-flymake helm-flx helm-emmet helm-dictionary helm-dash helm-css-scss helm-anything helm-ag helm-ack handlebars-sgml-mode handlebars-mode hackernews gruvbox-theme grunt golden-ratio go-eldoc go gmail-message-mode github-theme github-pullrequest git-timemachine git-gutter git-gutter+ git gist fuzzy flymake-ruby figlet fancy-narrow exec-path-from-shell etable eslint-fix eshell-prompt-extras ergoemacs-mode erc-crypt encourage-mode emojify emoji-fontset emoji-cheat-sheet-plus elpy elisp-lint editorconfig dumb-jump drag-stuff dracula-theme discover-js2-refactor dired-ranger dired-k dired+ diff-hl dictionary deft dash-at-point darkburn-theme csv-mode counsel company-web company-php company-jedi company-go company-flx color-theme-sanityinc-tomorrow color-theme coffee-mode circe chess calfw cabledolphin bongo blgrep blackboard-theme bitly autopair aurora-theme atom-one-dark-theme atom-dark-theme apache-mode anzu anaphora anaconda-mode adoc-mode ac-inf-ruby)))
 '(pdf-view-midnight-colors (quote ("#969896" . "#f8eec7")))
 '(pos-tip-background-color "#303030")
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "dist" "public")))
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(uniquify-buffer-name-style nil nil (uniquify))
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
 '(vc-annotate-very-old-color nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-expanding t)
 '(web-mode-enable-current-element-highlight t)
 '(xterm-color-names
   ["#303030" "#D66F84" "#D79887" "#D49A8A" "#94B1A3" "#A8938C" "#989584" "#BAB2A9"])
 '(xterm-color-names-bright
   ["#3A3A3A" "#E47386" "#CC816B" "#769188" "#7D6F6A" "#9C8772" "#BAB2A9"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
