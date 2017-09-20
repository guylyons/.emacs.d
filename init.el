;; init.el
;; 

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24) 
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; (add-to-list 'load-path "~/.emacs.d/elpa/async-20150203.2127")
(add-to-list 'load-path "~/.emacs.d/lisp/helm/")

;; configure path
(defconst user-init-dir 
  (cond ((boundp 'user-emacs-directory) user-emacs-directory) 
        ((boundp 'user-init-directory) user-init-directory) 
        (t "~/.emacs.d/")))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

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

(nyan-mode 1)
(projectile-global-mode)
(drag-stuff-global-mode)
(autopair-global-mode)
(ivy-mode 1)
(add-hook 'after-init-hook 'global-company-mode)

(require 'company)
(require 'company-web-html)
(setq company-tooltip-limit 20)         ; bigger popup window
(setq company-tooltip-align-annotations 't) ; align annotations to the right tooltip border
(setq company-idle-delay .3) ; decrease delay before autocompletion popup shows
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(global-set-key (kbd "C-c /") 'company-files)

(add-hook 'web-mode-hook (lambda () 
                           (set (make-local-variable 'company-backends) 
                                '(company-web-html)) 
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
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; c-mode
(require 'cc-mode)
(define-key c-mode-base-map (kbd "C-x c") 'compile)
(add-hook 'c-mode-hook 'linum-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

;; use visual line mode with erc. looks better
(add-hook 'erc-mode-hook 'visual-line-mode)

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

(add-hook 'emmet-mode-hook (lambda () 
                             (setq emmet-indent-after-insert nil)))
(add-hook 'emmet-mode-hook (lambda () 
                             (setq emmet-indentation 2))) ;; indent 2 spaces.
(setq emmet-move-cursor-between-quotes t)    ;; default nil
(setq emmet-move-cursor-after-expanding nil) ;; default

;; Web-mode
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'drag-stuff-mode)
(add-hook 'web-mode-hook 'toggle-truncate-lines)
(add-hook 'web-mode-hook 'linum-mode)
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))

(autoload 'scss-mode "scss-mode")

(setq default-tab-width 2)

(defun my-web-mode-hook () 
  "Hooks for Web mode." 
  (setq web-mode-markup-indent-offset 2) 
  (setq web-mode-css-indent-offset 2) 
  (setq web-mode-code-indent-offset 2) 
  (setq web-mode-indent-style 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)

(with-eval-after-load 'scss-mode (define-key scss-mode-map (kbd "C-c u") 'helm-css-scss))

(defvar css-indent-offset 2)

(setq debug-on-error t)

(require 'drag-stuff)
(require 'flycheck)
(require 'less-css-mode)
(require 'web-mode)
(require 'autopair)

(add-to-list 'default-frame-alist '(font . "Source Code Pro-15" ))
(set-face-attribute 'default t 
                    :font "Source Code Pro-15")

;; no backups
(setq make-backup-files nil)          ; stop creating backup~ files
(setq auto-save-default nil)          ; stop creating #autosave# files
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-auto-complete t)
 '(company-begin-commands (quote (self-insert-command)))
 '(company-dabbrev-minimum-length 1)
 '(company-idle-delay 0.3)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 20)
 '(compilation-message-face (quote default))
 '(css-indent-offset 2)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-mode t nil (cua-base))
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (smart-mode-line-light)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "12ae26f3493216be1bc0bbd28732671e8672bc3c631f1cea042a1040b136058a" "8ec2e01474ad56ee33bc0534bdbe7842eea74dccfb576e09f99ef89a705f5501" "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "fe1682ca8f7a255cf295e76b0361438a21bb657d8846a05d9904872aa2fb86f2" "1012cf33e0152751078e9529a915da52ec742dabf22143530e86451ae8378c1a" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "ba7917b02812fee8da4827fdf7867d3f6f282694f679b5d73f9965f45590843a" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "a4c9e536d86666d4494ef7f43c84807162d9bd29b0dfd39bdf2c3d845dcc7b2e" "4e4d9f6e1f5b50805478c5630be80cce40bee4e640077e1a6a7c78490765b03f" "3b5ce826b9c9f455b7c4c8bff22c020779383a12f2f57bf2eb25139244bb7290" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "d5b121d69e48e0f2a84c8e4580f0ba230423391a78fcb4001ccb35d02494d79e" "790e74b900c074ac8f64fa0b610ad05bcfece9be44e8f5340d2d94c1e47538de" "44c566df0e1dfddc60621711155b1be4665dd3520b290cb354f8270ca57f8788" "0f97285f9e0c7d9cad04f2130859d20d6c9b3142877b2bca52d958f4f1cf346f" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "2f427a54ff2c7ae9e61fde650037c1590e35922fefafa47c162a94e4d6d422a6" "14e6b27c801eece344f7b11337076d6e56ff444588c5f51c5f14854befc6c3b1" "7ed277f12c9fd279f258a34cc618d203243dd56e7f37feac892bb26ad7a1cad2" "2f2d9b9827bcfd4dab08df6e0f5cbf347b65c8fc112d947cf696234b80261ebf" "54dd7c04a3fcebbf69ba8f9a9f116699e7247b6fdbd46a917914364426634747" "332b53f3331d7280557d79b3398e376fc290c299ac1c4870d51b9019c2831362" "18a393331877d6332e859d36ec6abf755f560437c2b14f4513a0745ff0d420a5" "8ccfc8f0da78d622fd390e84fdc955e404478b4a6c97a421fcea2b914bdea5ff" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "a74c967cabd9218c477507ba82eee52cb5683078e2a39c8ef90c051a692aea80" "ee0e801a44255007eb4ad1d9aebc923f237ffcf5101ad35f131e1a4b062ff6f4" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "47744f6c8133824bdd104acc4280dbed4b34b85faa05ac2600f716b0226fb3f6" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(emmet-indent-after-insert t)
 '(emmet-indentation 2)
 '(emmet-move-cursor-after-expanding t t)
 '(emmet-move-cursor-between-quotes t t)
 '(emmet-preview-default nil)
 '(erc-autojoin-channels-alist
   (quote
    (("freenode.net" "##javascript" "#wordpress" "#javascript" "#emacs"))))
 '(erc-autojoin-mode t)
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services smiley stamp spelling track)))
 '(erc-nick "cstls")
 '(fci-rule-color "#383838")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote light))
 '(fringe-mode 15 nil (fringe))
 '(global-company-mode t)
 '(helm-always-two-windows nil)
 '(helm-full-frame nil)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-symbol-idle-delay 0)
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-sexp-background-color "#1c1f26")
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(linum-format "%3d  ")
 '(magit-diff-use-overlays nil)
 '(magit-dispatch-arguments nil)
 '(main-line-color1 "#222232")
 '(main-line-color2 "#333343")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(nyan-animate-nyancat t)
 '(nyan-cat-face-number 3)
 '(nyan-mode t)
 '(nyan-wavy-trail nil)
 '(package-selected-packages
   (quote
    (ivy-youtube xo elisp-format clues-theme gotham-theme smart-comment zenity-color-picker erc-colorize datetime restclient-helm elnode sublimity browse-kill-ring+ rebecca-theme babel babel-repl helm-spotify helm-youtube ac-js2 xah-elisp-mode highlight-unique-symbol date-field date-at-point zone-rainbow multi-eshell eshell-git-prompt bbdb github-modern-theme auto-highlight-symbol rjsx-mode jsx-mode prettier-js yaml-mode top-mode nlinum-hl nlinum ac-inf-ruby gist git-timemachine github-pullrequest github-theme company-web counsel use-package hledger-mode zenburn-theme yari xah-lookup xah-fly-keys writeroom-mode writegood-mode web-mode web-beautify vue-mode url-shortener unicode-fonts twittering-mode tramp-term thesaurus textmate tern-auto-complete tabbar-ruler syntactic-close synonyms swoop swiper-helm svg sublime-themes stem ssh sqlup-mode sos solarized-theme smex smartparens smart-mode-line-powerline-theme smart-forward smart-compile slime-theme slime slack shell-pop scss-mode sane-term rvm ruby-refactor robe rinari restclient replace-pairs replace+ regex-tool ranger rainbow-delimiters python-x python-mode pytest projectile-speedbar projectile-rails pastelmac-theme password-vault paper-theme pandoc-mode osx-lib osx-dictionary org-seek org-random-todo org-password-manager org-journal org-ac oceanic-theme ob-ipython nyan-mode nodejs-repl neotree names muse multi-term move-text meacupla-theme material-theme markdown-preview-mode markdown-mode+ majapahit-theme lorem-ipsum linum-relative leuven-theme less-css-mode ledger-mode kooten-theme json-mode js-comint jedi jdee indent-tools indent-guide impatient-mode hlinum hl-todo highlight-symbol highlight-parentheses highlight-indent-guides hideshow-org helm-robe helm-projectile helm-package helm-mode-manager helm-git helm-fuzzy-find helm-fuzzier helm-flyspell helm-flymake helm-flx helm-emmet helm-dictionary helm-dash helm-css-scss helm-anything helm-ag helm-ack handlebars-sgml-mode handlebars-mode hackernews grunt golden-ratio go-eldoc go gmail-message-mode git-gutter git-gutter+ git fuzzy flymake-ruby figlet fancy-narrow exec-path-from-shell etable eslint-fix eshell-prompt-extras ergoemacs-mode erc-crypt encourage-mode emoji-fontset emoji-cheat-sheet-plus elpy elisp-lint editorconfig dumb-jump drag-stuff dracula-theme discover-js2-refactor dired-ranger dired-k dired+ diff-hl dictionary deft dash-at-point darkburn-theme csv-mode company-php company-jedi company-flx color-theme-sanityinc-tomorrow color-theme coffee-mode chess calfw cabledolphin bongo blgrep blackboard-theme bitly avy autopair aurora-theme atom-one-dark-theme atom-dark-theme apache-mode anzu anaphora anaconda-mode adoc-mode)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(powerline-color1 "#222232")
 '(powerline-color2 "#333343")
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/mode-width
   (if
       (eq
        (powerline-current-separator)
        (quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote sml/global))))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote sml/global)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active2)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-background-mode nil)
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
 '(vc-annotate-very-old-color "#DC8CC3")
 '(web-mode-auto-close-style 1)
 '(web-mode-commands-like-expand-region
   (quote
    (web-mode-mark-and-expand emmet-expand-line mc/mark-next-like-this)))
 '(web-mode-jsx-depth-faces t)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-symbol-face ((t (:background "#ddd" :foreground "#d33682"))))
 '(swiper-minibuffer-match-face-1 ((t :background "#dddddd")))
 '(swiper-minibuffer-match-face-2 ((t :background "#bbbbbb" :weight bold)))
 '(swiper-minibuffer-match-face-3 ((t :background "#bbbbff" :weight bold)))
 '(swiper-minibuffer-match-face-4 ((t :background "#ffbbff" :weight bold))))
