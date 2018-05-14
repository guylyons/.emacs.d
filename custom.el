(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#424242" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#eaeaea"))
 '(beacon-color "#f2777a")
 '(compilation-message-face 'default)
 '(css-indent-offset 2)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-mode t nil (cua-base))
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-type 'bar)
 '(custom-enabled-themes '(rebecca))
 '(custom-safe-themes
   '("ed0b4fc082715fc1d6a547650752cd8ec76c400ef72eb159543db1770a27caa7" "021720af46e6e78e2be7875b2b5b05344f4e21fad70d17af7acfd6922386b61e" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "10a0ea0ae9fd29052b2a485dd32f34c2796de9e3cb5c92ec850188cd852a158a" "72a097f48e588eaa08b17027ac20304dd3b3ea8ceaca4ca553fb2577b64f4d09" default))
 '(display-battery-mode t)
 '(emmet-move-cursor-after-expanding t t)
 '(erc-hl-nicks-mode t)
 '(erc-scrolltobottom-mode t)
 '(fci-rule-color "#eee8d5")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(fringe-mode 0 nil (fringe))
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-sexp-background-color "#1c1f26")
 '(indicate-empty-lines t)
 '(kaolin-hl-line-colored t)
 '(kaolin-themes-hl-line-colored t)
 '(linum-format "%d ")
 '(magit-diff-use-overlays nil)
 '(muse-project-alist nil)
 '(neo-show-hidden-files nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(org-confirm-babel-evaluate nil)
 '(package-selected-packages
   '(format-all github-stars git-lens git-gutter-fringe doom-themes all-the-icons key-chord rubocop which-key ac-c-headers vlf moe-theme erc-scrolltoplace erc-colorize erc-hl-nicks helm-books helm-c-moccur helm-company helm-helm-commands helm-pass helm-rails helm-swoop bbdb helm-bbdb easy-hugo cmake-mode cmake-ide kaolin-themes exwm highlight2clipboard emms calfw-ical calfw-cal calfw-org zenburn-theme yari xah-lookup xah-fly-keys writeroom-mode writegood-mode web-mode web-beautify vue-mode use-package url-shortener unicode-fonts twittering-mode treemacs-projectile tramp-term thesaurus textmate tern-auto-complete term+ tabbar-ruler syntactic-close synonyms swoop swiper-helm svg sublime-themes stem ssh sqlup-mode sos solarized-theme smex smartparens smart-mode-line-powerline-theme smart-forward smart-compile slime-theme slime shell-pop scss-mode sane-term rvm ruby-refactor robe rjsx-mode rinari restclient-test replace-pairs replace+ regex-tool rebecca-theme ranger rainbow-mode rainbow-delimiters python-x python-mode pytest projectile-speedbar projectile-rails prettier-js pastelmac-theme password-vault pass paper-theme pandoc-mode osx-lib osx-dictionary org-seek org-random-todo org-password-manager org-journal org-ac oceanic-theme ob-ipython oauth2 nyan-mode nodejs-repl nlinum-hl neotree names muse multi-term move-text meacupla-theme material-theme markdown-preview-mode markdown-mode+ majapahit-theme lorem-ipsum linum-relative leuven-theme less-css-mode ledger-mode kooten-theme json-mode js-comint jedi jdee indent-tools indent-guide impatient-mode hlinum hledger-mode hl-todo hl-line+ highlight-symbol highlight-parentheses highlight-indent-guides highlight-blocks hideshow-org helm-robe helm-projectile helm-package helm-mode-manager helm-git helm-fuzzy-find helm-fuzzier helm-flyspell helm-flymake helm-flx helm-emmet helm-dictionary helm-dash helm-css-scss helm-anything helm-ag helm-ack handlebars-sgml-mode handlebars-mode hackernews gruvbox-theme grunt golden-ratio go-eldoc go gmail-message-mode github-theme github-pullrequest git-timemachine git-gutter git-gutter+ git gist fuzzy flymake-ruby figlet fancy-narrow exec-path-from-shell etable eslint-fix eshell-prompt-extras ergoemacs-mode erc-crypt encourage-mode emojify emoji-fontset emoji-cheat-sheet-plus elpy elisp-lint editorconfig dumb-jump drag-stuff dracula-theme discover-js2-refactor dired-ranger dired-k dired+ diff-hl dictionary deft dash-at-point darkburn-theme csv-mode counsel company-web company-php company-jedi company-go company-flx color-theme-sanityinc-tomorrow color-theme coffee-mode circe chess calfw cabledolphin bongo blgrep blackboard-theme bitly autopair aurora-theme atom-one-dark-theme atom-dark-theme apache-mode anzu anaphora anaconda-mode adoc-mode ac-inf-ruby))
 '(pdf-view-midnight-colors '("#969896" . "#f8eec7"))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "dist" "public"))
 '(sane-term-shell-command "/bin/zsh")
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tramp-syntax 'default nil (tramp))
 '(uniquify-buffer-name-style nil nil (uniquify))
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#c9485ddd1797")
     (60 . "#bf7e73b30bcb")
     (80 . "#b58900")
     (100 . "#a5a58ee30000")
     (120 . "#9d9d91910000")
     (140 . "#9595943e0000")
     (160 . "#8d8d96eb0000")
     (180 . "#859900")
     (200 . "#67119c4632dd")
     (220 . "#57d79d9d4c4c")
     (240 . "#489d9ef365ba")
     (260 . "#3963a04a7f29")
     (280 . "#2aa198")
     (300 . "#288e98cbafe2")
     (320 . "#27c19460bb87")
     (340 . "#26f38ff5c72c")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-expanding t)
 '(web-mode-enable-current-element-highlight t)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
