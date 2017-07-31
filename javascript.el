;; javascript.el

(require 'rjsx-mode)

(add-hook 'rjsx-mode-hook 'drag-stuff-mode)
(add-hook 'rjsx-mode-hook 'linum-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'hl-line-mode)
(add-hook 'rjsx-mode-hook 'indent-guide-mode)
(add-hook 'rjsx-mode-hook 'auto-complete-mode)

(setq js-indent-level 2)
