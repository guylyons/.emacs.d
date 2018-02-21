;; Ruby.el

(setq ruby-indent-level 2)

(require 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)
