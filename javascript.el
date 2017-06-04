;; javascript.el
;; ------------------------------
;; Author: gl
;; Email: guylyons@protonmail.com
;; ------------------------------
;;

(require 'js2-mode)

(add-hook 'js2-mode-hook 'drag-stuff-mode)
(add-hook 'js2-mode-hook 'linum-mode)

(setq js-indent-level 2)

(setq js2-highlight-level 3)
