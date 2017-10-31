;; javascript.el

(require 'rjsx-mode)

(add-hook 'rjsx-mode-hook 'drag-stuff-mode)
(add-hook 'rjsx-mode-hook 'linum-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'hl-line-mode)

(setq js-indent-level 2)

(add-hook 'js2-mode-hook 'prettier-js-mode)

(defun rjsx-mode-check ()
  "Checking if is JSX..."
  (when (string-match "import" (buffer-string))
    (message "boom")))

(add-hook 'js2-mode-hook 'rjsx-mode-check)
