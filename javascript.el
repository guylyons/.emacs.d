;; javascript.el
;; ------------------------------
;; Author: gl
;; Email: guylyons@protonmail.com
;; ------------------------------
;;

;; Web Dev packages
(add-hook 'js2-mode-hook 'drag-stuff-mode)
(add-hook 'js2-mode-hook
	  (lambda () (flycheck-mode t)))
(add-hook 'js2-mode-hook 'company-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'js2-mode-hook 'linum-mode)


(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

(require 'js2-mode)

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

