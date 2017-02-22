;; JavaScript configuration

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

