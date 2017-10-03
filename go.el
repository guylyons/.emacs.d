;; go.el
;; go lang stuffs


(defun gofmt-before-save-hook ()
  (when eq major-mode 'go-mode)
  (message "Saving and formatting!"))

(add-hook 'before-save-hook (lambda()
                              message "HELOS"))
