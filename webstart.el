(defun eshell-window-new ()
  "Creating an Eshell window, sir."
  (interactive)
  (progn
    (split-window-below)
    (eshell)))

(defun open-project ()
  "Opening a new project..."
  (interactive)
  (delete-other-windows)
  (helm-projectile "~/github")
  (neotree))

(eval-after-load "helm-regexp"
    '(setq helm-source-moccur
           (helm-make-source "Moccur"
               'helm-source-multi-occur :follow 1)))

(defun my-helm-multi-all ()
  "multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
         (mapcar (lambda (b)
                   (when (buffer-file-name b) (buffer-name b)))
                 (buffer-list)))))
