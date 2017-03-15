(defun webstart ()

  "Starting up project..."
  (interactive)
  (helm-projectile-switch-project)
  
  (progn
    (split-window-below)
    (sane-term-create)
    (split-window-right)
    (projectile-find-file (app.js)))

  (projectile-run-project (gulp serve)))
