(defun webstart ()

  "Let's get your web project started..."
  (interactive)
  (helm-projectile-switch-project)
  
  (progn (split-window-below)
	 (sane-term-create)
	 (split-window-right)
	 (projectile-find-file (app.js))
	 )
  
  (global-hl-line-mode 1)

  (projectile-run-project (gulp serve))
  
  )
