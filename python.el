(elpy-enable)
(require 'ob-python)
(require 're-builder)

(setq reb-re-syntax 'string)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")
