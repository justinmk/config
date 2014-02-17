(require-package 'helm)
(require-package 'helm-swoop)

(after 'projectile
  (require-package 'helm-projectile))

(require 'helm-config)
(require 'helm-swoop)
; enable partial matches separated by SPC
(helm-match-plugin-mode)

(setq helm-command-prefix-key "C-c h")
(setq helm-quick-update t)

(provide 'init-helm)
