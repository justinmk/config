(require 'speedbar)

(setq speedbar-show-unknown-files t)

(add-hook 'speedbar-mode-hook
          (lambda () (setq truncate-lines t)))

(provide 'init-speedbar)
