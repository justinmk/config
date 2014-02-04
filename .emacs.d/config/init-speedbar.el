(require-package 'sr-speedbar)
(require 'sr-speedbar)

(setq speedbar-show-unknown-files t)
(sr-speedbar-refresh-turn-off)

(add-hook 'speedbar-mode-hook
          (lambda () (setq truncate-lines t)))

(provide 'init-speedbar)
