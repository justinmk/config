(require-package 'company)
(require-package 'company-cider)
(require 'company)

(setq company-auto-complete t)
(setq company-global-modes t)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)
(setq company-show-numbers t)
(setq company-tooltip-limit 30)

(add-hook 'cider-repl-mode-hook (add-to-list 'company-backends 'company-cider))
(add-hook 'cider-mode-hook (add-to-list 'company-backends 'company-cider))
(after 'ac-js2-autoloads
  (add-to-list 'company-backends 'ac-js2-company))

(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-company)
