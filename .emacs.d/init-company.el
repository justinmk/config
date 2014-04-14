(require-package 'company)
(require-package 'company-cider)
(require 'company)

(setq company-auto-complete t)
(setq company-global-modes t)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)
(setq company-show-numbers t)
(setq company-tooltip-limit 30)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)

(add-hook 'cider-repl-mode-hook (add-to-list 'company-backends 'company-cider))
(add-hook 'cider-mode-hook (add-to-list 'company-backends 'company-cider))
(after 'ac-js2-autoloads
  (add-to-list 'company-backends 'ac-js2-company))

(add-hook 'after-init-hook 'global-company-mode)

(set-face-attribute 'company-tooltip nil :background "black" :foreground "white")
(set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "DodgerBlue" :foreground "white")
(set-face-attribute 'company-preview nil :background "black")
(set-face-attribute 'company-preview-common nil :inherit 'company-preview :foreground "white")
(set-face-attribute 'company-tooltip-common nil :inherit 'company-tooltip :foreground "green")
(set-face-attribute 'company-tooltip-common-selection nil :inherit 'company-tooltip-selection :foreground "white")
(set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "gray20")
(set-face-attribute 'company-scrollbar-fg nil :background "gray40")

(provide 'init-company)
