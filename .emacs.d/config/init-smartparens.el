(require-package 'smartparens)
(require 'smartparens-config)

(setq sp-autoskip-closing-pair 'always
      sp-show-pair-delay 0
      sp-show-pair-from-inside t)

(smartparens-global-mode t)
(show-smartparens-global-mode)

(defun my-open-block-c-mode (id action context)
  (when (eq action 'insert)
    (newline)
    (indent-according-to-mode)
    (previous-line)
    (indent-according-to-mode)))

(sp-local-pair 'js2-mode "{" nil :post-handlers '(:add (my-open-block-c-mode "C-j")))

;; http://stackoverflow.com/a/2665369/152142
(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens)
(defun conditionally-enable-smartparens ()
  "enable smartparens-mode during eval-expression"
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(provide 'init-smartparens)
