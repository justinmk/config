(require-package 'smartparens)
(require 'smartparens-config)

(setq sp-autoskip-closing-pair 'always
      sp-show-pair-delay 0
      sp-show-pair-from-inside t)

(smartparens-global-mode t)
(show-smartparens-global-mode)
(show-paren-mode -1) ; not needed with smartparens

(defun my-open-block-c-mode (id action context)
  (when (eq action 'insert)
    (newline)
    (indent-according-to-mode)
    (forward-line -1) ;(previous-line)
    (indent-according-to-mode)))

;; auto-pair {} and [] everywhere
(sp-pair "{" nil :post-handlers '(:add (my-open-block-c-mode "RET")))
(sp-pair "[" nil :post-handlers '(:add (my-open-block-c-mode "RET")))

;; http://stackoverflow.com/a/2665369/152142
(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens)
(defun conditionally-enable-smartparens ()
  "enable smartparens-mode during eval-expression"
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(provide 'init-smartparens)
