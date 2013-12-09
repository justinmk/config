(require-package 'rainbow-delimiters)
(require-package 'cider)

(defun my-lisp-hook ()
  (progn
    (turn-on-eldoc-mode)
    (rainbow-delimiters-mode t)))

(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-hook)
(add-hook 'ielm-mode-hook 'my-lisp-hook)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(provide 'init-lisp)
