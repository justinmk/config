(require-package 'rainbow-delimiters)
(require-package 'elisp-slime-nav)
(require-package 'clojure-mode)
(require-package 'cider)
(require-package 'ac-nrepl)
(require 'ac-nrepl)

;; rainbows everywhere!
(global-rainbow-delimiters-mode)

(defun my-lisp-hook ()
  (progn
    (elisp-slime-nav-mode)
    (turn-on-eldoc-mode)))

(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-hook)
(add-hook 'ielm-mode-hook 'my-lisp-hook)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)

(provide 'init-lisp)
