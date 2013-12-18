(require-package 'auto-complete)
(require 'auto-complete)
(require 'auto-complete-config)

(setq ac-auto-show-menu t)
(setq ac-auto-start t)
(setq ac-comphist-file (concat user-emacs-directory ".cache/ac-comphist.dat"))
(setq ac-quick-help-delay 0.3)
(setq ac-quick-help-height 30)
(setq ac-show-menu-immediately-on-auto-complete t)

(define-key ac-complete-mode-map (kbd "SPC") 'ac-complete)
(dolist (mode '(vimrc-mode
                html-mode stylus-mode
                ;; shell-mode term-mode terminal-mode eshell-mode comint-mode
                ))
  (add-to-list 'ac-modes mode))

(custom-set-faces
 '(ac-selection-face ((t (:foreground "white" :background "DodgerBlue" )))))

(ac-config-default)

(after 'linum
  (ac-linum-workaround))

(provide 'init-auto-complete)
