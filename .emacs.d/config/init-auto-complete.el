(require-package 'auto-complete)
(require 'auto-complete)
(require 'auto-complete-config)

(setq ac-auto-show-menu t)
(setq ac-auto-start t)
(setq ac-comphist-file (concat user-emacs-directory ".cache/ac-comphist.dat"))
(setq ac-quick-help-delay 0.3)
;; (setq ac-delay 0.0)
;; (setq ac-use-fuzzy t)
(setq ac-quick-help-height 30)
(setq ac-show-menu-immediately-on-auto-complete t)

(define-key ac-complete-mode-map (kbd "SPC") 'ac-complete)

(eval-after-load "auto-complete"
  (dolist (mode '(vimrc-mode
                  html-mode stylus-mode cider-repl-mode

                  ;; shell-mode term-mode terminal-mode eshell-mode comint-mode
                  ))
    (add-to-list 'ac-modes mode)))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(ac-config-default)

(after 'linum
  (ac-linum-workaround))

(provide 'init-auto-complete)
