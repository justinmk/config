(require-package 'undo-tree)
(require 'undo-tree)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist
      `((,(concat user-emacs-directory ".cache/undo") . ".")))
(global-undo-tree-mode)

(add-hook 'text-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq tab-width 4)
             (setq indent-line-function (quote insert-tab))))

(require-package 'multiple-cursors)
(require 'multiple-cursors)
(after 'evil
  (add-hook 'multiple-cursors-mode-enabled-hook 'evil-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook 'evil-normal-state))

(require-package 'expand-region)
(setq expand-region-fast-keys-enabled nil)

(provide 'init-editor)
