(require-package 'magit)
(require-package 'git-messenger)

(setq magit-diff-options '("--histogram"))

(require 'git-messenger)
(setq git-messenger:show-detail t)

(if (display-graphic-p)
    (progn
      (require-package 'git-gutter-fringe+)
      (require 'git-gutter-fringe+)
      (git-gutter+-toggle-fringe))
  (require-package 'git-gutter+))

(after 'git-gutter+-autoloads
  (global-git-gutter+-mode))

(provide 'init-git)
