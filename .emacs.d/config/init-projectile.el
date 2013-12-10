(require-package 'projectile)
(require 'projectile)

(setq projectile-cache-file (concat user-emacs-directory ".cache/projectile.cache"))
(setq projectile-known-projects-file (concat user-emacs-directory ".cache/projectile-bookmarks.eld"))

(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)

(add-to-list 'projectile-globally-ignored-directories "elpa")
(add-to-list 'projectile-globally-ignored-directories ".cache")
(add-to-list 'projectile-globally-ignored-directories "node_modules")

(projectile-global-mode t)

(provide 'init-projectile)
