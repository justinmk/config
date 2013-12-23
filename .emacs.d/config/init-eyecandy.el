(require-package 'smart-mode-line)
(require 'smart-mode-line)
(setq sml/theme 'dark)
(sml/setup)


(require-package 'diminish)
(after 'diminish-autoloads
  (diminish 'visual-line-mode)
  (after 'undo-tree (diminish 'undo-tree-mode))
  (after 'auto-complete (diminish 'auto-complete-mode))
  (after 'projectile (diminish 'projectile-mode))
  (after 'guide-key (diminish 'guide-key-mode))
  (after 'eldoc (diminish 'eldoc-mode))
  (after 'smartparens (diminish 'smartparens-mode))
  (after 'company (diminish 'company-mode))
  (after 'git-gutter+ (diminish 'git-gutter+-mode))
  (after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
  )


(pcase (window-system)
  (`ns (set-face-attribute 'default nil :height 135))
  (`w32 (set-face-attribute 'default nil
                        :family "Consolas" :height 110)))

(global-hl-line-mode +1)

;; force sRGB on Mac OS X   http://www.reddit.com/r/emacs/comments/1thijn/
(setq ns-use-srgb-colorspace t)

;; (require-package 'color-theme-sanityinc-tomorrow)
;; (require-package 'zenburn-theme)
(require-package 'moe-theme)

(after 'moe-theme-autoloads
  ;; (load-theme 'sanityinc-tomorrow-night)
  (load-theme 'moe-dark t))

;; fight the "low contrast" fad.
(set-face-attribute 'default nil
                    :background "#222222"
                    :foreground "white")


(provide 'init-eyecandy)
