(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

(add-to-list 'load-path user-emacs-directory) 
(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(setq package-enable-at-startup nil)

(require 'init-util)
(require 'init-core)
(require 'init-eyecandy)

(require 'init-editor)
(require 'init-smartparens)

; (require 'init-yasnippet)
(require 'init-auto-complete)
; (require 'init-company)

(require 'init-projectile)
(require 'init-helm)
(require 'init-ido)
(require 'init-speedbar)

(require 'init-git)
(require 'init-flycheck)

(require 'init-vim)
(require 'init-web)
(require 'init-lisp)
(require 'init-markdown)

(require 'init-evil)
(require 'init-bindings)

(setq ring-bell-function 'ignore)
;;; flash top/bottom line only
; (setq visible-bell 'top/bottom)

;; session save/restore
;; TODO: this always complains about the lock file it _just_ created...
;; (desktop-save-mode 1)
