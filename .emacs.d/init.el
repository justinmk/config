;; Windows build (includes libxml2, gnutls, etc):
;;   http://emacsbinw64.sourceforge.net/
;;   http://semantic.supelec.fr/popineau/programming-emacs.html
;;
;; use-package: https://github.com/vermiculus/dotfiles/blob/de9deb57317c7a10dd533e0e80a1d90a9b97f640/.emacs.d/init.el#L313
;; TODO: https://github.com/skeeto/.emacs.d#skewer

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; disable menu except on Mac OS X
(when (fboundp 'menu-bar-mode) (unless (and (window-system) (eq system-type 'darwin)) (menu-bar-mode -1)))

(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(require 'package)
(require 'package-helper)
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(require 'init-util)
(require 'init-core)

;; VCS
(with-package* (magit git-messenger)
  (setq magit-diff-options '("--histogram"))
  (setq git-messenger:show-detail t))
(if (display-graphic-p)
    (progn
      (with-package* git-gutter-fringe+
        (git-gutter+-toggle-fringe)))
    (with-package git-gutter+*
      (after 'git-gutter+-autoloads
        (global-git-gutter+-mode))))

(with-package* (smart-mode-line moe-theme)
  ;; (with 'color-theme-sanityinc-tomorrow)
  ;; (with 'zenburn-theme)

  (moe-dark)
  (moe-theme-set-color 'w/b)

  (custom-set-faces
   ;; init file should contain only one custom-set-faces. If there is more than one, they won't work right.
   '(ac-selection-face ((t (:foreground "white" :background "DodgerBlue"))))
   '(cursor ((t (:foreground "white" :background "DodgerBlue")))) ;'(cursor ((t (:background "chartreuse"))))
   '(hl-line ((t (:inherit highlight :background "#333333"))))
   '(show-paren-match ((t (:background "#4e4e4e"))))
   '(sp-show-pair-match-face ((t (:underline "Green"))))
   '(vertical-border ((t (:background "#8a8a8a" :foreground "lemon chiffon")))))

  ;; fight the "low contrast" fad.
  (set-face-attribute 'default nil
                      :background "#1B1D1E"
                      :foreground "white")

  (pcase (window-system)
    (`ns (set-face-attribute 'default nil :height 135))
    (`w32 (set-face-attribute 'default nil
                              :family "Consolas" :height 110)))

  (global-hl-line-mode +1)
)

;; shell
;; (with-package multi-term)
;;   (setq multi-term-program "cmd.exe")

(with-package exec-path-from-shell ;; make sure $PATH is set correctly
  (ignore-errors ;; windows
    (exec-path-from-shell-initialize)))

;; editor
(with-package* undo-tree
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        `((,(concat user-emacs-directory ".cache/undo") . ".")))
  (global-undo-tree-mode))

(add-hook 'text-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq tab-width 4)
             (setq indent-line-function 'insert-tab)))

(with-package expand-region
  (setq expand-region-fast-keys-enabled nil))

(with-package highlight-parentheses
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode
    (lambda ()
      (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t)
  ;; (add-hook 'find-file-hook
  ;;           'highlight-parentheses-mode
)

(with-package* smartparens

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
)

(with-package* (company company-quickhelp pos-tip company-cider)
  (setq company-auto-complete t)
  (setq company-global-modes t)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 30)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)

  (company-quickhelp-mode 1)

  (add-hook 'cider-repl-mode-hook (add-to-list 'company-backends 'company-cider))
  (add-hook 'cider-mode-hook (add-to-list 'company-backends 'company-cider))
  (after 'ac-js2-autoloads
    (add-to-list 'company-backends 'ac-js2-company))

  (add-hook 'after-init-hook 'global-company-mode)

  (set-face-attribute 'company-tooltip nil :background "black" :foreground "white")
  (set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "DodgerBlue" :foreground "white")
  (set-face-attribute 'company-preview nil :background "black")
  (set-face-attribute 'company-preview-common nil :inherit 'company-preview :foreground "white")
  (set-face-attribute 'company-tooltip-common nil :inherit 'company-tooltip :foreground "green")
  (set-face-attribute 'company-tooltip-common-selection nil :inherit 'company-tooltip-selection :foreground "white")
  (set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "gray20")
  (set-face-attribute 'company-scrollbar-fg nil :background "gray40")
)

(with-package* (helm helm-config helm-swoop pt)
  ;; enable partial matches separated by SPC
  (helm-match-plugin-mode)

  (setq helm-command-prefix-key "C-c h")
  (setq helm-quick-update t)
  (setq helm-buffers-fuzzy-matching t))

;; ido-mode
(with-package* ido
  (setq ido-enable-prefix nil)
  (setq ido-use-virtual-buffers t)
  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'prompt)
  (setq ido-use-filename-at-point 'guess)
  (setq ido-save-directory-list-file (concat user-emacs-directory ".cache/ido.last"))
  (ido-mode t)
  (ido-everywhere t))
(with-package ido-ubiquitous
  (ido-ubiquitous-mode t))
(with-package flx-ido
  (flx-ido-mode t))
(with-package* ido-vertical-mode
  (ido-vertical-mode))
(with-package* smex
  (setq smex-save-file (concat user-emacs-directory ".cache/smex-items"))
  (smex-initialize)
  (setq smex-history-length 100))

(with-package* speedbar
  (setq speedbar-show-unknown-files t)
  (add-hook 'speedbar-mode-hook
            (lambda () (setq truncate-lines t))))

(with-package flycheck*
  (after 'flycheck
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
    (setq flycheck-checkers (delq 'html-tidy flycheck-checkers)))

  (global-flycheck-mode t))

(with-package (js2-mode* ac-js2 tern* tern-auto-complete jade-mode skewer-mode web-mode)
  (setq js2-highlight-level 3)
  (setq-default js2-basic-offset 2)
  (after 'js2-mode-autoloads
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  (after 'tern
    (after 'auto-complete
      (require 'tern-auto-complete)
      (tern-ac-setup)))

  (skewer-setup)

  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(with-package (lua-mode flymake-lua))

(with-package* (elisp-slime-nav clojure-mode cider ac-cider)

  (defun my-lisp-hook ()
    (progn
      (elisp-slime-nav-mode)
      (turn-on-eldoc-mode)))

  (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
  (add-hook 'lisp-interaction-mode-hook 'my-lisp-hook)
  (add-hook 'ielm-mode-hook 'my-lisp-hook)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-mode-hook 'ac-cider-setup)
  (add-hook 'cider-repl-mode-hook 'ac-cider-setup)

  (setq nrepl-buffer-name-show-port t)
  (setq cider-repl-history-file "~/.emacs.d/var/cider-repl-history")
  (setq cider-repl-history-size 2000)
  (setq cider-repl-popup-stacktraces t))

(setq evilnc-hotkey-comment-operator "gc") ;must happen before (require 'evil-nerd-commenter)
(with-package* (evil evil-visualstar evil-nerd-commenter evil-args evil-matchit evil-surround evil-jumper)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-w-in-emacs-state t)

  (setq evil-search-module 'evil-search)
  (setq evil-magic 'very-magic)

  (setq evil-emacs-state-cursor '("red" box))
  ;; (setq evil-normal-state-cursor '("green" box))
  ;; (setq evil-insert-state-cursor '("orange" bar))

  (evil-mode t)
  (global-evil-surround-mode 1)

  ;; enable evil-mode everywhere!
  ;; TODO: this is probably more reasonable: https://github.com/prooftechnique/.emacs.d/blob/master/config/jhenahan-evil.el
  ;;       also: https://github.com/edwtjo/evil-org-mode
  (setq evil-normal-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
  (setq evil-emacs-state-modes nil)

  ;;remove RET and SPC from evil-motion-state-map so they are available directly for modes that define them.
  (defun my-move-key (keymap-from keymap-to key)
    "Moves key binding from one keymap to another and delete from the old location. "
    (define-key keymap-to key (lookup-key keymap-from key))
    (define-key keymap-from key nil))
  (my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
  (my-move-key evil-motion-state-map evil-normal-state-map " ")

  (defun my-send-string-to-terminal (string)
    (unless (display-graphic-p) (send-string-to-terminal string)))

)

(require 'init-bindings)

(with-package (markdown-mode ace-jump-mode))

(with-package* (symon)
  (symon-mode))

(with-package* (restclient company-restclient))

(setq ring-bell-function 'ignore)
;;; flash top/bottom line only
; (setq visible-bell 'top/bottom)

;; session save/restore
;; TODO: this always complains about the lock file it _just_ created...
;; (desktop-save-mode 1)
