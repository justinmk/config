(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      suggest-key-bindings nil)

;; relax the GC frequency
(setq gc-cons-threshold 20000000)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;; move cursor to the last position upon open
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory ".cache/places"))
(setq-default save-place t)


;; minibuffer history
(require 'savehist)
(setq savehist-file (concat user-emacs-directory ".cache/savehist")
      savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60)
(savehist-mode +1)


;; recent files
(require 'recentf)
(setq recentf-save-file (concat user-emacs-directory ".cache/recentf")
      recentf-max-saved-items 1000
      recentf-max-menu-items 500)
(recentf-mode +1)


;; eshell
(setq eshell-directory-name (concat user-emacs-directory ".cache/eshell"))
(setq eshell-aliases-file (concat user-emacs-directory ".eshell-aliases"))


;; erc
(setq erc-log-channels-directory (concat user-emacs-directory ".cache/erc/logs"))


;; vc
(setq vc-make-backup-files t)

;; narrowing
(put 'narrow-to-region 'disabled nil)

;; dired
(require 'dired-x)

;; ediff
(setq ediff-split-window-function 'split-window-horizontally)


;; store most files in the cache
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory ".cache/backups")))
      auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory ".cache/backups") t))
      auto-save-list-file-prefix
      (concat user-emacs-directory ".cache/auto-save-list/.saves-"))


;; better scrolling
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)

;; better buffer names for duplicates
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*" ; leave special buffers alone
      uniquify-after-kill-buffer-p t)


(defalias 'yes-or-no-p 'y-or-n-p)
(xterm-mouse-mode t)


(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq sentence-end-double-space nil
      mark-ring-max 64
      global-mark-ring-max 128
      save-interprogram-paste-before-kill t)

;; highlight entire sexp instead of just the parens.
(show-paren-mode t)
(setq show-paren-style 'expression)

(which-function-mode t)
(global-visual-line-mode)
(global-auto-revert-mode 1)
(electric-indent-mode t)
(blink-cursor-mode -1)
(line-number-mode)
(column-number-mode)
(setq-default indent-tabs-mode nil)

(setq x-select-enable-clipboard t
  save-interprogram-paste-before-kill t
  ;; search everything, including noninteractive functions, non-customizable variables, ...
  apropos-do-all t
  ;; paste at cursor location instead of click location.
  mouse-yank-at-point t)

;; defer font-lock (improves scrolling speed, esp. for large files)
(setq jit-lock-defer-time 0.05)

(add-hook 'find-file-hook
          (lambda ()
            ;; disable word-wrap by default
            (setq truncate-lines t)
            (setq truncate-partial-width-windows nil)
            ;; highlight trailing whitespace in files _only_
            (setq show-trailing-whitespace t)))

;; disable expensive features for very large files
;;     http://stackoverflow.com/a/18317181/152142
(defun my-large-file-on_enter-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    ;; (fundamental-mode)
    ))

(add-hook 'find-file-hook 'my-large-file-on_enter-hook)


(provide 'init-core)
