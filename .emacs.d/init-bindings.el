;; https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-evil.el

(require 'evil)
(require 'evil-args)
(require 'clojure-mode)
(require 'key-chord)
(require 'speedbar)

(defmacro bind (&rest commands)
  "Convenience macro which creates a lambda interactive command."
  `(lambda ()
     (interactive)
     ,@commands))

(require-package 'guide-key)
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c"))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)

(after 'smex
  (global-set-key (kbd "M-x") 'smex))

(after 'evil
  (require-package 'key-chord)
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.5)

  ; exclusive line selection, like Vim. http://dnquark.com/blog/2012/02/emacs-evil-ecumenicalism/
  (setq evil-want-visual-char-semi-exclusive t)

  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (define-key evil-normal-state-map "ZQ" (lambda () (interactive) (evil-quit-all t)))
  (define-key evil-normal-state-map "Zq" 'evil-quit-all)
  (define-key evil-normal-state-map "ZZ" 'evil-save-and-quit)
  (define-key evil-normal-state-map "Zb" 'kill-this-buffer)

  ;; obliterate unwanted emacs default key bindings.
  (define-key evil-normal-state-map (kbd "g /") nil)
  (define-key evil-normal-state-map (kbd "g w") nil)
  (define-key evil-normal-state-map (kbd "RET") nil)
  (define-key evil-visual-state-map (kbd "RET") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (global-unset-key (kbd "M-v"))
  (global-unset-key (kbd "C-l"))

  (define-key evil-normal-state-map (kbd "C-l") 'evil-ex-nohighlight)
  (define-key evil-normal-state-map (kbd "M-v") (bind (term "vim")))
  (define-key evil-normal-state-map (kbd ", w") 'evil-write)

  (after 'evil-matchit
    (define-key evil-normal-state-map "%" 'evilmi-jump-items))

  (after 'evil-args
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

  (after 'git-gutter+-autoloads
    (define-key evil-normal-state-map (kbd "[ c") 'git-gutter+-previous-hunk)
    (define-key evil-normal-state-map (kbd "] c") 'git-gutter+-next-hunk)
    (define-key evil-normal-state-map (kbd ", v a") 'git-gutter+-stage-hunks)
    (define-key evil-normal-state-map (kbd ", v r") 'git-gutter+-revert-hunks))

  (after 'smex
    (define-key evil-visual-state-map (kbd "SPC") 'smex)
    (define-key evil-normal-state-map (kbd "SPC") 'smex))

  (after 'helm-autoloads
    (define-key evil-normal-state-map (kbd "M-o") 'helm-imenu) ;'ido-goto-symbol
    (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile)
    (define-key evil-normal-state-map (kbd "g / F") 'helm-recentf)
    (define-key evil-normal-state-map (kbd "g / .") 'helm-find-files)
    (define-key evil-normal-state-map (kbd "g / l") 'helm-occur) ;search lines
    (define-key evil-normal-state-map (kbd "g / *") 'helm-swoop)
    (define-key evil-normal-state-map (kbd "g l") 'helm-buffers-list) ;'switch-to-buffer
    (define-key evil-normal-state-map (kbd "M-t") 'helm-etags-select)
    (define-key evil-normal-state-map (kbd "M-y") 'helm-show-kill-ring)
    (define-key evil-insert-state-map (kbd "M-y") 'helm-show-kill-ring))

  (define-key evil-motion-state-map (kbd "g w") 'evil-window-map)
  (define-key evil-normal-state-map (kbd "TAB") 'evil-window-mru)

  (define-key evil-normal-state-map (kbd "g / r") (bind (evil-ex "%s/"))) ;search/replace
  (define-key evil-normal-state-map (kbd "s") 'evil-ace-jump-char-mode)
  (define-key evil-normal-state-map (kbd "S") 'evil-ace-jump-char-mode)

  (define-key evil-normal-state-map (kbd "[ SPC") (bind (evil-insert-newline-above) (forward-line)))
  (define-key evil-normal-state-map (kbd "] SPC") (bind (evil-insert-newline-below) (forward-line -1)))
  (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
  (define-key evil-normal-state-map (kbd "[ q") 'previous-error)
  (define-key evil-normal-state-map (kbd "] q") 'next-error)

  (define-key evil-normal-state-map (kbd "g V") (kbd "` [ v ` ]"))

  (define-key evil-normal-state-map (kbd "C-q") 'universal-argument)

  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "-") 'evil-last-non-blank)
  (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

  ;; version control
  (define-key evil-normal-state-map (kbd "U d") 'vc-diff)
  (define-key evil-normal-state-map (kbd "U s") 'magit-status)
  (define-key evil-normal-state-map (kbd "U l") 'magit-log)
  (define-key evil-normal-state-map (kbd "U b") 'magit-blame-mode)
  (define-key evil-normal-state-map (kbd "U B") 'git-messenger:popup-message)

  ;; "The end user almost never has to use defadvice despite what the wiki tells you"
  ;;    http://stackoverflow.com/questions/14606037/advising-an-emacs-interactive-function-before
  ;; (define-key evil-normal-state-map (kbd "cow") 'toggle-truncate-lines)

  (define-key evil-normal-state-map (kbd "g o t")
    (bind
     (evil-window-split)
     (eshell)))

  ;; file management
  (define-key evil-normal-state-map "^" nil)
  (define-key evil-normal-state-map (kbd "^")
    (bind
     (sr-speedbar-open)
     (sr-speedbar-refresh)
     (sr-speedbar-select-window)))
  ;; unbind default 'g' binding in speedbar
  (define-key speedbar-mode-map (kbd "g") nil)
  (evil-define-key 'normal speedbar-mode-map
    (kbd "-") 'speedbar-up-directory)
  (define-key evil-normal-state-map (kbd "g x") 'browse-url-at-point)
  (define-key evil-visual-state-map (kbd "g x") 'my-google)

  ;; sexp manipulation
  (define-key evil-normal-state-map (kbd "g RET") nil)
  (define-key evil-normal-state-map "gs" nil)
  (dolist (pair '("(" "[" "{"))
    (define-key evil-normal-state-map (kbd (concat "g s " pair))
      `(lambda ()
        (interactive)
        (sp-select-next-thing)
        (sp-wrap-with-pair ,pair)
        (sp-beginning-of-sexp)
        (evil-insert 1)
        (insert " ")
        (left-char 1))))
  (define-key evil-normal-state-map (kbd "g s )")
    (bind (sp-select-next-thing) (sp-wrap-with-pair "(") (sp-end-of-sexp 1) (evil-insert 1)))
  (define-key evil-normal-state-map (kbd "g s ]")
    (bind (sp-select-next-thing) (sp-wrap-with-pair "[") (sp-end-of-sexp 1) (evil-insert 1)))
  (define-key evil-normal-state-map (kbd "g s }")
    (bind (sp-select-next-thing) (sp-wrap-with-pair "{") (sp-end-of-sexp 1) (evil-insert 1)))
  (define-key evil-normal-state-map (kbd "g s s") 'sp-slurp-hybrid-sexp)
  (define-key evil-normal-state-map (kbd "g s S") 'sp-backward-slurp-sexp)
  (define-key evil-normal-state-map (kbd "g s b") 'sp-forward-barf-sexp)
  (define-key evil-normal-state-map (kbd "g s B") 'sp-backward-barf-sexp)
  (define-key evil-normal-state-map (kbd "g s t") 'transpose-sexps)
  ;; for lisps, use non-hybrid commands.
  (dolist (modemap '(clojure-mode-map lisp-mode-shared-map elisp-slime-nav-mode-map emacs-lisp-mode-map lisp-interaction-mode-map lisp-mode-map))
    (evil-define-key 'normal modemap
      (kbd "g s s") 'sp-forward-slurp-sexp))

  ;; sexp motion
  (global-unset-key (kbd "M-j"))
  (global-unset-key (kbd "M-k"))
  (global-unset-key (kbd "M-h"))
  (global-unset-key (kbd "M-l"))
  (define-key evil-normal-state-map (kbd ")") (bind
                                                 (evil-append 1)
                                                 (sp-beginning-of-next-sexp)
                                                 (evil-normal-state)))
  (define-key evil-normal-state-map (kbd "(") (bind
                                                 (evil-append 1)
                                                 (sp-beginning-of-previous-sexp)
                                                 (evil-normal-state)))
  (define-key evil-normal-state-map (kbd "M-j") (bind
                                                 (evil-append 1)
                                                 (sp-down-sexp)
                                                 (evil-normal-state)))
  (define-key evil-normal-state-map (kbd "M-k") (bind
                                                 (sp-backward-up-sexp)))
  ;; (define-key evil-normal-state-map (kbd "< I") (bind
  ;;                                                (sp-beginning-of-sexp)
  ;;                                                (evil-insert 1)))
  ;; (evil-define-key 'normal evil-normal-state-map
  ;;   (kbd "< I") (bind (sp-beginning-of-sexp)
  ;;                     (evil-insert 1)))

  ;; expression evaluation

  ;; emacs lisp
  (after 'elisp-slime-nav-autoloads
    (evil-define-key 'normal emacs-lisp-mode-map
      (kbd "g d") 'elisp-slime-nav-find-elisp-thing-at-point
      (kbd "K")   'elisp-slime-nav-describe-elisp-thing-at-point
      (kbd "RET") 'eval-defun
      (kbd "g X") 'eval-buffer)
    (evil-define-key 'visual emacs-lisp-mode-map
      (kbd "RET") 'eval-region))

  ;; clojure / cider
  (evil-define-key 'normal clojure-mode-map
    (kbd "g d") 'cider-jump
    (kbd "K") 'cider-doc
    (kbd "g K") 'cider-javadoc
    ;Evaluate the current toplevel form. PREFIX => print in buffer.
    (kbd "RET") 'cider-eval-defun-at-point
    (kbd "g X") 'cider-eval-buffer)
  (evil-define-key 'visual clojure-mode-map
    (kbd "RET") 'cider-eval-region)
  (evil-define-key 'normal cider-repl-mode-map (kbd "g K") 'cider-javadoc)
  (evil-define-key 'normal cider-mode-map (kbd "g K") 'cider-javadoc)
  
  ;; proper jump lists
  ;; (require-package 'jumpc)
  ;; (jumpc)
  ;; (define-key evil-normal-state-map (kbd "C-o") 'jumpc-jump-backward)
  ;; (define-key evil-normal-state-map (kbd "C-i") 'jumpc-jump-forward)

  (after 'company
    (define-key evil-insert-state-map (kbd "TAB") 'company-complete-common)
    (define-key evil-insert-state-map (kbd "C-SPC") 'company-complete-common)
    (define-key company-active-map (kbd "TAB") 'company-complete-common)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous))

  (after 'multiple-cursors
    (global-unset-key (kbd "C-<down-mouse-1>"))
    (global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)
    (define-key evil-emacs-state-map (kbd "C->") 'mc/mark-next-like-this)
    (define-key evil-emacs-state-map (kbd "C-<") 'mc/mark-previous-like-this)
    (define-key evil-visual-state-map (kbd "C->") 'mc/mark-all-like-this)
    (define-key evil-normal-state-map (kbd "C->") 'mc/mark-next-like-this)
    (define-key evil-normal-state-map (kbd "C-<") 'mc/mark-previous-like-this))

  (define-key evil-visual-state-map "m" 'er/expand-region)
  (define-key evil-visual-state-map (kbd "M-m") nil)
  (define-key evil-visual-state-map (kbd "M-m") (lambda ()
                                                  (interactive)
                                                  (er/expand-region -1)))

  (after 'magit
    (evil-add-hjkl-bindings magit-status-mode-map 'emacs
      "l" 'magit-key-mode-popup-logging
      "h" 'magit-toggle-diff-refine-hunk)
    (evil-define-key 'normal magit-status-mode-map
      (kbd "[ c") 'magit-goto-previous-section
      (kbd "] c") 'magit-goto-next-section))
  (evil-define-key 'normal diff-mode-map
    (kbd "[ c") 'diff-hunk-prev
    (kbd "] c") 'diff-hunk-next)

  ;; minibuffer keymaps
  ;;    esc key
  (define-key minibuffer-local-map [escape] 'my-minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'my-minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'my-minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'my-minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'my-minibuffer-keyboard-quit))

;;    other
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-u") 'backward-kill-sentence)

(after 'comint
  (define-key comint-mode-map [up] 'comint-previous-input)
  (define-key comint-mode-map [down] 'comint-next-input))

;; mouse scrolling in terminal
(unless (display-graphic-p)
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1))))


;; have no use for these default bindings
(global-unset-key (kbd "C-x m"))
(global-set-key (kbd "C-x C-c") (bind (message "Thou shall not quit!")))
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

(provide 'init-bindings)
