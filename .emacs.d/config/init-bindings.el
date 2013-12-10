(defmacro bind (&rest commands)
  "Conveniece macro which creates a lambda interactive command."
  `(lambda ()
     (interactive)
     ,@commands))

(require-package 'guide-key)
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c"))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)

(global-set-key (kbd "M-o") 'helm-imenu) ;'ido-goto-symbol
(global-set-key (kbd "M-l") 'helm-buffers-list) ;'switch-to-buffer
(global-set-key (kbd "C-p") 'helm-projectile)

(after 'smex
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "C-x C-m") 'smex)
  (global-set-key (kbd "C-c C-m") 'smex))

(after 'evil
  (require-package 'key-chord)
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "ZZ") 'evil-quit-all)

  (global-unset-key (kbd "C-l"))
  (define-key evil-normal-state-map (kbd "C-l") 'evil-ex-nohighlight)

  (after 'evil-leader
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "w" 'evil-write
      "e" (kbd "C-x C-e") ;;'eval-last-sexp
      "E" (kbd "C-M-x") ;;'eval-defun
      "c" (bind
           (evil-window-split)
           (eshell))
      "C" 'customize-group
      "b d" 'kill-this-buffer
      "v s" 'magit-status
      "v l" 'magit-log
      "v d" 'vc-diff
      "v m" 'git-messenger:popup-message
      "V" (bind (term "vim"))
      "h" help-map
      "h h" 'help-for-help-internal))

  (after 'evil-matchit
    (define-key evil-normal-state-map "%" 'evilmi-jump-items))

  (after 'git-gutter+-autoloads
    (define-key evil-normal-state-map (kbd "[ c") 'git-gutter+-previous-hunk)
    (define-key evil-normal-state-map (kbd "] c") 'git-gutter+-next-hunk)
    (define-key evil-normal-state-map (kbd ", v a") 'git-gutter+-stage-hunks)
    (define-key evil-normal-state-map (kbd ", v r") 'git-gutter+-revert-hunks))

  (after 'smex
    (define-key evil-visual-state-map (kbd "SPC") 'smex)
    (define-key evil-normal-state-map (kbd "SPC") 'smex))

  (after 'helm-autoloads
    (define-key evil-normal-state-map (kbd "g / F") 'helm-recentf)
    (define-key evil-normal-state-map (kbd "M-t") 'helm-etags-select)
    (define-key evil-normal-state-map (kbd "M-y") 'helm-show-kill-ring)
    (define-key evil-insert-state-map (kbd "M-y") 'helm-show-kill-ring)
    (define-key evil-motion-state-map (kbd "C-f") nil)
    (global-set-key (kbd "C-f") 'helm-swoop))

  (define-key evil-normal-state-map "gw" nil)
  (define-key evil-motion-state-map "gw" 'evil-window-map)

  (define-key evil-normal-state-map "g/" nil)
  (define-key evil-normal-state-map (kbd "g / r") (bind (evil-ex "%s/"))) ;search/replace
  (define-key evil-normal-state-map (kbd "g / l") 'helm-swoop) ;search lines

  (define-key evil-normal-state-map (kbd "[ SPC") (bind (evil-insert-newline-above) (forward-line)))
  (define-key evil-normal-state-map (kbd "] SPC") (bind (evil-insert-newline-below) (forward-line -1)))
  (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
  (define-key evil-normal-state-map (kbd "[ q") 'previous-error)
  (define-key evil-normal-state-map (kbd "] q") 'next-error)

  (define-key evil-normal-state-map (kbd "C-p") nil)
  (define-key evil-normal-state-map (kbd "C-q") 'universal-argument)

  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "0") 'evil-first-non-blank)
  (define-key evil-normal-state-map (kbd "-") 'evil-last-non-blank)
  (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))
  
  ;; file management
  (define-key evil-normal-state-map "^" nil)
  (define-key evil-normal-state-map (kbd "^")
    (bind
     (sr-speedbar-open)
     (sr-speedbar-refresh)
     (sr-speedbar-select-window)))
  (define-key evil-normal-state-map (kbd "g x") 'browse-url-at-point)
  (define-key evil-visual-state-map (kbd "g x") 'my-google)
  ;; unbind default 'g' binding in speedbar
  (define-key speedbar-mode-map (kbd "g") nil)

  ;; [s]-expression manipulation
  (define-key evil-normal-state-map (kbd "g RET") nil)
  (define-key evil-normal-state-map "gs" nil)
  (define-key evil-normal-state-map (kbd "g s s") 'sp-forward-slurp-sexp)
  (define-key evil-normal-state-map (kbd "g s S") 'sp-backward-slurp-sexp)
  (define-key evil-normal-state-map (kbd "g s b") 'sp-forward-barf-sexp)
  (define-key evil-normal-state-map (kbd "g s B") 'sp-backward-barf-sexp)

  ;; emacs lisp
  (evil-define-key 'normal emacs-lisp-mode-map
    (kbd "gd") 'find-function
    (kbd "K") (kbd ", h f RET")
    (kbd "g s RET") 'eval-last-sexp
    (kbd "g RET") 'eval-buffer)
  (evil-define-key 'visual emacs-lisp-mode-map
    (kbd "g RET") 'eval-region)

  ;; clojure / cider
  (evil-define-key 'normal clojure-mode-map
    (kbd "gd") 'cider-jump
    (kbd "K") 'cider-doc
    (kbd "g s RET") 'cider-eval-last-sexp
    (kbd "g RET") 'cider-eval-buffer)
  (evil-define-key 'visual clojure-mode-map
    (kbd "g RET") 'cider-eval-region)
  
  ;; proper jump lists
  ;; (require-package 'jumpc)
  ;; (jumpc)
  ;; (define-key evil-normal-state-map (kbd "C-o") 'jumpc-jump-backward)
  ;; (define-key evil-normal-state-map (kbd "C-i") 'jumpc-jump-forward)

  (after 'company
    (define-key evil-insert-state-map (kbd "TAB") 'my-company-tab)
    (define-key evil-insert-state-map [tab] 'my-company-tab))
  
  (after 'auto-complete
    (define-key evil-insert-state-map (kbd "C-SPC") 'auto-complete))

  (after 'multiple-cursors
    (define-key evil-emacs-state-map (kbd "C->") 'mc/mark-next-like-this)
    (define-key evil-emacs-state-map (kbd "C-<") 'mc/mark-previous-like-this)
    (define-key evil-visual-state-map (kbd "C->") 'mc/mark-all-like-this)
    (define-key evil-normal-state-map (kbd "C->") 'mc/mark-next-like-this)
    (define-key evil-normal-state-map (kbd "C-<") 'mc/mark-previous-like-this))

  (after 'magit
    (evil-add-hjkl-bindings magit-status-mode-map 'emacs
      "K" 'magit-discard-item
      "l" 'magit-key-mode-popup-logging
      "h" 'magit-toggle-diff-refine-hunk))

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

(after 'auto-complete
  (define-key ac-completing-map "\t" 'ac-expand)
  (define-key ac-completing-map [tab] 'ac-expand)
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous))


(after 'company
  (define-key company-active-map "\t" 'my-company-tab)
  (define-key company-active-map [tab] 'my-company-tab)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))


;; mouse scrolling in terminal
(unless (display-graphic-p)
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1))))


;; have no use for these default bindings
(global-unset-key (kbd "C-x m"))
(global-set-key (kbd "C-x C-c") (bind (message "Thou shall not quit!")))
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)


(provide 'init-bindings)
