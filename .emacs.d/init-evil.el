(setq evil-want-C-u-scroll t)
(setq evil-want-C-w-in-emacs-state t)

(setq evil-search-module 'evil-search)
(setq evil-magic 'very-magic)

(setq evil-emacs-state-cursor '("red" box))
;; (setq evil-normal-state-cursor '("green" box))
;; (setq evil-insert-state-cursor '("orange" bar))

(setq evilnc-hotkey-comment-operator "gc")

(require-package 'evil)
(require-package 'evil-leader)
(require-package 'evil-visualstar)
(require-package 'evil-nerd-commenter)
(require-package 'evil-indent-textobject)
(require-package 'evil-matchit)
(require-package 'surround)

(require 'evil)
(require 'evil-leader)
(require 'evil-nerd-commenter)
(require 'evil-indent-textobject)
(require 'evil-visualstar)
(require 'evil-matchit)
(require 'surround)

(global-evil-leader-mode)
(evil-mode t)
(global-surround-mode 1)

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

(defun my-evil-modeline-change (default-color)
  "changes the modeline color when the evil mode changes"
  (let ((color (cond 
                ((minibufferp) default-color)
                ((evil-emacs-state-p)  '("#5f0000" . "#ffffff"))
                ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                (t default-color))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

;; (lexical-let ((default-color (cons (face-background 'mode-line)
;;                                    (face-foreground 'mode-line))))
;;   (add-hook 'post-command-hook (lambda () (my-evil-modeline-change default-color))))

(evil-define-text-object my-evil-next-match (count &optional beg end type)
  "Select next match."
  (evil-ex-search-previous 1)
  (evil-ex-search-next count)
  (list evil-ex-search-match-beg evil-ex-search-match-end))

(evil-define-text-object my-evil-previous-match (count &optional beg end type)
  "Select previous match."
  (evil-ex-search-next 1)
  (evil-ex-search-previous count)
  (list evil-ex-search-match-beg evil-ex-search-match-end))

(define-key evil-motion-state-map "gn" 'my-evil-next-match)
(define-key evil-motion-state-map "gN" 'my-evil-previous-match)

(provide 'init-evil)
