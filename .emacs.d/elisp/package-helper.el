;;; package-helper.el --- nice macros/functions for use with package
;;; https://github.com/skeeto/.emacs.d

;;; Commentary:

;; The two important macros are `with-package' and `with-package*'.
;; Package names given to these are automatically installed by
;; `package-install' and the body of these macros are not evaluated
;; until the listed packages are actually loaded, similar to
;; `eval-after-load'. The latter macro will automatically `require'
;; the listed packages.

;; Examples:

;;     (with-package skewer-mode
;;       ... <config for skewer-mode> ...)

;;     (with-package (js2-mode less-css-mode)
;;       ... <config depending on both packages> ...)

;; To set up configuration after package has been activated but before
;; it has been loaded, append a * to the package name. It's useful for
;; adding keybindings to autoloaded functions. The macros provided
;; here are smart enough to find the real package name from the
;; special name.

;; Remember to call `package-initialize' in your config *before*
;; making use of these macros.

;;; Code:

(require 'cl)
(require 'package)

(defvar package-blacklist ()
  "List of packages that should never be installed.")

(defun packagep (name)
  "Return t if NAME is an available package."
  (unless package-archive-contents
    (package-refresh-contents))
  (not (null (assoc name package-archive-contents))))

(defun package-real-name (package)
  "PACKAGE may be the name of an autoloads; return the actual package name."
  (intern (replace-regexp-in-string "\\*$" "" (symbol-name package))))

(defun package-preload-p (package)
  "Return non-nil if PACKAGE is marked for preloading."
  (not (null (string-match-p "\\*$" (symbol-name package)))))

(defmacro with-package (packages &rest body)
  "Like `eval-after-load', but also automatically register
PACKAGES for installation by `package-install'. PACKAGES can be
either a single symbol or a list of symbols.

  BODY is only ever evaluated once, and only after all PACKAGES
have been loaded. This means if any of the listed packages are
loaded, the others will be immediately loaded as well.

  The body is wrapped in a function so that it gets properly
compiled. Normally with `eval-after-load' it gets quoted for a
literal future `eval', so it appears as data to the compiler."
  (declare (indent defun))
  (let ((has-run-sym (make-symbol "has-run-p"))
        (f-sym (make-symbol "f")))
    (when (symbolp packages)
      (setf packages (list packages)))
    `(progn
       (setq ,has-run-sym nil)
       (fset ',f-sym (lambda ()
                       (unless ,has-run-sym
                         (setq ,has-run-sym t)
                         ,@(loop for package in packages collect
                                 `(require ',(package-real-name package)))
                         ,@body)))
       ,@(loop for package in packages
               for real-name = (package-real-name package)
               collect `(when (and (not (member ',real-name package-blacklist))
                                   (packagep ',real-name)
                                   (not (package-installed-p ',real-name)))
                          (package-install ',real-name)))
       ,@(loop for package in packages
               when (package-preload-p package)
               collect `(eval-after-load 'emacs '(,f-sym))
               else
               collect `(eval-after-load ',package '(,f-sym))))))

(defmacro with-package* (packages &rest body)
  "Like `with-package*' but also `require' all of the packages.
This is mostly for code organization purposes."
  (declare (indent defun))
  (when (symbolp packages)
    (setf packages (list packages)))
  `(progn
     (with-package ,packages ,@body)
     ,@(loop for package in packages
             collect `(require ',(package-real-name package)))))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(with-package\\*?\\)\\(?:\\s-+(?\\([^()]+\\))?\\)?"
    (1 'font-lock-keyword-face)
    (2 'font-lock-constant-face nil t))))

(provide 'package-helper)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; package-helper.el ends here
