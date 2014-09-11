;; (require-package 'multi-term)
;; (require 'multi-term)
;; (setq multi-term-program "cmd.exe")

(pcase (window-system)
  (`w32 (progn 
          (setq explicit-shell-file-name "C:/Program Files (x86)/Git/bin/bash")
          (setq explicit-bash-args '("--login" "-i"))
          (setq shell-file-name explicit-shell-file-name)
          (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin/"))))

(provide 'init-shell)
