((emacs-lisp-mode
  (eval . (setq flycheck-emacs-lisp-load-path load-path
                flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (eval . (flycheck-color-mode-line-mode -1))))
