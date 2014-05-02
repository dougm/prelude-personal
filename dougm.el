;; load prelude modules
(require 'prelude-ido)
(require 'prelude-helm)
(require 'prelude-company)
(require 'prelude-c)
(require 'prelude-emacs-lisp)
(require 'prelude-js)
(require 'prelude-key-chord)
(require 'prelude-lisp)
(require 'prelude-org)
(require 'prelude-perl)
(require 'prelude-python)
(require 'prelude-ruby)
(require 'prelude-shell)
(require 'prelude-go) ;; in vendor/

;; ido
(prelude-require-package 'ido-vertical-mode)

(ido-vertical-mode t)

;; mac
(when (memq window-system '(mac ns))
  (set-face-font 'default "Monaco-13")
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

;; solarized
(prelude-require-package 'solarized-theme)
(disable-theme 'zenburn)
(load-theme 'solarized-dark t)

;; whitespace
(setq whitespace-line-column 120)

;; keys
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)

;; turn off the beep
(setq visible-bell t)

;; disable scroll bars
(scroll-bar-mode -1)

;; prog
(add-hook 'prog-mode-hook 'linum-mode)

;; install tools for go-mode
(eval-after-load 'go-mode
  '(prelude-go-install-tools))

;; flycheck - don't enable flycheck unless we modify the buffer
(eval-after-load 'flycheck
  '(delq 'mode-enabled flycheck-check-syntax-automatically))

;; git
(prelude-require-package 'magit-gerrit)
(eval-after-load 'magit
  '(require 'magit-gerrit))

;; projectile
(setq projectile-use-git-grep t)
(prelude-require-packages '(ack-and-a-half ag))

;; .dir-locals.el
(setq enable-local-eval t)
(setq enable-local-variables :all)

;; server
(require 'server)
(unless (server-running-p) (server-start))