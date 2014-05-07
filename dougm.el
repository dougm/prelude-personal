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

;; go
(eval-after-load 'go-mode
  '(progn
     (prelude-go-install-tools)
     (setq go-test-verbose t)))

;; flycheck
(prelude-require-packages '(flycheck-cask flycheck-color-mode-line))
(eval-after-load 'flycheck
  '(progn
     (delq 'mode-enabled flycheck-check-syntax-automatically)
     (add-hook 'flycheck-mode-hook 'flycheck-cask-setup)
     (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

;; git
(prelude-require-package 'magit-gerrit)
(eval-after-load 'magit
  '(require 'magit-gerrit))

;; projectile
(setq projectile-use-git-grep t)
(prelude-require-packages '(ack-and-a-half ag))

;; vagrant
(prelude-require-packages '(vagrant vagrant-tramp))
(require 'vagrant)
(eval-after-load 'tramp
  '(vagrant-tramp-enable))

;; .dir-locals.el
(setq enable-local-eval t)
(setq enable-local-variables :all)

;; server
(require 'server)
(unless (server-running-p) (server-start))
