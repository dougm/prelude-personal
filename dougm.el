;; load prelude modules
(require 'prelude-ido)
(require 'prelude-helm)
(require 'prelude-company)
(require 'prelude-c)
(require 'prelude-emacs-lisp)
(require 'prelude-go)
(require 'prelude-js)
(require 'prelude-key-chord)
(require 'prelude-lisp)
(require 'prelude-org)
(require 'prelude-perl)
(require 'prelude-python)
(require 'prelude-ruby)
(require 'prelude-shell)

;; ido
(prelude-require-package 'ido-vertical-mode)
(ido-vertical-mode t)

;; mac/gui
(when (memq window-system '(mac ns))
  (setenv "VAGRANT_DEFAULT_PROVIDER" "vmware_fusion")
  (set-face-font 'default "Monaco-13")
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (scroll-bar-mode -1)
  (prelude-require-package 'solarized-theme)
  (disable-theme 'zenburn)
  (load-theme 'solarized-dark t))

;; whitespace
(setq whitespace-line-column 120)

;; keys
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)

;; turn off the beep
(setq visible-bell t)

;; go
(eval-after-load 'go-mode
  '(progn
     (go-projectile-install-tools)
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

;; js
(setq-default js-indent-level 2)

;; sh
(setq-default sh-tab-width 2
              sh-basic-offset 2
              sh-indentation 2)

;; projectile
(setq projectile-use-git-grep t)
(prelude-require-packages '(ack-and-a-half ag))
(setq projectile-sort-order 'recentf)

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

;; saveplace
;; ignore tramp files and anything in .git
(setq save-place-ignore-files-regexp "\\(?:^/[a-z]+:\\|/.git/\\)")

;; term
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-'") 'term-line-mode)
            (define-key term-mode-map (kbd "C-'") 'term-char-mode)
            (define-key term-raw-map (kbd "C-y") 'term-paste)))
