;; load prelude modules
(require 'prelude-ido)
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
(global-set-key (kbd "M-c") 'recompile)

;; turn off the beep
(setq visible-bell t)

;; projectile
(prelude-require-packages '(ag))
(setq ag-highlight-search t)
(setq grep-highlight-matches 'auto)
(setq projectile-use-git-grep t)
(setq projectile-sort-order 'recentf)
(setq go-projectile-tools-path (expand-file-name "~/gotools"))
(global-set-key (kbd "s-f") 'projectile-find-file)
(global-set-key (kbd "s-g") 'projectile-grep)
(global-set-key (kbd "s-s") 'projectile-ag)
(add-hook 'prelude-mode-hook
          (lambda ()
            (let ((map prelude-mode-map))
              (define-key map (kbd "s-g") 'projectile-grep))))
(add-to-list 'clean-buffer-list-kill-regexps "^\\*ag search ")

;; go
(eval-after-load 'go-mode
  '(progn
     (add-to-list 'clean-buffer-list-kill-regexps "^\\*godoc ")
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
(prelude-require-packages '(magit-gerrit magit-gh-pulls))
(eval-after-load 'magit
  '(require 'magit-gerrit))
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; js
(setq-default js-indent-level 2)

;; sh
(setq-default sh-tab-width 2
              sh-basic-offset 2
              sh-indentation 2)

;; nix-mode
(prelude-require-package 'nix-mode)

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

;; bats
(prelude-require-package 'bats-mode)

(add-hook 'bats-mode-hook
          (lambda ()
            (let ((map bats-mode-map))
              (define-key map (kbd "C-c a") 'bats-run-all)
              (define-key map (kbd "C-c m") 'bats-run-current-file)
              (define-key map (kbd "C-c .") 'bats-run-current-test))))

;; misc
(add-to-list 'auto-mode-alist '("\\.vmx$" . conf-mode))
