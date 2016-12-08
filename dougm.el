;; load prelude modules
(require 'prelude-ido)
(require 'prelude-company)
(require 'prelude-c)
(require 'prelude-emacs-lisp)
(require 'prelude-go)
(require 'prelude-js)
(require 'prelude-key-chord)
(require 'prelude-org)
(require 'prelude-shell)

;; ido
(prelude-require-package 'ido-vertical-mode)
(ido-vertical-mode t)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; mac/gui
(when (memq window-system '(mac ns))
  (setenv "VAGRANT_DEFAULT_PROVIDER" "vmware_fusion")
  (set-face-font 'default "Monaco-13")
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq mouse-drag-copy-region t))

;; whitespace
(setq whitespace-line-column 120)
(defun dougm-auto-fill-mode ()
  (setq fill-column whitespace-line-column)
  (auto-fill-mode))
(add-hook 'text-mode-hook 'dougm-auto-fill-mode)

;; keys
(defun backward-whitespace ()
  (interactive)
  (forward-whitespace -1))

(global-set-key (kbd "M-*") 'pop-tag-mark)
(global-set-key (kbd "M-<right>") 'forward-whitespace)
(global-set-key (kbd "M-<left>") 'backward-whitespace)
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)
(global-set-key (kbd "C-x e") 'compile)
(global-set-key (kbd "M-c") 'recompile)
(global-set-key (kbd "C--") 'negative-argument)

;; turn off the beep
(setq visible-bell t)

;; projectile
(prelude-require-packages '(ag))
(setq ag-highlight-search t)
(setq grep-highlight-matches 'auto)
(setq projectile-use-git-grep t)
(setq projectile-sort-order 'recentf)
(setq projectile-switch-project-action 'dougm-projectile-switch-project-hook)

(let ((map prelude-mode-map))
  (define-key map (kbd "s-g") 'projectile-grep)
  (define-key map (kbd "s-f") 'projectile-find-file)
  (define-key map (kbd "s-s") 'projectile-ag)
  (define-key map (kbd "C-c W") 'browse-url-at-point)
  (define-key map (kbd "C-c w") (lambda ()
                                  (interactive)
                                  (eww (browse-url-url-at-point)))))

;; dir-locals based project config without the .dir-locals.el file
(dir-locals-set-class-variables
 'project-locals
 '((nil . ((eval . (dougm-projectile-project-locals))))))

(defun dougm-projectile-project-locals ()
  (let ((project (projectile-project-name)))
    (cond
     ((string= project "govmomi")
      (progn
        (setq-local projectile-project-compilation-cmd "go install -v ./govc")
        (setq-local go-guru-scope "github.com/vmware/govmomi/govc")
        (setq-default sh-basic-offset 2 sh-indentation 2)))
     ((string= project "vic")
      (progn
        (setq projectile-project-type 'go)
        (setq-local projectile-project-compilation-cmd "go install -v ./cmd/vcsim")
        (setq-local robot-program (concat (projectile-project-root) "tests/local-integration-test.sh"))
        (setq-default sh-basic-offset 4 sh-indentation 4)))
     ((string= project "machine")
      (progn
        (setq-local projectile-project-compilation-cmd "make clean build")
        (setq-local projectile-project-test-cmd "make GOLINT=$(which golint) test")
        (setq-local go-guru-scope "github.com/docker/machine/cmd"))))))

(defun dougm-projectile-switch-project-hook ()
  (dir-locals-set-directory-class (projectile-project-root) 'project-locals)
  (projectile-vc))

;; midnight.el
(dolist (re '("^\\*ag search " "^\\*godoc "))
  (add-to-list 'clean-buffer-list-kill-regexps re))

;; go
(defun go-test-current-package-coverage ()
  (interactive)
  (shell-command (concat "go test . -coverprofile=" go--coverage-current-file-name) (messages-buffer))
  (if current-prefix-arg
      (shell-command (concat "go tool cover -html=" go--coverage-current-file-name))
    (go-coverage)))

(setq go-projectile-tools-path (expand-file-name "~/gotools")
      go-test-verbose t
      go--coverage-current-file-name "cover.out")

(add-to-list 'go-projectile-tools '(gvt . "github.com/FiloSottile/gvt"))
(add-to-list 'go-projectile-tools '(github-release . "github.com/aktau/github-release"))

(eval-after-load 'go-mode
  '(progn
     (setenv "GOPATH" (getenv "HOME"))
     (go-projectile-install-tools)
     (remove-hook 'projectile-after-switch-project-hook 'go-projectile-switch-project)

     (let ((map go-mode-map))
       (define-key map (kbd "C-c c") 'go-test-current-package-coverage))))

;; elisp
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)

;; flycheck
(prelude-require-packages '(flycheck-cask flycheck-color-mode-line))
(setq flycheck-emacs-lisp-load-path 'inherit)
(eval-after-load 'flycheck
  '(progn
     (delq 'mode-enabled flycheck-check-syntax-automatically)
     (add-hook 'flycheck-mode-hook 'flycheck-cask-setup)
     (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
     (setq flycheck-go-vet-shadow t)))

;; git
(prelude-require-packages '(magit-gerrit magit-gh-pulls git-link))
(eval-after-load 'magit
  '(progn
     (setq magit-revision-show-gravatars nil)
     (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
     (require 'magit-gerrit)))

;; js/json
(setq-default js-indent-level 2
              json-reformat:indent-width 2
              jsons-path-printer 'jsons-print-path-jq)

;; sh
(setq-default sh-basic-offset 2
              sh-indentation 2)

;; docker
(prelude-require-package 'docker)
(docker-global-mode)

(defadvice docker-containers (before docker-containers-url-at-point)
  "If `url-get-url-at-point' returns a tcp:// url, setenv DOCKER_HOST url."
  (let ((url (url-get-url-at-point)))
    (when (and url (s-starts-with? "tcp://" url))
      (message "setenv DOCKER_HOST=%s" url)
      (setenv "DOCKER_HOST" url))))

(ad-activate 'docker-containers)

;; govc
(prelude-require-package 'govc)
(govc-global-mode)

;; vagrant
(prelude-require-packages '(vagrant vagrant-tramp))

;; .dir-locals.el
(setq enable-local-eval t
      enable-local-variables :all)

;; server
(require 'server)
(unless (server-running-p) (server-start))

;; saveplace/recentf
;; ignore tramp files and anything in .git
(setq save-place-ignore-files-regexp "\\(?:^/[a-z]+:\\|/.git/\\)")
(dolist (e '("vendor/" "/_workspace/" "/var" "/usr/local/" "/sudo:" "/ssh:" "/vagrant:" "/tmp/"
             "\\.zip'" "\\.gz'"))
  (add-to-list 'recentf-exclude e))

;; term
(add-hook 'term-mode-hook
          (lambda ()
            (prelude-off)
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

;; auto-mode
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.vmx$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))

;; misc
(prelude-require-packages '(list-environment
                            powershell
                            strace-mode))

(setq dired-listing-switches "-laX")

(setq ping-program-options '("-c" "10"))

(setq diff-switches "-u")

(setq sort-fold-case t)

;; I like clocks
(display-time-mode 1)
(setq display-time-default-load-average nil)
