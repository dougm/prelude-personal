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
(require 'prelude-latex)

(setenv "TMPDIR" (expand-file-name "~/tmp"))
(setenv "LOG_DIR" (concat (getenv "TMPDIR") "/logs"))
(setenv "GITHUB_USER" (getenv "USER"))

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
(setq whitespace-line-column 160)
(defun dougm-auto-fill-mode ()
  (setq fill-column whitespace-line-column)
  (auto-fill-mode))
(add-hook 'text-mode-hook 'dougm-auto-fill-mode)

;; keys
(defun backward-whitespace ()
  (interactive)
  (forward-whitespace -1))

(defun compile-defaults ()
  (interactive)
  (let ((file buffer-file-name))
    (setq compile-command (or (if file
                                  (or (--first (string-prefix-p file it) compile-history) file))
                              (car compile-history)))
    (call-interactively 'compile)))

(global-set-key (kbd "M-*") 'pop-tag-mark)
(global-set-key (kbd "M-<right>") 'forward-whitespace)
(global-set-key (kbd "M-<left>") 'backward-whitespace)
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)
(global-set-key (kbd "C-c m") 'compile-defaults)
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
  (define-key map (kbd "C-a") nil)
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
        (setq-local projectile-project-compilation-cmd "make install")
        (setq-local go-guru-scope "github.com/vmware/govmomi/govc")
        (setq-default sh-basic-offset 2 sh-indentation 2)))
     ((string= project "kubernetes")
      (progn
        (setq projectile-project-type 'go)
        (setq-local projectile-project-test-cmd "make TMPDIR=$HOME/tmp/k8s test")
        (setq-local projectile-project-compilation-cmd "make WHAT=\"cmd/kubectl cmd/kubelet\""))))))

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
     (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

;; git
(prelude-require-packages '(git-link))
(eval-after-load 'magit
  '(progn
     (setq magit-revision-show-gravatars nil)
     (setq magit-diff-refine-hunk t)))

;; js/json
(setq-default js-indent-level 2
              json-reformat:indent-width 2
              jsons-path-printer 'jsons-print-path-jq)

;; sh
(setq-default sh-basic-offset 2
              sh-indentation 2)

(defun bash-command-on-region ()
  (interactive)
  (if (eq (shell-command-on-region (region-beginning) (region-end) "bash") 0)
      (with-current-buffer "*Shell Command Output*"
        (if (and (> (buffer-size) 0) (< (buffer-size) 512))
            (kill-new (car (split-string (buffer-string) "\n" t)))))))

(defun term-command-on-region ()
  (interactive)
  (display-buffer "*ansi-term*")
  (comint-send-string "*ansi-term*" (concat (buffer-substring (mark) (point)) "\n")))

(let ((map sh-mode-map))
  (define-key map (kbd "C-c x") 'term-command-on-region)
  (define-key map (kbd "C-c C-b") 'bash-command-on-region))

;; docker
(prelude-require-package 'docker)

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
(setenv "GOVC_DEBUG_PATH_RUN" "last")
(setenv "GOVC_DEBUG" "true")
(setenv "GOVC_TLS_KNOWN_HOSTS" (expand-file-name "~/.govmomi/known_hosts"))
(setenv "GOVC_INSECURE" "true")
(setenv "GOVMOMI_INSECURE" "true")
(setenv "GOVC_GUEST_LOGIN" "vagrant:vagrant")
;; (setenv "HTTPS_PROXY" "socks5://localhost:12345")

(defun govc-debug ()
  (interactive)
  (shell-command "$GOPATH/src/github.com/vmware/govmomi/scripts/debug-xmlformat.sh" "*govc*")
  (with-current-buffer "*govc*"
    (xml-mode)))

(defun govc-sim()
  (interactive)
  (require 'filenotify)
  (unless (get-buffer "*vcsim*")
    (compile "vcsim")
    (with-current-buffer compilation-last-buffer (rename-buffer "*vcsim*"))
    (file-notify-add-watch (executable-find "vcsim") '(change)
                           (lambda (event)
                             (with-current-buffer "*vcsim*" (recompile))
                             (message "vcsim: %S" event)))))

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
            (define-key term-raw-map (kbd "C-'") 'term-line-mode)
            (define-key term-mode-map (kbd "C-'") 'term-char-mode)
            (define-key term-raw-map (kbd "C-y") 'term-paste)))

;; bats
(prelude-require-package 'bats-mode)
(setq bats-check-program (executable-find "shellcheck"))

(add-hook 'bats-mode-hook
          (lambda ()
            (let ((map bats-mode-map))
              (define-key map (kbd "C-c x") 'term-command-on-region)
              (define-key map (kbd "C-c a") 'bats-run-all)
              (define-key map (kbd "C-c m") 'bats-run-current-file)
              (define-key map (kbd "C-c .") 'bats-run-current-test))))

;; auto-mode
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.vmx$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.kubeconfig$" . yaml-mode))

;; misc
(prelude-require-packages '(list-environment
                            powershell
                            kubernetes
                            strace-mode
                            string-inflection))

(setq dired-listing-switches "-laX")

(setq ping-program-options '("-c" "10"))

(setq netstat-program-options '("-a" "-n" "-t" "-p"))

(setq diff-switches "-u")

(setq sort-fold-case t)

(setq auto-revert-verbose nil)

(setq rng-nxml-auto-validate-flag nil)

(setq dired-auto-revert-buffer t)

(setq markdown-fontify-code-blocks-natively t)

(setq compilation-scroll-output t)

;; I like clocks
(display-time-mode 1)
(setq display-time-default-load-average nil)
