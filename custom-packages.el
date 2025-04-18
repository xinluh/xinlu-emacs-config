(when (and (>= emacs-major-version 24) (< emacs-major-version 19))
  (require 'package)

  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
;  (package-initialize)
;  (unless package-archive-contents
;	(package-refresh-contents))

  (unless (package-installed-p 'use-package)
    (when (yes-or-no-p "Need 'use-package' package to install other packages. Continue?")
	    (package-refresh-contents)
		(package-install 'use-package)))
  )

;; make sure external packages that I use are installed
(when (>= emacs-major-version 29)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (require 'use-package-ensure))
(setq use-package-always-ensure t)

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(use-package smex
  :config (setq smex-save-file (concat user-emacs-directory "personal/smex.save")))

(require 'desktop)
(setq desktop-save t)
(setq desktop-restore-eager 4)
(setq desktop-files-not-to-save "----------------------------------")
(setq desktop-dirname (concat user-emacs-directory "personal/"))
(setq desktop-path '("~" "."))
(add-to-list 'desktop-path (concat user-emacs-directory "personal/"))
;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
				(closed-files             . 20)
                tags-file-name
                register-alist)))
;; 				saved-window-configuration)))
(desktop-save-mode 1)

(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-persistency-file-name (concat user-emacs-directory "personal/tramp"))
(setq dabbrev-case-fold-search t)
(setq password-cache-expiry nil)

;; (require 'psvn)
;; (autoload 'svn-status "psvn")
;; (setq svn-status-hide-unmodified t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(use-package magit
  :config
  (setq magit-commit-show-diff nil)
  (setq magit-revert-buffers 1)
  (setq magit-commit-all-when-nothing-staged t)
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (setq magit-stage-all-confirm nil)

  (with-eval-after-load 'transient
    (transient-bind-q-to-quit))

  (defun gh-pr-create (&optional args)
    (interactive)
    (shell-command "gh pr create -f --web")
    )
  (defun gh-pr-view (&optional args)
    (interactive)
    (shell-command "gh pr view --web `git branch --show-current`")
    )

  (transient-define-prefix gh-cmd ()
    ["Arguments"
     ("-w" "web" "--web")]
    [["PR"
      ("h" "create"         gh-pr-create)
      ("v" "view"           gh-pr-view)]
     ])

  (defun magit-hook ()
    (local-set-key (kbd "H") (lambda () (interactive) (gh-cmd)))
    )
  (add-hook 'magit-mode-hook 'magit-hook)
  )


;; (use-package magithub
  ;; :after magit
  ;; :config (magithub-feature-autoinject t))

;;(use-package magit-gh-pulls
;;  :after magit
;;  :config
;;  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package noflet)

;; (require 'bm)

;; (require 'dired-isearch)

(use-package highlight-symbol)

(use-package browse-kill-ring
  :config (browse-kill-ring-default-keybindings))
;; (setq browse-kill-ring-quit-action 'save-and-restore)

(use-package bind-key)

(use-package projectile
  :config
  (setq projectile-keymap-prefix (kbd "C-'"))
  (bind-key* (kbd "C-'") 'projectile-command-map)
  ;; (global-set-key (kbd "M-p M-p") 'projectile-find-file)
  ;; (defun my-projectile-test-suffix (project-type)
  ;;   (cond
  ;;    ((member project-type '(rails-rspec ruby-rspec)) "_spec")
  ;;    ((member project-type '(rails-test ruby-test lein-test go)) "_test")
  ;;    ((member project-type '(django python-pip python-pkg python-tox)) "_tests")
  ;;    ((member project-type '(maven symfony)) "Test")
  ;;    ((member project-type '(gradle grails)) "Spec")
  ;;    (message project-type)
  ;;    (projectile-find-file project-type))
  ;;    )
  ;; (setq projectile-test-suffix-function 'my-projectile-test-suffix)

  (setq projectile-mode-line-function '(lambda ()
	(if
            (file-remote-p default-directory)
            " {.}"
          (format " {%s|%s}"
                  (projectile-project-name) (projectile-project-type)))))

  ;; (defun npm-related-files (path)
  ;;   (let ((js-suffixes concat("js" "ts" "jsx" "tsx")))
  ;;     (if-let ((suffix js-suffixes))
  ;;         if (string-suffix-p ) (list :test (concat ))))
  ;;   )
  (projectile-register-project-type 'npm '("package.json")
                                    :compile "npm run build"
                                    :test "npm run test"
                                    :run "npm start"
                                    :test-dir "__tests__"
                                    :test-suffix ".test")
  (setq projectile-create-missing-test-files t)
  (projectile-global-mode)
  )


(use-package ido-grid-mode
  :config
  (setq ido-grid-mode-start-collapsed t)
  (ido-grid-mode 1))

(use-package flx-ido
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  (setq gc-cons-threshold 20000000) ; better GC threshold for flx
  )

;; (use-package ag
  ;; :config
  ;; (setq ag-highlight-search t)
  ;; (setq ag-reuse-window nil)
  ;; (setq ag-reuse-buffers t)
  ;; (setq ag-arguments '("--smart-case" "--stats" "--width=500" "--hidden"))
  ;; )

(use-package wgrep-ag)

;; (use-package zoom-frm
  ;; :config
  ;; (define-key ctl-x-map [(control ?+)] 'zoom-in/out)
  ;; (define-key ctl-x-map [(control ?-)] 'zoom-in/out)
  ;; (define-key ctl-x-map [(control ?=)] 'zoom-in/out)
  ;; (define-key ctl-x-map [(control ?0)] 'zoom-in/out))

;; also need: sudo pip install jedi flake8 importmagic autopep8 yapf
(use-package elpy
  :config
  (elpy-enable)
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1") ;; for ipython prompts
  (ignore-errors (elpy-use-ipython))
  (defun my-elpy-setup ()
	(local-set-key [M-left] 'pop-tag-mark)
	(local-set-key [M-down] 'comment-and-go-down)
	(local-set-key [M-up] 'uncomment-and-go-up)
	(local-set-key (kbd "<f1> M-.") 'elpy-goto-definition-other-window)
	)
  (setq elpy-test-runner 'elpy-test-pytest-runner)
  (add-hook 'elpy-mode-hook 'my-elpy-setup))

(use-package poetry)

(use-package flycheck
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package ein
  :config
  (defun my-ein-setup ()
	(local-set-key (kbd "C-RET") 'ein:worksheet-execute-cell)
	)
  (add-hook 'ein:notebook-mode-hook 'my-ein-setup)
  )

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

  ;; don't quote = signs (super annoying in javascript mode)
  (setq web-mode-enable-auto-quoting nil)

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-enable-auto-indentation nil)
  (local-set-key (kbd "{") 'self-insert-command)

  (add-hook 'web-mode-hook
            (make-local-variable 'company-minimum-prefix-length)
            (setq company-minimum-prefix-length 1)


            (local-set-key (kbd "<f5>") (lambda() (interactive)
                                          (save-buffer)
                                          (save-some-buffers)
                                          (shell-command "osascript -e 'tell application \"Chrome\" to activate'")))
            )
  )

(use-package prettier-js
  :config
  (add-hook 'web-mode-hook 'prettier-js-mode))

(use-package jest-test-mode)

(use-package tide
  :after web-mode
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)

    ;; enable typescript-tslint checker
    ;; need to install separately: yarn global add tslint typescript eslint_d
    (flycheck-add-mode 'typescript-tide 'web-mode)
    (flycheck-add-mode 'typescript-tide 'typescript-mode)
    (flycheck-add-mode 'javascript-eslint 'typescript-mode)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-next-checker 'typescript-tide '(t . javascript-eslint) 'append)
    )

  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  ;; (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  (require 'web-mode)
  (require 'jest-test-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-t") 'jest-test-run-at-point)
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))
              (when (string-equal "ts" (file-name-extension buffer-file-name))
                (setup-tide-mode))
              ))

  (setq flycheck-javascript-eslint-executable "eslint_d")
  ;; this causes problem apparently
  ;; (setq flycheck-eslint-args "--rule 'prettier/prettier: 0'")
)

(use-package go-mode
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
            (lambda ()
              (company-mode)
              ;; go get golang.org/x/tools/cmd/goimports
              (setq gofmt-command "goimports")

              (flycheck-mode)
              (local-set-key (kbd "C-c C-s") 'godoc-at-point)
              (local-set-key (kbd "C-c C-t") 'go-test-current-file)
              ;;(local-set-key (kbd "C-c C-a") 'go-imports-insert-import) use go-imports instead
              (go-guru-hl-identifier-mode)

              ;; gometalinter is little slow :(
              (make-variable-buffer-local 'flycheck-idle-change-delay)
              (setq flycheck-idle-change-delay 2)

              (local-set-key (kbd "M-.") 'godef-jump)))
  )

;; go get -u golang.org/x/tools/cmd/gorename
(use-package go-rename)

;; go get -u go get golang.org/x/tools/cmd/guru
(use-package go-guru)

(use-package go-imports
  :after go-mode
  :config
  (add-hook 'go-mode-hook (lambda () (local-set-key (kbd "C-c C-a") 'go-imports-insert-import))))

;; go get github.com/fatih/gomodifytags
(use-package go-tag
  :after go-mode
  :config
  (define-key go-mode-map (kbd "C-c t") 'go-tag-add)
  (define-key go-mode-map (kbd "C-c T") 'go-tag-remove)
  (setq go-tag-args (list "-transform" "camelcase")))

(use-package go-playground
  :after go-mode
  :config
  (add-hook 'go-playground-mode-hook (lambda ()
                                       (local-set-key (kbd "<f5>") 'go-playground-exec)
                                       )))

(use-package go-eldoc
  :after go-mode
  :config
  ;; requires go get -u github.com/nsf/gocode
  ;; may need to run `gocode set autobuild true` for dep dependencies completion to work
  (add-hook 'go-mode-hook 'go-eldoc-setup))


(use-package flycheck-gometalinter
  :after go-mode
  :config
  (progn
    (flycheck-gometalinter-setup)))

(use-package company-go
  :after go-mode
  :config
  (add-hook 'go-mode-hook (lambda () (require 'company-go) (company-mode))))

(use-package pug-mode)

(use-package expand-region)

(use-package ido-at-point
  :config
  (ido-at-point-mode))

(use-package solarized-theme
  :config
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-distinct-doc-face t)
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-more-italic t)
  (setq solarized-use-less-bold t)
  (setq solarized-emphasize-indicators nil))

(use-package multiple-cursors
  :config
  (global-set-key [f12]	        'mc/mark-next-like-this-symbol)
  (global-set-key [C-f12]	        'mc/mark-all-dwim)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
  )

(use-package avy
  :bind (("C-l" . avy-goto-char-timer)))

;(use-package company
;  :defer t
;  :idle (global-company-mode))

;; (use-package kubernetes
;;   :ensure t
;;   :commands (kubernetes-overview)
;;   :config
;;   (setq kubernetes-poll-frequency 10000)
;;   )

(use-package rvm
  :config
  (require 'rvm)
  (rvm-use-default))


(use-package protobuf-mode)

(use-package rust-mode)

(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode)
  )

(use-package so-long
  :config
  (global-so-long-mode t)
)

; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; this causes problems :(
;(straight-use-package 'use-package)
(straight-use-package '(beancount
             :type git
             :host github
             :repo "cnsunyour/beancount.el"))
;; also made a modification of the source file locally to fix the account regex string
;; "\\(?::[[:upper:]][[:alnum:]-_]+\\)+"  ->  "\\(?::[[:upper:]][[:alnum:]-_]*\\)+"
(add-to-list 'auto-mode-alist '("\\.bean\\(?:count\\)?\\'" . beancount-mode))
(add-hook 'beancount-mode-hook (lambda () (outline-minor-mode) (outline-hide-sublevels 1)))

