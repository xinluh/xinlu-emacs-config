(when (>= emacs-major-version 24)
  (require 'package)
  
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize)  
;  (unless package-archive-contents
;	(package-refresh-contents))

  (unless (package-installed-p 'use-package)
    (when (yes-or-no-p "Need 'use-package' package to install other packages. Continue?")
	    (package-refresh-contents)
		(package-install 'use-package)))
  )

;; make sure external packages that I use are installed
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
  (setq magit-commit-all-when-nothing-staged t))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package noflet)
(use-package solarized-theme)

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
  (defun my-projectile-test-suffix (project-type)
	(if (member project-type '(django python-pip python-pkg python-tox))
		"_tests"
	  (projectile-find-file project-type)))
  (setq projectile-test-suffix-function 'my-projectile-test-suffix)
  
  (setq projectile-mode-line '(:eval
							   (if
								   (file-remote-p default-directory)
								   " {.}"
								 (format " {%s}"
										 (projectile-project-name)))))
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

(use-package ag
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-window t)
  (setq ag-reuse-buffers t)
  )

(use-package zoom-frm
  :config
  (define-key ctl-x-map [(control ?+)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?-)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?=)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?0)] 'zoom-in/out))

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
  (add-hook 'elpy-mode-hook 'my-elpy-setup)
  
  )

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
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 4)
  (local-set-key (kbd "{") 'self-insert-command)
  )

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

;(use-package company
;  :defer t
;  :idle (global-company-mode))
