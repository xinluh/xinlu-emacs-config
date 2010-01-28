(defun my-programming-mode-hook ()
  (local-set-key "\C-m" 'newline-and-indent)
  (local-set-key [tab] 'indent-or-expand)
  (custom-face-dark)
  ; hightlight TODO: FIXME, marks
  (font-lock-add-keywords nil
  '(("\\<\\([t|T][o|O][d|D][o|O]:*\\)" 1 font-lock-warning-face prepend)
	("\\<\\([f|F][i|I][x|X][m|M][e|E]:*\\)" 1 font-lock-warning-face prepend)
    ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)))
  (setq truncate-lines t)
)

(defun my-c-mode-hook ()
  (my-programming-mode-hook)
  (setq c-basicc-indent 4)
;;   (local-set-key [f5] (lambda () (interactive)
;; 			(cd (get-above-makefile-directory))
;;             (compile compile-command)))
  (local-set-key [f5] 'compile)
  (local-set-key [f7] 'next-error)
  (local-set-key "(" 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "C-.")      (lambda () (interactive) (insert "->")))

  ;; (folding-mode)
  
;;   (setq compile-output-file nil "the output file for compilation")
;;   (make-variable-buffer-local 'compile-output-file)
  (yas/minor-mode-on)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (c-subword-mode 1)
  (c-toggle-hungry-state 1)
  (c-set-offset 'substatement-open 0)
  (setq compilation-finish-functions 'my-compile-finish-function)

  (unless (file-exists-p "Makefile")
	(set (make-local-variable 'compile-command)
		 ;; emulate make's .c.o implicit pattern rule, but with
		 ;; different defaults for the CC, CPPFLAGS, and CFLAGS
		 ;; variables:
		 ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
		 (let ((file (file-name-nondirectory buffer-file-name)))
		   (format "g++ -Wall %s -o %s `root-config --cflags --libs`"
				   file
				   "a.out" ))))

  ; untabify when saving 
;  (add-hook 'write-file-hooks
 ;    (function (lambda () (untabify (point-min) (point-max)))))
)
(defun my-compile-finish-function (buf result)
  (when (string-match "finished" result)
    (let ((target-window (window-at (- (frame-width) 4) (- (frame-height) 4)))
          (pop-up-windows t))
	  (select-window target-window)
;; 	  (when (and (stringp compile-output-file)
;; 				 (not (string= compile-output-file "")))
	  ;; TODO!! use the variable compile-output-file instead
		;; (when (file-exists-p "a.out")
		  ;; (kill-buffer (get-buffer "*a.out*"))
		  ;; (comint-run "a.out"));)
;      (set-window-buffer target-window )
      target-window))					;)

	)
  
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'csharp-mode-hook 'my-c-mode-hook)


(defun my-org-mode-hook ()
    (local-set-key (kbd "<S-return>") 'org-insert-todo-heading)
)
(add-hook 'org-mode-hook 'my-org-mode-hook)

(defun my-ahk-mode-hook ()
  (local-set-key [f5] (lambda() (interactive) (save-buffer)
		 (call-process "d:/Programs/AutoHotkey/AutoHotkey.exe" nil 0 nil
					   (concat "" buffer-file-name))))
  (local-set-key "%" 'skeleton-pair-insert-maybe)
  (local-set-key [f4] 'ahk-complete)
  (my-programming-mode-hook))
(add-hook 'ahk-mode-hook 'my-ahk-mode-hook)

(defun my-lisp-mode-hook ()
  (my-programming-mode-hook)
  (eldoc-mode t)
  (local-set-key [f5] 'eval-buffer)
)
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)

(defun my-html-mode-hook()
  (custom-face-html)
)
(add-hook 'html-mode-hook 'my-html-mode-hook)

(defun my-shell-mode-hook()
  (custom-face-dark)
  (local-set-key [up] 'comint-previous-input)
  (local-set-key [down] 'comint-next-input) 
;  (call-process-shell-command (concat ". " emacsd-dir "/.emacs_bash"))
)
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

(defun my-dired-mode-hook()
  (setq truncate-lines t)
  (when (require 'wdired nil t)
	(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))
  (local-set-key (kbd "O") 'dired-do-open-files)
  (local-set-key (kbd "F") 'dired-find-multiple-file)
  (local-set-key (kbd "C-o") 'dired-find-files-other-window)
  (local-set-key (kbd "o") 'dired-display-file)
  (local-set-key (kbd "W") 'dired-copy-filename-as-kill-newline)
  (local-set-key (kbd "\\") 'dired-do-get-size)
  (local-set-key (kbd "C-\`") 'dired-up-directory)
  (local-set-key (kbd "C-s") 'dired-isearch-forward)
  (local-set-key (kbd "I") 'dired-do-insert-subdir-maybe)
  (local-set-key (kbd "j") 'dired-isearch-forward) ; was dired-do-goto-line
  (local-set-key (kbd "C-r") 'dired-isearch-backward)
  (local-set-key (kbd "ESC C-s") 'dired-isearch-forward-regexp)
  (local-set-key (kbd "ESC C-r") 'dired-isearch-backward-regexp)
  (local-set-key (kbd "<delete>") 'dired-do-delete)
;;   (local-set-key "(" '(lambda () "sort by eXtension"
;; 						(interactive) (dired-sort-other (concat dired-listing-switches "G0"))))
  
  (defvar dired-sort-map (make-sparse-keymap))
  (define-key dired-mode-map "s" dired-sort-map)
  (define-key dired-sort-map "s" '(lambda () "sort by Size"
		(interactive) (dired-sort-other (concat dired-listing-switches "S"))))
  (define-key dired-sort-map "x" '(lambda () "sort by eXtension"
		 (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
  (define-key dired-sort-map "t" '(lambda () "sort by Time"
		 (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
  (define-key dired-sort-map "n" '(lambda () "sort by Name"
		 (interactive) (dired-sort-other (concat dired-listing-switches ""))))
)
(add-hook 'dired-mode-hook 'my-dired-mode-hook)

(defun my-text-mode-hook()
  (auto-fill-mode -1)
;  (longlines-mode)
  )

(add-hook 'text-mode-hook 'my-text-mode-hook)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

(defun my-occur-mode-hook()
  (local-set-key (kbd "o") 'occur-mode-display-occurrence)
  (local-set-key (kbd "f") 'next-error-follow-minor-mode)
  )
(add-hook 'occur-mode-hook 'my-occur-mode-hook)
