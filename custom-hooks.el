(defun my-programming-mode-hook ()
  (local-set-key "\C-m" 'newline-and-indent)
  ;; (local-set-key [tab] 'indent-or-expand)
  ;; (custom-face-dark)
  ; hightlight TODO: FIXME, marks
  (font-lock-add-keywords nil
  '(("\\<\\([t|T][o|O][d|D][o|O]:*\\)" 1 font-lock-warning-face prepend)
	("\\<\\([f|F][i|I][x|X][m|M][e|E]:*\\)" 1 font-lock-warning-face prepend)
    ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)))
  (setq truncate-lines t)
)

(defun my-c-mode-hook ()
  (my-programming-mode-hook)
  ;; (setq c-basic-indent 4)
;;   (local-set-key [f5] (lambda () (interactive)
;; 			(cd (get-above-makefile-directory))
;;             (compile compile-command)))
  (local-set-key [f5] 'compile)
  (local-set-key [f7] 'next-error)
  (local-set-key "(" 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "C-.")      (lambda () (interactive) (insert "->")))

  ;; (folding-mode)
  ;; (linum-mode)
  (imenu-add-menubar-index)
  
;;   (setq compile-output-file nil "the output file for compilation")
;;   (make-variable-buffer-local 'compile-output-file)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2)
  (setq cpp-face-type 'light)
  (subword-mode 1)
  (c-toggle-hungry-state 1)
  (c-set-offset 'substatement-open 0)
  (setq compilation-finish-functions 'my-compile-finish-function)

  (setq ac-sources '(ac-source-filename ac-source-yasnippet ac-source-semantic ac-source-semantic-raw))
  
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
	(local-set-key (kbd "<f5>") 'org-ctrl-c-ctrl-c)
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
  (local-set-key (kbd "C-\\")  'lisp-complete-symbol)
  (local-set-key [f5] (lambda() (interactive)
						(save-buffer)
						(eval-buffer)
						(message "Buffer evaluated")))

  ; pretty lambda
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil)))))
  
  ;If you're saving an elisp file, likely the .elc is no longer valid.
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c")))))
  
)
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)

(defun my-html-mode-hook()
  ;; (custom-face-html)
)
(add-hook 'html-mode-hook 'my-html-mode-hook)

(defun my-shell-mode-hook()
  ;; (custom-face-dark)
  (local-set-key [up]          ; cycle backward through command history
				 '(lambda () (interactive)
					(if (comint-after-pmark-p)
						(comint-previous-input 1)
					  (previous-line 1))))
  (local-set-key [down]        ; cycle forward through command history
				 '(lambda () (interactive)
					(if (comint-after-pmark-p)
						(comint-next-input 1)
					  (forward-line 1))))
	 
;  (call-process-shell-command (concat ". " emacsd-dir "/.emacs_bash"))
)
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

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

(defun reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
		  (list (query-replace-read-to (reb-target-binding reb-regexp)
									   "Query replace"  t))))
  (reb-quit)
  (switch-to-buffer reb-target-buffer)
  (query-replace-regexp (reb-target-binding reb-regexp) to-string))

(setq reb-re-syntax 'string)
(defun my-re-builder-hook()
  (local-set-key (kbd "M-n") 'reb-next-match)
  (local-set-key (kbd "M-p") 'reb-prev-match)
  (local-set-key (kbd "C-M-%") 'reb-query-replace)
  (local-set-key (kbd "<escape>") 'reb-quit)
  (message "C-M-% to start query-replace")
  ;; (reb-change-syntax 'string)

 )
(add-hook 'reb-mode-hook 'my-re-builder-hook)

(add-hook 'snippet-mode-hook '(lambda () (local-set-key (kbd "<f5>")
												   'yas/load-snippet-buffer)))

;; (add-hook 'after-save-hook
		  ;; #'(lambda ()
			  ;; (and (save-excursion
					 ;; (save-restriction
					   ;; (widen)
					   ;; (goto-char (point-min))
					   ;; (save-match-data
						 ;; (looking-at "^#!"))))
				   ;; (not (file-executable-p buffer-file-name))
				   ;; (shell-command (concat "chmod u+x " (shell-quote-argument buffer-file-name)))
				   ;; ;; (if is-win32
				   ;; ;; (set-buffer-file-coding-system 'emacs-mule-unix))
				   ;; (message
					;; (concat "Saved as script: " buffer-file-name)))))

(require 'generic-x) ;; we need this
;; make sure to use js-mode instead the js mode in generic-x
(when (locate-library "javascript")
  (autoload 'javascript-mode "javascript" nil t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode)))

; major mode for SLHA files
(define-generic-mode 
  'slha-mode                         ;; name of the mode to create
  nil                           ;; comments start with '!!'
  '("BLOCK" "Block")                     ;; some keywords
  '(("\\(#.*\\)" 1 'font-lock-comment-face)
	("\\(Block [A-z]+\\)" 1 'font-lock-keyword-face))     ;; ';' is a a built-in 
  nil                      ;; files for which to activate this mode
  '(lambda () (interactive) (setq comment-start "#"))
	 ;; (setq comment-start-skip " +#")
	 ;; (setq comment-end "")
	 ;; (setq comment-end-skip nil))      ;; other functions to call
  "A mode for SLHA files"            ;; doc string for this mode
  )


(defun my-js-chrome-extension-mode()
  (interactive)
  (js-mode)
  (local-set-key [f5] (lambda() (interactive) (save-buffer)
						(call-process "gnome-open" nil 0 nil "http://reload.extensions")))

  )
(add-hook 'js-mode-hook 'my-programming-mode-hook)
