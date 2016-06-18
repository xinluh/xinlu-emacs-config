; -----general settings----
(setq inhibit-startup-message t)
(setq disabled-command-function nil)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p) ;make the y or n suffice for a yes or no question
(setq frame-title-format "%b %+%+ (Emacs)") ;; set emacs title to the document
(set-scroll-bar-mode 'right )
(setq parens-require-spaces nil)
(windmove-default-keybindings) ;use shift+up,down,etc. for changing window
(tool-bar-mode 0)
(when (eq system-type 'gnu/linux);; copy & paste properly on linux
	  (setq x-select-enable-clipboard t)
	  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))
(setq fill-column 90)
(icomplete-mode 1) ;shows completions in minibuffer
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(if (not emacs-runned-once)
	(add-to-list 'kill-emacs-query-functions
      (lambda () (yes-or-no-p "Really kill Emacs? "))))

(ido-mode t)
(setq ido-create-new-buffer 'always)
(setq ido-use-filename-at-point nil)
(setq ido-enable-flex-matching t)
(setq ido-max-prospects 8)
(setq ido-max-work-file-list 50)
(setq ido-save-directory-list-file (concat user-emacs-directory "personal/.ido.last"))
(setq ido-ignore-buffers '("^\\*svn" "\\` " "\\*Kill Ring\\*" "\\*Warnings\\*"
						   "^\\*tramp" "\\*Completions\\*"
						   "^\\*Ido" "\\*shell\\*" "\\*Help"
						   "^\\*.*output\\*" "^\\*TeX Help\\*"
						   "^\\*magit-.*\\*" "^\\*imenu-select\\*"))


(winner-mode t)
(setq ediff-split-window-function (lambda (&optional arg)
									(if (> (frame-width) 150)
										(split-window-horizontally arg)
									  (split-window-vertically arg))))
;; (which-function-mode t)
(goto-address-mode t)
(setq resize-mini-windows nil)

;-----editing settings----
(cua-mode t)
(setq cua-prefix-override-inhibit-delay 0.5)
;(setq cua-enable-cua-keys nil)
;(setq cua-keep-region-after-copy t)
(setq cua-auto-mark-last-change t)
(setq-default transient-mark-mode t)
(setq scroll-step 1) ;smooth-scrolling
(setq kill-whole-line t)
(setq kill-read-only-ok t) ; don't beep when killing line in readonly buffer
(setq kill-do-not-save-duplicates t)
(show-paren-mode 1)
(delete-selection-mode 1)
(setq default-tab-width 4)
(setq view-read-only t) ;enter view mode automatically when file is readonly
(add-hook 'auto-fill-mode-hook (lambda() (message "Auto-Fill mode toggled")))
(setq-default truncate-lines t)
;(setq eldoc-echo-area-use-multiline-p nil)

; -----shell settings----
(ansi-color-for-comint-mode-on)
(add-hook 'comint-output-filter-functions ; don't display password in shell
          'comint-watch-for-password-prompt nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq explicit-shell-file-name "zsh")
(setq comint-prompt-read-only t)

; -----other settings----
(setq compilation-scroll-output t) ; always scroll *compilation* buffer
;; (setq grep-find-command "find . -type f -not -name \"*.svn-base\" -print0 | xargs -0 -e grep -nH -e ")
(setq diff-switches "-u") ; I like unified diff
(setq compilation-read-command nil)

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "msmtp")
(setq message-sendmail-extra-arguments (list "-C"
										 (expand-file-name "~/.emacs.d/personal/msmtp.config")))

; -----keyboard bindings-----
(global-set-key (kbd "<escape>")      'keyboard-escape-quit)
(global-set-key (kbd "C-;")     'dabbrev-expand)
(global-set-key (kbd "C-c m")   'imenu-selection-buffer)
(global-set-key (kbd "C-c g")   'rgrep)
(global-set-key (kbd "C-c c")   'calc-eval-region)
(global-set-key "\M-m"          'er/expand-region) ;was newline-and-indent
(global-set-key "\C-z"          'undo)
(global-set-key "\C-v"          'yank)
(global-set-key (kbd "C-S-y")   'duplicate-line)
;(global-set-key "%"             'match-paren)
(global-set-key (kbd "M-x")     'smex)
(global-set-key (kbd "M-S-x")     'execute-extended-command)
(global-set-key (kbd "C--")     (lambda () (interactive) (cua-set-mark 5)))
(global-set-key (kbd "C-c o")   'occur)
(global-set-key (kbd "C-c i")   'insert-path)
(global-set-key (kbd "C-c I")   'insert-path-short)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-j")     'backward-char) ; was indent-new-comment-line
(global-set-key (kbd "M-l")     'forward-char)  ; was downcase-word
(global-set-key (kbd "M-i")     'previous-line) ; was tab-to-tab-stop
(global-set-key (kbd "M-k")     'next-line) ; was kill-sentence
(global-set-key (kbd "M-s")     'backward-char)
(global-set-key (kbd "M-f")     'forward-char)  ; was foward-word
(global-set-key (kbd "M-e")     'previous-line) ; was foward-sentence
(global-set-key (kbd "M-d")     'next-line)		; was kill-word
(global-set-key (kbd "M-#")     'quick-calc)
(global-set-key [C-tab]         'next-buffer)
(global-set-key [C-S-tab]       'previous-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>")  'previous-buffer)
(global-set-key [M-right]       'next-buffer)
(global-set-key [M-left]        'previous-buffer)
;; (global-set-key (kbd "S-SPC")   'pager-page-down)
;; (global-set-key (kbd "C-S-SPC") 'pager-page-up)
(global-set-key "\M-gg"         'goto-line)
(global-set-key "\M-h"          'query-replace)
(global-set-key (kbd "<f2> <f1>")	(lambda() (interactive) (select-window (get-window-at-corner 1))))
(global-set-key (kbd "<f2> <f2>")	(lambda() (interactive) (select-window (get-window-at-corner 2))))
(global-set-key (kbd "<f2> <f3>")	(lambda() (interactive) (select-window (get-window-at-corner 4))))
(global-set-key [f3]			'ido-switch-buffer)
(global-set-key [M-f3]			'ido-switch-buffer-other-window)
(global-set-key [C-f3]			'insert-path)
(global-set-key [S-f3]			'ffap)
(global-set-key [f4]			'yas/expand)
(global-set-key [M-f4]			'delete-frame-or-exit)
(global-set-key [S-f4]  		'bury-buffer)
(global-set-key [C-f4]			'kill-this-buffer)
(global-set-key [M-S-f4]		'delete-window)
(global-set-key [C-S-f4]        (lambda() (interactive) (kill-buffer (current-buffer))
                                               (delete-window)))
(global-set-key [f5]            'compile)
(global-set-key [M-f5]			'reload-file)
(global-set-key [f6]			'shell-command-mod)
(global-set-key [C-f6]			'shell)
(global-set-key [f7]			'next-error)
(global-set-key [S-f7]			'previous-error)
(global-set-key [f8]	        'vcs-start)
(global-set-key [C-f9]	        'highlight-symbol-at-point)
(global-set-key [f9]	        'highlight-symbol-next)
(global-set-key [S-f9]	        'highlight-symbol-prev)
(global-set-key [M-f9]	        'highlight-symbol-remove-all)
(global-set-key [C-f10]	        'kmacro-start-macro)
(global-set-key [f10]	        'kmacro-end-or-call-macro)
(global-set-key [f11]	        'visual-line-mode)
(global-set-key [M-f11]			'toggle-truncate-lines)
(global-set-key [C-f11]			'auto-fill-mode)
(global-set-key [f12]	        'google)
(global-set-key (kbd "<pause>")  'view-mode)
(global-set-key (kbd "<f13>")  'view-mode)
(global-set-key (kbd "<Scroll_Lock>")  'restore-windows-config)
(global-set-key (kbd "<scroll>")  'restore-windows-config)
(global-set-key (kbd "<apps>")  'smex)
(global-set-key (kbd "<menu>")  'smex)
(bind-key* [M-down] 'comment-and-go-down)
(bind-key* [M-up] 'uncomment-and-go-up)
(global-set-key (kbd "M-SPC") 'cua-set-mark)
(global-set-key (kbd "C-/") 'comment-region)
(when (eq system-type 'darwin) ;; super is remapped in mac
  (global-set-key (kbd "s-<backspace>") 'backward-kill-word))


; set saner keys for isearch-mode
(define-key isearch-mode-map (kbd "C-o")  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))
(define-key isearch-mode-map (kbd "<f3>")
  (lambda () (interactive) (isearch-repeat-forward)))
(define-key isearch-mode-map (kbd "<escape>")
  (lambda () (interactive) (isearch-cancel)))
(define-key isearch-mode-map (kbd "C-v") ;yank in current word
  (lambda () (interactive) (save-excursion (skip-syntax-backward "w_")
		   (isearch-yank-internal (lambda () (skip-syntax-forward "w_") (point))))))

;completion shortcut in minibuffer
;; (define-key minibuffer-local-completion-map (kbd "<f3>")
                          ;; 'complete-minibuffer-path)

; insert current buffer name into minibuffer
;; (define-key minibuffer-local-map [f3]
  ;; (lambda () (interactive)
	;; (insert (buffer-name (window-buffer (minibuffer-selected-window))))))

(define-key ido-buffer-completion-map (kbd "<f3>") 'ido-enter-find-file)
(define-key ido-file-completion-map (kbd "<f3>") 'ido-enter-switch-buffer)
(define-key ido-buffer-completion-map (kbd "C-o") 'ido-enter-find-file)
(define-key ido-file-completion-map (kbd "C-o") 'ido-enter-switch-buffer)

;; possibly needed
;; (add-hook 'ido-setup-hook 
          ;; (lambda () 
            ;; (define-key ido-completion-map [tab] 'ido-complete)))


(define-key minibuffer-local-map (kbd "C-i") 'completion-at-point)

;;automatically close brackets, quotes, etc when typing
(setq skeleton-pair t)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir (concat user-emacs-directory "autosave/"))
(make-directory autosave-dir t)
(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))
(defun make-auto-save-file-name ()
  (concat autosave-dir
		  (if buffer-file-name
			  (concat "#" (file-name-nondirectory buffer-file-name) "#")
			(expand-file-name
			 (concat "#%" (buffer-name) "#")))))
(setq backup-directory-alist (list (cons autosave-dir "."))) 

;; ========================================

;; settings so that windows will open the places I want 
(defun get-window-at-corner (corner &optional FRAME)
  "return windows at corner 1 = upper left, 2=upper right, etc"
  (let ((x (cond ((or (= corner 1) (= corner 3)) 4)
				 (t (- (frame-width) 4))))
		(y (cond ((or (= corner 1) (= corner 2)) 4)
				 (t (- (frame-height) 4)))))
	(window-at x y)))
(defun my-display-buffer-23 (buf &optional args)
  "put all buffers in a window other than the one in the bottom right"
  "for emacsen 23 or above"
  (if (member (buffer-name buf) special-display-buffer-names)
	  (display-special-buffer buf)
	(progn
	  (let ((pop-up-windows t)
			(windows (delete (minibuffer-window) (window-list))))
		(if (not (equal (get-window-at-corner 4) (get-window-at-corner 2)))
			(setq windows (delete (get-window-at-corner 4) windows)))
		(set-window-buffer (car (cdr windows)) buf)
		(car (cdr windows))))))

(setq special-display-function 'my-display-buffer-23) 
; shown theses in separate frame
(setq special-display-buffer-names '("*compilation*" "*Help*" "*shell*"
									 "*magit-rebase-popup*" "*magit-commit-popup*" "*magit-push-popup*"
									 "*Completions*" "*Buffer List*" "*Deletions*" "*Warnings*"
									 "*Ido Completions*" "*svn-process*"
									 "*svn-log-edit*" "*Kill Ring*"
									 "*imenu-select*" "*Popup Help*"))
(setq special-display-regexps '(".*"))
(setq special-display-frame-alist '((height . 14)
									(width . 80)
									(unsplittable . t)
									(menu-bar-lines nil)))

;================================================
; advices
(require 'noflet)

(defadvice split-window-vertically
	(after my-window-splitting-advice first () activate)
  "activate the next buffer whenever window splits"
  (set-window-buffer (next-window) (other-buffer)))
(defadvice split-window-horizontally
    (after my-window-splitting-advice first () activate)
  "activate the next buffer whenever window splits"
  (set-window-buffer (next-window) (other-buffer)))

;; don't quit frame ???
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (noflet ((one-window-p (&optional nomini all-frames) t)) ad-do-it))

(defadvice compile (before my-compile activate)
  "save buffer before compile"
  (save-buffer))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying 'Active processes exist' query when you quit Emacs."
  (noflet ((process-list ())) ad-do-it))

;; doens't work :(
;; (defadvice shell-command (around my-shell-command (COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER) activate)
  ;; (noflet ((read-shell-command (PROMPT &optional INITIAL-CONTENTS HIST &rest ARGS)
							  ;; (funcall this-fn (concat default-directory " $: ") INITIAL-CONTENTS HIST ARGS)))
	;; ad-do-it))

;; make parenthesis behave normally: no need to escape
;; (defadvice query-replace-regexp (before my-query-replace-regexp activate))
;; (when (interactive-p)
;; (ad-set-arg 0 (replace-regexp-in-string "(" "\\\\(" (ad-get-arg 0)))
;; (ad-set-arg 0 (replace-regexp-in-string ")" "\\\\)" (ad-get-arg 0)))))

(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

(defadvice kill-new (before kill-new-push-xselection-on-kill-ring activate)
  "Before putting new kill onto the kill-ring, add the clipboard/external selection to the kill ring"
  (let ((have-paste (and interprogram-paste-function
						 (funcall interprogram-paste-function))))
	(when have-paste (push have-paste kill-ring))))

(defadvice yank (after indent-region activate)
  (if (member major-mode
			  '(emacs-lisp-mode scheme-mode lisp-mode
								c-mode c++-mode objc-mode
								latex-mode plain-tex-mode))
	  (let ((mark-even-if-inactive t))
		(indent-region (region-beginning) (region-end) nil))))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
			  '(emacs-lisp-mode scheme-mode lisp-mode
								c-mode c++-mode objc-mode
								latex-mode plain-tex-mode))
	  (let ((mark-even-if-inactive t))
		(indent-region (region-beginning) (region-end) nil))))

(defvar ido-enable-replace-completing-read t
  "If t, use ido-completing-read instead of completing-read if possible.")
(defadvice completing-read (around my-completing-read-ido activate)
  (if (or (not ido-enable-replace-completing-read)
		  (boundp 'ido-cur-item))  ad-do-it
	(let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
									 allcomp
									 nil require-match initial-input hist def))
        ad-do-it))))
; need this so that the above advice will not screw up reading file names
(setq read-file-name-function 'ido-read-file-name)
(defadvice dired-do-rename (around my-dired-do-rename activate)
  (let ((ido-enable-replace-completing-read nil)) ad-do-it))
; don't screw up rgrep etc. either
(defadvice grep-read-files (around my-rgrep activate)
  (let ((ido-enable-replace-completing-read nil))
	ad-do-it))
										

;=================================================
; functions

;for efficient commenting
(defun comment-and-go-down ()
  "Comments the current line and goes to the next one" (interactive)
  (condition-case nil (comment-region (point-at-bol) (point-at-eol)) (error nil))
  (forward-line 1))
(defun uncomment-and-go-up ()
  "Uncomments the current line and goes to the previous one" (interactive)
  (condition-case nil (uncomment-region (point-at-bol) (point-at-eol)) (error nil))
  (forward-line -1))

(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding point."
  (interactive "*P")
 (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand)
    (smart-indent)))

(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
	  (indent-region (region-beginning)
					 (region-end))
    (indent-according-to-mode)))

(defun duplicate-line ()
  "Clones the current line of text."
  (interactive)
  (save-excursion
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    (end-of-line) (newline) (yank) (forward-line 2) (current-kill 1)))

(defun reload-file () (interactive)
  (revert-buffer t t t) (message "File reloaded"))

(defun reload-file-force () (interactive)
  (let ((filename (buffer-file-name))
		(start (window-start (selected-window))))
	(kill-buffer (current-buffer))
	(find-file filename)
	(set-window-start (selected-window) start)
	(message "File reloaded")))

(defun dot-emacs ()  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun ecustom ()  (interactive)
  (find-file (concat user-emacs-directory "custom.el")))

(defun insert-path (&optional arg)
  "Inserts a path into the buffer with completion and strip the path of tramp syntaxes"
  (interactive "p*")
  (let ((filename (ido-read-file-name "Insert Path: ")))
	(setq filename (replace-regexp-in-string
					"^\\(/[a-z0-9]\\{2,5\\}:[-a-zA-Z0-9.]+:\\)" "" filename))
  (insert filename)))

(defun insert-path-short (&optional arg)
  "Inserts a path into the buffer with completion and strip the path of tramp syntaxes"
  (interactive "p*")
  (let ((filename (ido-read-file-name "Insert Filename: ")))
	(setq filename (replace-regexp-in-string
					"^\\(/[a-z0-9]\\{2,5\\}:[-a-zA-Z0-9.]+:\\)" "" filename))
	(insert (file-name-nondirectory filename))))

(defun shell-command-mod
  (command &optional output-buffer error-buffer)
  "shell-command that displays the working direction as prompt"
  (interactive (list (read-shell-command (concat default-directory " $: ")
                      nil nil nil 'shell-command-history)
             current-prefix-arg))
  (shell-command command output-buffer error-buffer))

(defun get-above-makefile-directory ()
  (let (makefile-dir) (setq makefile-dir
	   (loop as d = default-directory then (expand-file-name
       ".." d) if (file-exists-p (expand-file-name "Makefile" d))
       return d)) makefile-dir ))

;; (defun complete-minibuffer-path ()
;;   "Extension to the complete word facility of the minibuffer by
;; replacing matching strings to a specific path"
;;   (interactive)
;;   (let ((directory) (found t))
;;   (cond
;;      ; just add new entries if needed; shortcut up to 4 letters will work
;;      ((looking-back "j" 5 nil) (setq directory "D:/Desktop/"))
;;      ((looking-back "k" 5 nil) (setq directory "D:/Documents/"))
;;      ((looking-back "l" 5 nil) (setq directory (concat home-dir ".emacs.d/")))
;;      ((looking-back "i" 5 nil) (setq directory "D:/Programs/"))
;;      ((looking-back "o" 5 nil) (setq directory "D:/Documents/Visual Studio 2008/Projects/MusicDataBase/MusicLib/"))
;;      (t (setq found nil)))
;;   (cond (found (beginning-of-line)
;;                 (kill-line)
;;                 (insert directory))
;;          (t (minibuffer-complete)))))

(defun delete-frame-or-exit ()
  "Delete frame if more than one frame are present; otherwise exit emacs"
  (interactive)
  (if (= (length (frame-list)) 1)
	  (save-buffers-kill-emacs) (delete-frame)))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
		(filename (buffer-file-name)))
	(if (not filename)
		(message "Buffer '%s' is not visiting a file!" name)
	  (if (get-buffer new-name)
		  (message "A buffer named '%s' already exists!" new-name)
		(progn 	 (rename-file name new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil)))))) ;;

(defun move-file-and-buffer (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
		 (filename (buffer-file-name))
		 (dir
		  (if (string-match dir "\\(?:/\\|\\\\)$")
			  (substring dir 0 -1) dir))
		 (newname (concat dir "/" name)))
	(if (not filename)
		(message "Buffer '%s' is not visiting a file!" name)
	  (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) 	(set-buffer-modified-p nil) 	t))))

(defvar html-header "MIME-Version: 1.0\nContent-Type: text/html;\n\n")

(defun send-email (recipient subject text)
  "Send email with msmtp"
  (with-temp-buffer
	(insert (concat "Subject: " subject "\n"))
	;; (insert html-header) (newline) ;for blogger emails
	(insert text)
	(call-process-region (point-min) (point-max) "msmtp" nil "temp" t "-C"
						 (expand-file-name "~/.emacs.d/personal/msmtp.config") recipient)))


(defun note-to-self ()
  "Send region as email to myself"
  (interactive)
  (let ((text (buffer-substring-no-properties (region-beginning) (region-end)))
		(subject (read-from-minibuffer "Subject: ")))
	(message "Sending note as email...")
	(send-email user-mail-address subject text)
	(message "Sending note as email... Done")))

(defun fullscreen ()
  "toggles whether the currently selected frame consumes the entire
   display or is decorated with a window border"
  (interactive)
  (let ((f (selected-frame)))
    (modify-frame-parameters f `((fullscreen . ,
			  (if (eq nil (frame-parameter f 'fullscreen)) 'fullboth nil))))))



(defun display-special-buffer (buf)
  "put the special buffer in the right spot (bottom rigt)"
;;   (let ((windows (delete (minibuffer-window) (window-list))))
;;     (if (eq 1 (length windows))st
;;         (progn
;;           (select-window (car windows))
;;           (split-window-vertically)))
    (let ((target-window (get-window-at-corner 4))
          (pop-up-windows t))
      (set-window-buffer target-window buf)
      target-window));)

(defun my-display-buffer (buf)
  "put all buffers in a window other than the one in the bottom right"
  "for emacsen 22 or below"
  (if (member (buffer-name buf) special-display-buffer-names)
	  (display-special-buffer buf)
	  (progn
		(let ((pop-up-windows t)
			  (windows (delete (get-window-at-corner 4)
                         (delete (minibuffer-window) (window-list)))))
		  (message (buffer-name (window-buffer (car windows))))
		  (set-window-buffer (car (cdr windows)) buf)
		  (car (cdr windows))))))


(defun swap-windows ()
  "Swap the buffers in the 2 vertically split windows"
  (interactive)
  (let* ((w1 (get-window-at-corner 1))
		 (w2 (get-window-at-corner 2))
		 (b1 (window-buffer w1))
		 (b2 (window-buffer w2))
		 (s1 (window-start w1))
		 (s2 (window-start w2)))
	(set-window-buffer w1 b2)
	(set-window-buffer w2 b1)
	(set-window-start w1 s2)
	(set-window-start w2 s1)
	(select-window (get-window-at-corner 1))
	))

(defun save-mark ()
  "save push mark on mark-ring for coming back"
  (interactive)
  (push-mark nil nil nil))

(defun restore-windows-config ()
  "Put windows in the right configuration"
  (interactive)
  (select-window (window-at 2 2))
  (delete-other-windows)  (split-window-horizontally)
  (other-window 1) (split-window-vertically)
  (other-window 1) (set-window-text-height nil 12)
  (other-window 1))

(defun google ()
  (interactive)
  (let ((class (read-string "google for: ")))
	(browse-url (concat "http://www.google.com/search?q=" class "&ie=utf-8&oe=utf-8&aq=t")))
  (shell-command "wmctrl -a firefox")
  )

;; (defun browse-img ()
  ;; (interactive)
  ;; (make-frame '((name . "View output -Emacs-") (width . 358) (height . 64) ))
  ;; ;; (raise-frame)
  ;; ;; (split-window-horizontally)
  ;; )

(defun build-tag-table (dir-name)
  "Create tag files"
  (interactive "DDirectory: ")
  (shell-command
   (format
	"find %s -type f  -name \"*.cpp\" -print   -or   -name \"*.hpp\" -print | etags -"
	dir-name)))

(defun home-quick-set ()
  (interactive)
  ;; (custom-face-dark)
  (set-face-attribute 'default (selected-frame) :height 120)
  ;; (w32-maximize-frame)
  (restore-windows-config))

(defun larger-font () (interactive)
  (set-face-attribute 'default (selected-frame) :height 120))


;=============================
; provides simple mechanism for recovering accidentally closed files
(defvar closed-files (list))

(defun track-closed-file ()
  (and buffer-file-name
	   (or (delete buffer-file-name closed-files) t)
	   (add-to-list 'closed-files buffer-file-name)))
(add-hook 'kill-buffer-hook 'track-closed-file)

(defun last-closed-files ()
  (interactive)
  (find-file (ido-completing-read "Last closed: " closed-files)))
;==============================


(define-derived-mode imenu-selection-mode fundamental-mode "imenu"
  "Major mode for imenu selection."
  (suppress-keymap imenu-selection-mode-map)
  (define-key imenu-selection-mode-map "s" 'isearch-forward)
  (define-key imenu-selection-mode-map "r" 'isearch-backward)
  (define-key imenu-selection-mode-map "j" 'next-line)
  (define-key imenu-selection-mode-map "k" 'previous-line)
  (define-key imenu-selection-mode-map "n" 'next-line)
  (define-key imenu-selection-mode-map "p" 'previous-line)
  (define-key imenu-selection-mode-map "l" 'imenu-selection-select)
  (define-key imenu-selection-mode-map "\C-m" 'imenu-selection-select)
  (define-key imenu-selection-mode-map "q" 'imenu-selection-quit)
  )
(defvar imenu-selection-buffer-name "*imenu-select*")
(defvar imenu-selection-target-buffer nil)
(defvar imenu-selection-orig-windows-config nil)
(defun imenu-selection-buffer (&optional index-alist)
  (interactive)
  (require 'which-func)
  (setq index-alist (if index-alist index-alist (imenu--make-index-alist)))
  (setq imenu-selection-orig-windows-config
		(current-window-configuration))
  (let ((cur (which-function))
		(buf (get-buffer-create imenu-selection-buffer-name)))
    (when (listp cur)
      (setq cur (car cur)))
    (setq imenu-selection-target-buffer (current-buffer))
	(with-current-buffer buf
	  (setq buffer-read-only nil)
	  (buffer-disable-undo)
	  (erase-buffer)
	  (save-excursion 
		(dolist (x index-alist)
		  (insert (car x) "\n"))
		(if cur (search-backward (concat cur "\n") nil t)))
	  (imenu-selection-mode)
	  (setq buffer-read-only t))
  (pop-to-buffer buf)))

(defun imenu-selection-select ()
  (interactive)
  (if (eq major-mode 'imenu-selection-mode)
  (let ((sel (substring (thing-at-point 'line) 0 -1)))
    (bury-buffer)
	(imenu-selection-quit)
    (imenu sel))))

(defun imenu-selection-quit ()
  (interactive)
  (if (eq major-mode 'imenu-selection-mode)
	  (let (buf (current-buffer))
		(set-window-configuration imenu-selection-orig-windows-config)
		(kill-buffer buf))
	(pop-to-buffer imenu-selection-target-buffer)))


;; (defun rename-frame (name)
  ;; "rename frame to NAME"
  ;; (interactive "sName: ")
  ;; (modify-frame-parameters nil '((title . name))))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun vcs-start ()
  (interactive)
  (let ((backend (vc-backend (buffer-file-name))))
	(when (string-equal backend "SVN") (svn-status default-directory))
	(when (string-equal backend "Git") (magit-status))
	(when (string-equal backend "CVS") (cvs-examine default-directory nil))))

(defun find-definition (arg)
  "Jump to the definition of the symbol, type or function at point.
  With prefix arg, find in other window."
  (interactive "P")
  (let* ((tag (or (semantic-idle-summary-current-symbol-info-context)
				  (semantic-idle-summary-current-symbol-info-brutish)
				  (error "No known tag at point")))
		 (pos (or (semantic-tag-start tag)
				  (error "Tag definition not found")))
		 (file (semantic-tag-file-name tag)))
	(if file
		(if arg (find-file-other-window file) (find-file file))
	  (if arg (switch-to-buffer-other-window (current-buffer))))
	(push-mark)
	(goto-char pos)
	(end-of-line)))

(defun calc-eval-region (arg beg end)
  "Calculate the region and display the result in the echo area.
With prefix ARG non-nil, insert the result at the end of region."
  (interactive "P\nr")
  (let* ((expr (buffer-substring-no-properties beg end))
         (result (calc-eval expr)))
    (if (null arg)
        (message "%s = %s" expr result)
      (goto-char end)
      (save-excursion
        (insert result)))))
