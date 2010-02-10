; -----general settings----
(setq inhibit-startup-message t)
(setq disabled-command-function nil)
(fset 'yes-or-no-p 'y-or-n-p) ;make the y or n suffice for a yes or no question
(setq frame-title-format "%b %+%+ (Emacs)") ;; set emacs title to the document
(set-scroll-bar-mode 'right )
(when (not is-win32) (tool-bar-mode 0))  ;; don't like toolbar!
(setq parens-require-spaces nil)
(windmove-default-keybindings) ;use shift+up,down,etc. for changing window
; shown theses in separate frame
(setq special-display-buffer-names '("*compilation*" "*Help*" "*shell*"
									 "*Completions*" "*Buffer List*"
									 "*Ido Completions*" "*svn-process*"
									 "*svn-log-edit*" "*Kill Ring*"
									 "*imenu-select*"))
(if is-emacs23
	(setq special-display-function 'my-display-buffer-23)
  (setq special-display-function 'my-display-buffer))
(setq special-display-regexps '(".*"))
(setq special-display-frame-alist '((height . 14)
                                  (width . 80)
                                  (unsplittable . t)
                                  (menu-bar-lines nil)))
(add-hook 'after-make-frame-functions 'custom-face-all)
(if (not emacs-runned-once) (custom-face-all))
(setq fill-column 90)
(icomplete-mode 1) ;shows completions in minibuffer
(when is-win32 ;(setq w32-pass-alt-to-system t)
      (setq w32-get-true-file-attributes t))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq kill-emacs-query-functions
      (cons (lambda () (yes-or-no-p "Really kill Emacs? "))
            kill-emacs-query-functions))

(ido-mode t)
(setq ido-create-new-buffer 'always)
(setq ido-use-filename-at-point nil)
(setq ido-enable-flex-matching t)
(setq ido-max-prospects 8)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")

(setq ido-ignore-buffers '("^\\*svn" "\\` " "\\*Kill Ring\\*"
						   "^\\*tramp" "*Completions*"
						   "^\\*Ido" "\\*shell\\*" "\\*Help"
						   "^\\*.*output\\*" "^\\*TeX Help\\*"
						   "^\\*magit-.*\\*" "^\\*imenu-select\\*"))

(winner-mode t)
(setq ediff-split-window-function (lambda (&optional arg)
									(if (> (frame-width) 150)
										(split-window-horizontally arg)
									  (split-window-vertically arg))))
(which-function-mode t)
(goto-address-mode t)
(setq resize-mini-windows nil)
(setq cpp-face-type 'dark)
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
(show-paren-mode 1)
(delete-selection-mode 1)
(setq default-tab-width 4)
(setq view-read-only t) ;enter view mode automatically when file is readonly
(ansi-color-for-comint-mode-on)
;(add-hook 'longlines-mode-hook 'longlines-show-hard-newlines)
(add-hook 'longlines-mode-hook (lambda() (setq fill-column 90)))
(add-hook 'longlines-mode-off-hook 'longlines-unshow-hard-newlines)
(add-hook 'auto-fill-mode-hook (lambda() (message "Auto-Fill mode toggled")))
(setq-default truncate-lines t)
;(setq eldoc-echo-area-use-multiline-p nil)
; -----shell settings----
(add-hook 'comint-output-filter-functions ; don't display password in shell
          'comint-watch-for-password-prompt nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)

; -----other settings----
(setq compilation-scroll-output t) ; always scroll *compilation* buffer
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.cmd$" . cmd-mode))
(add-to-list 'auto-mode-alist '("\\.bat$" . cmd-mode))
(add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))
(setq ahk-syntax-directory (concat emacsd-dir "Syntax/"))
(setq grep-find-command "find . -type f -not -name \"*.svn-base\" -print0 | xargs -0 -e grep -nH -e ")
(setq diff-switches "-u") ; I like unified diff
(setq compilation-read-command nil)
(setq tramp-default-method "ssh")
(setq dabbrev-case-fold-search t)

(require 'psvn)
(setq svn-status-hide-unmodified t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

; -----keyboard bindings-----
(global-set-key (kbd "<escape>")      'keyboard-escape-quit)
(global-set-key (kbd "C-;")     'dabbrev-expand)
(global-set-key (kbd "C-c m")   'imenu-selection-buffer)
(global-set-key "\M-m"          'extend-selection) ;was newline-and-indent
(global-set-key "\C-z"          'undo)
(global-set-key "\C-v"          'yank)
(global-set-key (kbd "C-S-y")   'duplicate-line)
(global-set-key [next]          'pager-page-down)
(global-set-key "\ev"           'pager-page-up)
(global-set-key [prior]         'pager-page-up)
;(global-set-key "%"             'match-paren)
(global-set-key (kbd "M-x")     'smex)
(global-set-key (kbd "M-S-x")     'execute-extended-command)
(global-set-key (kbd "C--")     (lambda () (interactive) (cua-set-mark 5)))
(global-set-key (kbd "C-c o")   'occur)
(global-set-key (kbd "C-c i")   'insert-path)
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
(global-set-key (kbd "<f2> 1")	(lambda() (interactive) (select-window (get-window-at-corner 1))))
(global-set-key (kbd "<f2> 2")	(lambda() (interactive) (select-window (get-window-at-corner 2))))
(global-set-key (kbd "<f2> 3")	(lambda() (interactive) (select-window (get-window-at-corner 4))))
(global-set-key [f3]			'ido-switch-buffer)
(global-set-key [M-f3]			'ido-find-file)
(global-set-key [C-f3]			'insert-path)
(global-set-key [S-f3]			'ffap)
(global-set-key [f4]			'yas/expand)
(global-set-key [M-f4]			'delete-frame-or-exit)
(global-set-key [S-f4]  		'bury-buffer)
(global-set-key [C-f4]			'kill-this-buffer)
(global-set-key [M-S-f4]		'delete-window)
(global-set-key [C-S-f4]        (lambda() (interactive) (kill-buffer (current-buffer))
                                               (delete-window)))
(global-set-key [M-f5]			'reload-file)
(global-set-key [f6]			'shell-command-mod)
(global-set-key [C-f6]			'shell)
(global-set-key [f7]			'next-error)
(global-set-key [f8]	        'dired)
(global-set-key [C-f8]	        'svn-status)
(global-set-key [C-f9]	        'highlight-symbol-at-point)
(global-set-key [f9]	        'highlight-symbol-next)
(global-set-key [S-f9]	        'highlight-symbol-prev)
(global-set-key [M-f9]	        'highlight-symbol-remove-all)
(global-set-key [C-f10]	        'kmacro-start-macro)
(global-set-key [f10]	        'kmacro-end-or-call-macro)
(global-set-key [f11]	        'longlines-mode)
(global-set-key [M-f11]			'toggle-truncate-lines)
(global-set-key [C-f11]			'auto-fill-mode)
(global-set-key [f12]	        'google)
(global-set-key [C-f12]	        'browse-root-doc)
(global-set-key (kbd "<pause>")  'view-mode)
(global-set-key (kbd "<Scroll_Lock>")  'restore-windows-config)
(global-set-key (kbd "<apps>")  'smex)
(global-set-key (kbd "<menu>")  'smex)
(global-set-key [M-down] 'comment-and-go-down)
(global-set-key [M-up] 'uncomment-and-go-up)
(global-set-key (kbd "M-SPC") 'cua-set-mark)

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
;; (add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
;; (defun my-goto-match-beginning ()
;;     (when isearch-forward (goto-char isearch-other-end)))

;comcplete shortcut in minibuffer
(define-key minibuffer-local-completion-map (kbd "<f3>")
                          'complete-minibuffer-path)
(define-key minibuffer-local-map [f3]
  (lambda () (interactive)
     (insert (buffer-name (window-buffer (minibuffer-selected-window))))))
(define-key minibuffer-local-map (kbd "C-i") 'comint-dynamic-complete)

;;automatically close brackets, quotes, etc when typing
(setq skeleton-pair t)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)

;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-save 'ask)
(setq desktop-restore-eager 4)
(setq desktop-files-not-to-save "----------------------------------")
(desktop-save-mode 1)

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
                tags-file-name
                register-alist)))
;; 				saved-window-configuration)))

(when is-win32
  ; make the ugly \m at end of line go away
  (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

  (when has-cygwin
       ; getting cygwin bash as shell
    (setenv "PATH" (concat cygwin-path "bin;"
                           cygwin-path "usr/local/bin;"
                           (getenv "PATH")))
    (setenv "PS1" "\\[\\e[32m\\]\\w\\[\\e[0m\\] \\$ ")
    (add-to-list 'exec-path (concat cygwin-path "bin"))
    (add-to-list 'exec-path (concat cygwin-path "usr/local/bin"))
    (require 'cygwin-mount)
    (cygwin-mount-activate)

    (setq explicit-shell-file-name "bash.exe")
    (setq shell-file-name explicit-shell-file-name)
	(setq explicit-bash-args (quote ("--noediting" "-i" "--login"))))

  (when (not has-cygwin)
    (setq explicit-shell-file-name "cmdproxy")
    (add-to-list 'exec-path (concat home-dir "bin"))
    (setq shell-file-name explicit-shell-file-name)
    (setenv "PATH" (concat (concat home-dir "bin;") (getenv "PATH")))))

;(setq explicit-bash-args (quote ("--noediting" "--login")))


;=================================================

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
      (dabbrev-expand-multiple)
    (smart-indent)))

(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
	  (indent-region (region-beginning)
					 (region-end))
    (indent-according-to-mode)))
;(indent-for-tab-command)))

(defun duplicate-line ()
  "Clones the current line of text."
  (interactive)
  (save-excursion
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    (end-of-line) (newline) (yank) (forward-line 2) (current-kill 1)))

(defun reload-file () (interactive)
  (revert-buffer t t t) (message "File reloaded"))

(defun reload-file-force () (interactive)
  (let ((filename (buffer-file-name)))
	(kill-buffer (current-buffer))
	(find-file filename)
	(message "File reloaded")))

(defun dot-emacs ()  (interactive)
  (find-file (concat emacsd-dir "init.el")))

(defun ecustom ()  (interactive)
  (find-file (concat emacsd-dir "custom.el")))

(defun insert-path (&optional arg)
  "Inserts a path into the buffer with completion"
  (interactive "p*")
  (let ((filename (ido-read-file-name "Insert Path: ")))
	;; (if (not (null arg)) (setq filename (expand-file-name filename)))
	(setq filename (replace-regexp-in-string
					"^\\(/[a-z0-9]\\{2,5\\}:[-a-zA-Z0-9.]+:\\)" "" filename))
  (insert filename)))

(defun shell-command-mod
  (command &optional output-buffer error-buffer)
  "shell-command that displays the working direction as prompt"
  (interactive (list (read-from-minibuffer (concat default-directory " $: ")
                      nil nil nil 'shell-command-history)
             current-prefix-arg))
  (shell-command command output-buffer error-buffer))

(defun get-above-makefile-directory ()
  (let (makefile-dir) (setq makefile-dir
	   (loop as d = default-directory then (expand-file-name
       ".." d) if (file-exists-p (expand-file-name "Makefile" d))
       return d)) makefile-dir ))

(defun complete-minibuffer-path ()
  "Extension to the complete word facility of the minibuffer by
replacing matching strings to a specific path"
  (interactive)
  (let ((directory) (found t))
  (cond
     ; just add new entries if needed; shortcut up to 4 letters will work
     ((looking-back "j" 5 nil) (setq directory "D:/Desktop/"))
     ((looking-back "k" 5 nil) (setq directory "D:/Documents/"))
     ((looking-back "l" 5 nil) (setq directory (concat home-dir ".emacs.d/")))
     ((looking-back "i" 5 nil) (setq directory "D:/Programs/"))
     ((looking-back "o" 5 nil) (setq directory "D:/Documents/Visual Studio 2008/Projects/MusicDataBase/MusicLib/"))
     (t (setq found nil)))
  (cond (found (beginning-of-line)
                (kill-line)
                (insert directory))
         (t (minibuffer-complete)))))

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

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn (skip-syntax-forward "^\"")
               (goto-char (1+ (point)))
               (decf arg)) (skip-syntax-backward "^\"")
               (goto-char (1- (point))) (incf arg))) (up-list arg))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun extend-selection  (arg &optional incremental)
  "Select the current word. Subsequent calls expands the selection
   to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental (progn (semnav-up (- arg)) (forward-sexp)
                         (mark-sexp -1))
    (if (> arg 1) (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\")) (forward-sexp)))
      (mark-sexp -1))))

(defadvice split-window-vertically
  (after my-window-splitting-advice first () activate)
  "activate the next buffer whenever window splits"
    (set-window-buffer (next-window) (other-buffer)))
(defadvice split-window-horizontally
    (after my-window-splitting-advice first () activate)
  "activate the next buffer whenever window splits"
    (set-window-buffer (next-window) (other-buffer)))

(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (flet  ((one-window-p (&optional nomini all-frames) t)) ad-do-it))

(defadvice compile (before my-compile activate)
  (save-buffer))

(defadvice TeX-command-master (before my-TeX-command-master activate)
  (save-buffer))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(defadvice vc-version-diff (around my-vc-version-diff activate)
  "Don't shrink buffer when displaying vc-diff !!"
  (flet ((shrink-window-if-larger-than-buffer ())) ad-do-it))

(defun fullscreen ()
  "toggles whether the currently selected frame consumes the entire
   display or is decorated with a window border"
  (interactive)
  (if is-win32 (w32-maximize-frame)
  (progn (let ((f (selected-frame)))
    (modify-frame-parameters f `((fullscreen . ,
			  (if (eq nil (frame-parameter f 'fullscreen)) 'fullboth nil))))))))

(defun w32-maximize-frame ()
  "Maximize the current frame (windows only)"
  (interactive)
  (w32-send-sys-command 61488))

(defun w32-restore-frame ()
  "Restore a minimized/maximized frame (windows only)"
  (interactive)
  (w32-send-sys-command 61728))

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

(defun get-window-at-corner (corner &optional FRAME)
  "return windows at corner 1 = upper left, 2=upper right, etc"
  (let ((x (cond ((or (= corner 1) (= corner 3)) 4)
				 (t (- (frame-width) 4))))
		(y (cond ((or (= corner 1) (= corner 2)) 4)
				 (t (- (frame-height) 4)))))
	(window-at x y)))

(defun display-special-buffer (buf)
  "put the special buffer in the right spot (bottom rigt)"
;;   (let ((windows (delete (minibuffer-window) (window-list))))
;;     (if (eq 1 (length windows))
;;         (progn
;;           (select-window (car windows))
;;           (split-window-vertically)))
    (let ((target-window (get-window-at-corner 4))
          (pop-up-windows t))
      (set-window-buffer target-window buf)
      target-window));)

(defun my-display-buffer (buf)
  "put all buffers in a window other than the one in the bottom right"
  (if (member (buffer-name buf) special-display-buffer-names)
	  (display-special-buffer buf)
	  (progn
		(let ((pop-up-windows t)
			  (windows (delete (get-window-at-corner 4)
                         (delete (minibuffer-window) (window-list)))))
;; 		(if (<= 1 (length windows))
;; 			(progn
;; 			  (select-window (car windows))
;; 			  (split-window-vertically)))
		  (message (buffer-name (window-buffer (car windows))))
		  (set-window-buffer (car (cdr windows)) buf)
		  (car (cdr windows))))))

(defun my-display-buffer-23 (buf &optional args)
  "put all buffers in a window other than the one in the bottom right"
  (if (member (buffer-name buf) special-display-buffer-names)
	  (display-special-buffer buf)
	  (progn
		(let ((pop-up-windows t)
			  (windows (delete (minibuffer-window) (window-list))))
		  (if (not (equal (get-window-at-corner 4) (get-window-at-corner 2)))
			  (setq windows (delete (get-window-at-corner 4) windows)))
			  ;; 		(if (<= 1 (length windows))
;; 			(progn
;; 			  (select-window (car windows))
;; 			  (split-window-vertically)))
		  ;; (message (buffer-name (window-buffer (car windows))))
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
	(set-window-start w2 s1)))

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

(defun browse-root-doc ()
  (interactive)
  (let ((class (read-string "class name: ")))
	(browse-url (concat "http://root.cern.ch/root/html522/" class ".html")))
  (shell-command "wmctrl -a firefox")
  )

(defun google ()
  (interactive)
  (let ((class (read-string "google for: ")))
	(browse-url (concat "http://www.google.com/search?q=" class "&ie=utf-8&oe=utf-8&aq=t")))
  (shell-command "wmctrl -a firefox")
  )

(defun browse-img ()
  (interactive)
  (make-frame '((name . "View output -Emacs-") (width . 358) (height . 64) ))
  ;; (raise-frame)
  ;; (split-window-horizontally)
  )

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

(defadvice completing-read (around my-completing-read-ido activate)
  (if (boundp 'ido-cur-list)  ad-do-it
	(setq ad-return-value
		  (ido-completing-read
		   prompt
		   (all-completions "" collection predicate)
		   nil require-match initial-input hist def))))
; need this so that the above advice will not screw up reading file names
(setq read-file-name-function 'ido-read-file-name)

(defun build-tag-table (dir-name)
  "Create tag files"
  (interactive "DDirectory: ")
  (shell-command
   (format
	"find %s -type f  -name \"*.cpp\" -print   -or   -name \"*.hpp\" -print | etags -"
	dir-name)))

(defun home-quick-set ()
  (interactive)
  (custom-face-dark)
  (set-face-attribute 'default (selected-frame) :height 120)
  ;; (w32-maximize-frame)
  (restore-windows-config))

(defun larger-font () (interactive)
  (set-face-attribute 'default (selected-frame) :height 120))


(defvar closed-files (list))

(defun track-closed-file ()
  (and buffer-file-name
	   (add-to-list 'closed-files buffer-file-name)))

(defun last-closed-files ()
  (interactive)
  (find-file (ido-completing-read "Last closed: " closed-files)))

(add-hook 'kill-buffer-hook 'track-closed-file)


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

; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir (concat emacsd-dir "autosave/"))
(make-directory autosave-dir t)
(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
	   (concat "#" (file-name-nondirectory buffer-file-name) "#")
	 (expand-file-name
	  (concat "#%" (buffer-name) "#")))))

(setq backup-directory-alist (list (cons "." autosave-dir)))

(add-hook 'after-save-hook
		  #'(lambda ()
			  (and (save-excursion
					 (save-restriction
					   (widen)
					   (goto-char (point-min))
					   (save-match-data
						 (looking-at "^#!"))))
				   (not (file-executable-p buffer-file-name))
				   (shell-command (concat "chmod u+x " (shell-quote-argument buffer-file-name)))
				   (if is-win32
					   (set-buffer-file-coding-system 'emacs-mule-unix))
				   (message
					(concat "Saved as script: " buffer-file-name)))))
