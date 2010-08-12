(setq dired-listing-switches "-alh") ; human readable sizes
  
(when is-win32 (setq ls-lisp-ignore-case t)
      ;(setq ls-lisp-verbosity (delq 'uid ls-lisp-verbosity))
      (setq ls-lisp-verbosity (delq 'gid ls-lisp-verbosity))
      (setq ls-lisp-dirs-first t)
	  (setq w32-get-true-file-attributes t))
(setq dired-dwim-target t)
;; (put 'dired-find-alternate-file 'disabled nil)

(setq image-dired-thumb-height 400)
(setq image-dired-thumb-width 400)

;for image-dired mode
(when is-win32
  (setq image-dired-external-viewer  "D:/Programs/IrfanView/i_view32.exe"))

(defun dired-do-cat-file ()
  "run 'cat' on the marked or current file; useful when visiting file is slow over tramp"
  (interactive)
  (when (not (string-match "\.root" (dired-get-filename t)))
	(shell-command (concat "cat " (dired-get-filename t)))))

(defun dired-do-tail-file ()
  "run 'tail' on the marked or current file; useful when visiting file is slow over tramp"
  (interactive)
  (when (not (string-match "\.root" (dired-get-filename t)))
	(shell-command (concat "tail -n 35 " (dired-get-filename t)))))

(defun dired-do-open-files ()
  "Open marked or current file in dired buffer with proper file
  association using the 'open' program"
  (interactive)
  (let (files)
    (if (= (length (dired-get-marked-files)) 0)
        (setq files (dired-get-filename))
	  (setq files (mapconcat (function (lambda (x) (concat "\"" x "\"")))
							 (dired-get-marked-files) " ")))
    (shell-command (concat "open-cyg " files))))

(defun dired-find-multiple-file (&optional arg)
  "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
  (interactive "P")
  (let* ((fn-list (dired-get-marked-files nil arg)))
	(mapc 'find-file fn-list)))

(defun dired-do-insert-subdir-maybe ()
  "Insert current or marked lines in dired buffer just as dired-maybe-insert-subdir "
  (interactive)
  (if (= (length (dired-get-marked-files)) 0)
	  (dired-maybe-insert-subdir (dired-get-filename))
	(dired-map-over-marks-check (lambda()
				  (dired-maybe-insert-subdir (dired-get-filename)  nil t) ) nil 'display t)
	))

(defun dired-do-get-size ()
  "Use du to find out the total size of all marked files"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (message "Getting size of marked file(s)...")
    (with-temp-buffer
      (apply 'call-process "du" nil t nil "-sch" files)
      (message "Size of marked files: %s"
               (progn
                 (re-search-backward "\\(^[0-9.]+[A-Za-z]*\\).*total$")
				 (match-string 1))))))

(defun dired-copy-filename-as-kill-newline (&optional arg)
  "same as dired-copy-filename-as-kill but use newline instead of space as separator"
  (interactive "P")
  (let ((string
         (or (dired-get-subdir)
             (mapconcat (function identity)
                        (if arg
                            (cond ((zerop (prefix-numeric-value arg))
                                   (dired-get-marked-files))
                                  ((consp arg)
                                   (dired-get-marked-files t))
                                  (t
                                   (dired-get-marked-files
									'no-dir (prefix-numeric-value arg))))
                          (dired-get-marked-files 'no-dir))
                        "\n"))))
    (if (eq last-command 'kill-region)
		(kill-append string nil)
      (kill-new string))
	(message "Filenames copied to kill ring.")))


(defun my-dired-mode-hook()
  (setq truncate-lines t)
  (when (require 'wdired nil t)
	(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))
  (local-set-key (kbd "O") 'dired-do-open-files)
  (local-set-key (kbd "F") 'dired-find-multiple-file)
  (local-set-key (kbd "C-o") 'dired-find-files-other-window)
  (local-set-key (kbd "o") 'dired-display-file)
  (local-set-key (kbd "E") 'dired-do-cat-file)
  (local-set-key (kbd "c") 'dired-do-tail-file)
  (local-set-key (kbd "W") 'dired-copy-filename-as-kill-newline)
  (local-set-key (kbd "\\") 'dired-do-get-size)
  (local-set-key (kbd "\`") 'dired-up-directory)
  (local-set-key (kbd "C-s") 'dired-isearch-forward)
  (local-set-key (kbd "I") 'dired-do-insert-subdir-maybe)
  (local-set-key (kbd "j") 'dired-isearch-forward) ; was dired-do-goto-line
  (local-set-key (kbd "C-r") 'dired-isearch-backward)
  (local-set-key (kbd "ESC C-s") 'dired-isearch-forward-regexp)
  (local-set-key (kbd "ESC C-r") 'dired-isearch-backward-regexp)
  (local-set-key (kbd "<delete>") 'dired-do-delete)
  (local-set-key (kbd "<")   'dired-prev-subdir)
  (local-set-key (kbd ">")   'dired-next-subdir)
  
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
