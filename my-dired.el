(setq dired-listing-switches "-alh") ; human readable sizes
(when is-win32 (setq ls-lisp-ignore-case t)
      ;(setq ls-lisp-verbosity (delq 'uid ls-lisp-verbosity))
      (setq ls-lisp-verbosity (delq 'gid ls-lisp-verbosity))
      (setq ls-lisp-dirs-first t))
(setq dired-dwim-target t)
;; (put 'dired-find-alternate-file 'disabled nil)

(setq image-dired-thumb-height 400)
(setq image-dired-thumb-width 400)

;for image-dired mode
(when is-win32
  (setq image-dired-external-viewer  "D:/Programs/IrfanView/i_view32.exe"))

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
