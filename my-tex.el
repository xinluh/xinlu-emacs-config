(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq LaTeX-math-menu-unicode t)
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
(setq-default TeX-master nil)

(when is-win32
  (setq TeX-output-view-style
		'(("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "dvips %d -o && start \"\" %f")
		  ("^dvi$" "." "yap -1 %dS %d")
		  ("^pdf$" "." "start \"\" %o")
		  ("^html?$" "." "start \"\" %o")))
  (setq TeX-source-specials-view-position-flags "-s %n%b")
  (setq TeX-source-specials-view-editor-flags ""))
  
  ;; add miktex to path
  (add-to-list 'exec-path "D:/Programs/miktex/miktex/bin")
  (setenv "PATH" (concat "D:/Programs/miktex/miktex/bin;" (getenv "PATH")))

(defadvice TeX-command-master (before my-TeX-command-master activate)
  (save-buffer))
  
(defun guess-TeX-master (filename)
  "Guess the master file for FILENAME from currently open .tex files."
   (let ((candidate nil)
	 (filename (file-name-nondirectory filename)))
	 (save-excursion
	   (dolist (buffer (buffer-list))
		 (with-current-buffer buffer
		   (let ((name (buffer-name))
				 (file buffer-file-name))
			 (if (and file (string-match "\\.tex$" file))
				 (progn
				   (goto-char (point-min))
				   (if (re-search-forward (concat "\\\\input{" filename "}") nil t)
					   (setq candidate file))
				   (if (re-search-forward (concat "\\\\input{.*/" (file-name-sans-extension filename) "}") nil t)
					   (setq candidate file))
				   (if (re-search-forward (concat "\\\\include{" (file-name-sans-extension filename) "}") nil t)
					   (setq candidate file))))))))
	 (if candidate
		 (message "TeX master document: %s" (file-name-nondirectory candidate)))
	 candidate))
   
;; I keep all the electronic versions of the papers in my bibliography (PostScript or PDF)
;; in a single directory, with the name of the file corresponding to the BibTeX key. When
;; I'm writing a paper and want to refresh my memory about some work I cite, the following
;; code gives me instant access to the electronic version of a papers. Just put the
;; TextCursor on a BibTeX key, press S-F6, and GV or AcrobatReader? opens the
;; corresponding file.
   
;; ;; Change this to the place where you store all the electronic versions.
;; (defvar bibtex-papers-directory "~/workspace/bibliographie/papers/")
   
;; ;; Translates a BibTeX key into the base filename of the corresponding
;; ;; file. Change to suit your conventions.
;; ;; Mine is:
;; ;; - author1-author2-author3.conferenceYYYY for the key
;; ;; - author1-author2-author3_conferenceYYYY.{ps,pdf} for the file
;; (defun bibtex-key->base-filename (key)
   ;; (concat bibtex-papers-directory
   ;; (replace-regexp-in-string "\\." "_" key)))
   
;; ;; Finds the BibTeX key the point is on.
;; ;; You might want to change the regexp if you use other strange characters in the keys.
;; (defun bibtex-key-at-point ()
   ;; (let ((chars-in-key "A-Z-a-z0-9_:-\\."))
   ;; (save-excursion
   ;; (buffer-substring-no-properties
   ;; (progn (skip-chars-backward chars-in-key) (point))
   ;; (progn (skip-chars-forward chars-in-key) (point))))))
   
;; ;; Opens the appropriate viewer on the electronic version of the paper referenced at point.
;; ;; Again, customize to suit your preferences.
;; (defun browse-paper-at-point ()
   ;; (interactive)
   ;; (let ((base (bibtex-key->base-filename (bibtex-key-at-point))))
   ;; (cond
   ;; ((file-exists-p (concat base ".pdf"))
   ;; (shell-command (concat "acroread " base ".pdf &")))
   ;; ((file-exists-p (concat base ".ps"))
   ;; (shell-command (concat "gv " base ".ps &")))
   ;; (t (message (concat "No electronic version available: " base ".{pdf,ps}"))))))
   
;; (global-set-key [S-f6] 'browse-paper-at-point)
   
(defun my-tex-mode-hook ()
  (visual-line-mode)
  (LaTeX-math-mode)
  (turn-on-reftex)
  (setq TeX-electric-sub-and-superscript t)
  (setq TeX-electric-escape t)
  (setq TeX-newline-function 'newline-and-indent)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (auto-fill-mode 1)
  ;; (setq-default TeX-master nil)
  
  (setq TeX-master (guess-TeX-master (buffer-file-name)))
  
  (local-set-key "$" 'skeleton-pair-insert-maybe)
  ;; (local-set-key "\C-m" reindent-then-newline-and-indent)
  ;; (local-set-key (kbd "<f5>") (lambda () (interactive) (save-buffer) (tex-file)))
  (local-set-key (kbd "<f5>") (kbd "C-c C-c C-j"))
  (local-set-key (kbd "<f7>") 'TeX-next-error)

  (add-to-list 'TeX-command-list
			   '("DVI to PDF" "dvipdfm %d" TeX-run-command t t) t)
  )
;; (add-hook 'tex-mode-hook 'my-tex-mode-hook)
(add-hook 'TeX-mode-hook 'my-tex-mode-hook)

(defadvice LaTeX-insert-item (around my-LaTeX-insert-item activate)
  "Workaround for undesirable behavior when an item in latex ended with comment"
  (let  ((LaTeX-insert-into-comments nil)) ad-do-it))

(defadvice reftex-offer-label-menu (around no-delete-windows activate)
  (flet ((delete-other-windows ())) ad-do-it))

