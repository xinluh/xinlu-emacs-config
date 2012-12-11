(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq LaTeX-math-menu-unicode t)
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
(setq-default TeX-master nil)

(when (not is-win32)
  (setq TeX-output-view-style
		'(("^dvi$" "." "xdvi -expertmode 0 %d")
		  ("^pdf$" "." "SumatraPDF \"\" %o")
		  ("^html?$" "." "start \"\" %o")))
  (setq TeX-view-program-selection
		'(((output-dvi style-pstricks)
		   "dvips and start")
		  (output-dvi "xdvi")
		  ;; (output-pdf "SumatraPDF")
		  ;; (output-html "start")
		  ))
  (setq TeX-view-program-list
		'(
		  ("xdvi" "xdvi -expertmode 0 -unique -watchfile 2 %o")))
)
(when is-win32
  (setq TeX-output-view-style
		'(("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "dvips %d -o && start \"\" %f")
		  ("^dvi$" "." "yap -1 -s%n%b %d")
		  ("^pdf$" "." "SumatraPDF \"\" %o")
		  ("^html?$" "." "start \"\" %o")))
  (setq TeX-view-program-selection
		'(((output-dvi style-pstricks)
		   "dvips and start")
		  (output-dvi "Yap")
		  (output-pdf "SumatraPDF")
		  (output-html "start")))
  (setq TeX-view-program-list
		'(("SumatraPDF" "SumatraPDF -reuse-instance -view \"continuous single page\" %o")))
  ;; (setq TeX-source-specials-view-position-flags "-s %n%b")
  ;; (setq TeX-source-specials-view-editor-flags "")

  (defun substitute-character (the-string from-car to-car)
	(let ((i (- (length the-string) 1)))
	  (while (>= i 0)
		(if (= (aref the-string i) from-car)
			(aset the-string i to-car))
		(setq i (- i 1)))
	  the-string))
  
  (defun sumatra-jump-to-line() (interactive)
	(save-excursion
	  (let* (;;; current line in file, as found in the documentation
	   ;;; of emacs. Slightly non-intuitive.
			 (current-line (format "%d" (+ 0 (count-lines (point-min) (point)))))
	   ;;; name of the `main' .tex file, which is also used as .dvi basename:
			 (master-file (expand-file-name (TeX-master-file)
						   ;; (if (fboundp 'TeX-master-file)
								  ;; (TeX-master-file t)
								;; (kdvi-get-masterfile (kdvi-master-file-name)))
						   ))
        ;;; .pdf file name:
			 (pdf-file (concat (file-name-sans-extension master-file) ".pdf"))
			 (pdf-file-dos (substitute-character pdf-file ?/ ?\\))
        ;;; current source file name.
			 (filename (file-relative-name (expand-file-name (buffer-file-name)) (file-name-directory master-file) ))
			 (filename-dos (substitute-character filename ?/ ?\\))
        ;;; DDE message: uncomment one of the following two lines.
		;;; The first one shows SumatraPDF in the foreground, the second keeps it in the background.
										;(dde-message (concat "[ForwardSearch(\"" pdf-file-dos "\",\"" filename-dos "\"," current-line ",0,0,1)]"))
			 (dde-message (concat "[ForwardSearch(\"" pdf-file-dos "\",\"" filename-dos "\"," current-line ",0,0,0)]"))        
			 )
		(set-buffer (get-buffer-create " *ddeclient"))
		(erase-buffer)
		(message dde-message)
		(insert dde-message)
		(call-process-region (point-min) (point-max) "ddeclient" t t nil "SUMATRA" "control")
		;;  (if (= 0 (string-to-int (buffer-string))) t nil)	      
		)
	  )
	))
  
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

(defun my-tex-insert-item-maybe ()
  (interactive)
  (reindent-then-newline-and-indent)
  (when (or (string= "itemize" (LaTeX-current-environment))
			(string= "enumerate" (LaTeX-current-environment)))
	(insert "\\item ")
	(indent-region (line-beginning-position) (line-end-position))))

(defun my-tex-mode-hook ()
  ;; (visual-line-mode)
  (LaTeX-math-mode)
  (turn-on-reftex)
  (setq TeX-electric-sub-and-superscript t)
  (setq TeX-electric-escape t)
  (setq TeX-newline-function 'newline-and-indent)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  (setq TeX-source-correlate-method 'synctex) 
  (setq TeX-source-specials-mode t)

  (auto-fill-mode 1)
  (TeX-PDF-mode)
  ;; (setq-default TeX-master nil)
  
  (setq TeX-master (guess-TeX-master (buffer-file-name)))
  
  (local-set-key "$" 'skeleton-pair-insert-maybe)
  ;; (local-set-key "\C-m" reindent-then-newline-and-indent)
  ;; (local-set-key (kbd "<f5>") (lambda () (interactive) (save-buffer) (tex-file)))
  (local-set-key (kbd "<f5>") (kbd "C-c C-c C-j"))
  (local-set-key (kbd "<f7>") 'TeX-next-error)
  (local-set-key (kbd "C-c C-u") (lambda () (interactive) (insert "itemize")(yas/expand)))
  (local-set-key (kbd "C--") (lambda () (interactive) (insert "_")))
  (local-set-key (kbd "C-6") (lambda () (interactive) (insert "^")))
  (local-set-key (kbd "C-2") (lambda () (interactive) (insert "\\sqrt{}") (backward-char)))
  (local-set-key (kbd "C-j") 'my-tex-insert-item-maybe)
  (local-set-key "\\" (lambda () (interactive)
				 (if (texmathp) (insert "\\") (TeX-insert-backslash 0))))

  (fset 'compile-n-run (kbd "C-c C-c C-j"))
  (add-hook 'auto-save-hook (lambda () (interactive) (if (buffer-modified-p) (execute-kbd-macro (kbd "C-c C-c C-j")))) nil t)
  
  (when is-win32
	(local-set-key (kbd "<f9>") 'sumatra-jump-to-line))
  
  (add-to-list 'TeX-command-list
			   '("DVI to PDF" "dvipdfm %d" TeX-run-command t t) t)

  ;; highlight TODO
  (font-lock-add-keywords nil
						  '(("\\<\\([t|T][o|O][d|D][o|O]:*\\)" 1 font-lock-warning-face prepend)
							("\\<\\([f|F][i|I][x|X][m|M][e|E]:*\\)" 1 font-lock-warning-face prepend)
							("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)))


  ;; (define-abbrev-table 'TeX-mode-math-abbrev-table (make-abbrev-table))
  ;; ;; (add-hook 'TeX-mode-hook (lambda ()
							 ;; ;; (setq abbrev-mode t)
							 ;; ;; (setq local-abbrev-table TeX-mode-abbrev-table)))
  ;; (defun tex-math-mode-abbrev-expand-function (expand)
	;; (if (not (texmathp))
		;; ;; Performs normal expansion.
		;; (funcall expand)
	  ;; ;; We're inside a math environment:
	  ;; (let ((local-abbrev-table TeX-mode-abbrev-table))
		;; (funcall expand))))  
  ;; (add-hook 'abbrev-expand-functions 'tex-math-mode-abbrev-expand-function
						  ;; nil t)
  )

;; (add-hook 'tex-mode-hook 'my-tex-mode-hook)
(add-hook 'TeX-mode-hook 'my-tex-mode-hook)

(defadvice LaTeX-insert-item (around my-LaTeX-insert-item activate)
  "Workaround for undesirable behavior when an item in latex ended with comment"
  (let  ((LaTeX-insert-into-comments nil)) ad-do-it))

(defadvice reftex-offer-label-menu (around no-delete-windows activate)
  (flet ((delete-other-windows ())) ad-do-it))

