(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
(setq-default TeX-master nil)
(autoload 'magit-status "magit" nil t)

(when is-win32
  (setq TeX-output-view-style
		'(("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "dvips %d -o && start \"\" %f")
		  ("^dvi$" "." "yap -1 %dS %d")
		  ("^pdf$" "." "start \"\" %o")
		  ("^html?$" "." "start \"\" %o")))
  (setq TeX-source-specials-view-position-flags "-s %n%b")
  (setq TeX-source-specials-view-editor-flags "")
  
  ;; add miktex to path
  (add-to-list 'exec-path "D:/Programs/miktex/miktex/bin")
  (setenv "PATH" (concat "D:/Programs/miktex/miktex/bin;" (getenv "PATH")))
  
  (setq magit-git-executable "git.cmd"))

	
(defun my-tex-mode-hook ()
  (visual-line-mode)
  (LaTeX-math-mode)
  (turn-on-reftex)
  (setq TeX-electric-sub-and-superscript t)
  (setq TeX-electric-escape t)
  (setq TeX-newline-function 'newline-and-indent)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;; (setq-default TeX-master nil)
  
  (setq TeX-master (guess-TeX-master (buffer-file-name)))
  
  (local-set-key "$" 'skeleton-pair-insert-maybe)
  ;; (local-set-key "\C-m" reindent-then-newline-and-indent)
  ;; (local-set-key (kbd "<f5>") (lambda () (interactive) (save-buffer) (tex-file)))
  (local-set-key (kbd "<f5>") (kbd "C-c C-c C-j"))
  (local-set-key (kbd "<f7>") 'TeX-next-error)
  )
;; (add-hook 'tex-mode-hook 'my-tex-mode-hook)
(add-hook 'TeX-mode-hook 'my-tex-mode-hook)
