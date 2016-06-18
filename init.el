(defvar emacs-runned-once nil "whether .emacs file has been runned once")

(setq custom-file (concat user-emacs-directory ".emacs-custom.el"))
(load custom-file)

(defun load-custom (file)
  (load (concat user-emacs-directory file ".el")))
(load-custom "custom-packages")
(load-custom "custom")
(load-custom "custom-hooks")

(when (file-exists-p (concat user-emacs-directory "local.el"))
  (load-custom "local"))

;; (byte-compile-if-newer-and-load "my-tex")
;; (byte-compile-if-newer-and-load "my-dired")

;; ; set some private variables
;; (load (concat emacsd-dir "personal/personal.el"))

(if (not emacs-runned-once) (server-start))
;; (if (not emacs-runned-once) (restore-windows-config))

(setq solarized-high-contrast-mode-line t)
(setq solarized-distinct-doc-face t)
(setq solarized-distinct-fringe-background t)
(setq solarized-use-more-italic t)
(setq solarized-use-less-bold t)
(setq solarized-emphasize-indicators nil)
(load-theme 'solarized-light)

;; this need to be near the end of all customization so that the custom functions are scanned.
(smex-initialize)

(setq emacs-runned-once t)

(message "emacs is ready to go!")
