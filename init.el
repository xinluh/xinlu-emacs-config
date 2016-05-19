(defvar emacs-runned-once nil "whether .emacs file has been runned once")

(setq custom-file (concat user-emacs-directory ".emacs-custom.el"))
(load custom-file)

(defun load-custom (file)
  (load (concat user-emacs-directory file ".el")))
(load-custom "custom-packages")
(load-custom "custom")
(load-custom "custom-hooks")

;; (byte-compile-if-newer-and-load "my-tex")
;; (byte-compile-if-newer-and-load "my-dired")

;; ; set some private variables
;; (load (concat emacsd-dir "personal/personal.el"))

(if (not emacs-runned-once) (server-start))
;; (if (not emacs-runned-once) (restore-windows-config))

(load-theme 'solarized-light)
(setq solarized-high-contrast-mode-line t)

;; this need to be near the end of all customization so that the custom functions are scanned.
(smex-initialize)

(setq emacs-runned-once t)

(message "emacs is ready to go!")
