
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
(load-custom "my-dired")

;; ; set some private variables
;; (load (concat emacsd-dir "personal/personal.el"))

(if (not emacs-runned-once) (server-start))
;; (if (not emacs-runned-once) (restore-windows-config))

(load-theme 'solarized-light)

;; this need to be near the end of all customization so that the custom functions are scanned.
(smex-initialize)

(setq emacs-runned-once t)

(message "emacs is ready to go!")
