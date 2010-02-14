(defvar emacs-runned-once nil "whether .emacs file has been runned once")
(defconst is-win32 (eq system-type 'windows-nt) "Running on windows")
(defconst is-linux (or (eq system-type 'gnu/linux) (eq system-type 'linux))
	"Running on linux")
(defconst is-emacs21 (eq emacs-major-version 21))
(defconst is-emacs23 (eq emacs-major-version 23))
(when is-win32
  (defvar program-dir (substring data-directory 0 -4) "Where the emacs binary sits")
  (defvar cygwin-path "D:/cygwin/")
  (defconst has-cygwin (file-exists-p cygwin-path))) 
(defvar home-dir (if is-win32 (concat (getenv "HOME") "/") "~/") "My home directory.")
(defvar emacsd-dir (concat home-dir ".emacs.d/"))
(setq user-emacs-directory emacsd-dir)

(add-to-list 'load-path emacsd-dir)
(add-to-list 'load-path (concat emacsd-dir "other"))

(require 'yasnippet) 
(yas/initialize)
(yas/load-directory (concat emacsd-dir "snippets"))

(autoload 'ahk-mode "ahk-mode")
(setq ahk-syntax-directory (concat emacsd-dir "other/ahk-syntax-files"))

(autoload 'cmd-mode "cmd-mode" "CMD mode." t)
(autoload 'csharp-mode "csharp-mode")
(autoload 'ahk-mode "ahk-mode")
(autoload 'svn-status "psvn")
(autoload 'folding-mode "folding" "Folding mode" t)

(require 'pager)
(require 'bm)
(require 'dired-isearch)
(require 'highlight-symbol)
(require 'dabbrev-expand-multiple)

(require 'mk-project)
(if (file-exists-p (concat emacsd-dir "personal/projects.el"))
	(load (concat emacsd-dir "personal/projects.el")))

(require 'dired-details)
(dired-details-install)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-quit-action 'save-and-restore)

(require 'smex)
(setq smex-save-file (concat emacsd-dir "personal/smex.save"))

(when is-emacs21 (require 'prev-next-buffer))

(setq custom-file (concat emacsd-dir ".emacs-custom.el"))
(load custom-file)

(defun byte-compile-if-newer-and-load (file)
   "Byte compile file.el if newer than file.elc"
   (let ((path (concat emacsd-dir file)))
    (if (or (file-newer-than-file-p (concat path ".el") (concat path ".elc"))
			(not (file-exists-p (concat path ".elc"))))
		(byte-compile-file (concat path ".el") t))
	    (load file)))

(byte-compile-if-newer-and-load "custom-faces")
(byte-compile-if-newer-and-load "custom")
(byte-compile-if-newer-and-load "custom-hooks")

(byte-compile-if-newer-and-load "my-tex")
(byte-compile-if-newer-and-load "my-dired")

;========== emacs server ==========
(if (and is-win32 (file-exists-p "d:/Users/xinlu/Documents/"))
	(setq server-auth-dir "d:/Users/xinlu/Documents/"))
(setq server-use-tcp t)
(setq server-host "myPC")
(server-start)

; maximize emacs on startup
(when (not emacs-runned-once)
  (restore-windows-config)
  (when is-win32 (fullscreen)))

;; this need to be near the end of all customization so that the custom functions
;; are scanned.
(smex-initialize)

(setq emacs-runned-once t)

(message "emacs is ready to go!")
