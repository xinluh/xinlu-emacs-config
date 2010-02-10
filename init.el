;(setq initial-frame-alist '((width . 160) (height . 60)))
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
;(add-to-list 'load-path (concat emacsd-dir "w3m"))
;(add-to-list 'load-path (concat emacsd-dir "g-client"))

(require 'yasnippet) 
(yas/initialize)
(yas/load-directory (concat emacsd-dir "snippets"))

(setq ahk-syntax-directory (concat emacsd-dir "Syntax/"))
(autoload 'ahk-mode "ahk-mode")
(add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))

(autoload 'cmd-mode "cmd-mode" "CMD mode." t)
(autoload 'csharp-mode "csharp-mode")
(autoload 'ahk-mode "ahk-mode")
(autoload 'svn-status "psvn")
(autoload 'folding-mode "folding" "Folding mode" t)

(autoload 'magit-status "magit" nil t)
(when is-win32
  (setq magit-git-executable "git.cmd"))
   
;; (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; (require 'key-chord)
;; (key-chord-mode 1)
;; (key-chord-define-global "hj"     'pager-page-down)
;; (key-chord-define-global "jk"     'pager-page-up)

;(require 'w3m-load)
;; (require 'grep-edit)
;(load-library "g")
(require 'pager)
(require 'bm)
(require 'dired-isearch)
(require 'highlight-symbol)
(require 'dabbrev-expand-multiple)

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
    (if (file-newer-than-file-p (concat file ".el") (concat file ".elc"))
		(byte-compile-file (concat file ".el") t)
	    (load file)))

(byte-compile-if-newer-and-load "custom-faces")
(byte-compile-if-newer-and-load "custom")
(byte-compile-if-newer-and-load "custom-hooks")

(byte-compile-if-newer-and-load "my-tex")
(byte-compile-if-newer-and-load "my-dired")

(if (and is-win32 (file-exists-p "d:/Users/xinlu/Documents/"))
	(setq server-auth-dir "d:/Users/xinlu/Documents/"))
(setq server-use-tcp t)
(setq server-host "myPC")
(server-start)

; maximize emacs on startup
(when (not emacs-runned-once)
  (restore-windows-config)
  (when is-win32   (fullscreen)))
;; (toggle-fullscreen)

;; this need to be near the end of all customization so that the custom functions
;; are scanned.
(smex-initialize)

(setq emacs-runned-once t)

(message "emacs is ready to go!")


;needed on Windows, otherwise My Documents folder is not recognized
;put last, as this will cause an error and stop evaluation
;(when is-win32
;  (setenv "HOME" home-dir)
;)

;(setq emacs-load-start-time (current-time))
;
;(when (require 'time-date nil t)
;  (message "Emacs startup time: %d seconds"
;		   (time-to-seconds (time-since emacs-load-start-time))))
;
;) ; when (not load-once)
