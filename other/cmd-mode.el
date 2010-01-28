;;; cmd-mode.el --- Edit of MS Windows cmd and bat files 
;;
;; Copyright (C) 2001-2005 by Tadamegu Furukawa. <tfuruka1 at nifty dot com>
;;
;; Author: Tadamegu Furukawa <tfuruka1 at nifty dot com>
;; Maintainer: Lennart Borgman <lennart dot borgman dot 073 at student dot lu dot se>
;; Created: 2001-08-28
;; Version: 1.4
;; Keywords:
;;
;;
;; Kindly translated to English 2005 by Kenichi Handa after an inquiry
;; by Lennart Borgman.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file is not part of GNU Emacs.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; Tiny mode for editing batch files of MS-DOS and command scripts of
;; Windows NT.  The purpose is to improve the readablity of those
;; files that contains many `%' and are difficult to read.  So,
;; basically this provides only a proper font-lock setting.  This
;; program has been tested for Emacs 20.7 "Meadow-1.14 (AWSAKA:62)
;; (the other versions of Emacsen are out of my focus).
;;
;; It has now been tested also on Emacs 21.3 and 22.0.50 on W2k.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Usage:
;;
;; Please input the following code somewhere in your .emacs.  Then,
;; cmd-mode is automatically activated when you open a file whose
;; extention is "cmd" or "bat".

;;    (autoload 'cmd-mode "cmd-mode" "CMD mode." t)
;;    (setq auto-mode-alist (append '(("\\.\\(cmd\\|bat\\)$" . cmd-mode))
;;                                  auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; History:
;; 
;;;;;;;;;;;;;;;;; What is this:
;; $Id: cmd-mode.el,v 1.1 2005/07/18 14:38:50 Administrator Exp $
;; $Log: cmd-mode.el,v $
;; Revision 1.1  2005/07/18 14:38:50  Administrator
;; *** empty log message ***
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Revision 1.4  2005/10/18
;; 
;; o Translation to English. (Kenichi Handa)
;; o Changed set-mark to push-mark. (Lennart Borgman)


;; Revision 1.3  2001/09/11 12:39:03  tfuruka1
;;
;; o Add menu.
;;
;; o Add a facility to jump to any label.


;; Revision 1.2  2001/08/31 13:25:50  tfuruka1
;;
;; o Fix the problme of incorrect coloring in the case that an
;;   environment varialbe of SET contains a keyword string.
;;
;; o FIx the problem of incorrectly coloring a part of operand in SET
;;   /A as an environment variable in

;; Revision 1.1  2001/08/28 13:14:43  tfuruka1
;;
;; o Initial version.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:


;; Added by L Borgman:
(require 'font-lock)

;; (replace-regexp "[\t ]+$" "")

(defconst cmd-mode-revision-number "1.4" "cmd-mode.el version number.")

;;; *** menu bar
(defvar cmd-menu-bar (make-sparse-keymap "cmd-mode-menu")
  "Menu bar entry.")
(defvar cmd-submenu-jump (make-sparse-keymap "cmd-mode-submenu-jump")
  "Popup menu for jump.")

;;; *** hook
(defvar cmd-mode-hook nil
  "Hook called when `cmd-mode' is entered.")
(defvar cmd-help-mode-hook nil
  "Hook called when `cmd-help-mode' is entered.")

;;; *** External command
(defvar cmd-help-command "help"
  "Name of `help' command.  It exists only on Windows NT.")

;;; *** Buffer related things.
(defvar cmd-temp-buf " *cmd-temp-buf*"
  "Hidden buffer for work.")
(defvar cmd-help-buf-base-name "*cmd-help "
  "Base name of help buffer.")

;;; *** regexp
(defvar cmd-full-size-space-etc-pattern "\\(邵ｲﾂ+\\)"
  "Regular expression for fullwidth space sequence.")
(defvar cmd-tab-pattern "\\(\t+\\)"
  "Regular expression for TAB character sequence.")
(defvar cmd-comment-pattern "^[ \t]*\\(@*\\)\\(rem[\t ]\\|::\\)\\(.*$\\|$\\)"
  "Regular expression for a comment line.")
(defvar cmd-variable-pattern
  (concat "\\("
          "%\\("                     ;  the first char is `%'
          "\\*"                      ; in case of "%*"
          "\\|"
          "~[^0-9 \n]+[0-9a-z]"      ;	e.g. ~f1
          "\\|"
          "[0-9a-z]\\b"              ;	e.g. %1
          "\\|"
          "[^%\n 邵ｲﾂ-魄ｴ�]+%"           ;normal env. var.
          "\\|"
          "%[0-9a-z]"               ;	e.g. %$
          "\\)"
          "\\)")
  "Regular expression for expanding an environment variable.

The following patterns are concerned.

example		meaning
-------		-------
%*		refer to all of the arguments
%~f1, %~dp1	replacement for batch parameter (%n) and FOR variable reference
%STRING%	normal reference of an environment variable
%1, %i		batch parameter (%n) and FOR variable reference
%%, %%1, %%i	% itself, batch parameter (%n) and FOR variable reference
		(when used in a batch file)"
)
(defvar cmd-const-pattern
  "\\(^[ \t]*@\\|nul\\|:eof\\|&&\\||\\|\\^\\|&[12]?\\|[,;]\\)"
  "Regular expression for conditional symbols.")
(defvar cmd-set-pattern
  (concat "\\b"
          "\\(set\\b\\)\\([ \t]+/a\\)*"
          "\\(\\([ \t]+[_A-Za-z-][_A-Za-z0-9-]*\\)*\\)"
          "\\([-+/\\*]\\|\\W\\)*")
  "Regular expression for SET command.")
(defvar cmd-label-pattern "^[ \t]*\\(:[A-Za-z0-9_-]+\\)"
  "Regular expression for a label.")
(defvar cmd-redirect-pattern
  (concat "\\("
          "[12]?>>?"                 ;1> 2> 1>> 2>> > >>
          "\\|"
          "<[12]?"                      ;<1 <2 <
          "\\|"
          "2>&1"
          "\\|"
          "1>&2"
          "\\)")
  "Regular expression for redirection.")
(defvar cmd-option-pattern
  (concat "\\b"
          (regexp-opt
           '(
             ;; if
             "not" "exist" "defined" "errorlevel" "cmdextversion"
             "equ" "neq" "lss" "leq" "gtr" "geq"
             ;; for
             "eol" "skip" "delims" "tokens" "in" "do") t) "\\b")
  "Regular expression for options to a statement like IF.")
(defvar cmd-command-pattern
  (concat "\\b"
          (regexp-opt '("assoc" "break" "call" "cd" "chdir" "cls"
                        "color" "copy" "date" "del" "dir" "echo"
                        "echo." "endlocal" "erase" "exit" "for"
                        "ftype" "goto" "if" "md" "mkdir" "move" "path"
                        "pause" "popd" "prompt" "pushd" "rd" "ren"
                        "rename" "rmdir" "setlocal" "shift" "start"
                        "time" "title" "type" "ver" "verify" "vol" )
                      t) "\\b")
  "Regular expression for internal commands.

Only internal commands.  Actually they are commands listed by
HELP command of Windows NT (such external commands as *.ext and
*.com are excluded).  SET and REM are also excluded because they
are defined in the other place.  As for ECHO, ECHO.  is included
too.")

(defvar cmd-font-lock-keywords
  (list
   ;; command
   (list cmd-comment-pattern
         '(1 font-lock-constant-face)   ;@ at bol
         '(2 font-lock-comment-face)    ;rem
         '(3 font-lock-comment-face)    ;command character
         )
   ;; SET
   (list cmd-set-pattern
         '(1 font-lock-keyword-face)    ;SET
         '(3 font-lock-type-face)       ;霑ｺ�ｰ陟�ｿｽ�､逕ｻ辟夊惺ﾂ	name of env. var.
         )
   ;; label
   (list cmd-label-pattern
         '(1 (cons font-lock-function-name-face '(underline))))
   ;; redirect symbol
   (list cmd-redirect-pattern 1 font-lock-warning-face)
   ;; reference of environment variable
   (list cmd-variable-pattern 1 font-lock-variable-name-face)
   ;; internal command
   (list cmd-command-pattern 1 font-lock-keyword-face)
   ;; e.g. conditional symbols
   (list cmd-const-pattern 1 font-lock-constant-face)
   ;; e.g. options of IF and FOR
   (list cmd-option-pattern 1 font-lock-builtin-face)
   ;; fullwidth space
   (list cmd-full-size-space-etc-pattern '(1 '(underline)))
   ;; TAB character
   (list cmd-tab-pattern '(1 '(highlight)))
   )
  "Setting for font-lock used in `cmd-mode' and `cmd-help-mode'.
See `font-lock-defaults' for detail.")

(defun cmd-mode-version ()
  "Show the version of `cmd-mode'."
  (interactive)
  (message
   (concat "cmd-mode version " cmd-mode-revision-number)))

(defun cmd-help (arg)
  "Execute help command and show the result.
This functinos works only on Windows NT.

Implementers note: I intended that this work also on Windows
\\(9[58]\\|Me\\), but actually failed.
Argument ARG is the command to describe."
  (interactive
   (list (let* ((command (current-word))
                (input (read-string
                        (format "Help for command%s: " ;; "郢ｧ�ｳ郢晄ｧｭﾎｦ郢敖%s: "
                                (if (string-equal command "")
                                    ""
                                  (format " (%s)" command))))))
                (if (string-equal input "")
                    (if (string-equal command "")
                        "help"
                      command)
                  input))))

  (let* ((case-fold-search t)
        (cmd-help-buffer (format "%s%s*"
                                 cmd-help-buf-base-name (downcase arg)))
        (comspec (getenv "ComSpec"))
        (cmd-help-arg
         (cond
          ((string-match "cmd\\.exe$" comspec)
           (list "\\/c" cmd-help-command
                 (if (string-equal arg "help") "" arg)))
          ((string-match "command\\.com$" comspec)
           (if (string-equal arg "help")
               (error "Insert command" ;; "郢ｧ�ｳ郢晄ｧｭﾎｦ郢晏ｳｨ�定怦�･陷牙ｸ呻ｼ�邵ｺ�ｦ邵ｺ荳岩味邵ｺ霈費ｼ"
		      )
             (list "\\/c" arg "\\/?")))
          (t
           (error (concat "Work only on WindowsXX ComSpec="
			  comspec)))))
        )
    (set-buffer (get-buffer-create cmd-help-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (apply (function call-process) comspec nil t nil cmd-help-arg)
    (goto-char (point-min))
    (display-buffer cmd-help-buffer)
    (cmd-help-mode)
    ))

(defun cmd-help-mode-exit ()
  "Terminate `cmd-help-mode'."
  (interactive)
  (if (eq major-mode 'cmd-help-mode)
      (kill-buffer (current-buffer)))
  )

(defun cmd-help-mode ()
  "Mode for `cmd-help'.

o Execute help command and show the result.
  \\[cmd-help]

o Terminate `cmd-help-mode'.
  \\[cmd-help-mode-exit]"
  (interactive)
  (kill-all-local-variables)
  ;; Setting the mode name and the mode-name field of the mode-line.
  (setq major-mode 'cmd-help-mode
        mode-name "cmd-help"
        buffer-auto-save-file-name nil  ; suppresss auto saving
        buffer-read-only t              ; for read only
        )
  ;; Setting keymap.
  (setq cmd-help-local-map (make-keymap))
  ;; Assigning keys
  (define-key cmd-help-local-map "\C-ch" 'cmd-help)
  (define-key cmd-help-local-map "\C-m" 'cmd-help)
  (define-key cmd-help-local-map " " 'cmd-help)
  (define-key cmd-help-local-map "q" 'cmd-help-mode-exit)
  (define-key cmd-help-local-map "Q" 'cmd-help-mode-exit)
  (define-key cmd-help-local-map "n" 'next-line)
  (define-key cmd-help-local-map "p" 'previous-line)

  ;; Declare to use of the local map
  (use-local-map cmd-help-local-map)

  ;; Setting for font-lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((cmd-font-lock-keywords) t t))
  (font-lock-mode t)

  (run-hooks 'cmd-help-mode-hook)
  )

(defun cmd-right-trim-region (start end)
  "Delete space characters at the end of line of the specified region.
Region is between START and END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "[\t ]+$" end t)
      (replace-match ""))))

(defun cmd-exec ()
  "Save the buffer (if necessary) and execute it.
corner-cutting work."
  (interactive)
  (save-buffer)
  (shell-command (buffer-file-name))
  )

(defun cmd-recenter ()
  "Set the point to the center, and delete extra spaces at the end of line."
  (interactive)
  (recenter)
  (cmd-right-trim-region (point-min) (point-max)))

(defun cmd-next-label ()
  "Jump to the next label."
  (interactive)
  (if (re-search-forward cmd-label-pattern nil t)
      (goto-char (match-end 0))))
(defun cmd-prev-label ()
  "Jump to the previous label."
  (interactive)
  (if (re-search-backward cmd-label-pattern nil t)
      (goto-char (match-beginning 0))))

(defun cmd-fill-paragraph ()
  "Fill comment lines.
Fairly corner-cutting (is it allowed to do this kind of thing?)."
  (interactive)
  (save-excursion
    (let (
	  ;;Ignore case
	  (case-fold-search t)
          (cmd-rem-regexp
           "\\([\t ]*@?\\(rem\\|echo\\)\\)\\([ \t].*$\\|$\\)")
          rem-begin
          rem-end
          rem-paragraph
          match-str
          (cmd-fill-column fill-column)
          (current-buffer (buffer-name))
          )
      (beginning-of-line)
      (if (looking-at cmd-rem-regexp)
          (progn
            (setq match-str (buffer-substring
                             (match-beginning 1) (match-end 1))
                  rem-begin (point))
            (message 
	     "cmd-fill-paragraph [%s]" match-str)
            (while (not (bobp))
              (forward-line -1)
              (if (looking-at (concat match-str "\\([\t ]\\|$\\)"))
                  (setq rem-begin (point))
                (goto-char (point-min))))

            (goto-char rem-begin)
            (setq cmd-rem-regexp
                  (concat "\\("
                          "\\(^" match-str "[^\n]*\n\\)+"
                          "\\(^" match-str "[^\n]*\\)?\\|"
                          "^" match-str ".*$\\)"))
            (if (looking-at cmd-rem-regexp)
                (progn
                  (setq rem-end (match-end 0)
                        rem-paragraph (buffer-substring rem-begin rem-end))
                  (set-buffer (get-buffer-create cmd-temp-buf))
                  (erase-buffer)
                  (insert rem-paragraph)
                  (indented-text-mode)

                  (goto-char (point-min))
                  (while (re-search-forward
                          (concat "^" match-str " ?") nil t)
                    (replace-match ""))

                  (setq fill-column (- cmd-fill-column (length match-str)))
                  (goto-char (point-min))
                  (while (not (eobp))
                    (fill-paragraph nil)
                    (forward-paragraph))

                  (goto-char (point-min))
                  (while (not (eobp))
                    (beginning-of-line)
                    (insert (concat match-str
                                    (if (looking-at "$") "" " ")))
                    (beginning-of-line)
                    (if (looking-at "[ \t]*@?echo$")
                        (progn
                          (end-of-line);
                          (insert ".")))
                    (forward-line 1))

                  (goto-char (point-max))
                  (beginning-of-line)
                  (if (looking-at (concat match-str "$"))
                      (replace-match ""))

                  (cmd-right-trim-region (point-min) (point-max))

                  (setq rem-paragraph
                        (buffer-substring (point-min) (point-max)))
                  (kill-buffer cmd-temp-buf)
                  (set-buffer current-buffer)
                  (delete-region rem-begin rem-end)
                  (insert rem-paragraph)
                  (message "done.")
              )
              (progn
                (ding)
                (error 
		 "cmd-fill-paragraph [seems like a bug]")))
            )
        (message
	 "cmd-fill-paragraph: The line doesn't start with REM nor ECHO.")
        )
      )
    )
  )

(defun cmd-goto-label ()
  "Jump to the specified label.

Label can be completed at the mini-buffer.  A word at the cursor
position is shown as default (if it matches one of existing
labels)."
  (interactive)
  (let ((label-alist '())
        (label nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*:\\([:A-Za-z0-9_-]+\\)\\([\t ]\\|$\\)" nil t)
        (setq label-alist (cons (list (buffer-substring (match-beginning 1)
                                                        (match-end 1)))
                                label-alist))))
    (if (not label-alist)
        (error 
	       "No label found"))
    (setq label (completing-read "Label:" label-alist nil t
                                 (if (assoc (current-word) label-alist)
                                     (current-word) "")))
    (when (and label (not (string= label "")))
      (push-mark (point))                
      (goto-char (point-min))
      (re-search-forward (concat "^[\t ]*:" label "\\([ \t]\\|$\\)"))
      )
    )
  )

(defun cmd-mode ()
  "Tiny mode for editing batch files (MS-DOS) and command scripts (Windows NT).

* Save buffer and execute it.

  \\[cmd-exec]

* Show help of a command.
  \\[cmd-help]

* Jump to the specified label.
  \\[cmd-goto-label]

* Set the point to the center, and delete extra spaces at the end
  of line.
  \\[cmd-recenter]

* Fill comment lines (e.g REM, ECHO).
  \\[cmd-fill-paragraph]

* Jump to the previous label.
  \\[cmd-prev-label]

* Jump to the next label.
  \\[cmd-next-label]"
  (interactive)
  (kill-all-local-variables)
  ;; Setting the mode name and the mode-name field of the mode-line.
  (setq major-mode 'cmd-mode
        mode-name "CMD")
  ;; Setting keymap.
  (setq cmd-local-map (make-keymap))
  ;; Assigning keys
  (define-key cmd-local-map "\C-c\C-c" 'cmd-exec)
  (define-key cmd-local-map "\C-cg" 'cmd-goto-label)
  (define-key cmd-local-map "\C-ch" 'cmd-help)
  (define-key cmd-local-map "\C-l" 'cmd-recenter)
  ;;(define-key cmd-local-map "\eq" 'cmd-fill-paragraph)
  (define-key cmd-local-map "\M-q" 'cmd-fill-paragraph)
  ;;(define-key cmd-local-map "\e\C-a" 'cmd-prev-label)
  (define-key cmd-local-map "\M-\C-a" 'cmd-prev-label)
  ;;(define-key cmd-local-map "\e\C-e" 'cmd-next-label)
  (define-key cmd-local-map "\M-\C-e" 'cmd-next-label)

  ;; Create the menu bar.
  (define-key cmd-submenu-jump [sub-goto-label]
    '( "Specified label" . cmd-goto-label))
  (define-key cmd-submenu-jump [sub-next-label]
    '( "Next label" . cmd-next-label))
  (define-key cmd-submenu-jump [sub-prev-label]
    '( "Previous label" . cmd-prev-label))

  (define-key cmd-local-map [menu-bar cmd] (cons mode-name cmd-menu-bar))
  (define-key cmd-menu-bar [submenu-jump] (cons "Jump" 
						cmd-submenu-jump))
  (define-key cmd-menu-bar [sep-1] '("--"))
  (define-key cmd-menu-bar [help] '("Help" 
				    . cmd-help))
  (define-key cmd-menu-bar [sep-2] '("--"))
  (define-key cmd-menu-bar [recenter] '("Delete trailing spaces"
					. cmd-recenter))
  (define-key cmd-menu-bar [fill] '("Fill comments" 
				    . cmd-fill-paragraph))
  (define-key cmd-menu-bar [exec] '("Execute" . cmd-exec))

  ;; Declare to use of the local map
  (use-local-map cmd-local-map)

  ;; Setting for font-lock.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((cmd-font-lock-keywords) t t))
  (font-lock-mode t)

  (make-local-variable 'dabbrev-abbrev-skip-leading-regexp)
  (setq dabbrev-abbrev-skip-leading-regexp "%")

  (run-hooks 'cmd-mode-hook)
)

;; Added by L Borgman
(provide 'cmd-mode)

;;; cmd-mode.el ends here
