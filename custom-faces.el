; default face for all other custom face
(defun custom-face-all(&optional frame)
  (if (null frame) (setq frame (selected-frame)))
  (set-face-attribute 'default frame :stipple nil 
					               :inverse-video nil 
                                   :box nil 
								   :strike-through nil 
								   :overline nil 
								   :underline nil 
								   :slant 'normal 
								   :weight 'normal 
								   :width 'normal
								   :height 90)
  (if (or is-win32 is-emacs23)
	(set-face-attribute 'default frame
						:family "outline-consolas"
;; 						:family "bitstream vera sans mono"
	                    :height 90)
	(set-face-attribute 'default frame :family "b&h-lucidatypewriter"))

  ;; for XMing 
  ;; (if (and (not is-win32) is-emacs23 (string= x-display-name "localhost:10.0"))
	  ;; (set-face-attribute 'default frame :height 120))

  (set-face-attribute 'diff-added frame :foreground "green")
  (set-face-attribute 'diff-removed frame :foreground "red")
  (set-face-attribute 'diff-header frame :background "grey30" :weight 'bold)
  (set-face-attribute 'diff-file-header frame :foreground "gold")
  (set-face-attribute 'svn-status-directory-face frame
					  :foreground "cyan"
					  :weight 'bold)
)

(defun custom-face-dark (&optional frame)
  (interactive)
  (if (null frame) (setq frame (selected-frame)))
  (custom-face-all frame) 
  (set-background-color "black")
  (set-face-attribute 'cursor frame :background "white")
  (set-face-attribute 'default frame 
					  :foreground "white")
  (set-face-attribute 'font-lock-builtin-face frame 
					  :foreground "slate blue")
  (set-face-attribute 'font-lock-comment-face frame 
					  :foreground "SeaGreen")
  (set-face-attribute 'font-lock-function-name-face frame 
					  :foreground "cyan" )
					  ;; :weight 'bold)
  (set-face-attribute 'font-lock-keyword-face frame 
					  :foreground "green")
  (set-face-attribute 'font-lock-string-face frame 
					  :foreground "hot pink")
  (set-face-attribute 'font-lock-type-face frame 
					  :foreground "green yellow")
  (set-face-attribute 'font-lock-variable-name-face frame 
					  :foreground "gold")
 (set-face-attribute 'font-lock-warning-face frame 
					  :foreground "red" 
					  :weight 'bold)
)


(defun custom-face-html (&optional frame)
  (interactive)
  (if (null frame) (setq frame (selected-frame)))
  (custom-face-all)
  (set-background-color "white")
  (set-face-attribute 'cursor frame :background "black")
  (set-face-attribute 'default frame 
					  :foreground "black")
  (set-face-attribute 'font-lock-builtin-face frame 
					  :foreground "light blue")
  (set-face-attribute 'font-lock-constant-face frame 
					  :foreground "dark gray")
  (set-face-attribute 'font-lock-function-name-face frame 
					  :foreground "light blue")
  (set-face-attribute 'font-lock-keyword-face frame 
					  :foreground "thistle")
  (set-face-attribute 'font-lock-variable-name-face frame 
					  :foreground "pink")
)

(defun custom-face-white (&optional frame)
  (interactive)
  (if (null frame) (setq frame (selected-frame)))
  (custom-face-all)
  (set-background-color "white")
  (set-face-attribute 'cursor frame :background "black")
  (set-face-attribute 'default frame 
					  :foreground "black")

  (set-face-attribute 'font-lock-builtin-face frame 
					  :foreground "#d82ca2")
  (set-face-attribute 'font-lock-comment-face frame 
					  :foreground "ForestGreen")
  (set-face-attribute 'font-lock-function-name-face frame
					  :foreground "#1610c1" )
  ;; :weight 'bold)
  (set-face-attribute 'font-lock-keyword-face frame 
					  :foreground "Red")
  (set-face-attribute 'font-lock-string-face frame 
					  :foreground "#c77429")
  (set-face-attribute 'font-lock-type-face frame 
					  :foreground "#931b81")
  (set-face-attribute 'font-lock-variable-name-face frame 
					  :foreground "#10b8d1")
)
;; (custom-face-dark)
