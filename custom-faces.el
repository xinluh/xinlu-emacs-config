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

(eval-when-compile    (require 'color-theme))
(defun my-color-theme ()
  "Color theme by xinlu, created 2010-05-15."
  (interactive)
  (color-theme-install
   '(my-color-theme
     ((background-color . "ivory1")
      (background-mode . light)
      (background-toolbar-color . "#cf3ccf3ccf3c")
      (border-color . "Blue")
      (bottom-toolbar-shadow-color . "#79e77df779e7")
      (cursor-color . "DarkOliveGreen4")
      (foreground-color . "black")
      (mouse-color . "sienna3")
      (top-toolbar-shadow-color . "grey78"))

     (region ((t (:background "goldenrod2"))))
     (default ((t (:stipple nil
				   :background "white"
				   :foreground "black"
				   :inverse-video nil
				   :box nil
				   :strike-through nil
				   :overline nil
				   :underline nil
				   :slant normal
				   :weight normal
				   :height 90
				   :width normal
				   :foundry "outline"
				   :family "Consolas"))))
	 
     (border ((t (:background "Blue"))))
     (border-glyph ((t (:bold t :weight bold))))
     (font-lock-builtin-face ((t (:foreground "black"))))
     ;; (font-lock-comment-delimiter-face ((t (nil))))
     (font-lock-comment-face ((t (:italic t :foreground "ForestGreen" :slant italic :weight normal))))
     (font-lock-constant-face ((t (:foreground "black" :weight bold))))
     (font-lock-doc-face ((t (:foreground "forestgreen"))))
     (font-lock-doc-string-face ((t (:foreground "light coral"))))
     (font-lock-emphasized-face ((t (:bold t :background "lightyellow2" :weight bold))))
     (font-lock-exit-face ((t (:foreground "green"))))
     (font-lock-function-name-face ((t (:bold t :foreground "blue1" :weight bold))))
     (font-lock-keyword-face ((t (:bold t :foreground "blue" :weight bold))))
     (font-lock-negation-char-face ((t (nil))))
     ;; (font-lock-other-emphasized-face ((t (:bold t :foreground "gold1" :weight bold))))
     ;; (font-lock-other-type-face ((t (:bold t :foreground "gold1" :weight bold))))
     (font-lock-preprocessor-face ((t (:foreground "steelblue1"))))
     (font-lock-reference-face ((t (:foreground "cadetblue2"))))
     ;; (font-lock-regexp-grouping-backslash ((t (:foreground "red"))))
     ;; (font-lock-regexp-grouping-construct ((t (nil))))
     (font-lock-special-comment-face ((t (:foreground "blue4"))))
     (font-lock-special-keyword-face ((t (:foreground "red4"))))
     (font-lock-string-face ((t (:foreground "deeppink"))))
     (font-lock-type-face ((t (:italic t :foreground "deepskyblue3" :slant italic))))
     (font-lock-variable-name-face ((t (:foreground "chocolate"))))
     (font-lock-warning-face ((t (:bold t :foreground "Red" :weight bold))))
	 (semantic-decoration-on-unknown-includes ((t (:background "rosybrown1"))))
	 
     (fringe ((t (:background "grey88"))))
	 
     (magit-branch ((t (:foreground "red" :weight bold))))
     (magit-diff-add ((t (:foreground "green3"))))
     (magit-diff-del ((t (:foreground "red"))))
     (magit-diff-file-header ((t (:foreground "deeppink" :weight bold))))
     (magit-diff-hunk-header ((t (:foreground "blue violet" :weight bold))))
     (magit-diff-none ((t (:foreground "grey76"))))
     ;; (magit-header )
     (magit-item-highlight ((t (:background "mistyrose"))))
     ;; (magit-item-mark ((t (nil))))
     ;; (magit-log-head-label ((t (nil))))
     ;; (magit-log-tag-label ((t (nil))))
     (magit-section-title ((t (:foreground "blue" :weight bold))))

	 (svn-status-directory-face ((t (:foregroud "blue" :weight bold))))
	 
     (mode-line ((t (:background "dark olive green" :foreground "khaki"
					 :box (:line-width 1 :style released-button)))))
     (mode-line-buffer-id ((t (:slant italic :weight bold))))
     ;; (mode-line-emphasis ((t (nil))))
     ;; (mode-line-highlight ((t (nil))))
     (mode-line-inactive ((t (:background "beige" :foreground "black"
							  :box (:line-width 1 :style released-button)))))
     (modeline-mousable ((t (:background "gold2" :foreground "black"))))
     (modeline-mousable-minor-mode ((t (:background "gold2" :foreground "black"))))

	 (eldoc-highlight-function-argument ((t (:inherit bold :foreground "red"))))
	 
	 (diff-removed ((t (:foreground "red" :weight bold))))
	 (diff-added ((t (:foreground "forestgreen" :weight bold))))
	 (diff-context ((t (:foreground "grey70"))))

	 (org-done ((t (:foreground "green" :weight bold))))
	 (org-todo ((t (:foreground "Red" :weight bold))))

	 
     (mouse ((t (:background "white")))))))
;(add-to-list 'color-themes '(my-color-theme  "THEME NAME" "YOUR NAME"))
(my-color-theme)





