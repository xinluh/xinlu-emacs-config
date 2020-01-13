
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.4)
 '(company-minimum-prefix-length 1)
 '(cpp-face-dark-name-list
   (quote
    ("dim gray" "blue" "cyan" "yellow" "red" "dark green" "brown" "dark orange" "dark khaki" "dark violet" "purple" "dark turquoise" "sandle brown" "dark blue" "SlateBlue4")))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(flycheck-check-syntax-automatically (quote (save idle-change new-line mode-enabled)))
 '(gnutls-trustfiles
   (quote
    ("/etc/ssl/certs/ca-certificates.crt" "/etc/pki/tls/certs/ca-bundle.crt" "/etc/ssl/ca-bundle.pem" "/usr/ssl/certs/ca-bundle.crt" "/usr/local/share/certs/ca-root-nss.crt" "/private/etc/ssl/cert.pem")))
 '(magit-log-arguments (quote ("--graph" "--decorate" "-n256")))
 '(package-selected-packages
   (quote
    (rust-mode protobuf-mode prettier-js prettier-js-mode rvm yasnippet-snippets web-mode color-theme yafolding magithub yasnippet magit-gh-pulls multiple-compile go-playground go-tag go-guru go-rename go-imports go-impl company-go go-eldoc flycheck-gometalinter tide go-mode magit restclient yaml-mode wgrep-ag json-mode github-browse-file avy multiple-cursors pug-mode zoom-frm use-package solarized-theme smex projectile pcache noflet logito ido-grid-mode ido-at-point highlight-symbol flycheck flx-ido expand-region elpy browse-kill-ring ag)))
 '(projectile-other-file-alist
   (quote
    (("cpp" "h" "hpp" "ipp")
     ("ipp" "h" "hpp" "cpp")
     ("hpp" "h" "ipp" "cpp" "cc")
     ("cxx" "h" "hxx" "ixx")
     ("ixx" "h" "hxx" "cxx")
     ("hxx" "h" "ixx" "cxx")
     ("c" "h")
     ("m" "h")
     ("mm" "h")
     ("h" "c" "cc" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm")
     ("cc" "h" "hh" "hpp")
     ("hh" "cc")
     ("vert" "frag")
     ("frag" "vert")
     (nil "lock" "gpg")
     ("lock" "")
     ("gpg" "")
     ("js" "css" "jade")
     ("jade" "js")
     ("css" "js"
      (\, "tsx")
      (\, "jsx"))
     ("tsx" "css"))))
 '(safe-local-variable-values
   (quote
    ((projectile-project-type . python-pip)
     (compile-output-file . "a.out"))))
 '(sql-sqlite-program "sqlite3"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flx-highlight-face ((t (:foreground "#268bd2" :weight bold))))
 '(highlight-indentation-face ((t (:background "#f4f0e3"))))
 '(ido-first-match ((t (:foreground "#b58900" :weight bold))))
 '(ido-grid-mode-match ((t nil))))
