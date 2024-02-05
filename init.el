;; TODO: Why isn't this working
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setenv "LIBRARY_PATH"
      (string-join
       '("/opt/homebrew/opt/gcc/lib/gcc/13"
         "/opt/homebrew/opt/libgccjit/lib/gcc/13"
         "/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin21/13")
       ":"))

(setq debug-on-error t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(magit-todos magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
