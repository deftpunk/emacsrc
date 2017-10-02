;;;; config,.el -*- lexical-binding: t -*-

;;; Packaging {{{
;; Initialize the package plumbing -> so that we can use-package.
(package-initialize nil)
(setq packages-user-dir (concat user-emacs-directory "packages")
      package-enable-at-startup nil
      package--init-file-ensured t)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
;(package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t
      use-package-always-ensure t)
;;; }}}

;;; Custom {{{
;; Make a special place for the files Emacs leaves around.
(defconst deftmacs-crumbs-directory
  (expand-file-name (concat user-emacs-directory "crumbs/"))
  "Deftmacs crumbs directory.")
(unless (file-exists-p deftmacs-crumbs-directory)
  (make-directory deftmacs-crumbs-directory))

;; Set up the custom file early so that custom stuff doesn't get written to
;; this file.
(setq custom-file (expand-file-name "custom.el" deftmacs-crumbs-directory))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file 'noerror)
;;; }}}

;; More gc-cons-threshold optimization when we are running.
(add-hook 'minibuffer-setup-hook
	  (lambda ()
	    (setq gc-cons-threshold most-positive-fixnum)))
(dolist (hook '(after-init-hook minibuffer-exit-hook))
  (add-hook hook
	    (lambda ()
	      (setq gc-cons-threshold (* 1000 1000 10)))))

;;; Display {{{

;; Get extraneous GUI elements out of the way early.
(menu-bar-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq ring-bell-function 'ignore
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      use-dialog-box nil                        ; All questions in the minibuffer
      display-time-default-load-average nil)
(fset 'yes-or-no-p 'y-or-n-p)

;; frame transparency for Mac.
(set-frame-parameter (selected-frame) 'alpha '(92 92))
(add-to-list 'default-frame-alist '(alpha 92 92))

;; Set the frame title to the full path of the buffer we are working on.
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Hide the modeline - this makes the graphics less jerky at startup.  I got
;; this from Spacemacs, which got it from http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))
 (hidden-mode-line-mode)

;; Themes
(use-package gruvbox-theme)

;; Fonts: I use Roboto Mono and also use Symbola as a fallback to display
;; unicode characters.
;; https://github.com/ryanoasis/nerd-fonts
;; http://www.fontspace.com/unicode-fonts-for-ancient-scripts/symbola
(set-frame-font "RobotoMono Nerd Font 12")
(when (member "Symbola" (font-family-list))
	      (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; all-the-icons.el
(use-package all-the-icons)

;; Anzu
;; Show the total number of search hits and where you are in the modeline.
;; You have to initialize it before you initialize Spaceline/Powerline/modeline
(use-package anzu
  :defer t
  :config
  (global-anzu-mode +1))

;; Spaceline
;; https://github.com/TheBB/spaceline
(use-package info+)
(use-package spaceline)
(require 'spaceline-config)
(spaceline-emacs-theme)
(spaceline-helm-mode)
(spaceline-info-mode)  ; depends on info+

(spaceline-toggle-anzu-on)
(spaceline-toggle-auto-compile-on)
(spaceline-toggle-buffer-modified-on)
(spaceline-toggle-buffer-size-on)
(spaceline-toggle-buffer-id-on)
(spaceline-toggle-remote-host-on)
(spaceline-toggle-major-mode-on)
(spaceline-toggle-flycheck-error-on)
(spaceline-toggle-flycheck-warning-on)
(spaceline-toggle-flycheck-info-on)
(spaceline-toggle-minor-modes-on)
(spaceline-toggle-version-control-on)
(spaceline-toggle-which-function-on)
(spaceline-toggle-python-pyvenv-on)
(spaceline-toggle-selection-info-on)
(spaceline-toggle-buffer-encoding-abbrev-on)
(spaceline-toggle-line-column-on)
(spaceline-toggle-buffer-position-on)
(spaceline-toggle-projectile-root-on)

(spaceline-toggle-process-off)

(spaceline-compile)

;; spaceline-all-the-icons
(use-package spaceline-all-the-icons
  :after spaceline
  :config
  (spaceline-all-the-icons--setup-package-updates)
  (spaceline-all-the-icons--setup-git-ahead)

  ;; enable some segments
  (spaceline-toggle-all-the-icons-bookmark-on)

  (spaceline-all-the-icons-theme))
;;; }}}

;;; Defaults {{{

;; utf-8 encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;;; }}}

;;; Libraries & Dependencies {{{

;; Exec Path
;; Start off with exec-path-from-shell in case we need it early.  Otherwise things
;; like magit have a hard time finding git in MacOSX.
;; Install exec-path in case we need it early.  Otherwise plugins like magit
;; might have a hard time finding git on a Mac.
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  ;; Set up correct PATH, etc. for Mac
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; Crux - a collection of ridiculously useful extensions.
(use-package crux
  :config
  (crux-reopen-as-root-mode))

;; popwin
;; https://github.com/m2ym/popwin-el
;; A popup window manager for Emacs; helps with all of the windows that magically pop in and out.
;; (use-package popwin
;;  :config
;;  (popwin-mode 1))

;; which-key
;; https://github.com/justbur/emacs-which-key
;; Displays available keybindings in a popup.
(use-package which-key
    :defer t
    :init
    (setq which-key-idle-delay 0.5)
    (which-key-mode))

;;; }}}

;;; Minor Modes {{{

;;; }}}


;;; Plugins {{{

;; ace-window
;; https://github.com/abo-abo/ace-window
;; Selecting a window to switch to
(use-package ace-window
  :defer t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
	aw-leading-char-style 'path)
  (set-face-attribute 'aw-leading-char-face nil :height 3.0))

;; Helm
;; https://github.com/emacs-helm-helm
(use-package helm
  :init
  (use-package helm-config)
  (use-package helm-man)
  (use-package helm-org)
  (use-package helm-mt)
  (use-package helm-ring)
  (use-package helm-ag
    :init
    (setq helm-ag-fuzzy-match t
	  helm-ag-use-agignore t
	  helm-ag-command-option "--ignore-dir elpa"))

  ;; options
  (setq helm-idle-delay                        0.0    ; Update fast sources immediately (doesn't).
            helm-move-to-line-cycle-in source
            helm-input-idle-delay                  0.01   ; This actually updates things reeeelatively quickly.
            helm-quick-update                      t
            helm-M-x-requires-pattern              nil
            helm-candidate-number-limit            99     ; Setting this above 100 will slow down fuzzy matching
            helm-autoresize-max-height             45     ; Set the max window height to 45% of current frame.
            helm-mode-fuzzy-match                  t      ; Turn on fuzzy matching for buffers, semantic, recentf
            helm-completion-in-region-fuzzy-match  t      ; Completion, imenu, apropos, M-x
            helm-buffer-skip-remote-checking       t      ; Ignore checking if file exists on remote files, ie. Tramp
            helm-tramp-verbose                     6      ; See Tramp messages in helm
            helm-ff-skip-boring-files              t)

      (helm-autoresize-mode t)

      ;; helm-ag and find-grep in find-files or helm-mini
      (when (executable-find "ag")
        (setq helm-grep-default-command "ag "))

      ;; Save the current position to mark ring when jumping around.
      (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

      (helm-mode))
  
;; ivy, counsel and swiper
;; https://github.com/abo-abo/swiper
;; ivy, counsel and swiper]] from the great abo-abo; who also came up with hydra.

;; - Ivy, a generic completion mechanism for Emacs.
;; - Counsel, a collection of Ivy-enhanced versions of common Emacs commands.
;; - Swiper, an Ivy-enhanced alternative to isearch.
(use-package counsel
    :bind (("C-h f" . counsel-describe-function)
           ("C-h v" . counsel-describe-variable)
           ("C-h i" . counsel-info-lookup-symbol)))

(use-package swiper
    :bind (:map ivy-minibuffer-map
                ("C-w" . ivy-yank-word)
                ([escape] . minibuffer-keyboard-quit))
    :config
    (ivy-mode 1))

(use-package avy
    :config
    (setq avy-background t
          avy-all-windows nil))

;; Magit for version control
(use-package magit
  :defer t
  :config
  (setq  magit-log-arguments '("--graph" "--decorate" "--color")
          magit-save-repository-buffers 'dontask
          magit-revert-buffers 'silent))

;; zzz-to-char
;; Quickly select the char you want to zap to.
(use-package zzz-to-char)

;;; }}}

;;; Keybindings {{{

(global-set-key (kbd "s-i") 'helm-mini)

(global-unset-key (kbd "s-l"))
(bind-keys :map global-map
	   :prefix "s-l"
	   :prefix-map super-l-map
	   ("f" . avy-goto-char-in-line))

(global-unset-key (kbd "s-z"))
(global-set-key (kbd "s-z") 'zzz-up-to-char)

(global-unset-key (kbd "s--"))
(global-set-key (kbd "s--") 'ace-window)

;;; }}}

;; Server
(require 'server)
(unless (server-running-p) (server-start))
