;;; core.el -*- lexical-binding: t; -*-

(defvar flex-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all doom functions will be verbose. Set DEBUG=1 in the command
line or use --debug-init to enable this.")

;;; Variables to be used later.
;; (defvar flex-emacs-dir (file-truename user-emacs-directory)
(defvar flex-emacs-dir (file-truename "~/.emacs.d/")
  "The path to this emacs.d directory.")

(defvar flex-core-dir (concat flex-emacs-dir "core/")
  "The path to all of the flex files.")

;; The local, cache & ets directories are created (if they don't exist) in the
;; core-packaging.el file.
(defvar flex-local-dir (concat flex-emacs-dir ".local/")
  "Root directory for local Emacs files. Use this as permanent storage for files
that are safe to share across systems (if this config is symlinked across
several computers).")

(defvar flex-cache-dir (concat flex-local-dir "cache/")
  "Directory for volatile storage.

Use this for files that change often, like cache files.")

(defvar flex-etc-dir (concat flex-emacs-dir "etc/")
  "This is a storage location for files that don't change much, e.g. custome.el - no-littering
will use this later.")

(defvar flex-packages-dir (concat flex-emacs-dir "packages/")
  "Directory for the package files directing what we want installed.")

(defvar flex-autoload-file (concat flex-local-dir "autoloads.el")
  "Where `flex//reload-autoloads' will generate its autoloads file.")

(defvar flex-orgfile
	  (expand-file-name "config.org" user-emacs-directory)
	  "The Org mode configuration file.")

(defvar flex-elfile
	  (expand-file-name "config.el" user-emacs-directory)
	  "The generated elisp configuration file.")

;;;
;;; Hooks
;;;

;; I want to know how long it took to load & how many garbage collections. Use
;; a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Custom init hooks; clearer than `after-init-hook', `emacs-startup-hook', and
;; `window-setup-hook'.
(defvar flex-init-hook nil
  "A list of hooks run when Flex Emacs is initialized, before `flex-post-init-hook'.")

(defvar flex-post-init-hook nil
  "A list of hooks run after Flex Emacs initialization is complete, and after
`flex-init-hook'.")

(defun flex-try-run-hook (fn hook)
  "Runs a hook wrapped in a `condition-case-unless-debug' block; its objective
is to include more information in the error message, without sacrificing your
ability to invoke the debugger in debug mode."
  (condition-case-unless-debug ex
      (if noninteractive
          (quiet! (funcall fn))
        (funcall fn))
    ('error
     (lwarn hook :error
          "%s in '%s' -> %s"
          (car ex) fn (error-message-string ex))))
  nil)

;;;
;;; Settings
;;;

;; Get the tool-bar and scroll-bar out of the way because I hat looking at them.
;; (menu-bar-mode -1)
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; UTF-8 prettiness.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; Get rid of the startup message nonsense.
(unless noninteractive
  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (setq inhibit-startup-message t
        inhibit-startup-echo-area-message user-login-name
        inhibit-default-init t
        initial-major-mode 'fundamental-mode
        initial-scratch-message nil
	mode-line-format nil))

(setq-default
 ad-redefinition-action 'accept   ; silence advised function warnings
 apropos-do-all t                 ; make `apropos' more useful
 compilation-always-kill t        ; kill compilation process before starting another
 compilation-ask-about-save nil   ; save all buffers on `compile'
 compilation-error-screen-columns nil
 compilation-context-lines 2
 compilation-scroll-output t
 confirm-nonexistent-file-or-buffer t
 enable-recursive-minibuffers nil
 debug-on-error (and (not noninteractive) flex-debug-mode)
 idle-update-delay 2              ; update ui less often
 load-prefer-newer (or noninteractive flex-debug-mode)
 ;; keep *Messages* from being truncated.
 message-log-max t
 ;; keep the point out of the minibuffer
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil
 ;; Miscellaneous files
 bookmark-default-file (concat flex-etc-dir "bookmarks")
 large-file-warning-threshold       100000000    ; warn when opening files bigger than 100MB
 abbrev-file-name             (concat flex-local-dir "abbrev.el")
 auto-save-list-file-name     (concat flex-cache-dir "autosave")
 backup-directory-alist       (list (cons "." (concat flex-cache-dir "backup/")))
 pcache-directory             (concat flex-cache-dir "pcache/")
 mc/list-file                 (concat flex-etc-dir "mc-lists.el")
 server-auth-dir              (concat flex-cache-dir "server/")
 tramp-auto-save-directory    (concat flex-cache-dir "tramp-auto-save/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name  (concat flex-cache-dir "tramp-persistency.el")
 url-cache-directory          (concat flex-cache-dir "url/")
 url-configuration-directory  (concat flex-etc-dir "url/"))

;; don't ask to kill buffers
(setq kill-buffer-query-functions
    (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Move custom defs out of init.el & try to keep stuff out of custom.el
(setq custom-file (concat flex-etc-dir "custom.el"))
(load custom-file t t)

(eval-and-compile
  (setq after-init-time nil)
  (defvar flex--file-name-handler-alist file-name-handler-alist)
  (unless (or after-init-time noninteractive)
    ;; One of the contributors to long startup times is the garbage collector,
    ;; so we up its memory threshold, temporarily. It is reset later.
    (setq gc-cons-threshold 402653184
          gc-cons-percentage 0.6
          file-name-handler-alist nil))

  (require 'cl-lib)

  ;; This makes the UI draw quickly at first and the graphics less jerky at startup.
  ;; I got this from Spacemacs, which got it from http://bzg.fr/emacs-hide-mode-line.html
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

  ;;;
  ;;; core*.el file loading
  ;;;

  ;; core-packaging installs "use-package" & "paradox" - then "requires" them.
  ;; makes sure that the flex-local-dir, flex-etc-dir & flex-cache-dir are present.
  (load (concat flex-core-dir "core-packaging") nil t)
  (eval-when-compile (flex-initialize t))

  ;; Set up load-path & require use-package + paradox.  FYI, the "require" is expensive.
  (setq flex--package-load-path (directory-files package-user-dir t "^[^.]" t)
        load-path (append flex--base-load-path flex--package-load-path))
  (require 'use-package)
  (require 'paradox)

  (load! core-lib)

  ;; Create the autoloads if necessary and load them.
  ;; *** Recreate the autoloads.el file if you add/remove functions. ***
  (flex-initialize-autoloads)
  (condition-case-unless-debug ex
      (require 'autoloads flex-autoload-file t)
    ('error
     (lwarn 'doom-autoloads :warning
            "%s in autoloads.el -> %s"
            (car ex) (error-message-string ex))))

  (load! core-os) ; consistent behavior across OSes

  (unless noninteractive
    (load! core-ui)         ; draw me like one of your French editors
    (load! core-popups)     ; taming sudden yet inevitable windows
    )

  ;;;
  ;;; Load up the Org configuration file.
  ;;;

  ;; Optimized Orgmode loading - faster than org-babel-tangle-file
  ;; See - http://www.holgerschurig.de/en/emacs-efficiently-untangling-elisp/
  (add-hook 'after-save-hook (apply-partially #'my-tangle-config-org-hook-func flex-orgfile flex-elfile))
  (when (or (not (file-exists-p flex-elfile))
	    (file-newer-than-file-p flex-orgfile flex-elfile))
    (my-tangle-config-org flex-orgfile flex-elfile))
  (load-file flex-elfile)

  ;;;
  ;;; Finalize loading Emacs
  ;;;

  (defun flex|finalize ()
    "Run `flex-init-hook', `flex-post-init-hook' and reset `gc-cons-threshold',
    `gc-cons-percentage' and `file-name-handler-alist'."
    (unless (or (not after-init-time) noninteractive)
      (dolist (hook '(flex-init-hook flex-post-init-hook))
        (run-hook-wrapped hook #'flex-try-run-hook hook)))

    ;; If you forget to reset this, you'll get stuttering and random freezes!
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1
          file-name-handler-alist flex--file-name-handler-alist)
    t)

  (add-hook! '(emacs-startup-hook flex-reload-hook)
    #'flex|finalize)) ; end of the eval-compile

(provide 'core)
;;; core.el ends here
