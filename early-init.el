;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;; Backup of `gc-cons-threshold' and `gc-cons-percentage' before startup.
(defvar backup-gc-cons-threshold gc-cons-threshold)
(defvar backup-gc-cons-percentage gc-cons-percentage)

;; Temporarily raise the garbage collection threshold to its maximum value.
;; It will be restored later to controlled values.
(setq gc-cons-threshold (if noninteractive
                            268435456 ; 256 Mb
                          most-positive-fixnum)
      gc-cons-percentage 1.0)

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Use plists for deserialization for lsp-mode
(setenv "LSP_USE_PLISTS" "true")

;; Get rid of some window chrome.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Remove the startup nonsense and replace w/ mine.
(setq
    ;; Font compacting can be very resource-intensive, especially when rendering
   ;; icon fonts on Windows. This will increase memory usage.
   inhibit-compacting-font-caches t

   ;; Resizing the Emacs frame can be costly when changing the font. Disable this
   ;; to improve startup times with fonts larger than the system default.
   frame-resize-pixelwise t

   ;; Without this, Emacs will try to resize itself to a specific column size
   frame-inhibit-implied-resize t
   
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message user-login-name
      inhibit-splash-screen t
      initial-buffer-choice nil
      inhibit-x-resources t
      
;; Give up some bidirectional functionality for slightly faster re-display.
   bidi-inhibit-bpa t)

  ;; Disable bidirectional text scanning for a modest performance boost.
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; Remove "For information about GNU Emacs..." message at startup
  (advice-add 'display-startup-echo-area-message :override #'ignore)

  ;; Suppress the vanilla startup screen completely. We've disabled it with
  ;; `inhibit-startup-screen', but it would still initialize anyway.
  (advice-add 'display-startup-screen :override #'ignore)

;; Disable GUIs, they are ugly and inconsistent.
(setq use-file-dialog nil
      use-dialog-box nil)

;;; Security

(setq
 ;; Defining TLS and security variables in early-init.el guarantees that any
 ;; network connection made during the initialization sequence is secure.
 ;; Prompts if there are cert issues.
 gnutls-verify-error t
 ;; Ensure SSL/TLS connections checks
 tls-checktrust gnutls-verify-error
 ;; Stronger GnuTLS encryption
 gnutls-min-prime-bits 3072)

;;; Package
(setq
 ;; Defining these early guarantees that the behavior and macro expansion of
 ;; use-package are configured before the first use-package form is evaluated in
 ;; post-early-init.el, pre-init.el, init.el, or post-init.el.
 use-package-expand-minimally t
 use-package-minimum-reported-time (if init-file-debug 0 0.1)
 use-package-verbose init-file-debug
 use-package-always-ensure (not noninteractive)
 use-package-enable-imenu-support t

 ;;; package.el

 ;; Placing the use-package-* in early-init.el ensures the package variables are
 ;; populated before package.el is initialized. This prevents cases where Emacs
 ;; might attempt to fetch from default repositories before it evaluates the
 ;; overridden variables in init.el. (This also offers the possibility to
 ;; download packages in post-early-init.el, for users who need it.)
 package-enable-at-startup nil  ; Let the init.el file handle this

 package-archives '(("melpa"        . "https://melpa.org/packages/")
                    ("gnu"          . "https://elpa.gnu.org/packages/")
                    ("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
 package-archive-priorities '(("gnu"    . 99)
                              ("nongnu" . 80)
                              ("melpa"  . 70)))



;; Unbind some Super keys on my Mac
(global-unset-key (kbd "s-a"))
(global-unset-key (kbd "s-d"))
(global-unset-key (kbd "s-f"))
(global-unset-key (kbd "s-g"))
(global-unset-key (kbd "s-h"))
(global-unset-key (kbd "s-i"))
(global-unset-key (kbd "s-j"))
(global-unset-key (kbd "s-k"))
(global-unset-key (kbd "s-m"))
(global-unset-key (kbd "s-o"))
(global-unset-key (kbd "s-p"))
(global-unset-key (kbd "s-s"))
(global-unset-key (kbd "s-t"))
(global-unset-key (kbd "s-u"))
(global-unset-key (kbd "s-w"))
(global-unset-key (kbd "s-y"))

;;; Miscellaneous

(set-language-environment "UTF-8")

(setq
 ;; Increase how much is read from processes in a single chunk
 read-process-output-max (* 1024 1024)
 process-adaptive-read-buffering nil

 ;; Don't ping things that look like domain names.
 ffap-machine-p-known 'reject

 warning-minimum-level (if init-file-debug :warning :error)

  ;; Establish a strict baseline for suppressed warnings.
 ;; - defvaralias: Emacs emits warnings when an alias is defined for a variable
 ;;   that already exists. In modern, lazy-loaded configurations, this occurs
 ;;   frequently and is almost always benign.
 ;; - lexical-binding: Emacs warns about third-party packages that lack
 ;;   lexical-binding. Because end users cannot easily fix upstream source code,
 ;;   these warnings create noise without providing actionable value.
 warning-suppress-types '((defvaralias) (lexical-binding))
 warning-inhibit-types '((files missing-lexbind-cookie))

 ;; Disable warnings from the legacy advice API. They aren't useful.
 ad-redefinition-action 'accept)

;; Set gc-cons back to original values.
(setq gc-cons-threshold backup-gc-cons-threshold
          gc-cons-percentage backup-gc-cons-percentage)
