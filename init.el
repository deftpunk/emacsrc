;;; init.el --- Init -*- lexical-binding: t; -*-

;;; Before package

(setq
 ;; The initial buffer is created during startup even in non-interactive
 ;; sessions, and its major mode is fully initialized. Modes like `text-mode',
 ;; `org-mode', or even the default `lisp-interaction-mode' load extra packages
 ;; and run hooks, which can slow down startup.
 ;;
 ;; Using `fundamental-mode' for the initial buffer to avoid unnecessary
 ;; startup overhead.
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil

 ;; Set-language-environment sets default-input-method, which is unwanted.
 default-input-method nil

 ;; Ask the user whether to terminate asynchronous compilations on exit.
 ;; This prevents native compilation from leaving temporary files in /tmp.
 native-comp-async-query-on-exit t

 ;; Allow for shorter responses: "y" for yes and "n" for no.
 read-answer-short t
 revert-buffer-quick-short-answers t)

;; Allow for shorter responses: "y" for yes and "n" for no.
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;;; Package setup

;; Define an ignore macro that doesn't even evaluate the argument. This is useful for
;; display purposes when using elispdoc rather than commenting whole regions out.
(defmacro my-ignore (_form))

;;; Elpaca

;; https://deepwiki.com/progfolio/elpaca/3.2-package-manager-ui

;; elpaca bootstrap -- verbatim from the installed elpaca's own
;; doc/installer.el (progfolio/elpaca), not a memorized/older copy, since
;; the internal var names (`elpaca-sources-directory', `elpaca-activate')
;; have changed across elpaca releases.
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; use-package has been part of core emacs since version 29 so I assume its OK
;; to just require it.
(require 'use-package)

;; Bridge elpaca into use-package's :ensure keyword and make elpaca+its
;; use-package integration ready before any use-package form below runs.
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))
(elpaca-wait)

;; early on setup follow-symlinks to true for loaded files
(setq vc-follow-symlinks t)

;; GUI Emacs on macOS is launched by launchd, not a login shell, so it only
;; gets a minimal PATH/exec-path -- Homebrew-installed tools like aspell,
;; jdtls and pgformatter aren't visible to `executable-find' without this.
;; But exec-path-from-shell is relatively expensive so as compromise
;; just add homebrew onto the path as needed

(unless (member "/opt/homebrew/bin" exec-path)
  (add-to-list 'exec-path "/opt/homebrew/bin"))

;;; Customizations

;;; Font setup
;; This needs to be done prior to theme setup.

;; Mixed-pitch mode. I use this in markdown and org modes currently.
(defvar my-default-fixed-pitch-font "DejaVuSansM Nerd Font"
  "Default fixed-pitch font family.")
(defvar my-default-variable-pitch-font "Helvetica"
  "Default variable-pitch font family.")

;; Expand the default font height enough that emacs looks normal when opened.
(set-face-attribute 'default nil :height 160)

;; Tooltips are set to 1.3 for readability
(set-face-attribute 'tooltip nil :height 1.3)

(use-package mixed-pitch
  :ensure t
  :init
  (set-face-attribute 'variable-pitch nil
                      :font my-default-variable-pitch-font
                      :height 1.0)

  ;; Ensure the fixed-pitch face strictly inherits from the default font
  (set-face-attribute 'fixed-pitch nil
                      :inherit 'default)

  ;; Leave cursor-type alone; don't let mixed-pitch swap it to a bar cursor.
  (setq mixed-pitch-variable-pitch-cursor nil)

  :hook ((org-mode . mixed-pitch-mode)
	 (markdown-mode . mixed-pitch-mode)))

;;; modus theme configuration.

;;
;; Color name redirection for use with custom faces
;; requires manual editing of custom-set-faces or modus definitions
;; to  use i.e with the  ` back tick operator.
;;

(defvar margin-tan-bg "#EEE8D5")
(defvar margin-gray-bg "gray20")
(defvar margin-light-gray-bg "gray95")

;; Disable the theme safety check.
(setq custom-safe-themes t)

;; Disable all previously loaded themes before loading another one.
(advice-add 'load-theme :before
            (lambda (&rest _varargs)
              (mapc #'disable-theme custom-enabled-themes)))

;; These mostly global level changes make switching around easier between themes
;; They preserve the tabbing styling I use and mute the colors a bit.
;; The consequence of moving over to modus is the need to not generally customize faces in
;; custom.el.

;; Specifically:
;; * Override all modus themes to use the background color from tab-line
;; * This keeps visual parity with what I currently use
;; * Make headers all the same color as foreground

(setq modus-themes-common-palette-overrides
      `((bg-margins ,margin-tan-bg)  ;; common setup for a color alias to override.
	(bg-tab-bar bg-margins)
        (bg-tab-current bg-main)
        (bg-tab-other bg-margins)
	(bg-line-number-inactive bg-margins)

	;; custom hl face for imenu-list
	(fg-hl-imenu  "DarkOrange2")

	;; Tone down the headings: use the default foreground instead
        ;; of the theme's per-level accent colors.
        (fg-heading-0 fg-main)
        (fg-heading-1 fg-main)
        (fg-heading-2 fg-main)
        (fg-heading-3 fg-main)
        (fg-heading-4 fg-main)
        (fg-heading-5 fg-main)
        (fg-heading-6 fg-main)
        (fg-heading-7 fg-main)
        (fg-heading-8 fg-main)

	;; tone down code blocks
	(bg-prose-block-contents unspecified)
	(bg-prose-code unspecified)
        (bg-prose-block-delimiter unspecified)
        (fg-prose-block-delimiter fg-dim)

	;;diffs - duplicate modus deuteranopia colors (I don't like red/green)
	(bg-added             bg-yellow-subtle)
        (bg-added-faint       bg-yellow-faint)
        (bg-added-refine      bg-yellow-refine)
        (bg-added-intense     bg-yellow-intense)
        (fg-added             yellow)
        (fg-added-intense     yellow-intense)
        (bg-removed           bg-blue-subtle)
        (bg-removed-faint     bg-blue-faint)
        (bg-removed-refine    bg-blue-refine)
        (bg-removed-intense   bg-blue-intense)
        (fg-removed           blue)
        (fg-removed-intense   blue-intense)

	;; default red-syntax is too close to error
	(warning  yellow-syntax)

	))

;; enable fixed fonts for code and variable for text
(setq modus-themes-mixed-fonts t)

;; bold works well for coding faces
(setq modus-themes-bold-constructs t)

;; Tone down the cursor specifically for modus-operandi-tinted.
(setq modus-operandi-tinted-palette-overrides
      '((cursor "gray60")
	(comment fg-main)
	(keyword yellow-intense)
	(bg-completion bg-yellow-nuanced)
	))

;; Specific folio theme overrides
(setq folio-theme-palette-overrides
      ;; headers need some color and overlines stripped
      `(
	(bg-margins ,margin-tan-bg)
	(bg-heading-2 unspecified)
	(overline-heading-1 unspecified)
        (overline-heading-2 unspecified)
	(overline-heading-3 unspecified)
        (overline-heading-4 unspecified)
	(bg-prose-block-contents bg-tab-bar)

	;; I like a slightly bolder mode-line background
	(bg-mode-line-active "gray75")
	(bg-mode-line-emphasis "gray85")

	;; paren-matching
	(fg-paren-match red)
	(bg-paren-match bg-red-intense)

	;; Current set of coding keyword color selections.
	(keyword green-warmer)
	(type unspecified)
	(fnname yellow-cooler)
	(fnname-call unspecified)
	))

(setq modus-vivendi-palette-overrides
      `((bg-margins ,margin-gray-bg)
	(bg-mode-line-emphasis "gray50")
	(fg-hl-imenu magenta-cooler)
	))

(setq nano-like-palette-overrides
      `((bg-margins "gray95")
	))

(setq modus-vivendi-embers-palette-overrides
      `((bg-margins ,margin-gray-bg)))

;; Loop through all the buffers and force mixed-pitch-mode ones to reload.
(defun my-reload-fonts ()
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p mixed-pitch-mode)
	(mixed-pitch-mode)))))

;; Add the font changes onto the enable theme hook
(add-hook 'enable-theme-functions
          (lambda (theme)
            (progn
		(set-face-attribute 'default nil :family my-default-fixed-pitch-font)
                (set-face-attribute 'fixed-pitch nil :family my-default-fixed-pitch-font)
                (set-face-attribute 'variable-pitch nil :family my-default-variable-pitch-font)
		(my-reload-fonts)
		

	    ;; I'd like to have select on the modeline change the foreground rather than
	    ;; than the background and that needs a custom hook.
	    (when (eq theme 'folio)
	      (set-face-attribute 'mode-line-highlight nil :foreground "DarkOrange4"
				  :background 'unspecified))

	    ;; I have my own handling for tab line modified outside of modus
	    (if (facep 'tab-line-tab-modified)
		(set-face-attribute 'tab-line-tab-modified nil :foreground 'unspecified))

            ;; Additional color overrides modus doesn't control by default
	    (if (facep 'tab-line-tab-inactive)
		(set-face-attribute 'tab-line-tab-inactive nil :foreground
				    (modus-themes-get-color-value 'fg-dim t)))

	    (if (facep 'my-hl-imenu-face)
		(set-face-attribute 'my-hl-imenu-face nil :foreground
				    (modus-themes-get-color-value 'fg-hl-imenu t)))

	    (if (facep 'my-modeline-position-face)
		(set-face-attribute 'my-modeline-position-face nil :background
				    (modus-themes-get-color-value 'bg-mode-line-emphasis t))))))


;; Modus doesn't handle fonts so just set this directly here where all other styling is
;; being done. I like using a 1.3 scaled version of the system UI font for the tabs.
(if (< emacs-major-version 31)
  (custom-set-faces
   '(tab-line ((t :family ".AppleSystemUIFont" :height 1.3))))

  (custom-set-faces
   '(tab-line-active ((t :family ".AppleSystemUIFont" :height 1.3))))

  (custom-set-faces
   '(tab-line-inactive ((t :family ".AppleSystemUIFont" :height 1.3)))))

;; Make locally-defined themes (e.g. modus-vivendi-embers-theme.el, which
;; lives alongside this file) discoverable by `load-theme'/`M-x customize-themes'
;; without needing a package wrapper.
(add-to-list 'custom-theme-load-path (file-name-directory (or load-file-name buffer-file-name)))

;; Currently trying out the folio theme as my main theme.
(use-package folio-theme
  :ensure (:host github :repo "kn66/folio-theme.el")
  :config
  (load-theme 'folio t))

;; Deal with dark/light mode macos ui elements like the scrollbar
(use-package ns-auto-titlebar
  :ensure t
  :config
  (ns-auto-titlebar-mode 1))

;; Make sure the theme and titlebar packages above are fully installed and
;; activated before custom.el (which enables the folio theme by name) loads.
(elpaca-wait)

;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html
;; All customizations are stored on the side in custom.el
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Misc

(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000)

      whitespace-line-column nil  ; Use the value of `fill-column'.

      ;; Disable ellipsis when printing s-expressions in the message buffer
      eval-expression-print-length nil
      eval-expression-print-level nil

      ;; This directs gpg-agent to use the minibuffer for passphrase entry
      epg-pinentry-mode 'loopback

      ;; By default, Emacs stores sensitive authinfo credentials as unencrypted
      ;; text in your home directory. Use GPG to encrypt the authinfo file for
      ;; enhanced security.
      auth-sources '("~/.authinfo.gpg")

      ;; Speed up 'find-library' and reduce completion clutter by excluding
      ;; internal helper files. This provides a library-focused list.
      find-library-include-other-files nil

      ;; Protect the system from code injection vulnerabilities when browsing
      ;; files. Disabling local 'eval' expressions ensures that opening a
      ;; malicious project or third-party script cannot execute arbitrary Lisp
      ;; code on your machine.
      enable-local-eval nil)

;;  `prettify-symbols-mode': Show unprettified symbol at point
(setq prettify-symbols-unprettify-at-point 'right-edge)

;;; Minibuffer

(setq enable-recursive-minibuffers t ; Allow nested minibuffers

      ;; Keep the cursor out of the read-only portions of the.minibuffer
      minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; Display and user interface

;; By default, Emacs "updates" its ui more often than it needs to
(setq which-func-update-delay 1.0)
(with-no-warnings
  ;; Obsolete in >= 30.1
  (setq idle-update-delay which-func-update-delay))

(defalias #'view-hello-file #'ignore)  ; Never show the hello file

(setq
 ;; No beeping or blinking
 visible-bell nil
 ring-bell-function #'ignore

 ;; Position underlines at the descent line instead of the baseline.
 x-underline-at-descent-line t

 truncate-string-ellipsis "..."

 display-time-default-load-average nil ; Omit load average

 ;; Force the mouse to paste text at the active cursor position.
 mouse-yank-at-point t)

;;; Show-paren

(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;;; Buffer management

(setq custom-buffer-done-kill t

      ;; Disable auto-adding a new line at the bottom when scrolling.
      next-line-add-newlines nil

      ;; This setting forces Emacs to save bookmarks immediately after each
      ;; change. Benefit: you never lose bookmarks if Emacs crashes.
      bookmark-save-flag 1

      uniquify-buffer-name-style 'forward

      ;; Disable fontification during user input to reduce lag in large buffers.
      ;; Also helps marginally with scrolling performance.
      redisplay-skip-fontification-on-input t)

;;; `display-line-numbers-mode'

(setq-default display-line-numbers-width 3
              display-line-numbers-widen t)

;;; imenu

;; Automatically rescan the buffer for Imenu entries when `imenu' is invoked
;; This ensures the index reflects recent edits.
(setq-default imenu-auto-rescan t)

;; Prevent truncation of long function names in `imenu' listings
(setq imenu-max-item-length 160)

;;; Tramp

(setq tramp-verbose 1
      remote-file-name-inhibit-cache 50
      ;; Disable lockfiles and auto-saves for remote files to eliminate lag
      remote-file-name-inhibit-locks t
      remote-file-name-inhibit-auto-save-visited t)

;;; Files

(setq
 ;; Delete by moving to trash in interactive mode
 delete-by-moving-to-trash (not noninteractive)
 remote-file-name-inhibit-delete-by-moving-to-trash t

 ;; Ignoring this is acceptable since it will redirect to the buffer regardless.
 find-file-suppress-same-file-warnings t

 ;; Automatically resolve symlinks to their true paths. This sets the correct
 ;; working directory so C-x C-f opens in the right folder and version control
 ;; tools recognize the Git repository.
 find-file-visit-truename t

 ;; Automatically follow a symlink to its source if that source is managed
 ;; by a version control system, rather than asking for permission.
 vc-follow-symlinks t

 ;; Prefer vertical splits over horizontal ones
 split-width-threshold 170
 split-height-threshold nil

 ;; Increase threshold for large-file warning to reduce prompts when opening
 ;; moderately large files while still preserving safeguards for large files.
 large-file-warning-threshold (* 100 1024 1024)) ; 100 Mb

;;; comint (general command interpreter in a window)

(setq ansi-color-for-comint-mode t ; Renders native ANSI colors in the shell
      comint-prompt-read-only t
      comint-buffer-maximum-size 4096
      ;; Move the cursor to the bottom when the process prints new output
      comint-move-point-for-output t
      ;; Scroll the window viewport down when new output arrives
      comint-scroll-to-bottom-on-output t
      ;; Snap the view back down to the prompt the moment you start typing
      comint-scroll-to-bottom-on-input t)

;;; Compilation

(setq compilation-ask-about-save nil
      compilation-always-kill t
      ;; Parse up to 2048 characters per line in compilation buffers. This
      ;; safely catches deep errors and long paths without risking hangs.
      compilation-max-output-line-length 2048
      compilation-scroll-output 'first-error

      ;; Skip confirmation prompts when creating a new file or buffer
      confirm-nonexistent-file-or-buffer nil)

;; Add the ANSI color filter to the compilation filter hook to apply colors
;; immediately during compilation output processing.
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;;; Backup files

(setq
 ;; Disable the creation of lockfiles (e.g., .#filename).
 ;; Modern workflows rely on `global-auto-revert-mode' to handle external file
 ;; changes gracefully, making the restrictive nature of lockfiles unnecessary.
 create-lockfiles nil

 ;; Disable backup files (e.g., filename~). Note that `auto-save-default'
 ;; remains enabled by default. Even with `make-backup-files' backups disabled,
 ;; Emacs will still generate temporary recovery files (e.g., #filename#) for
 ;; unsaved buffers. This protects your active work from sudden crashes while
 ;; ensuring the file system is cleaned up immediately upon a successful save.
 make-backup-files nil

 backup-directory-alist `(("." . ,(expand-file-name "backup" user-emacs-directory)))
 tramp-backup-directory-alist backup-directory-alist
 backup-by-copying-when-linked t
 backup-by-copying t  ; Backup by copying rather renaming
 delete-old-versions t  ; Delete excess backup versions silently
 version-control t  ; Use version numbers for backup files
 kept-new-versions 5
 kept-old-versions 5)

;;; Auto revert
;; Auto-revert in Emacs is a feature that automatically updates the contents of
;; a buffer to reflect changes made to the underlying file.

;; Revert other buffers (e.g, Dired)
(setq global-auto-revert-non-file-buffers t
      global-auto-revert-ignore-modes '(Buffer-menu-mode))  ; Resolve issue #29

;;; recentf

;; `recentf' is a mode that maintains a list of recently accessed files.
(setq recentf-max-saved-items 210
      recentf-save-file "~/.emacs.d/recentf.el"
      recentf-auto-cleanup 'never
      recentf-exclude (list "/scp:"
                         "/ssh:"
                         "/sudo:"
                         "/tmp/"
                         "~$"
                         "COMMIT_EDITMSG")                         
      recentf-max-menu-items 15)

;; Wait for all of the Emacs settings to be set & then start in on installing packages we want.
(elpaca-wait)

(use-package general)
(use-package transient)
(use-package blackout)

(use-package no-littering)

(elpaca-wait)

;; Leader key
  (general-create-definer deftpunk-leader-def
    :keymaps 'override
    :prefix-map 'deftpunk-leader-map
    :prefix "s-SPC")

  ;; local leader
  ;; This allows for finer granularity than hydra-major-mode by binding to individual key maps.
  (general-create-definer deftpunk-local-leader-def
    :keymaps 'override
    :prefix "C-;")

  (use-package which-key
    :blackout
    :commands (which-key-mode which-key-show-toplevel)
    :hook (on-first-input . which-key-mode)
    :custom
    (which-key-enable-exteded-define-key t)
    (which-key-idle-delay 0.5)
    :config
    (elpaca-wait)
    (which-key-mode +1))

;;; Vertico, Orderless, Marginalia, Consult, Embark, Embark-Consult

;; Enable Vertico.

;; https://github.com/daut/dotfiles/blob/2ad4f5a5e0f1e786c91f17460d8599ec5d57318b/.emacs.d/init.el#L105
(defun daut/minibuffer-backward-kill (arg)
  (interactive "p")
  (if (and minibuffer-completing-file-name
       (eq (char-before) ?/))
  (zap-up-to-char (- arg) ?/)
    (delete-backward-char arg)))

(use-package vertico
  :ensure t
  :after minibuffer
  :commands (vertico-mode
             vertico-insert
             vertico-exit)
  :hook (after-init . vertico-mode)
  :general
  (:keymaps 'vertico-map
            "C-e" #'vertico-move-end-of-line-or-insert
            "<backspace>" #'daut/minibuffer-backward-kill
            "<escape>" #'minibuffer-keyboard-quit)
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
 (vertico-resize t) ;; Grow and shrink the Vertico minibuffer based on vertico count.
  (vertico-count 45) ;; show more candidates
 (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
 :init
 
 (defun vertico-move-end-of-line-or-insert (arg)
    "Move to end of line or insert current candidate.
   ARG lines can be used.

   When only one candidate exists exit input after insert."
    (interactive "p")
    (if (eolp)
        (progn
          (vertico-insert)
          (when (= vertico--total 1)
            (vertico-exit)))
      (move-end-of-line arg)))
 
 (vertico-mode))

;; Using `find-file to initiate TRAMP connections.
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


;; Emacs minibuffer configurations.
(use-package emacs
  :ensure nil
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Enable orderless
(use-package orderless
  :ensure t
  :custom
  ;;(completion-styles '(orderless basic))
  (orderless-matching-styles '(orderless-prefixes))
  (completion-ignore-case t)
  (completion-styles '(basic substring initials orderless))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion behaves like substring
  :config
  ;; TODO: modify match faces in the future.
  ;;(set-face-attribute 'orderless-match-face-0 nil
  ;;                    :foreground "#d70000")
  ;;(set-face-attribute 'orderless-match-face-1 nil
  ;;                    :foreground "#005fd7")
  ;;(set-face-attribute 'orderless-match-face-2 nil
  ;;                    :foreground "#007f3a")
  ;;(set-face-attribute 'orderless-match-face-3 nil
  ;;                    :foreground "#d700d7")
  )
  
;; Enable marginalia
(use-package marginalia
  :ensure t
  :general
  (:keymaps 'minibuffer-local-map
            "s-a" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left)
  :init
  (marginalia-mode 1))

;; Enable consult
;; consult-focus-lines
;; consult-buffer
;; consult-yank-pop
;; consult-ripgrep
;; consult-outline
;; consult-imenu
;; consult-register
;; consult-history
;; consult-info
;; https://github.com/jdtsmith/consult-jump-project
(use-package consult
  :ensure t
  :bind (("s-i" . consult-buffer)
         ("s-s" . consult-line)
         ("s-r" . consult-ripgrep)
         ([remap Info-search] . consult-info)
         ;; Misc bindings
         ("s-y" . consult-yank-pop))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.25
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config

;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-man consult-find
   consult-bookmark consult-recent-file consult-xref consult-projectile-find-file
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "C-<SPC>")
  ;; :preview-key '(:debounce 0.4 any))

  ;; Narrowing
  (setq consult-narrow-key "<") ; Prefix key for narrowing results
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help) ; get help for narrowing

  (define-key consult-narrow-map [C-left] #'consult-narrow-cycle-backward)
  (define-key consult-narrow-map [C-right] #'consult-narrow-cycle-forward)

  (defun consult-narrow-cycle-backward ()
    "Cycle backward through the narrowing keys."
    (interactive)
    (when consult--narrow-keys
      (consult-narrow
       (if consult--narrow
           (let ((idx (seq-position consult--narrow-keys
                                    (assq consult--narrow consult--narrow-keys))))
             (unless (eq idx 0)
               (car (nth (1- idx) consult--narrow-keys))))
         (caar (last consult--narrow-keys))))))

  (defun consult-narrow-cycle-forward ()
    "Cycle forward through the narrowing keys."
    (interactive)
    (when consult--narrow-keys
      (consult-narrow
       (if consult--narrow
           (let ((idx (seq-position consult--narrow-keys
                                    (assq consult--narrow consult--narrow-keys))))
             (unless (eq idx (1- (length consult--narrow-keys)))
               (car (nth (1+ idx) consult--narrow-keys))))
         (caar consult--narrow-keys)))))
  )

;; https://protesilaos.com/codelog/2026-07-29-emacs-default-minibuffer-completion-overview/
;; https://github.com/mhayashi1120/Emacs-wgrep

;; Enable embark
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("s-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
   :map minibuffer-local-map
   ("C-c C-c" . embark-collect)
   ("C-c C-e" . embark-export))
   
  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Enable embark-consult
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  ) ; only need to install it, embark loads it after consult if found

;; Enable corfu
;; TAB key: fix indentation if needed, otherwise perform completion
(setq tab-always-indent 'complete)

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-cycle t) ; cycle around to first entry after reaching the last
  (corfu-preview-current nil) ; don't expand text at point until I press return
  (corfu-min-width 20)
  (corfu-on-exact-match 'insert) ; complete if there is only a single candidate
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  :config
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation next to completions

  ;; sort by input history
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  )

;; Nice icons for corfu.
(use-package kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; fix overly large icons (https://github.com/jdtsmith/kind-icon/issues/22)
(setq kind-icon-default-style
      '(:padding -1 :stroke 0 :margin 0 :radius 0 :height 0.4 :scale 1))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :custom
  (history-length 300)
  (savehist-additional-variables
   '(register-alist                    ; macros
     extended-command-history
     mark-ring
     global-mark-ring        ; marks
        search-ring
	regexp-search-ring))  ; searches
  :init
  (savehist-mode))

;; Limit some of the items in savehist
(put 'savehist-minibuffer-history-variables 'history-length 50)
(put 'org-read-date-history                 'history-length 50)
(put 'read-expression-history               'history-length 50)
(put 'org-table-formula-history             'history-length 50)
(put 'extended-command-history              'history-length 50)
(put 'ido-file-history                      'history-length 50)
(put 'helm-M-x-input-history                'history-length 50)
(put 'minibuffer-history                    'history-length 50)
(put 'ido-buffer-history                    'history-length 50)
(put 'buffer-name-history                   'history-length 50)
(put 'file-name-history                     'history-length 50)

;; Save your last place in Emacs.
(use-package saveplace
  :ensure nil
  :custom
  (save-place-limit 100)
  (save-place-forget-unreadable-files nil)  ; Setting to t has the potential to make exiting slow
  :config
  (save-place-mode 1)
  (add-hook 'save-place-find-file-hook 'recenter)
  (add-hook 'find-file-hook 'save-place-find-file-hook t))


;; Enable cape
(use-package cape
  :defer 1
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev 20) ; words from buffer
  (add-hook 'completion-at-point-functions #'cape-file 20))

;;; Frames, Windows & buffers

(use-package windmove
  :ensure nil
  :bind (("s-h" . windmove-left)
         ("s-j" . windmove-down)
         ("s-k" . windmove-up)
         ("s-l" . windmove-right)))

;;; Org

(use-package org)

;;; Keybindings & key-chord


(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-smart-kill-line)))

(global-set-key (kbd "s-o") 'other-window)

 (global-unset-key (kbd "M-u"))
(use-package unfill
  :bind
  (("M-u" . unfill-toggle)))

 ;; Trying out matching parens
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Matching-parentheses.html
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key "%" 'match-paren)

 ;; From https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;; Some final remapping of Escape to "Quit"

(global-set-key (kbd "<escape>") #'prot/keyboard-quit-dwim)

 (use-package key-chord
  :commands (key-chord-define-global)
  :init
  (key-chord-mode 1)
  :config
  (key-chord-define-global "jk" 'execute-extended-command)
  (key-chord-define-global "hh" 'split-window-below)
  (key-chord-define-global "vv" 'split-window-right))

;; (setq gc-cons-threshold 16777216) ; 16MB
(setq gc-cons-threshold most-positive-fixnum) ; 16MB
(run-with-idle-timer 1.2 t 'garbage-collect)

(server-start)
