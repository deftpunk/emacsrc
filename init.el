;;; init.el -*- lexical-binding: t; -*-

;;; Lots of stuff taken directly from Doom Emacs - he does a fantastic job!

;;
;;; Some Globals

(defvar deftpunk--interactive-mode (not noninteractive)
  "If non-nil Emacs is in interactive mode.")

(defvar deftpunk--fringe-size '12
  "Make the fringe wide enough but not too wide")

;; I only use Mac & occasionally Linux
(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))

;;; Some global directories and files.
(defconst deftpunk--emacs-dir (file-truename "~/.emacs.d")
  "The path to the emacs.d directory")

(defconst deftpunk--local-dir (concat deftpunk--emacs-dir "/local/")
  "Storage for files that are safe to share across systems, e.g. symlinked
  across serveral computers.")

(defconst deftpunk--cache-dir (concat deftpunk--emacs-dir "/cache/")
  "Directory of volatile files.")

(defconst deftpunk--etc-dir (concat deftpunk--emacs-dir "/etc/")
  "This is a storage location for files that don't change much, e.g. custome.el - no-littering
will use this later.")

(defconst deftpunk--core-packages
  '(use-package paradox)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(defconst deftpunk--org-configuration-file (concat deftpunk--emacs-dir "/config.org")
  "The Org mode literate configuration file.")

(defconst deftpunk--org-elfile (concat deftpunk--cache-dir "/config.el")
  "The Org mode literate configuration file.")

(defconst deftpunk-autoload-file (concat deftpunk--emacs-dir "/autoloads.el")
  "Where the dynamically generated autoloads file ends up.")

(defconst deftpunk--src-dir (concat deftpunk--emacs-dir "/src")
  "Where I put packages that I clone manually from github, etc.")

;; Some hooks
(defvar deftpunk-init-ui-hook nil
  "List of hooks to run when the theme and font is initialized.")

;;
;;; The really big eval-and-compile

(eval-and-compile

  ;; A big contributor to startup times is garbage collection. We up the gc
  ;; threshold to temporarily prevent it from running, then reset it later by
  ;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
  (setq gc-cons-threshold most-positive-fixnum)

  ;; I want to know how long it took to load & how many garbage collections. Use
  ;; a hook so the message doesn't get clobbered by other messages.
  (add-hook 'emacs-startup-hook (lambda ()
				  (message "Emacs ready in %s with %d garbage collections."
					   (format "%.2f seconds"
						   (float-time
						     (time-subtract after-init-time before-init-time)))
					   gcs-done)))

  (let (file-name-handler-alist)
    (setq user-emacs-directory (file-name-directory load-file-name)))

  ;; Load up core-lib which has a bunch of stuff we need/use to get started.
  ;; Don't put the other code in this file into separate files, we tried that
  ;; but its just slower.
  (load (concat user-emacs-directory "core/core-lib")
        nil 'nomessage)

  ;;
  ;;; Optimizations from Doom Emacs
  ;;; I copied these wholesale because ... they work!

  ;; Disable bidirectional text rendering for a modest performance boost. I've set
  ;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
  ;; is an undefined state and suggest this to be just as good:
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
  ;; in non-focused windows.
  (setq-default cursor-in-non-selected-windows nil)
  (setq highlight-nonselected-windows nil)

  ;; More performant rapid scrolling over unfontified regions. May cause brief
  ;; spells of inaccurate syntax highlighting right after scrolling, which should
  ;; quickly self-correct.
  (setq fast-but-imprecise-scrolling t)

  ;; Resizing the Emacs frame can be a terribly expensive part of changing the
  ;; font. By inhibiting this, we halve startup times, particularly when we use
  ;; fonts that are larger than the system default (which would resize the frame).
  (setq frame-inhibit-implied-resize t)

  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject)

  ;; Font compacting can be terribly expensive, especially for rendering icon
  ;; fonts on Windows. Whether it has a noteable affect on Linux and Mac hasn't
  ;; been determined, but we inhibit it there anyway.
  (setq inhibit-compacting-font-caches t)

  ;; Remove command line options that aren't relevant to our current OS; means
  ;; slightly less to process at startup.
  (unless IS-MAC   (setq command-line-ns-option-alist nil))
  (unless IS-LINUX (setq command-line-x-option-alist nil))

  ;; Delete files to trash on macOS, as an extra layer of precaution against
  ;; accidentally deleting wanted files.
  (setq delete-by-moving-to-trash IS-MAC)

  ;; Adopt a sneaky garbage collection strategy of waiting until idle time to
  ;; collect; staving off the collector while the user is working.
  (when deftpunk--interactive-mode
    (add-to-list 'load-path (concat deftpunk--src-dir "/gcmh"))
    (require 'gcmh)
    (setq gc-cons-percentage 0.6)
    (add-transient-hook! 'pre-command-hook (gcmh-mode +1))
    (with-eval-after-load 'gcmh
      (setq gcmh-idle-delay 30
            gcmh-high-cons-threshold 16777216
            gcmh-verbose nil ); I don't want to see any messaging when it runs.
            ;;gc-cons-percentage 0.1)
      (add-hook 'focus-out-hook #'gcmh-idle-garbage-collect)))

  ;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
  ;;      reason. Disabling it completely could have many side-effects, so we
  ;;      defer it until later, at which time it (somehow) runs very quickly.
  (unless (daemonp)
    (advice-add #'tty-run-terminal-initialization :override #'ignore)
    (add-hook! 'window-setup-hook
	       (defun doom-init-tty-h ()
		 (advice-remove #'tty-run-terminal-initialization #'ignore)
		 (tty-run-terminal-initialization (selected-frame) nil t))))

  ;; In noninteractive sessions, prioritize non-byte-compiled source files to
  ;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
  ;; to skip the mtime checks on every *.elc file.
  (setq load-prefer-newer noninteractive)

  ;; It may also be wise to raise gc-cons-threshold while the minibuffer is active, so the GC doesn’t
  ;; slow down expensive commands (or completion frameworks, like helm and ivy). Here is how Doom does
  ;; it:
  (defun doom-defer-garbage-collection-h ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun doom-restore-garbage-collection-h ()
    ;; Defer it so that commands launched immediately after will enjoy the
    ;; benefits.
    (run-at-time
     1 nil (lambda () (setq gc-cons-threshold 16777216)))) ; 16mb

  (add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
  (add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

  ;;
  ;;; My optimizations

  ;; clear out the file-name-handler-alist
  (setq after-init-time nil)
  (defvar deftpunk--file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  (defun deftpunk/file-name-handler-alist-reset ()
    (dolist (handler file-name-handler-alist)
      (add-to-list 'deftpunk--file-name-handler-alist handler))
    (setq file-name-handler-alist deftpunk--file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'deftpunk/file-name-handler-alist-reset)

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

  ;;
  ;;; Package initialization.
  ;;; Using straight and use-package together - straight does the actual installation and use-package is
  ;;; sugar around it.

  ;; Straight
  (setq straight-base-dir deftpunk--local-dir
        ;; change to "master" if we need a more stable straight.el
        straight-repository-branch "develop"
        ;; Per Doom, byte-code is rarely compatible across emacs versions, so
        ;; build them in separate directories.
        straight-build-dir (format "build-%s" emacs-version)
        ;; We don't want the startup penalty
        ;; Run M-x straight-check-all occasionaly :)
        straight-check-for-modifications nil
        ;; use-package is the sugar; to prevent installation set ":straight nil" in the use-package
        ;; declaration.
        straight-use-package-by-default t
        ;; we want to handle Org ourselves.
        straight-fix-org nil)

  ;; bootstrap straight if we need to.
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" deftpunk--local-dir))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; Lets use use-package
  ;; Install delight here so that we can use it later on major/minor modes.
  (straight-use-package 'use-package)
  (use-package delight)

  (straight-use-package 'org)
  (straight-use-package 'org-plus-contrib)

    ;;
    ;;; Clean up the UI early.
    ;;; Get rid of the toolbar, scrollbar and startup message nonsense.
    (if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
    (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

    ;;
    ;;; Lets try this autoloads thing one more time.
    ; (defvar generated-autoload-file (concat deftpunk--emacs-dir "/loaddefs.el"))
    ; (defun update-all-autoloads ()
    ;   (interactive)
    ;   (let ((generated-autoload-file (concat deftpunk--emacs-dir "/loaddefs.el")))
	; (message "generated-autoload-file %s" generated-autoload-file)
	; (when (not (file-exists-p generated-autoload-file))
	  ; (with-current-buffer (find-file-noselect generated-autoload-file)
	    ; (insert ";;") ;; create the file with non-zero size to appease autoload
	    ; (save-buffer)))
	; (mapc #'update-directory-autoloads
	      ; '("/Users/ebodine/MyStuff/emacs-bankruptcy-plain/autoloads"))))
    ; (update-all-autoloads)
    ; (load (concat deftpunk--emacs-dir "/loaddefs.el"))

  ;;
  ;;; Emacs core configuration.

  ;; Make the *Messages* log much longer.
  (setq message-log-max 8192)

  ;; Make UTF-8 the default coding system.
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8
        selection-coding-system 'utf-8)

  ;; make `apropos' more useful
  (setq apropos-do-all t)

  ;; silence advised function warnings
  (setq ad-redefinition-action 'accept)

  ;; Don't pass over `auto-mode-alist' a second time.
  (setq auto-mode-case-fold nil)

  ;; update the UI less often
  (setq idle-update-delay 1)

  ;; don't ask to kill buffers
  (setq kill-buffer-query-functions
	(remq 'process-kill-buffer-query-function
	      kill-buffer-query-functions))

  ;; Get rid of "For information about GNU Emacs..." message at startup
  (unless noninteractive
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    (setq inhibit-startup-message t
	  inhibit-startup-echo-area-message user-login-name
	  inhibit-default-init t
	  ;; initial-major-mode 'fundamental-mode
	  initial-scratch-message nil
	  mode-line-format nil))
  ;; we're in a daemon session, where it'll say "Starting Emacs daemon." instead
  (unless (daemonp)
    (advice-add #'display-startup-echo-area-message :override #'ignore))

  ;; This file stores usernames, passwords, and such.
  (setq auth-sources (list (expand-file-name "authinfo.gpg" deftpunk--etc-dir)
                         "~/.authinfo.gpg"))

  ;; Some timezone stuff for logview.el and others.
  (setenv "TZ" "America/Denver")
  (setq datetime-timezone "America/Denver")

  ;; I find the custom system a nuisance, move it out of the way.
  (setq custom-file (concat deftpunk--local-dir "/custom.el"))
  (load custom-file t t)

  ;; No littering without `no-littering'
  (setq auto-save-list-file-name     (concat deftpunk--cache-dir "autosave")
        mc/list-file                 (concat deftpunk--etc-dir "mc-lists.el")
        pcache-directory             (concat deftpunk--cache-dir "pcache/")
        request-storage-directory    (concat deftpunk--cache-dir "request")
        server-auth-dir              (concat deftpunk--cache-dir "server/")
        tramp-auto-save-directory    (concat deftpunk--cache-dir "tramp-auto-save/")
        tramp-backup-directory-alist backup-directory-alist
        tramp-persistency-file-name  (concat deftpunk--cache-dir "tramp-persistency.el")
        url-cache-directory          (concat deftpunk--cache-dir "url/")
        url-configuration-directory  (concat deftpunk--etc-dir "url/"))

  ;;
  ;;; Abbreviations

  ;; Don't litter the abbreviations file and save it automatically without bugging me.
  (setq abbrev-file-name (concat deftpunk--etc-dir "abbrev.el"))
  (setq save-abbrevs 'silently)

  ;;
  ;;; Bookmarks

  ;; Bookmarks need to be saved often, save to bookmarks file each time we add one.
  (setq bookmark-save-flag 1)

  ;; We don't need versioned bookmark files.
  (setq bookmark-version-control nil)

  ;; Don't litter the bookmarks file.
  (setq bookmark-default-file (concat deftpunk--etc-dir "bookmarks"))

  ;;
  ;;
  ;;; Clipboard

  ;; Allow UTF or composed text from the clipboard.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

  ;; THis makes the kill-ring easier to peruse.
  (setq kill-do-not-save-duplicates t)

  ;; Save existing clipboard text into the kill ring before replacing it.
  (setq save-interprogram-paste-before-kill t)

  ;; Use a shared clipboard
  (setq select-enable-clipboard t
        select-enable-primary   t)

  ;; An MacOSX specific clipboard
  ;; It works in a terminal window and allows you to cut and paste via pbpaste & pbcopy. It is a noop
  ;; when running a graphical Emacs.
  (when IS-MAC
    (use-package osx-clipboard
		 :defer t
		 :commands (osx-clipboard-mode)
		 :init (osx-clipboard-mode +1)))

  ;; Make graphical Emacs use pbpaste & pbcopy on MacOSX.
  (when IS-MAC
    ;; paste-to-osx & copy-from-osx are autoloads from autoload/os.el - they make
    ;; use of pbpaste and pbcopy under the hood.
    ; (setq interprogram-cut-function 'paste-to-osx
    ;       interprogram-paste-function 'copy-from-osx)
    )

    (when IS-MAC
       (cond ((display-graphic-p)
              ;; A known problem with GUI Emacs on MacOS: it runs in an isolated
              ;; environment, so envvars will be wrong. That includes the PATH
              ;; Emacs picks up. `exec-path-from-shell' fixes this. This is slow
              ;; and benefits greatly from compilation.
              ;; https://github.com/purcell/exec-path-from-shell
              (use-package exec-path-from-shell
                :commands (exec-path-from-shell-initialize exec-path-from-shell-variables)
                :init
                (setq exec-path-from-shell-check-startup-files nil)
                (setq exec-path-from-shell-variables '("PATH" "GOPATH" "GOBIN" "GOROOT" "JAVA_HOME" "PYTHONPATH"))
                (exec-path-from-shell-initialize))
              )))

  ;;
  ;;; Mouse
  ;; Enable mouse in terminal Emacs.
  (add-hook 'tty-setup-hook #'xterm-mouse-mode)

  ;; MacOSX mouse stuff.
  (when IS-MAC
    (setq mac-redisplay-dont-reset-vscroll t
	  mac-mouse-wheel-smooth-scroll nil
	  mouse-wheel-scroll-amount '(5 ((shift) . 2))))

  ;; Allow the mouse to copy at the point.
  (setq mouse-yank-at-point t)

  ;;
  ;;; Scrolling
  (setq hscroll-margin 2
        hscroll-step 1
        ;; Emacs spends too much effort recentering the screen if you scroll the
        ;; cursor more than N lines past window edges (where N is the settings of
        ;; `scroll-conservatively'). This is especially slow in larger files
        ;; during large-scale scrolling commands. If kept over 100, the window is
        ;; never automatically recentered.
        scroll-conservatively 101
        scroll-margin 0
        scroll-preserve-screen-position t
        ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
        ;; for tall lines.
        auto-window-vscroll nil
        ;; mouse
        mouse-wheel-scroll-amount '(5 ((shift) . 2))
        mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

  ;; Remove hscroll-margin in shells, otherwise it causes jumpiness
  (setq-hook! '(eshell-mode-hook term-mode-hook) hscroll-margin 0)

  ;;
  ;;; Cursors
  (setq blink-cursor-mode -1                ; distraction prevention.
        blink-matching-paren nil            ; more distraction prevention.
        cursor-in-non-selected-windows nil  ; hide cursors in other windows.
        visible-cursor nil                  ; use the "normal" cursor.
        x-stretch-cursor nil)               ; don't stretch the cursor to fit wide characters.

  ;;
  ;;; Fringes
  ;; standardize the default fringe size
  (if (fboundp 'fringe-mode) (fringe-mode deftpunk--fringe-size))

  ;; Reduce the clutter in the fringes; we'd like to reserve that space for more
  ;; useful information, like git-gutter and flycheck.
  (setq indicate-buffer-boundaries nil
        indicate-empty-lines nil)

   ;; remove continuation arrow on right fringe
   (setq fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                              fringe-indicator-alist))

  ;;
  ;;; Windows and Frames.

  (when IS-MAC
    ;; Make my frames sort of transparent on a Mac.
    (set-frame-parameter (selected-frame) 'alpha '(97 97))
    (add-to-list 'default-frame-alist '(alpha 97 97))

    ;; Avoid MacOSX fullscreen.
    ;; Visit files opened outside of Emacs in existing frame, not a new one
    (setq ns-use-native-fullscreen nil
	  ns-pop-up-frames nil)

    ;; Sets `ns-transparent-titlebar' and `ns-appearance' frame parameters so
    ;; window borders will match the enabled theme.
    (and (or (daemonp)
	     (display-graphic-p))
	 (require 'ns-auto-titlebar nil t)
	 (ns-auto-titlebar-mode +1))

     ;; HACK On MacOS, disabling the menu bar makes MacOS treat Emacs as a
     ;;      non-application window -- which means it doesn't automatically capture
    ;;      focus when it is started, among other things. We enable menu-bar-lines
    ;;      there, but we still want it disabled in terminal frames because there
    ;;      it activates an ugly menu bar.
    (add-hook! '(window-setup-hook after-make-frame-functions)
	       (defun doom-init-menu-bar-in-gui-frames-h (&optional frame)
		 "Re-enable menu-bar-lines in GUI frames."
		 (when-let (frame (or frame (selected-frame)))
			   (when (display-graphic-p frame)
			     (set-frame-parameter frame 'menu-bar-lines 1))))))

  ;; Don't resize windows & frames in steps; it's prohibitive to prevent the user
  ;; from resizing it to exact dimensions, and looks weird.
  (setq window-resize-pixelwise t
        frame-resize-pixelwise t)

  ;; Set the frame title to the full path of the buffer we are working on.
  (setq frame-title-format
	'((:eval (if (buffer-file-name)
		     (abbreviate-file-name (buffer-file-name))
		     "%b")))
	icon-title-format frame-title-format)

  ;; No GUI dialog boxes
  (setq use-dialog-box nil)

  ;; Don't display floating tooltips; display their contents in the echo-area,
  ;; because native tooltips are ugly.
  (when (bound-and-true-p tooltip-mode)
    (tooltip-mode -1))
  ;; ...especially on linux
  (when IS-LINUX
    (setq x-gtk-use-system-tooltips nil))

  ;; Favor vertical splits over horizontal ones. Screens are usually wide.
  (setq split-width-threshold 160
        split-height-threshold nil)

  ;;
  ;;; Minibuffer

  ;; keep the point out of the minibuffer
  (setq minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

  ;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
  ;; while we're in the minibuffer.
  (setq enable-recursive-minibuffers t)

  ;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
  ;; feedback after typing is better UX than no feedback at all.
  (setq echo-keystrokes 0.02)

  ;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
  ;; doesn't look too great with direnv, however...
  (setq resize-mini-windows 'grow-only
        ;; But don't let the minibuffer grow beyond this size
        max-mini-window-height 0.3)

  ;; Typing yes/no is obnoxious when y/n will do
  (advice-add #'yes-or-no-p :override #'y-or-n-p)

  ;; Try really hard to keep the cursor from getting stuck in the read-only prompt
  ;; portion of the minibuffer.
  (setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; No need to confirm this
  (setq confirm-nonexistent-file-or-buffer nil)

  ;;
  ;;; Files

  ;; warn when opening files bigger than 100MB
  (setq large-file-warning-threshold 100000000)

  ;; Resolve symlinks when opening files, so that any operations are conducted
  ;; from the file's true directory (like `find-file').
  (setq find-file-visit-truename t
        vc-follow-symlinks t)

  ;; Disable the warning "X and Y are the same file". It's fine to ignore this
  ;; warning as it will redirect you to the existing buffer anyway.
  (setq find-file-suppress-same-file-warnings t)

  ;; History and Backup settings - save nothing, that's what git is for ... unless we have to :)
  (setq auto-save-default nil
        create-lockfiles nil
        make-backup-files nil
        auto-save-list-file-prefix (concat deftpunk--cache-dir "autosave/")
        auto-save-file-name-transforms `((".*" ,auto-save-list-file-prefix t))
        backup-directory-alist       (list (cons "." (concat deftpunk--cache-dir "/backup/"))))

  ;;
  ;;; Buffers

  ;; Don't accidentally kill the *scratch* buffer.
  (add-hook 'kill-buffer-query-functions #'flex|dont-kill-scratch-buffer)

  ;; Make `next-buffer', `other-buffer', etc. ignore unreal buffers.
  (push '(buffer-predicate . doom-buffer-frame-predicate) default-frame-alist)

  ;;
  ;;; Formatting

  ;; Spaces over tabs and 4 spaces at that ... as if there are other options, go-mode not withstanding.
  (setq-default indent-tabs-mode nil
                tab-width 4)

  ;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
  ;; middle of a line.
  (setq tabify-regexp "^\t* [ \t]+")

  ;; I want 105 columns, the rest of the world be damned.
  (setq-default fill-column 105)

  ;; Continue wrapped words at whitespace, rather than in the middle of a word.
  (setq-default word-wrap t)
  ;; ...but don't do any wrapping by default. It's expensive. Enable
  ;; `visual-line-mode' if you want soft line-wrapping. `auto-fill-mode' for hard
  ;; line-wrapping.
  (setq-default truncate-lines t)

  ;; If enabled (and `truncate-lines' was disabled), soft wrapping no longer
  ;; occurs when that window is less than `truncate-partial-width-windows'
  ;; characters wide. We don't need this, and it's extra work for Emacs otherwise,
  ;; so off it goes.
  (setq truncate-partial-width-windows nil)

  ;; The POSIX standard defines a line is "a sequence of zero or more non-newline
  ;; characters followed by a terminating newline", so files should end in a
  ;; newline. Windows doesn't respect this (because it's Windows), but we should,
  ;; since programmers' tools tend to be POSIX compliant.
  (setq require-final-newline t)

  ;; Default to soft line-wrapping in text modes. It is more sensibile for text
  ;; modes, even if hard wrapping is more performant.
  (add-hook 'text-mode-hook #'visual-line-mode)

  ;; Show trailing whitespace - ws-butler will handle deleting it.
  (add-hook 'prog-mode-hook '(lambda ()
			       (setq show-trailing-whitespace 1)))

  ;;
  ;;; Builtin Packages

  ;; Auto revert buffers.
  (use-package autorevert
    ;; revert buffers when their files/state have changed
    :hook (focus-in . doom-auto-revert-buffers-h)
    :hook (after-save . doom-auto-revert-buffers-h)
    :hook (doom-switch-buffer . doom-auto-revert-buffer-h)
    :hook (doom-switch-window . doom-auto-revert-buffer-h)
    :config
    (setq auto-revert-verbose t ; let us know when it happens
          auto-revert-use-notify nil
          auto-revert-stop-on-user-input nil
          ;; Only prompts for confirmation when buffer is unsaved.
          revert-without-query (list "."))

    ;; Instead of `auto-revert-mode' or `global-auto-revert-mode', we lazily auto
    ;; revert; when we save a file or switch buffers/windows (or focus on Emacs).
    ;;
    ;; Autorevert normally abuses the heck out of inotify handles which can grind
    ;; Emacs to a halt if you do expensive IO (outside of Emacs) on the files you
    ;; have open (like compression). The only alternative is aggressive polling,
    ;; which is unreliable and expensive with a lot of buffers open.
    (defun doom-auto-revert-buffer-h ()
      "Auto revert current buffer, if necessary."
      (unless (or auto-revert-mode (active-minibuffer-window))
        (auto-revert-handler)))

    (defun doom-auto-revert-buffers-h ()
      "Auto revert stale buffers in visible windows, if necessary."
      (dolist (buf (doom-visible-buffers))
        (with-current-buffer buf
          (doom-auto-revert-buffer-h)))))

  ;; column-number-mode
  ;; Display the column number in the modeline
  (column-number-mode 1)

  ;; Compilation mode.
  (setq ansi-color-for-comint-mode t)
  (after! compile
	  (setq compilation-always-kill t        ; kill compilation process before starting another
          compilation-ask-about-save nil   ; save all buffers on `compile'
          compilation-error-screen-columns nil
          compilation-context-lines 2
          compilation-scroll-output 'first-error))

  ;; Ediff
  (after! ediff
    (setq ediff-diff-options "-w" ; turn off whitespace checking
          ediff-use-long-help-message 1
          ediff-split-window-function #'split-window-horizontally
          ediff-window-setup-function #'ediff-setup-windows-plain))


  ;; Highlight the current line.
  ;; Use some Doom ideas to make the hl-line active only when we want.
  (use-package hl-line
    :defer t
    :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
    :config
    ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
    ;; performance boost. I also don't need to see it in other buffers.
    (setq hl-line-sticky-flag nil
          global-hl-line-sticky-flag nil)

    ;; Temporarily disable `hl-line' when selection is active, since it doesn't
    ;; serve much purpose when the selection is so much more visible.
    (defvar doom--hl-line-mode nil)

    (add-hook! '(evil-visual-state-entry-hook activate-mark-hook)
      (defun doom-disable-hl-line-h ()
        (when hl-line-mode
          (setq-local doom--hl-line-mode t)
          (hl-line-mode -1))))

    (add-hook! '(evil-visual-state-exit-hook deactivate-mark-hook)
      (defun doom-enable-hl-line-maybe-h ()
        (when doom--hl-line-mode
          (hl-line-mode +1)))))

  ;; Line Numbers
  ;; We will toggle line numbers when we need them but we want to make sure
  ;; that they are configured correctly.

  ;; Explicitly define a width to reduce computation
  (setq-default display-line-numbers-width 2)

  ;; Show absolute line numbers for narrowed regions makes it easier to tell the
  ;; buffer is narrowed, and where you are, exactly.
  (setq-default display-line-numbers-widen t)

  ;; Recentf
  (use-package recentf
    ;; Keep track of recently opened files
    :commands recentf-open-files
    :config
    (defun doom--recent-file-truename (file)
      (if (or (file-remote-p file nil t)
              (not (file-remote-p file)))
          (file-truename file)
        file))
    (setq recentf-filename-handlers
          '(substring-no-properties    ; strip out lingering text properties
            doom--recent-file-truename ; resolve symlinks of local files
            abbreviate-file-name)      ; replace $HOME with ~
          recentf-save-file (concat deftpunk--cache-dir "recentf")
          recentf-auto-cleanup 'never
	  recentf-exclude (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
				"^/var/folders/.+$"
				; ignore private temp files (but not all of them)
				(concat "^" (file-truename deftpunk--local-dir)))
          recentf-max-menu-items 0
          recentf-max-saved-items 200)

    (add-hook! '(doom-switch-window-hook write-file-functions)
      (defun doom--recentf-touch-buffer-h ()
        "Bump file in recent file list when it is switched or written to."
        (when buffer-file-name
          (recentf-add-file buffer-file-name))
        ;; Return nil for `write-file-functions'
        nil))

    (add-hook! 'dired-mode-hook
      (defun doom--recentf-add-dired-directory-h ()
        "Add dired directory to recentf file list."
        (recentf-add-file default-directory)))

    (unless noninteractive
      (add-hook 'kill-emacs-hook #'recentf-cleanup)
      (quiet! (recentf-mode +1))))

  (use-package savehist
    ;; persist variables across sessions
    :config
    (setq savehist-file (concat deftpunk--cache-dir "savehist")
          savehist-save-minibuffer-history t
          savehist-autosave-interval nil ; save on kill only
          savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
    (savehist-mode +1)

    (add-hook! 'kill-emacs-hook
      (defun doom-unpropertize-kill-ring-h ()
        "Remove text properties from `kill-ring' for a smaller savehist file."
        (setq kill-ring (cl-loop for item in kill-ring
                                 if (stringp item)
                                 collect (substring-no-properties item)
                                 else if item collect it)))))

  (use-package saveplace
    ;; persistent point location in buffers
    :config
    (setq save-place-file (concat deftpunk--cache-dir "saveplace")
	  save-place-forget-unreadable-files nil
          save-place-limit 100)

    (defadvice! doom--recenter-on-load-saveplace-a (&rest _)
      "Recenter on cursor when loading a saved place."
      :after-while #'save-place-find-file-hook
      (if buffer-file-name (ignore-errors (recenter))))

    (defadvice! doom--dont-prettify-saveplace-cache-a (orig-fn)
      "`save-place-alist-to-file' uses `pp' to prettify the contents of its cache.
  `pp' can be expensive for longer lists, and there's no reason to prettify cache
  files, so we replace calls to `pp' with the much faster `prin1'."
      :around #'save-place-alist-to-file
      (cl-letf (((symbol-function #'pp) #'prin1))
        (funcall orig-fn)))

    (save-place-mode +1))

  ;; undo/redo changes to Emacs' window layout
  (defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
  (autoload 'winner-mode "winner" nil t)
  (add-hook 'deftpunk-init-ui-hook #'winner-mode)

  ;; highlight matching delimiters
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t)
  (add-hook 'deftpunk-init-ui-hook #'show-paren-mode)

  ;; More reliable inter-window border
  ;; The native border "consumes" a pixel of the fringe on righter-most splits,
  ;; `window-divider' does not. Available since Emacs 25.1.
  (setq-default window-divider-default-places t
                window-divider-default-bottom-width 0
                window-divider-default-right-width 1)
  (add-hook 'deftpunk-init-ui-hook #'window-divider-mode)

  ;;
  ;;; Fontage

  ;; I use Roboto Mono and also use Symbola as a fallback to display unicode characters.
  ;; https://github.com/ryanoasis/nerd-fonts
  ;; http://www.fontspace.com/unicode-fonts-for-ancient-scripts/symbola
  (set-frame-font "RobotoMono Nerd Font 13")
  (when (member "Symbola" (font-family-list))
    (set-fontset-font t 'unicode "Symbola" nil 'prepend))

  ;; Underline looks a bit better when drawn lower
  (setq x-underline-at-descent-line t)

  ;;
  ;;; Start the server
  (use-package server
	       :when (display-graphic-p)
	       :defer 1
	       :init
	       (when-let (name (getenv "EMACS_SERVER_NAME"))
			 (setq server-name name))
	       :config
	       (unless (server-running-p)
		 (server-start)))

  ;;
  ;;; Run hooks
  (run-hooks 'deftpunk-init-ui-hook)

  ;;
  ;;; Tangle our literate config.
  ;;; Our tangle functions are faster than org-babel-load-file.
  (add-hook 'after-save-hook (apply-partially #'my-tangle-config-org-hook-func deftpunk--org-configuration-file deftpunk--org-elfile ))
  (when (or (not (file-exists-p deftpunk--org-elfile))
            (file-newer-than-file-p deftpunk--org-configuration-file deftpunk--org-elfile))
    (my-tangle-config-org deftpunk--org-configuration-file deftpunk--org-elfile))
  (load-file deftpunk--org-elfile)

  ;;
  ;;; Finalize the setup.
  (defun deftpunk/finalize ()
    "Set the file-name-handler-alist and gc-cons-threshold back."
    ;; If you forget to reset this, you'll get stuttering and random freezes!
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1)
    (delete-dups auto-mode-alist) ; cleanup auto-mode-alist if necessary
    t)

  (add-hook 'emacs-startup-hook #'deftpunk/finalize)

  ) ; end of the eval-and-compile
