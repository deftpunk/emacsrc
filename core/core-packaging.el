;;; core-packaging.el --- package management -*- lexical-binding: t; -*-
;;;
;;; More copying/emulating of doom-emacs.
;;; While Doom comes up with its own package management system, I stick with
;;; just vanilla 'use-package' and 'paradox'.

(defvar flex-init-p nil
  "Non-nil if doom is done initializing (once `doom-post-init-hook' is done). If
this is nil after Emacs has started something is wrong.")

(defvar flex--site-load-path load-path
  "The load path to Emacs libraries.")

(defvar flex--base-load-path
  (append (list flex-core-dir flex-packages-dir)
          flex--site-load-path)
  "A backup of `load-path' before it was altered by `flex-initialize'. Used as a
base by `flex!' and for calculating how many packages exist.")

(defvar flex-core-packages
  '(use-package paradox)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(defvar flex-reload-hook nil
  "A list of hooks to run when `flex/reload-load-path' is called.")

(defvar flex--refreshed-p nil)

(setq package--init-file-ensured t
      package-user-dir (expand-file-name "elpa" flex-emacs-dir)
      package-enable-at-startup nil
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))

      ;; security settings
      gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")

      use-package-verbose flex-debug-mode
      use-package-always-ensure t
      use-package-minimum-reported-time (if flex-debug-mode -1 0.1)

      byte-compile-dynamic nil
      byte-compile-verbose flex-debug-mode
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(defun flex-initialize (&optional force-p)
  "Initialize installed packages (using package.el) and ensure the core packages
are installed.

If you byte-compile core/core.el, this function will be avoided to speed up
startup."
  ;; Called early during initialization; only use native (and cl-lib) functions!
  (when (or force-p (not flex-init-p))
    ;; Speed things up with a `load-path' for only the bare essentials
    (let ((load-path flex--base-load-path))
      ;; Ensure core folders exist, otherwise we get errors
      (dolist (dir (list flex-local-dir flex-etc-dir flex-cache-dir))
        (unless (file-directory-p dir)
          (make-directory dir t)))
      ;; Ensure package.el is initialized; we use its state
      (setq package-activated-list nil)
      (condition-case _ (package-initialize t)
        ('error (package-refresh-contents)
                (setq flex--refreshed-p t)
                (package-initialize t)))
      ;; Ensure core packages are installed
      (let ((core-packages (cl-remove-if #'package-installed-p flex-core-packages)))
        (when core-packages
          (unless flex--refreshed-p
            (package-refresh-contents))
          (dolist (package core-packages)
            (let ((inhibit-message t))
              (package-install package))
            (if (package-installed-p package)
                (message "✓ Installed %s" package)
              (error "✕ Couldn't install %s" package)))
          (message "Installing core packages...done")))
      (setq flex-init-p t))))

(defun flex-initialize-autoloads ()
  "Ensures that `flex-autoload-file' exists and is loaded. Otherwise run
`flex/reload-autoloads' to generate it."
  (unless (file-exists-p flex-autoload-file)
    (flex//reload-autoloads)))

(defun flex-packages--read-if-cookies (file)
  "Returns the value of the ;;;###if predicate form in FILE."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 256)
    (if (and (re-search-forward "^;;;###if " nil t)
             (<= (line-number-at-pos) 3))
        (let ((load-file-name file))
          (eval (sexp-at-point)))
      t)))

(defun flex//reload-autoloads ()
  "Refreshes the autoloads.el file, specified by `flex-autoload-file'.

It scans and reads core/autoload/*.el, modules/*/*/autoload.el and
modules/*/*/autoload/*.el, and generates an autoloads file at the path specified
by `flex-autoload-file'. This file tells Emacs where to find lazy-loaded
functions.

This should be run whenever init.el or an autoload file is modified. Running
'make autoloads' from the commandline executes this command."
  (interactive)
  (message "in reload-autoloads")
  ;; This function must not use autoloaded functions or external dependencies.
  ;; It must assume nothing is set up!
  ;; (if (not noninteractive)
  ;;     ;; This is done in another instance to protect the current session's
  ;;     ;; state. `flex-initialize-packages' will have side effects otherwise.
  ;;     (and (flex-packages--async-run 'flex//reload-autoloads)
  ;;          (load flex-autoload-file))
  ;;  (doom-initialize-packages t)
    (let ((targets
           (file-expand-wildcards
            (expand-file-name "autoload/*.el" flex-core-dir))))
      (when (file-exists-p flex-autoload-file)
        (delete-file flex-autoload-file)
        (message "Deleted old autoloads.el"))
      (dolist (file (reverse targets))
	(message "found %s" file)
        (message
         (cond ((not (flex-packages--read-if-cookies file))
                "⚠ Ignoring %s")
               ((update-file-autoloads file nil flex-autoload-file)
                "✕ Nothing in %s")
               (t
                "✓ Scanned %s"))
         (file-relative-name file "~/.emacs.d/")))
         ;; (file-relative-name file flex-emacs-dir)))
      (make-directory (file-name-directory flex-autoload-file) t)
      (let ((buf (get-file-buffer flex-autoload-file))
            current-sexp)
        (unwind-protect
            (condition-case-unless-debug ex
                (with-current-buffer buf
                  (save-buffer)
                  (goto-char (point-min))
                  (while (re-search-forward "^(" nil t)
                    (save-excursion
                      (backward-char)
                      (setq current-sexp (read (thing-at-point 'sexp t)))
                      (eval current-sexp t))
                    (forward-char))
                  (message "Finished generating autoloads.el!"))
              ('error
               (delete-file flex-autoload-file)
               (error "Error in autoloads.el: (%s %s ...) %s -- %s"
                      (nth 0 current-sexp)
                      (nth 1 current-sexp)
                      (car ex) (error-message-string ex))))
          (kill-buffer buf)))));)


;; (autoload 'use-package "use-package" nil nil 'macro)

(defmacro load! (filesym &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILESYM is either a symbol or string representing the file to load. PATH is
where to look for the file (a string representing a directory path). If omitted,
the lookup is relative to `load-file-name', `byte-compile-current-file' or
`buffer-file-name' (in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (cl-assert (symbolp filesym) t)
  (let ((path (or path
                  (and load-file-name (file-name-directory load-file-name))
                  (and (bound-and-true-p byte-compile-current-file)
                       (file-name-directory byte-compile-current-file))
                  (and buffer-file-name
                       (file-name-directory buffer-file-name))
                  (error "Could not detect path to look for '%s' in" filesym)))
        (filename (symbol-name filesym)))
    (let ((file (expand-file-name (concat filename ".el") path)))
      (if (file-exists-p file)
          `(load ,(file-name-sans-extension file) ,noerror
                 ,(not flex-debug-mode))
        (unless noerror
          (error "Could not load file '%s' from '%s'" file path))))))

(provide 'core-packaging)
;;; core-packaging.el ends here.
