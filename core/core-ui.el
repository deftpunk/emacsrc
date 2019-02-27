;;; core.el -*- lexical-binding: t; -*-

(defvar flex-fringe-size '12
  "Default fringe width - set it to something big enough for line number, flycheck, etc.")

;; Hook(s)
(defvar flex-init-ui-hook nil
  "List of hooks to run when the theme and font is initialized (or reloaded with
`flex//reload-theme').")

;; Frame transparency for Mac
(when IS-MAC
  (set-frame-parameter (selected-frame) 'alpha '(95 95))
  (add-to-list 'default-frame-alist '(alpha 95 95)))

(setq-default
 bidi-display-reordering nil ; disable bidirectional text for tiny performance boost
 blink-matching-paren nil    ; don't blink--too distracting
 cursor-in-non-selected-windows nil  ; hide cursors in other windows
 indicate-buffer-boundaries 'right
 display-line-numbers-width 4
 display-time-default-load-average nil
 frame-inhibit-implied-resize t
 ;; remove continuation arrow on right fringe
 fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                              fringe-indicator-alist)
 highlight-nonselected-windows nil
 image-animate-loop t
 indicate-buffer-boundaries nil
 indicate-empty-lines nil
 max-mini-window-height 0.3
 mode-line-default-help-echo nil ; disable mode-line mouseovers
 mouse-yank-at-point t           ; middle-click paste at point, not at click
 ibuffer-use-other-window t
 resize-mini-windows 'grow-only  ; Minibuffer resizing
 show-help-function nil          ; hide :help-echo text
 split-width-threshold 160       ; favor horizontal splits
 uniquify-buffer-name-style 'forward
 use-dialog-box nil              ; always avoid GUI
 visible-cursor nil
 x-stretch-cursor nil
 ;; defer jit font locking slightly to [try to] improve Emacs performance
 jit-lock-defer-time nil
 jit-lock-stealth-nice 0.1
 jit-lock-stealth-time 0.2
 jit-lock-stealth-verbose nil
;; `pos-tip' defaults
 pos-tip-internal-border-width 6
 pos-tip-border-width 1
 ;; no beeping or blinking please
 ring-bell-function #'ignore
 visible-bell nil)

(fset #'yes-or-no-p #'y-or-n-p) ; y/n instead of yes/no

;; undo/redo changes to Emacs' window layout
(defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
(autoload 'winner-mode "winner" nil t)
(add-hook 'flex-init-ui-hook #'winner-mode)

;; highlight matching delimiters
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(add-hook 'flex-init-ui-hook #'show-paren-mode)

;;; More reliable inter-window border
;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq-default window-divider-default-places t
              window-divider-default-bottom-width 0
              window-divider-default-right-width 1)
(add-hook 'flex-init-ui-hook #'window-divider-mode)

;; prompts the user for confirmation when deleting a non-empty frame
(define-key global-map [remap delete-frame] #'flex/delete-frame)

;; Set the frame title to the full path of the buffer we are working on.
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; auto-enabled in Emacs 25+; I'll do it myself
(global-eldoc-mode -1)
;; a good indicator that Emacs isn't frozen
(add-hook 'flex-post-init-hook #'blink-cursor-mode)
;; standardize default fringe width
(if (fboundp 'fringe-mode) (fringe-mode flex-fringe-size))

;; I use Roboto Mono and also use Symbola as a fallback to display unicode characters.
;; https://github.com/ryanoasis/nerd-fonts
;; http://www.fontspace.com/unicode-fonts-for-ancient-scripts/symbola
(set-frame-font "RobotoMono Nerd Font 12")
(when (member "Symbola" (font-family-list))
	      (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; Highlight the current line.
(global-hl-line-mode 1)

;;;
;;; Packages from the intertubes.
;;;

;; all the icons
;; Make me look pretty.
;; https://github.com/domtronn/all-the-icons.el
;; A utility package to collect various Icon Fonts and propertize them within Emacs.
(use-package all-the-icons)

;; doom modeline
;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :init
  (setq doom-modeline-minor-modes t)
  :hook (after-init . doom-modeline-init))

;; doom themes
;; Make me look really pretty
;; https://github.com/hlissner/emacs-doom-themes
;; (use-package doom-themes
;;   :init
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)
;;   :config
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)

;;   ;; or for treemacs users
;;   (doom-themes-neotree-config)

;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))
;; (load-theme 'doom-molokai t)

;; If I get tired of the doom-themes, kaolin-ocean is a possibility
;; https://github.com/ogdenwebb/emacs-kaolin-themes

(use-package monokai-theme)
(load-theme 'monokai t)

;;;
;;; Highlighting
;;;

;;; beacon
;;; https://github.com/Malabarba/beacon
;;; Never lose your cursor.  Whenever the window scrolls a light will shine
;;; on top of your cursor so you know where it is.
(use-package beacon
  :config
  (beacon-mode 1))

;;; Highlight cl-lib
;;; https://github.com/skeeto/cl-lib-highlight
;;; Syntax highlighting for cl-lib, so that =cl-loop=, =cl-defun=, =cl-defstruct=
;;; and the like get highlighted
  (use-package cl-lib-highlight
    :config
    (cl-lib-highlight-initialize))

;;; Highlight Escape sequences
;;; https://github.com/dgutov/highlight-escape-sequences/blob/master/highlight-escape-sequences.el
(use-package highlight-escape-sequences
  :defer t
  :commands hes-mode
  :init
  (add-hook 'prog-mode-hook 'hes-mode)
  :config
  (put 'hes-escape-backslash-face 'face-alias 'font-lock-builtin-face)
  (put 'hes-escape-sequence-face 'face-alias 'font-lock-builtin-face))

;;; Highlight Numbers
;;; Highlight numeric literals in source code.
;;; https://github.com/Fanael/highlight-numbers
  (use-package highlight-numbers
    :defer t
    :commands highlight-numbers-mode
    :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;;; Highlight Quotes
;;; Highlight Lisp quotes and quoted symbols
;;; https://github.com/Fanael/highlight-quoted
(use-package highlight-quoted
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

;;; Highlight Symbols
;;; Highlight the symbol under point.
;;; https://github.com/gennad/auto-highlight-symbol
(use-package auto-highlight-symbol
  :defer t
  :commands auto-highlight-symbol-mode
  :init
  (setq ahs-case-fold-search nil
        ahs-default-range 'ahs-range-whole-buffer
        ahs-idle-interval 0.25
        ahs-inhibit-face-list nil)
  ;; but a box around the face.
  (custom-set-faces `(ahs-face ((t (:box t)))))
  (custom-set-faces `(ahs-definition-face ((t (:box t)))))
  (custom-set-faces `(ahs-plugin-whole-buffer-face ((t (:box t)))))
  :config
  (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode))

;;; Volatile highlights
;;; https://www.emacswiki.org/emacs/VolatileHighlights
;;; Temporarily highlights changes to the buffer associated with certain commands that
;;; add blocks of text at once. An example is that if you paste (yank) a block of text, it will be
;;; highlighted until you press the next key.
(use-package volatile-highlights
  :commands (vhl/define-extension vhl/install-extension volatile-highlights-mode)
  :defer t
  :config
  (volatile-highlights-mode 1))

;; Support evil-mode
(vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                      'evil-paste-pop 'evil-move)
(vhl/install-extension 'evil)

;; Supporting undo-tree.
(vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
(vhl/install-extension 'undo-tree)

;;; flex-init-ui-hook
;;; Run the UI hook - ideally this should make graphic, terminal & daemon Emacs look the same.
(run-hooks 'flex-init-ui-hook)

(provide 'core-ui)
;;; core-ui.el ends here.
