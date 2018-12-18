;;; core-os.el -*- lexical-binding: t; -*-
;;;
;;; This is a direct copy of the core/core-os.el file in doom-emacs, many
;;; thanks to Henrik - henrik@lissner.net

(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))

;; clipboard
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      ;; Use a shared clipboard
      select-enable-clipboard t
      select-enable-primary t)

;; paste-to-osx & copy-from-osx are autoloads from autoload/os.el - they make
;; use of pbpaste and pbcopy under the hood.
(setq interprogram-cut-function 'paste-to-osx
      interprogram-paste-function 'copy-from-osx)

(after! evil
  ;; stop copying each visual state move to the clipboard:
  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; Most of this code grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (advice-add #'evil-visual-update-x-selection :override #'ignore))

(cond (IS-MAC
       (setq
             ;; sane trackpad/mouse scroll settings
             mac-redisplay-dont-reset-vscroll t
             mac-mouse-wheel-smooth-scroll nil
             mouse-wheel-scroll-amount '(5 ((shift) . 2))  ; one line at a time
             mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
             ;; Curse Lion and its sudden but inevitable fullscreen mode!
             ;; NOTE Meaningless to railwaycat's emacs-mac build
             ns-use-native-fullscreen nil
             ;; Don't open files from the workspace in a new frame
             ns-pop-up-frames nil)

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
                (setq exec-path-from-shell-variables '("PATH" "GOPATH" "GOBIN" "GOROOT" "PYTHONPATH"))
                (exec-path-from-shell-initialize))
              (use-package osx-clipboard
                :commands (osx-clipboard-mode)
                :init
                (osx-clipboard-mode +1)))))
       )

(provide 'core-os)
;;; core-os.el ends here
