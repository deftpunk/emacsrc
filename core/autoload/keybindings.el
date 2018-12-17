;;; core/autoload/ui.el -*- lexical-binding: t; -*-

;; https://emacs.stackexchange.com/questions/278/how-do-i-display-line-numbers-in-emacs-not-in-the-mode-line

;;;###autoload
(defun goto-line-with-feedback ()   "Show line numbers temporarily, while prompting for the line number input"   (interactive)   (unwind-protect
      (progn
        (display-line-numbers 1)
        (goto-line (read-number "Goto line: ")))
    (display-line-numbers -1)))

;;;###autoload
(defun evil-open-above-no-insert (count)
  "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-insert-newline-above)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (when evil-auto-indent
    (indent-according-to-mode))
  (evil-previous-line count))

;;;###autoload
(defun evil-open-below-no-insert (count)
  "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-insert-newline-below)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (when evil-auto-indent
    (indent-according-to-mode))
  (evil-previous-line count))

;;;###autoload
(defun vim-return ()
  "Still want Return to do the right thing in dired mode."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (dired-find-file)
    (progn
      (next-line)
      (back-to-indentation-or-beginning))))

;; Borrowed from Sacha Chau who borrowed it from Steve Purcell - change
;; =C-x C-e= to evaluate regions as well as last sexp.
;;;###autoload
(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))
