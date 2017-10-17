

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

(defun vim-return ()
  "Still want Return to do the right thing in dired mode."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (dired-find-file)
    (progn
      (next-line)
      (back-to-indentation-or-beginning))))

(provide 'evil-functions)
