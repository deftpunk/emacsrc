;;; core/autoload/ui.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom/toggle-fullscreen ()
  "Toggle fullscreen Emacs (non-native on MacOS)."
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (unless (frame-parameter nil 'fullscreen)
     'fullboth)))

;;;###autoload
(defun doom/toggle-line-numbers (&optional arg)
  "Toggle `linum-mode'."
  (interactive "P")
  (cond ((boundp 'display-line-numbers)
         (setq display-line-numbers
               (pcase arg
                 ('(4) 'relative)
                 (1 t)
                 (-1 nil)
                 (_ (not display-line-numbers)))))
        ((featurep 'nlinum)
         (nlinum-mode (or arg (if nlinum-mode -1 +1))))
        (t
         (error "No line number plugin detected"))))

;;;###autoload
(defun flex/delete-frame ()
  "Delete the current frame."
  (interactive)
  (if (cdr (frame-list))
    (save-buffers-kill-emacs)))

;;;###autoload
(defun toggle-auto-highlight-symbol ()
  "Toggle the auto-highlight-symbol-mode"
  (interactive)
  (if auto-highlight-symbol-mode
      (auto-highlight-symbol-mode -1)
    (auto-highlight-symbol-mode)))
