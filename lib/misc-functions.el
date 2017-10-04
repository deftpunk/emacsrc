
;; Took the curx-duplicate-line-or-region function and modified it to just copy
;; the current line.
(defun deftmacs/copy-current-line-or-region (arg)
  "Duplicates the current line or region "
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (crux-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (kill-new region)))
