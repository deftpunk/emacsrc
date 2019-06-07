;;; core/autoload/ui.el -*- lexical-binding: t; -*-

;;;###autoload
(defun flex|dont-kill-scratch-buffer ()
  "Don't kill the scratch buffer."
  (or (not (string= (buffer-name) "*scratch*"))
      (ignore (bury-buffer))))

;;;###autoload
(defun flex*quit-window (orig-fn &optional kill window)
  "Temporary windows often have q bound to `quit-window', which only buries the
contained buffer. I rarely don't want that buffer killed, so...
"
  (message "running flex*quite-window")
  (funcall orig-fn (not kill) window))

;;;###autoload
(defun flex//kill-current-buffer ()
  "What it says."
  (interactive)
  (kill-buffer (current-buffer)))

;; not broken, toggle letter case from
;; http://ergoemacs.org/emacs/modernization_upcase-word.html][xah]] originally.
;; http://stackoverflow.com/questions/18257573/how-to-toggle-letter-cases-in-a-region-in-emacs
;;;###autoload
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
   Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower") ) ) ) )
    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")) )
    ) )

;; Full history of this function is foggy and lost to time.
;; https://stackoverflow.com/questions/234963/re-open-scratch-buffer-in-emacs
;;;###autoload
(defun eme-goto-scratch ()
  "this sends you to the scratch buffer"
  (interactive)
  (let ((eme-scratch-buffer (get-buffer-create "*scratch*")))
    (switch-to-buffer eme-scratch-buffer)
    (lisp-interaction-mode)))

;; Streamline the process for single file edit and commit.
;; From: https://emacs.stackexchange.com/a/20160
;;;###autoload
(defun my-magit-stage-all-and-commit()
  (interactive)
  (magit-stage-modified)
  (magit-commit))
