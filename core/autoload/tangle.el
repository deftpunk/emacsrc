;;; tangle.el -*- lexical-binding: t; -*-

;; The following functions for tangling/untangling Org files and ignoring CANCELED sections within the
;; Org files comes from http://www.holgerschurig.de/en/emacs-efficiently-untangling-elisp/
;; They are GPLv2, you can find license details here:
;; 	http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html
;; I have modified some of these functions.  Since this is on Github and publicly accessible I should be
;; okay as far as the license goes.  As I understand it.

;;;###autoload
(defun my-tangle-section-canceled ()
    "Return t if the current section header was CANCELED, else nil."
      (save-excursion
            (if (re-search-backward "^\\*+\\s-+\\(.*?\\)?\\s-*$" nil t)
                      (string-prefix-p "CANCELED" (match-string 1))
                            nil)))

;;;###autoload
(defun my-tangle-config-org (orgfile elfile)
     "This function will write all source blocks from =config.org= into
   =config.el= that are ...

   - not marked as :tangle no
   - have a source-code of =emacs-lisp=
   - don't have the todo-marker CANCELED"
     (let* ((body-list ())
            (org-babel-src-block-regexp   (concat
                                            ;; (1) indentation                (2) lang
                                            ; "^\\([ \t]*\\)#\\+begin_src [ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*] "
                                            "^\\([ \t]*\\)#\\+begin_src\s+\\([^ \f\t\n\r\v]+\\) "
                                            ;; (3) switches
                                            "\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)"
                                            ;; (4) header arguments
                                            "\\([^\n]*\\)\n"
                                            ;; (5) body
                                            "\\([^\000]*?\n\\)??[ \t]*#\\+end_src")))
       ;; (message "Parsing orgfile :> %s" orgfile)
       (with-temp-buffer
         (insert-file-contents orgfile)
         (goto-char (point-min))
         (while (re-search-forward org-babel-src-block-regexp nil t)
                (let ((lang (match-string 2))
                      (args (match-string 4))
                      (switches (match-string 3))
                      (body (match-string 5)))
                      ;;(canc (my-tangle-section-canceled)))
		  ;;(message "switches:> %s" switches)
                  ; (message "blah:> %s" body)
                  (when (and (string= lang "emacs-lisp")
                             (not (string-match-p ":tangle\\s-+no" args)))
                             ;;(not canc))
                    (add-to-list 'body-list body)))))
       (with-temp-file elfile
                       (insert (format ";; Don't edit this file, edit %s instead ...\n\n" orgfile))
                       (apply 'insert (reverse body-list)))
                   (message "Wrote elisp file %s ...'" elfile)))

;;;###autoload
 (defun my-tangle-config-org-hook-func (orgfile elfile)
   " Create a new elisp configuration file whenever we edit & save the Org configuration file. "
     (when (string= "config.org" (buffer-name))
             (my-tangle-config-org orgfile elfile)))
