                 ;;;;                                                          ;;;;
                ;;;;                                                            ;;;;
               ;;;;                                                              ;;;;
              ;;;;;         deftmacs --- Deftmacs Initialization file.           ;;;;;
             ;;;;;;                                                              ;;;;;;
             ;;;;;; Without this comment emacs25 adds (package-initialize) here. ;;;;;;
              ;;;;; (package-initialize)                                         ;;;;;
               ;;;;                                                              ;;;;
                ;;;;                                                            ;;;;
                 ;;;;                                                          ;;;;


(defconst org-configuation-file
	  (expand-file-name "deftmacs.org" user-emacs-directory)
	  "The Org mode configuration file.")

(defconst elisp-configuration-file
	  (expand-file-name "config.el" user-emacs-directory)
	  "The generated elisp configuration file.")

;; The following functions for tangling/untangling Org files and ignoring CANCELED sections within the
;; Org files comes from http://www.holgerschurig.de/en/emacs-efficiently-untangling-elisp/
;; They are GPLv2, you can find license details here:
;; 	http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html
;; I have modified some of these functions.  Since this is on Github and publicly accessible I should be
;; okay as far as the license goes.  As I understand it.
(defun my-tangle-section-canceled ()
    "Return t if the current section header was CANCELED, else nil."
      (save-excursion
            (if (re-search-backward "^\\*+\\s-+\\(.*?\\)?\\s-*$" nil t)
                      (string-prefix-p "CANCELED" (match-string 1))
                            nil)))

 (defun my-tangle-config-org (orgfile elfile)
     "This function will write all source blocks from =config.org= into
   =config.el= that are ...

   - not marked as :tangle no
   - have a source-code of =emacs-lisp=
   - don't have the todo-marker CANCELED"
     (let* ((body-list ())
            (gc-cons-threshold most-positive-fixnum)
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
       (message "Parsing orgfile :> %s" orgfile)
       (with-temp-buffer
         (insert-file-contents orgfile)
         (goto-char (point-min))
         (while (re-search-forward org-babel-src-block-regexp nil t)
                (let ((lang (match-string 2))
                      (args (match-string 4))
                      (body (match-string 5))
                      (canc (my-tangle-section-canceled)))
                  ; (message "blah:> %s" body)
                  (when (and (string= lang "emacs-lisp")
                             (not (string-match-p ":tangle\\s-+no" args))
                             (not canc))
                    (add-to-list 'body-list body)))))
       (with-temp-file elfile
                       (insert (format ";; Don't edit this file, edit %s instead ...\n\n" orgfile))
                       (apply 'insert (reverse body-list)))
                   (message "Wrote elisp file %s ...'" elfile)))

 (defun my-tangle-config-org-hook-func ()
   " Create a new elisp configuration file whenever we edit & save the Org configuration file. "
     (when (string= "config.org" (buffer-name))
           (let ((orgfile org-configuation-file)
		 (elfile elisp-configuration-file))
                   (my-tangle-config-org orgfile elfile))))
 (add-hook 'after-save-hook #'my-tangle-config-org-hook-func)

;; Change the gc-cons-threshold & file-name-handler-alist to optimize the startup of Emacs.  Then load
;; the generated elisp configuration file.
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(let ((gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil)
      (orgfile org-configuation-file)
      (elfile elisp-configuration-file))
  (message "Deftmacs Org mode configuration file: %s" orgfile)
  (message "Deftmacs generated elisp configuration file: %s" elfile)
  (when (or (not (file-exists-p elfile))
	    (file-newer-than-file-p orgfile elfile))
    (my-tangle-config-org orgfile elfile))
  (load-file elfile))

;; Start the server
(require 'server)
(unless (server-running-p) (server-start))

;; Call emacs-init-time from M-x to get a general idea of how long the initialization took.
