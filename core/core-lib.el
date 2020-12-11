;;; core-lib.el --- some serious schizel in here -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

;;
;;; Deftpunk functions, macros & variables

(defun flex|dont-kill-scratch-buffer ()
  "Don't kill the scratch buffer."
  (or (not (string= (buffer-name) "*scratch*"))
      (ignore (bury-buffer))))

(defun flex*quit-window (orig-fn &optional kill window)
  "Temporary windows often have q bound to `quit-window', which only buries the
contained buffer. I rarely don't want that buffer killed, so...
"
  (message "running flex*quit-window")
  (funcall orig-fn (not kill) window))

(defun flex//kill-current-buffer ()
  "What it says."
  (interactive)
  (kill-buffer (current-buffer)))

;; My tangle Org functions.
(defun my-tangle-config-org-hook-func (orgfile elfile)
   " Create a new elisp configuration file whenever we edit & save the Org configuration file. "
     (when (string= "config.org" (buffer-name))
             (my-tangle-config-org orgfile elfile)))

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


;;
;;; Doom functions and macros below here.

(defvar-local doom-real-buffer-p nil
  "If non-nil, this buffer should be considered real no matter what. See
`doom-real-buffer-p' for more information.")

(defvar doom-fallback-buffer-name "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist (will create
it if it doesn't exist).")

(defun doom-fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer. See `doom-fallback-buffer-name' to change this."
  (let (buffer-list-update-hook)
    (get-buffer-create doom-fallback-buffer-name)))

(defun doom-temp-buffer-p (buf)
  "Returns non-nil if BUF is temporary."
  (equal (substring (buffer-name buf) 0 1) " "))

(defun doom-real-buffer-p (buffer-or-name)
  "Returns t if BUFFER-OR-NAME is a 'real' buffer.

A real buffer is a useful buffer; a first class citizen in Doom. Real ones
should get special treatment, because we will be spending most of our time in
them. Unreal ones should be low-profile and easy to cast aside, so we can focus
on real ones.

The exact criteria for a real buffer is:

  1. A non-nil value for the buffer-local value of the `doom-real-buffer-p'
     variable OR
  2. Any function in `doom-real-buffer-functions' returns non-nil OR
  3. None of the functions in `doom-unreal-buffer-functions' must return
     non-nil.

If BUFFER-OR-NAME is omitted or nil, the current buffer is tested."
  (or (bufferp buffer-or-name)
      (stringp buffer-or-name)
      (signal 'wrong-type-argument (list '(bufferp stringp) buffer-or-name)))
  (when-let (buf (get-buffer buffer-or-name))
    (when-let (basebuf (buffer-base-buffer buf))
      (setq buf basebuf))
    (and (buffer-live-p buf)
         (not (doom-temp-buffer-p buf))
         (or (buffer-local-value 'doom-real-buffer-p buf)
             (run-hook-with-args-until-success 'doom-real-buffer-functions buf)
             (not (run-hook-with-args-until-success 'doom-unreal-buffer-functions buf))))))

(defun doom-buffer-frame-predicate (buf)
  "To be used as the default frame buffer-predicate parameter. Returns nil if
BUF should be skipped over by functions like `next-buffer' and `other-buffer'."
  (or (doom-real-buffer-p buf)
      (eq buf (doom-fallback-buffer))))

(defun doom-visible-buffers (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (if buffer-list
      (cl-remove-if-not #'get-buffer-window buffer-list)
    (delete-dups (mapcar #'window-buffer (window-list)))))

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun doom--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (doom-enlist (doom-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun doom-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun doom--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (doom-enlist (doom-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun doom--setq-hook-fns (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
                             vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (doom--resolve-hook-forms hooks)
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook
                          (intern (format "doom--setq-%s-for-%s-h"
                                          var mode))))))

;;
;;; Hooks
(defvar doom--transient-counter 0)
(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        ;; Avoid `make-symbol' and `gensym' here because an interned symbol is
        ;; easier to debug in backtraces (and is visible to `describe-function')
        (fn (intern (format "doom--transient-%d-h" (cl-incf doom--transient-counter)))))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (doom-unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append))))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

If N and M = 1, there's no benefit to using this macro over `add-hook'.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be one function, a quoted list
     thereof, a list of `defun's, or body forms (implicitly wrapped in a
     lambda).

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent (lambda (indent-point state)
                    (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (doom--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p
         local-p
         remove-p
         forms)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (let ((first (car-safe (car rest))))
      (cond ((null first)
             (setq func-forms rest))

            ((eq first 'defun)
             (setq func-forms (mapcar #'cadr rest)
                   defn-forms rest))

            ((memq first '(quote function))
             (setq func-forms
                   (if (cdr rest)
                       (mapcar #'doom-unquote rest)
                     (doom-enlist (doom-unquote (car rest))))))

            ((setq func-forms (list `(lambda (&rest _) ,@rest)))))
      (dolist (hook hook-forms)
        (dolist (func func-forms)
          (push (if remove-p
                    `(remove-hook ',hook #',func ,local-p)
                  `(add-hook ',hook #',func ,append-p ,local-p))
                forms)))
      (macroexp-progn
       (append defn-forms
               (if append-p
                   (nreverse forms)
                 forms))))))

(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (doom--setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _)
                       ,(format "%s = %s" var (pp-to-string val))
                       (setq-local ,var ,val))
            collect `(remove-hook ',hook #',fn) ; ensure set order
            collect `(add-hook ',hook #',fn))))

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (doom-enlist ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

If N and M = 1, there's no benefit to using this macro over `add-hook'.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be one function, a quoted list
     thereof, a list of `defun's, or body forms (implicitly wrapped in a
     lambda).

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent (lambda (indent-point state)
                    (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (doom--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p
         local-p
         remove-p
         forms)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (let ((first (car-safe (car rest))))
      (cond ((null first)
             (setq func-forms rest))

            ((eq first 'defun)
             (setq func-forms (mapcar #'cadr rest)
                   defn-forms rest))

            ((memq first '(quote function))
             (setq func-forms
                   (if (cdr rest)
                       (mapcar #'doom-unquote rest)
                     (doom-enlist (doom-unquote (car rest))))))

            ((setq func-forms (list `(lambda (&rest _) ,@rest)))))
      (dolist (hook hook-forms)
        (dolist (func func-forms)
          (push (if remove-p
                    `(remove-hook ',hook #',func ,local-p)
                  `(add-hook ',hook #',func ,append-p ,local-p))
                forms)))
      (macroexp-progn
       (append defn-forms
               (if append-p
                   (nreverse forms)
                 forms))))))

(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

;; 4/22/2020 - In after! below, I removed the check to see if the package was
;; in the list of doom-disabled-packages
(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol or list of them. These are package names, not modes,
functions or variables. It can be:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
  Without :or/:any/:and/:all, :and/:all are implied.

This is a wrapper around `eval-after-load' that:

1. Suppresses warnings for disabled packages at compile-time
2. No-ops for package that are disabled by the user (via `package!')
3. Supports compound package statements (see below)
4. Prevents eager expansion pulling in autoloaded macros all at once"
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              (let ((body (macroexp-progn body)))
                `(if (featurep ',package)
                     ,body
                   ;; We intentionally avoid `with-eval-after-load' to prevent
                   ;; eager macro expansion from pulling (or failing to pull) in
                   ;; autoloaded macros/packages.
                   (eval-after-load ',package ',body))))
    (let ((p (car package)))
      (cond ((not (keywordp p))
             `(after! (:and ,@package) ,@body))
            ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (cdr package))
               (setq body `((after! ,next ,@body))))
             (car body))))))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

If N and M = 1, there's no benefit to using this macro over `add-hook'.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be one function, a quoted list
     thereof, a list of `defun's, or body forms (implicitly wrapped in a
     lambda).

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (doom--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p
         local-p
         remove-p
         forms)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (let ((first (car-safe (car rest))))
      (cond ((null first)
             (setq func-forms rest))

            ((eq first 'defun)
             (setq func-forms (mapcar #'cadr rest)
                   defn-forms rest))

            ((memq first '(quote function))
             (setq func-forms
                   (if (cdr rest)
                       (mapcar #'doom-unquote rest)
                     (doom-enlist (doom-unquote (car rest))))))

            ((setq func-forms (list `(lambda (&rest _) ,@rest)))))
      (dolist (hook hook-forms)
        (dolist (func func-forms)
          (push (if remove-p
                    `(remove-hook ',hook #',func ,local-p)
                  `(add-hook ',hook #',func ,append-p ,local-p))
                forms)))
      (macroexp-progn
       (append defn-forms
               (if append-p
                   (nreverse forms)
                 forms))))))

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.
If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list
         (delq ,(if fetcher
                    `(funcall ,fetcher ,elt ,list)
                  elt)
               ,list)))

(defmacro quiet! (&rest forms)
  "Run FORMS without generating any output.

This silences calls to `message', `load-file', `write-region' and anything that
writes to `standard-output'."
  `(cond (t ,@forms)
         ((noninteractive)
          (let ((old-fn (symbol-function 'write-region)))
            (cl-letf ((standard-output (lambda (&rest _)))
                      ((symbol-function 'load-file) (lambda (file) (load file nil t)))
                      ((symbol-function 'message) (lambda (&rest _)))
                      ((symbol-function 'write-region)
                       (lambda (start end filename &optional append visit lockname mustbenew)
                         (unless visit (setq visit 'no-message))
                         (funcall old-fn start end filename append visit lockname mustbenew))))
              ,@forms)))
         ((let ((inhibit-message t)
                (save-silently t))
            (prog1 ,@forms (message ""))))))

(provide 'core-lib)
;;; core-lib.el ends here
