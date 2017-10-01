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


;; Start messing with the gc-cons-threshold & file-name-handler-alist to
;; optimize the startup of Emacs.
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(let ((gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil))
  (load "~/.emacs.d/config.el"))

