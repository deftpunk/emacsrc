      ;;;;                                                          ;;;;
     ;;;;                                                            ;;;;
    ;;;;                                                              ;;;;
    ;;;;         deftmacs --- Deftmacs Initialization file.           ;;;;
    ;;;;                                                              ;;;;
    ;;;; Without this comment emacs25 adds (package-initialize) here. ;;;;
    ;;;; (package-initialize)                                         ;;;;
    ;;;;                                                              ;;;;
     ;;;;                                                            ;;;;
      ;;;;                                                          ;;;;


;; Server
(require 'server)
(unless (server-running-p) (server-start))
