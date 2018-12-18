;;;; init.el -*- lexical-binding: t; -*-

;;;
;;;  _______  ___      _______  __   __
;;; |       ||   |    |       ||  |_|  |
;;; |    ___||   |    |    ___||       |
;;; |   |___ |   |    |   |___ |       |
;;; |    ___||   |___ |    ___| |     |
;;; |   |    |       ||   |___ |   _   |
;;; |___|    |_______||_______||__| |__|
;;;
;;;             Flex Emacs
;;;
;;; The urban dictionary usage of the word "flex" - reflexively
;;; saying things about oneself in order to look good to others :)
;;;
;;; Author: Erick Bodine erick.bodine@gmail.com

;;; I have tried out a couple of meta-configurations - doom-emacs, spacemacs.
;;; I have tried an all Orgmode configuration.
;;; I have tried out a mixed doom-emacs/Orgmode configuration.

;;; Without this comment Emacs adds (package-initialize) here...annoying.
;;; (package-initialize)

;; load core/core.el - the monster that starts it all.
(require 'core (concat user-emacs-directory "core/core"))

;; Start the server
(require 'server)
(unless (server-running-p) (server-start))
