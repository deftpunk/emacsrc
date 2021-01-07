
;; remove searching for .gz files, as all my packages are uncompressed
;; this halves how many files emacs tries to open (if you have a lot of
;; packages, emacs executes a lot of failing file opens)
(setq jka-compr-load-suffixes nil)
(jka-compr-update)
