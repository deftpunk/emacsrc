;; A template for a .dir-locals.el file.
((python-mode
  .
  ((eval
    .
    (progn
      (conda-env-activate "<path to env>")
      (setq python-indent-offset 4)
      (setq lsp-pyright-extra-paths [""]))))))
