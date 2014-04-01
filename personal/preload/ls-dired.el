(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program t)
  (setq insert-directory-program "gls"))
