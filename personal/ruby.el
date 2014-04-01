(prelude-require-packages '(robe
                            rbenv
                            bundler))

(global-rbenv-mode -1)
(rbenv-use-global)
(setq ruby-deep-indent-paren nil)
(add-hook 'ruby-mode-hook
          (lambda()(highlight-indentation-current-column-mode +1)))
