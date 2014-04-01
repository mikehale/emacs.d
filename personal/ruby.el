(prelude-require-packages '(robe
                            rbenv
                            bundler))

(global-rbenv-mode)
(add-hook 'ruby-mode-hook
          (lambda()(highlight-indentation-current-column-mode +1)))
