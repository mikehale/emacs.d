(prelude-require-packages '(robe
                            rbenv
                            bundler))

(global-rbenv-mode -1)
(rbenv-use-global)
(setq ruby-deep-indent-paren nil)
(require 'smartparens-ruby)

(defun my-ruby-mode-defaults()
  (highlight-indentation-current-column-mode +1)
  (smartparens-strict-mode))

(add-hook 'ruby-mode-hook 'my-ruby-mode-defaults)

(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))
