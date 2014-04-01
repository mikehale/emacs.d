;;; init.el --- initial customization

;;; Code:

(prelude-require-packages '(ag
                            dired-rainbow
                            rbenv
                            obsidian-theme
                            rainbow-delimiters
                            desktop
                            powerline
                            gitconfig-mode
                            bundler
                            ))

(defun kill-current-buffer () (interactive) (kill-buffer (buffer-name)))

(setq auto-mode-alist (cons '("zprofile" . shell-script-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("zshrc" . shell-script-mode) auto-mode-alist))

(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

(global-rbenv-mode)

(setq visible-bell nil)

(add-hook 'ruby-mode-hook 'highlight-indentation-current-column-mode)
