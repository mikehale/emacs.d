;;; init.el --- initial customization

;;; Code:

(prelude-require-packages '(ag
                            dired-rainbow
                            rainbow-delimiters
                            gitconfig-mode
                            dired-hacks-utils
                            git-commit-mode
                            git-rebase-mode
                            highlight-indentation
                            markdown-mode
                            gh))

(defun kill-current-buffer () (interactive) (kill-buffer (buffer-name)))

(setq auto-mode-alist (cons '("zprofile" . shell-script-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("zshrc" . shell-script-mode) auto-mode-alist))

(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

(setq visible-bell nil)

(provide '001_init)
;;; 001_init.el ends here
