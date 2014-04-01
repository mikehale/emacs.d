;;; init.el --- initial customization

;;; Code:

(prelude-require-packages '(ag
                            dired-rainbow
                            rainbow-delimiters
                            dired-hacks-utils
                            gitconfig-mode
                            git-commit-mode
                            git-rebase-mode
                            gh
                            highlight-indentation
                            markdown-mode
                            ))

(defun kill-current-buffer () (interactive) (kill-buffer (buffer-name)))

(setq auto-mode-alist (cons '("zprofile" . shell-script-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("zshrc" . shell-script-mode) auto-mode-alist))

(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

(setq prelude-guru nil)
(setq flx-ido-threshhold 1000)
(global-set-key [remap move-beginning-of-line]
                'move-beginning-of-line)

(provide '001_init)
;;; 001_init.el ends here
