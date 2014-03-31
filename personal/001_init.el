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

(powerline-default-theme)
(add-hook 'ruby-mode-hook 'highlight-indentation-current-column-mode)

(require 'auto-complete-config)
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'ruby-mode)
(add-to-list 'ac-modes 'web-mode)

(ac-set-trigger-key "TAB")

;; Add mouse support (helpful for resizing panes)
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
