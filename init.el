;; Ensure console and GUI emacs use the PATH and exec-path defined by the shell
;;

(if window-system
    (let ((path (shell-command-to-string "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
      (setenv "PATH" path)
      (setq exec-path (split-string path path-separator))))

;; GUI Frame settings
;;

(if window-system
    (set-frame-size (selected-frame) 200 30))

;; Server management
;;

(load "server")
(unless (server-running-p) (server-start))

;; Packages
;;

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      starter-kit
                      starter-kit-bindings
                      starter-kit-js
                      starter-kit-lisp
                      starter-kit-ruby
                      starter-kit-eshell
                      ruby-block
                      flymake
                      flymake-easy
                      flymake-ruby
                      zenburn-theme
                      desktop
                      markdown-mode
                      ruby-electric
                      heroku
                      gist
                      yasnippet-bundle
                      rspec-mode
                      ruby-interpolation
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/vendor")

;; Snippets
;;

(require 'yasnippet)
(yas/initialize)
(setq yas/root-directory "~/.emacs.d/vendor/yasnippets")
(yas/load-directory yas/root-directory)

;; Yasnippet workaround for ruby-electric-mode
;; See: http://code.google.com/p/yasnippet/issues/detail?id=71
(defun yas/advise-indent-function (function-symbol)
  (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
           ,(format
             "Try to expand a snippet before point, then call `%s' as usual"
             function-symbol)
           (let ((yas/fallback-behavior nil))
             (unless (and (called-interactively-p 'any)
                          (yas/expand))
               ad-do-it)))))
(yas/advise-indent-function 'ruby-indent-line)

;; Theme
;;

(load-theme 'zenburn t)
;; (load-theme 'manoj-dark t)

;; Line numbers
;;

(require 'linum+)
(global-linum-mode 1)

;; Auto-save desktop
;;

(desktop-save-mode 1)
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)

;; Functions
;;

(defun kill-current-buffer () (interactive) (kill-buffer (buffer-name)))


(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active. With argument N, make N copies. With negative N, comment out original line and use the absolute value."
  ; thanks to http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1))) ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n) ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

;; Spelling
;;

(require 'flyspell)
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;; Keybindings
;;

(global-set-key (kbd "C-c d") 'duplicate-line-or-region)
(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

;; Enable paredit for ruby
;;

(add-hook 'ruby-mode-hook 'esk-paredit-nonlisp)

;; Enable ruby-block mode
;;

(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; Enable on the fly syntax highlighting for ruby
;;

(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; Ensure colors are handled properly in inf-ruby
;;

(add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)

;; Enable ruby-electric-mode in ruby-mode
;;

(require 'ruby-electric)
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))

;; Enable markdown-mode
;;

(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; Stop asking whether to save newly added abbrev when quitting emacs
;;

(setq save-abbrevs nil)
