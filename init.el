;; Server management

(load "server")
(unless (server-running-p) (server-start))

;; Ensure console and GUI emacs use the PATH and exec-path defined by the shell
;;

(if window-system
    (let ((path (shell-command-to-string "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
      (setenv "PATH" path)
      (setq exec-path (split-string path path-separator))))

;; Add mouse support (helpful for resizing panes)
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))

;; GUI Frame settings
;;

(if window-system
    (set-frame-size (selected-frame) 200 30))


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
                      flymake-shell
                      obsidian-theme
                      desktop
                      markdown-mode
                      ruby-electric
                      heroku
                      gist
                      rspec-mode
                      ruby-interpolation
                      gitconfig-mode
                      go-mode
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Set theme when not in console

(if window-system
    (load-theme 'obsidian t))

;; (add-to-list 'load-path "~/.emacs.d/vendor")

;; Line numbers
;;

(global-linum-mode 0) ;; 0 disable, 1 enable

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


;; ;; ;; thanks to http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
;; ;; (defun duplicate-line-or-region (&optional n)
;; ;;   "Duplicate current line, or region if active. With argument N, make N copies. With negative N, comment out original line and use the absolute value."
;; ;;   (interactive "*p")
;; ;;   (let ((use-region (use-region-p)))
;; ;;     (save-excursion
;; ;;       (let ((text (if use-region ;Get region if active, otherwise line
;; ;;                       (buffer-substring (region-beginning) (region-end))
;; ;;                     (prog1 (thing-at-point 'line)
;; ;;                       (end-of-line)
;; ;;                       (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
;; ;;                           (newline))))))
;; ;;         (dotimes (i (abs (or n 1))) ;Insert N times, or once if not specified
;; ;;           (insert text))))
;; ;;     (if use-region nil ;Only if we're working with a line (not a region)
;; ;;       (let ((pos (- (point) (line-beginning-position)))) ;Save column
;; ;;         (if (> 0 n) ;Comment out original with negative arg
;; ;;             (comment-region (line-beginning-position) (line-end-position)))
;; ;;         (forward-line 1)
;; ;;         (forward-char pos)))))

;; ;; (defun move-line-up ()
;; ;;   (interactive)
;; ;;   (transpose-lines 1)
;; ;;   (previous-line 2))

;; ;; (defun move-line-down ()
;; ;;   (interactive)
;; ;;   (next-line 1)
;; ;;   (transpose-lines 1)
;; ;;   (previous-line 1))

;; Spelling
;;

(require 'flyspell)
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;; Keybindings
;;

;; ;; Useful urls for configuring iIterm to send correct xterm key codes:
;; ;;   http://offbytwo.com/2012/01/15/emacs-plus-paredit-under-terminal.html
;; ;;   https://github.com/emacsmirror/emacs/blob/master/lisp/term/xterm.el

;; ;; (global-set-key (kbd "C-S-<up>") 'move-line-up)
;; ;; (global-set-key (kbd "C-S-<down>") 'move-line-down)
;; ;; (global-set-key (kbd "C-S-<left>") 'previous-buffer)
;; ;; (global-set-key (kbd "C-S-<right>") 'next-buffer)
;; (global-set-key (kbd "C-c d") 'duplicate-line-or-region)

(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

;; ;; Enable shell script mode for zsh scripts
;; ;;

(setq auto-mode-alist (cons '("zprofile" . shell-script-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("zshrc" . shell-script-mode) auto-mode-alist))

;; Enable paredit for ruby
;;

(add-hook 'ruby-mode-hook 'esk-paredit-nonlisp)

;; ;; Enable ruby-block mode
;; ;;

;; (require 'ruby-block)
;; (ruby-block-mode t)
;; (setq ruby-block-highlight-toggle t)

;; ;; Enable on the fly syntax highlighting for ruby
;; ;;

;; (require 'flymake-ruby)
;; (add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; ;; Ensure colors are handled properly in inf-ruby
;; ;;

;; (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)

;; ;; Enable ruby-electric-mode in ruby-mode
;; ;;

;; (require 'ruby-electric)
;; (add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))

;; ;; Enable markdown-mode
;; ;;

(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; Stop asking whether to save newly added abbrev when quitting emacs
;;

(setq save-abbrevs nil)

;; (when (eq system-type 'darwin)
;;   (require 'ls-lisp)
;;     (setq ls-lisp-use-insert-directory-program nil))

(defun new-notes-buffer ()
  "Create and switch to a new notes buffer. Name based on current timestamp."
  (interactive)
  (switch-to-buffer
   (find-file-noselect
    (expand-file-name
     (concat
      "~/Documents/notes/notes-"
      (format-time-string "%Y-%m-%d-%H-%M-%S")
      ".txt")))))

(global-set-key (kbd "M-n") 'new-notes-buffer)

(setq interprogram-cut-function
      (lambda (text &optional push)
        (let* ((process-connection-type nil)
               (pbproxy (start-process "pbcopy" "pbcopy" "/usr/local/bin/pbcopy")))
          (process-send-string pbproxy text)
          (process-send-eof pbproxy))))

