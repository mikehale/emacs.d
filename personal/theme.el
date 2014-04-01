(prelude-require-packages '(obsidian-theme
                            powerline))

(disable-theme 'zenburn)
(powerline-default-theme)
(setq visible-bell nil)

(when window-system
  (set-frame-size (selected-frame) 130 30)
  (set-face-attribute 'default nil :family "Anonymous Pro")
  (set-face-attribute 'default nil :height 180)
  (add-hook 'after-init-hook (lambda()(load-theme 'obsidian +1))))
