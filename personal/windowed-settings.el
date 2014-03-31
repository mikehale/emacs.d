(defun my-theme-init ()
  (load-theme 'obsidian t))

(when window-system
  (set-frame-size (selected-frame) 130 30)
  (set-face-attribute 'default nil :family "Anonymous Pro")
  (set-face-attribute 'default nil :height 180)
  (add-hook 'after-init-hook 'my-theme-init)
  )
