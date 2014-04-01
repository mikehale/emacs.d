;;; notes.el --- notes

;;; Code:

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
