;;; clipboard.el --- sync clipboard with system

;;; Code:

(when (eq system-type 'darwin)
  (setq interprogram-cut-function
        (lambda (text &optional push)
          (let* ((process-connection-type nil)
                 (pbproxy (start-process "pbcopy" "pbcopy" "/usr/local/bin/pbcopy")))
            (process-send-string pbproxy text)
            (process-send-eof pbproxy)))))
