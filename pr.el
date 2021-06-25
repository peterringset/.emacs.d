;;; pr.el --- pr.el configuration -*- lexical-binding: t -*-

;;; Commentary:
;; The pr.el configuration.

;;; Code:

;; Set custom frame size
(set-frame-size (selected-frame) 150 50)

;; Remove confirm exit hook
(remove-hook 'kill-emacs-query-functions
             (lambda () (y-or-n-p "Dumme ape! Quitting already??? ")))

;; Select theme based on system's appearance
(defun set-system-dark-mode ()
  (interactive)
  (setq darkmode (shell-command-to-string "printf %s \"$( osascript -e \'tell application \"System Events\" to tell appearance preferences to return dark mode\' )\""))
  (if (string= darkmode "true")
      (progn
        (custom-set-variables)
        (custom-set-faces
         '(default ((t (:background "#1E1E1E")))))
        (load-theme 'doom-vibrant t))
    (load-theme 'doom-one-light t))
  )

(set-system-dark-mode)

;;; pr.el ends here
