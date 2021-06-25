;;; pr.el --- pr.el configuration -*- lexical-binding: t -*-

;;; Commentary:
;; The pr.el configuration.

;;; Code:

;; Set custom background color
(custom-set-variables)
(custom-set-faces
 '(default ((t (:background "#1E1E1E")))))

;; Set custom frame size
(set-frame-size (selected-frame) 150 50)

;; Remove confirm exit hook
(remove-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Dumme ape! Quitting already??? ")))

;;; pr.el ends here
