;;; t-magit.el --- Magit support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :commands magit-status
  :bind (("C-x g" . magit-status))
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "magit: ")
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . right)
                 (reusable-frames . visible)
                 (window-width    . 0.50))))

(provide 't-magit)
;;; t-magit.el ends here
