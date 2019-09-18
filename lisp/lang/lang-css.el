;;; lang-css.el --- JSON support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package less-css-mode
  :mode "\\.less$"
  :config
  (add-hook 'less-css-mode-hook (lambda ()
                                  (setq css-indent-offset 2))))

(provide 'lang-css)
;;; lang-css.el ends here
