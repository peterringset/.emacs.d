;;; lang-java.el --- Java support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-java
  :ensure t
  :after lsp-mode
  :config
  (add-hook 'java-mode-hook 'lsp))

(provide 'lang-java)
;;; lang-java.el ends here
