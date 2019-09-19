;;; lang-swift.el --- JSON support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Library/Developer/Toolchains/swift-latest.xctoolchain")
  (setq lsp-sourcekit-executable (expand-file-name "/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin/sourcekit-lsp")))

(use-package swift-mode
  :hook (swift-mode . (lambda () (lsp))))

(provide 'lang-swift)
;;; lang-swift.el ends here
