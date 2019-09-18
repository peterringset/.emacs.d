;;; t-editor.el --- Editor -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package doom-themes
  :defer t
  :config
  (progn
    (setq doom-themes-enable-bold t         ; if nil, bolding are universally disabled
          doom-themes-enable-italic t)      ; if nil, italics are universally disabled
    (doom-themes-visual-bell-config))
  :init
  (load-theme 'doom-vibrant t))

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode)
      :config
      (setq doom-modeline-lsp t))

(use-package all-the-icons
  :defer t)

(use-package neotree
  :commands (neotree-toggle
             neotree-show
             neotree-hide
             neotree-find)
  :bind (([f5] . neotree-toggle))
  :init
  (setq neo-window-width 35
        neo-smart-open nil
        neo-window-fixed-size nil
        neo-create-file-auto-open t
        neo-show-updir-line nil
        neo-dont-be-alone t
        neo-show-hidden-files t
        neo-auto-indent-point t)
  (when (eq system-type 'darwin)
    (setq neo-theme 'icons))
)

(use-package editorconfig
  :diminish editorconfig-mode
  :ensure t
  :config
  (editorconfig-mode 1))

(provide 't-editor)
;;; t-editor.el ends here
