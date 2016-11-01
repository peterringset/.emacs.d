(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Directories
(defconst +emacs-dir+ "~/.emacs.d")
(defconst +emacs-conf-dir+ (concat +emacs-dir+ "/configs"))
(defconst +emacs-snippets-dir+ (concat +emacs-dir+ "/snippets"))

(add-to-list 'load-path (concat +emacs-dir+ "/includes"))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-hl-line-mode 1)
(global-linum-mode 1)

(defun load-config (f)
  (load-file (concat +emacs-conf-dir+ "/" f ".el")))

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      shell-file-name "bash"
      use-package-always-ensure t
      x-select-enable-clipboard t)

(setq-default indent-tabs-mode nil)
(setq-default line-spacing 5)

(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))

;; --------------------------------------

(use-package spacemacs-theme
  :init
  (progn
    (load-theme 'spacemacs-dark t)))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package helm
  :bind (("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ("M-x" . helm-M-x))
  :diminish helm-mode
  :init
  (require 'helm-config)

  :config
  (helm-mode 1)

  (use-package helm-ls-git
    :bind (("C-x C-d" . helm-browse-project))))

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (setq yas-snippet-dirs '(+emacs-snippets-dir+))

  :config
  (yas-global-mode 1))

(use-package elm-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.elm$" . elm-mode)))

(use-package neotree
  :bind (("C-x p" . neotree-toggle)))

(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.css?\\'"
         "\\.less?\\'"
         "\\.scss?\\'"
         "\\.js?\\'"
         "\\.jsx?\\'")

  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package auto-complete
  :init
  (add-hook 'web-mode-hook 'auto-complete-mode))



;; --------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(magit-commit-arguments (quote ("--gpg-sign=999ABCF36AE3B637")))
 '(package-selected-packages
   (quote
    (tern-auto-complete tern yasnippet helm-ls-git helm web-mode sublime-themes spacemacs-theme spacegray-theme neotree markdown-mode magit less-css-mode jsx-mode js3-mode js2-mode elm-mode dash-functional ac-math ac-html)))
 '(scroll-bar-mode nil)
 '(standard-indent 2)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 120 :family "Menlo")))))
