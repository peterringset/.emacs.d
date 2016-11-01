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

(defun load-config (f)
  (load-file (concat +emacs-conf-dir+ "/" f ".el")))

(setq-default indent-tabs-mode nil)
(setq-default line-spacing 5)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(setq shell-file-name "bash")
(setq use-package-always-ensure t)
(setq x-select-enable-clipboard t)

(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-hl-line-mode 1)
(global-linum-mode 1)

;; --------------------------------------

(use-package spacemacs-theme
  :init
  (progn
    (load-theme 'spacemacs-dark t)))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package org
  :defer t)

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
 '(column-number-mode t)
 '(magit-commit-arguments (quote ("--gpg-sign=999ABCF36AE3B637")))
 '(package-selected-packages
   (quote
    (org yasnippet helm-ls-git helm web-mode sublime-themes spacemacs-theme spacegray-theme neotree markdown-mode magit less-css-mode jsx-mode js3-mode js2-mode elm-mode dash-functional)))
 '(scroll-bar-mode nil)
 '(standard-indent 2)
 '(tool-bar-mode nil))

(custom-set-faces
 '(default ((t (:height 120 :family "Menlo")))))
