(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stalbe" . "http://stable.melpa.org/packages/")))
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

(setq inhibit-startup-screen t)
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
(global-linum-mode 0)

;; --------------------------------------

(cond
 ((find-font (font-spec :family "Menlo"))
  (set-frame-font "Menlo:pixelsize=12"))
 ((find-font (font-spec :family "Monaco"))
  (set-fram-font "Monaco:pixelsize=12")))

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
  (custom-set-variables
   '(helm-follow-mode-persistent t))

  (use-package helm-ls-git
    :bind (("C-x C-d" . helm-browse-project)))

  (use-package helm-ag
    :defer t)

  (use-package helm-projectile
    :defer t))

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (setq yas-snippet-dirs '(+emacs-snippets-dir+))

  :config
  (yas-global-mode 1))

(use-package elm-mode
  :mode "\\.elm$")

(use-package neotree
  :bind (("C-x p" . neotree-toggle)))

(use-package erlang
  :mode "\\.erl$")

(use-package elixir-mode
  :mode "\\.\\(ex[s]\\|elixir\\)$")

(use-package ruby-end
  :diminish ruby-end-mode
  :init
  (defun ruby-end ()
    (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
         "\\(?:^\\|\\s-+\\)\\(?:do\\)")
    (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
    (ruby-end-mode +1))

  (add-hook 'elixir-mode-hook 'ruby-end)

  (remove-hook 'ruby-mode-hook 'ruby-end-mode)
  (remove-hook 'enh-ruby-mode-hook 'ruby-end-mode)

  :config
  (remove-hook 'ruby-mode-hook 'ruby-end-mode)
  (remove-hook 'enh-ruby-mode-hook 'ruby-end-mode)
)

(use-package alchemist
  :diminish alchemist-mode
  :init
  (add-to-list 'elixir-mode-hook (alchemist-mode)))

(use-package web-mode
  :mode "\\.\\(html\\|css\\|less\\|scss\\|js[x]\\)$"
  :init
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))
  (setq web-mode-content-types-alist
        '(("css" . "\\.\\(less\\|scss\\)$")))
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package auto-complete
  :diminish auto-complete-mode
  :init
  (add-hook 'web-mode-hook 'auto-complete-mode))


;; --------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(helm-follow-mode-persistent t)
 '(magit-commit-arguments (quote ("--gpg-sign=999ABCF36AE3B637")))
 '(package-selected-packages
   (quote
    (helm-projectile helm-ag ruby-end alchemist elixir-mode erlang org tern-auto-complete tern yasnippet helm-ls-git helm web-mode sublime-themes spacemacs-theme spacegray-theme neotree markdown-mode magit less-css-mode jsx-mode js3-mode js2-mode elm-mode dash-functional ac-math ac-html)))
 '(scroll-bar-mode nil)
 '(standard-indent 2)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
