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

(use-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode 1))

(use-package exec-path-from-shell
  :if (equal system-type 'darwin)
  :config
  (exec-path-from-shell-initialize))

(use-package company
  :init
  (setq company-idle-delay 0.2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-show-numbers t
        company-selection-wrap-around t
        company-require-match nil)

  :config
  (global-company-mode))

(use-package company-tern
  :after company
  :config
  (add-to-list 'company-backends 'company-tern)
  (setq tern-command (append tern-command '("--no-port-file"))))

(use-package spacemacs-theme
  :init
  (progn
    (load-theme 'spacemacs-dark t)))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package org
  :defer t)

(use-package helm
  :commands (helm-projectile helm-projectile-ag)
  :diminish helm-mode
  :bind (("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ("M-x" . helm-M-x))

  :init
  (require 'helm-config)
  (setq-default helm-display-header-line nil
                helm-M-x-fuzzy-match t
                helm-apropos-fuzzy-match t
                helm-buffers-fuzzy-matching t
                helm-completion-in-region-fuzzy-match t
                helm-file-cache-fuzzy-match t
                helm-lisp-fuzzy-completion t
                helm-mode-fuzzy-match t
                helm-projectile-fuzzy-match t
                helm-recentf-fuzzy-match t
                helm-candidate-number-limit 100
                helm-prevent-escaping-from-minibuffer t
                helm-always-two-windows t
                helm-echo-input-in-header-line t

                ;; keep follow mode on, after on once
                helm-follow-mode-persistent t
                helm-ff-skip-boring-files t
                helm-quick-update t
                helm-M-x-requires-pattern nil)

  :config
  (helm-mode 1)
  (custom-set-variables
   '(helm-follow-mode-persistent t)))

(use-package helm-ls-git
  :after helm
  :bind (("C-x C-d" . helm-browse-project)))

(use-package helm-ag
  :after helm
  :commands helm-ag
  :init
  (setq helm-ag-fuzzy-match t))

(use-package helm-projectile
  :after helm
  :commands helm-projectile)

(use-package projectile
  :commands (projectile-mode helm-projectile)
  :init
  (setq projectile-mode-line '(:eval (format "[%s]" (projectile-project-name)))
        projectile-known-projects-file (locate-user-emacs-file ".projectile-bookmarks.eld")
        projectile-completion-system 'helm)

  :config
  (add-to-list 'projectile-globally-ignored-directories "elpa-backups")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "target")
  (add-to-list 'projectile-globally-ignored-directories "dist")
  (add-to-list 'projectile-globally-ignored-directories ".idea")
  (add-to-list 'projectile-globally-ignored-files "**.bundle.js")
  (add-to-list 'projectile-globally-ignored-files "**.build.js")
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  (add-to-list 'grep-find-ignored-files "**.bundle.js")
  (add-to-list 'grep-find-ignored-files "**.build.js")
  (add-to-list 'grep-find-ignored-files ".DS_Store")
  (projectile-global-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (setq yas-snippet-dirs '(+emacs-snippets-dir+))

  :config
  (yas-global-mode 1))

(use-package elm-mode
  :mode "\\.elm$"
  :init
  (setq elm-format-on-save t))

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

(use-package flycheck
  :commands flycheck-mode
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package js2-mode
  :mode "\\.js$"
  :init
  (setq-default js2-show-parse-errors nil
                js2-strict-missing-semi-warning nil
                js2-strict-inconsistent-return-warning nil
                js2-strict-var-hides-function-arg-warning nil
                js2-strict-cond-assign-warning nil
                js2-strict-var-redeclaration-warning nil
                js2-strict-trailing-comma-warning t)

  (setq js2-highlight-level 3)
  (setq-default js2-basic-offset 2)
  (setq js-indent-level 2)

  :config
  (add-hook 'js2-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package web-mode
  :mode "\\.\\(html\\|css\\|less\\|scss\\|jsx\\)$"
  :init
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")
          ("css" . "\\.\\(less\\|scss\\)$")))

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
 '(helm-source-names-using-follow (quote ("Search at ~/.emacs.d/")))
 '(magit-commit-arguments (quote ("--gpg-sign=999ABCF36AE3B637")))
 '(package-selected-packages
   (quote
    (highlight-parentheses exec-path-from-shell helm-projectile helm-ag ruby-end alchemist elixir-mode erlang org tern-auto-complete tern yasnippet helm-ls-git helm web-mode sublime-themes spacemacs-theme spacegray-theme neotree markdown-mode magit less-css-mode jsx-mode js3-mode js2-mode elm-mode dash-functional ac-math ac-html)))
 '(scroll-bar-mode nil)
 '(standard-indent 2)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
