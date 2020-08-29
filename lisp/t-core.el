;;; t-core.el --- Core Packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package cc-mode)

(use-package projectile
  :commands (projectile-mode helm-projectile projectile-project-root)
  :init
  (setq projectile-mode-line '(:eval (format "[%s]" (projectile-project-name)))
        projectile-require-project-root nil
        projectile-completion-system 'helm)

  :config
  (add-to-list 'projectile-globally-ignored-directories "elpa-backups")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "target")
  (add-to-list 'projectile-globally-ignored-directories "dist")
  (add-to-list 'projectile-globally-ignored-directories "build")
  (add-to-list 'projectile-globally-ignored-directories ".idea")
  (add-to-list 'projectile-globally-ignored-files "**.bundle.js")
  (add-to-list 'projectile-globally-ignored-files "**.build.js")
  (add-to-list 'projectile-globally-ignored-files "**.bundle.css")
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  (add-to-list 'projectile-globally-ignored-files "**.min.js")
  (add-to-list 'projectile-globally-ignored-files "**.min.css")
  (add-to-list 'grep-find-ignored-files "**.bundle.js")
  (add-to-list 'grep-find-ignored-files "**.build.js")
  (add-to-list 'grep-find-ignored-files "**.bundle.css")
  (add-to-list 'grep-find-ignored-files "**.min.js")
  (add-to-list 'grep-find-ignored-files "**.min.css")
  (add-to-list 'grep-find-ignored-files "package-lock.json")
  (add-to-list 'grep-find-ignored-files ".DS_Store"))

(use-package ag
  :commands ag
  :config
  (setq ag-reuse-buffers t
        ag-highlight-search t
        ag-project-root-function (lambda () (projectile-project-root))))

(use-package wgrep
  :after ag)

(use-package wgrep-ag
  :after ag)

(use-package helm
  :commands (helm-mini helm-projectile helm-projectile-ag)
  :diminish helm-mode
  :bind (("C-x C-p" . helm-projectile-ag)
         ("C-x C-f" . helm-find-files)
         ("C-x C-o" . helm-projectile-find-file)
         ("C-x C-b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("C-x c s" . helm-ag-this-file)
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
                helm-follow-mode-persistent t
                helm-ff-skip-boring-files t
                helm-quick-update t
                helm-M-x-requires-pattern nil)

  :config
  (progn
    (helm-mode 1)
    (set-face-attribute 'helm-source-header nil :height 1)
    (add-hook 'helm-before-initialize-hook 'neotree-hide)))

(use-package helm-hunks
  :commands (helm-hunks
             helm-hunks-current-buffer
             helm-hunks-staged
             helm-hunks-staged-current-buffer)
  :init
  (progn
    (setq helm-hunks-preview-diffs t)))

(use-package helm-ag
  :after helm
  :commands (helm-ag helm-projectile-ag)
  :init
  (setq helm-ag-fuzzy-match t
        helm-ag-insert-at-point 'symbol
        helm-ag-use-grep-ignore-list t
        helm-ag-edit-save t))

(use-package helm-projectile
  :after helm
  :commands (helm-projectile helm-projectile-ag helm-projectile-find-file))

(use-package helm-ls-git
  :after helm
  :bind (("C-x C-d" . helm-browse-project)))

(use-package helm-dash
  :after helm
  :commands helm-dash)

(use-package multiple-cursors
  :ensure t
  :bind (("C-c C-. ."   . mc/mark-all-dwim)
         ("C-c C-. C-." . mc/mark-all-like-this-dwim)
         ("C-c C-. a"   . mc/mark-all-like-this)
         ("C-c C-. N"   . mc/mark-next-symbol-like-this)
         ("C-c C-. P"   . mc/mark-previous-symbol-like-this)
         ("C-c C-. A"   . mc/mark-all-symbols-like-this)
         ("C-c C-. f"   . mc/mark-all-like-this-in-defun)
         ("C-c C-. l"   . mc/edit-lines)
         ("C-c C-. e"   . mc/edit-ends-of-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this-dwim)))

(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :commands highlight-parentheses-mode
  :init
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

(use-package exec-path-from-shell
  :if (equal system-type 'darwin)
  :config
  (exec-path-from-shell-initialize))

(use-package company
  :commands company-mode
  :diminish company-mode
  :init
  (setq
   company-minimum-prefix-length 1
   company-idle-delay 0.0
   company-tooltip-align-annotations t
   company-tooltip-flip-when-above t
   company-show-numbers t
   company-selection-wrap-around t
   company-require-match nil)
  (add-hook 'prog-mode-hook 'company-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :config
  (progn
    (setq
      lsp-completion-provider :capf
      lsp-idle-delay 0.400))
  :hook (
         (rjsx-mode . lsp)
         (web-mode . lsp)
         (js2-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :init
  (progn
    (setq lsp-ui-sideline-show-code-actions t
          lsp-ui-peek-fontify t)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))
  :config
  (progn
     (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
     (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
  :commands lsp-ui-mode)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package dap-mode
  :ensure t
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package which-key
    :config
    (progn
      (setq which-key-idle-delay 0.05)
      (setq which-key-idle-secondary-delay 0.05)
      (which-key-mode)))

(use-package company-lsp
  :after company
  :init
  (setq company-lsp-cache-candidates t
        company-lsp-async t))

(use-package flycheck
  :commands flycheck-mode
  :diminish flycheck-mode
  :init (global-flycheck-mode)
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.25))))

(use-package swiper-helm
  :commands swiper-helm
  :bind (("C-s" . swiper-helm)
         ("C-r" . swiper-helm)))

(provide 't-core)
;;; t-core.el ends here
