(setq gc-cons-threshold 50000000)
(setq gnutls-min-prime-bits 4096)

(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
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

(setq-default indent-tabs-mode nil)
(setq-default line-spacing 6)
(setq-default tab-always-indent 'complete)
(setq tab-width 2)

(setq inhibit-startup-screen t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(setq shell-file-name "bash")
(setq use-package-always-ensure t)
(setq x-select-enable-clipboard t)

(when (eq system-type 'windows-nt)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-hl-line-mode 1)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(column-number-mode t)
 '(helm-follow-mode-persistent t)
 '(helm-source-names-using-follow (quote ("Search at ~/.emacs.d/")))
 '(magit-commit-arguments (quote ("--gpg-sign=999ABCF36AE3B637")))
 '(package-selected-packages
   (quote
    (wgrep-ag wgrep ag php-mode helm-hunks all-the-icons auto-complete flycheck ninja-mode json-mode highlight-parentheses exec-path-from-shell helm-projectile helm-ag ruby-end alchemist elixir-mode erlang org tern-auto-complete tern yasnippet helm-ls-git helm web-mode sublime-themes spacemacs-theme spacegray-theme neotree markdown-mode magit less-css-mode jsx-mode js3-mode js2-mode elm-mode dash-functional ac-math ac-html)))
 '(scroll-bar-mode nil)
 '(standard-indent 2)
 '(tool-bar-mode nil))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)


;; --------------------------------------

(cond
 ((find-font (font-spec :family "Fira Code Retina"))
  (set-frame-font "Fira Code Retina:pixelsize=12"))
 ((find-font (font-spec :family "Menlo"))
  (set-frame-font "Menlo:pixelsize=12"))
 ((find-font (font-spec :family "Monaco"))
  (set-fram-font "Monaco:pixelsize=12")))

(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; --------------------------------------

(require 'cl)

(use-package dash
  :ensure t
  :config (eval-after-load "dash" '(dash-enable-font-lock)))

(use-package s
  :ensure t)

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
  (add-to-list 'grep-find-ignored-files ".DS_Store"))

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
  :diminish company-mode tern-mode
  :init
  (setq company-idle-delay 0.2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-show-numbers t
        company-selection-wrap-around t
        company-require-match nil)
  (add-hook 'prog-mode-hook 'company-mode))

(use-package company-tern
  :after company
  :config
  (add-to-list 'company-backends 'company-tern)
  (setq tern-command (append tern-command '("--no-port-file"))))

(use-package doom-themes
  :defer t
  :init
  (load-theme 'doom-vibrant t))

(use-package magit
  :commands magit-status
  :bind (("C-x g" . magit-status)))

(use-package org
  :defer t)

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
  :bind (("C-x c f" . helm-projectile-ag)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ("C-x c y" . helm-show-kill-ring)
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
             helm-hunks-staged-current-buffer))

(use-package helm-ag
  :after helm
  :commands helm-ag
  :init
  (setq helm-ag-fuzzy-match t
        helm-ag-insert-at-point 'symbol
        helm-ag-use-grep-ignore-list t
        helm-ag-edit-save t))

(use-package helm-projectile
  :after helm
  :commands helm-projectile)

(use-package helm-ls-git
  :after helm
  :bind (("C-x C-d" . helm-browse-project)))

(use-package helm-dash
  :after helm
  :commands helm-dash)

(use-package yasnippet
  :diminish yas-minor-mode
  :defer 1
  :init
  (setq yas-snippet-dirs '(+emacs-snippets-dir+))

  :config
  (yas-global-mode 1))

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
        neo-create-file-auto-open t
        neo-show-updir-line nil
        neo-dont-be-alone t
        neo-show-hidden-files t
        neo-auto-indent-point t)
  (when (eq system-type 'darwin)
    (setq neo-theme 'icons))

  :config
  (add-hook 'neotree-mode-hook
            (lambda ()
              ;; (bind-key "R" 'neotree-rename-node)
              )))


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
  (remove-hook 'enh-ruby-mode-hook 'ruby-end-mode))

(use-package flycheck
  :commands flycheck-mode
  :diminish flycheck-mode
  :config
  (progn
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    (flycheck-add-mode 'css-csslint 'web-mode)
    (flycheck-add-mode 'css-csslint 'less-css-mode)))

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

(use-package less-css-mode
  :mode "\\.less$"
  :config
  (add-hook 'less-css-mode-hook (lambda ()
                                  (setq css-indent-offset 2))))


;; --------------------------------------


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
