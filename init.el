;;; init.el --- Load the configuration -*- lexical-binding: t -*-

;;; Commentary:
;; My Emacs configuration

;;; Code:

(require 'package)
(package-initialize)

(setq package-enable-at-startup nil)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t)
  (require 'cl))

(use-package diminish)
(use-package dash)
(use-package s)

(require 'cc-mode)
(require 'bind-key)

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024))
      (my-gnutls-min-prime-bits 4096))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (setq gnutls-min-prime-bits my-gnutls-min-prime-bits)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; -----------------------------------------------------------------------------
;; Directories
;; -----------------------------------------------------------------------------
(defconst user-emacs-directory "~/.emacs.d/")
(defun user-emacs-file (path)
  "Prefix PATH with user-emacs-eidrectory."
  (concat user-emacs-directory path))

(defconst user-dir-snippets (user-emacs-file "snippets"))
(defconst user-dir-lisp (user-emacs-file "lisp"))
(defconst user-dir-lisp-lang (user-emacs-file "lisp/lang"))
(defconst vendor-dir-lisp (user-emacs-file "vendor"))

(add-to-list 'load-path user-dir-lisp)
(add-to-list 'load-path user-dir-lisp-lang)
(add-to-list 'load-path vendor-dir-lisp)

;; -----------------------------------------------------------------------------
;; Bootstrap configs
;; -----------------------------------------------------------------------------

(setq custom-file (user-emacs-file "custom.el"))
(load custom-file)

(require 't-core)
(require 't-editor)
(require 't-magit)
(require 't-yasnippet)

(require 'lang-css)
(require 'lang-ember)
(require 'lang-html)
(require 'lang-java)
(require 'lang-js)
(require 'lang-json)
(require 'lang-kotlin)
(require 'lang-markdown)
(require 'lang-org)
(require 'lang-swift)
(require 'lang-ruby)
(require 'lang-yaml)

;; Vendor stuff
(require 'mustache-mode)


;; -----------------------------------------------------------------------------
;; ...
;; -----------------------------------------------------------------------------
(setq-default indent-tabs-mode nil)
(setq-default line-spacing 6)
(setq-default tab-always-indent 'complete)

(setq inhibit-startup-screen t)
(setq mac-command-modifier 'meta) ; Set cmd to meta key
(setq mac-option-modifier nil)
(setq select-enable-clipboard t)
(setq shell-file-name "/usr/local/bin/fish")
(setq tab-width 2)

(when (eq system-type 'windows-nt)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-hl-line-mode 1)


;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; --------------------------------------

(cond
 ((find-font (font-spec :family "SF Mono"))
  (set-frame-font "SF Mono:pixelsize=12"))
 ((find-font (font-spec :family "Fira Code Retina"))
  (set-frame-font "Fira Code Retina:pixelsize=12"))
 ((find-font (font-spec :family "Menlo"))
  (set-frame-font "Menlo:pixelsize=12"))
 ((find-font (font-spec :family "Monaco"))
  (set-fram-font "Monaco:pixelsize=12")))


(provide 'init)
;;; init.el ends here
