(require 'package)

(add-to-list 'package-archives
              '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
     (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(defconst +emacs-dir+ "~/.emacs.d")
(defconst +emacs-conf-dir+ (concat +emacs-dir+ "/configs"))
(defconst +emacs-snippets-dir+ (concat +emacs-dir+ "/snippets"))

(add-to-list 'load-path (concat +emacs-dir+ "/includes"))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setenv "NODE_PATH" "/usr/local/lib/node_modules")

(defun load-config (f)
  (load-file (concat +emacs-conf-dir+ "/" f ".el")))

(load-theme 'spacemacs-dark t)

(global-hl-line-mode 1)
(global-linum-mode 1)
(setq-default line-spacing 5)

(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))

(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; --------------------------------------
(load-config "helm")
(load-config "yasnippets")

(load-config "css")
(load-config "elm")
(load-config "magit")
(load-config "neotree")
(load-config "tern")
(load-config "webmode")

;; --------------------------------------

(require 'nodejs-repl)

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(helm-mode t)
 '(magit-commit-arguments (quote ("--gpg-sign=999ABCF36AE3B637")))
 '(package-selected-packages
   (quote
    (yasnippet helm-ls-git helm web-mode sublime-themes spacemacs-theme spacegray-theme neotree markdown-mode magit less-css-mode jsx-mode js3-mode js2-mode elm-mode dash-functional ac-math ac-html)))
 '(scroll-bar-mode nil)
 '(standard-indent 2)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 120 :family "Menlo")))))
