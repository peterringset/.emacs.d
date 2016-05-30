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


(load-theme 'spacegray t)

(global-hl-line-mode 1)
(global-linum-mode 1)
(setq-default line-spacing 5)

(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))


;; --------------------------------------

; (load-config "js")
(load-config "webmode")
(load-config "css")
(load-config "tern")

;; --------------------------------------


(require 'nodejs-repl)


(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)


(custom-set-variables
 '(column-number-mode t)
 '(scroll-bar-mode nil)
 '(standard-indent 2)
 '(tool-bar-mode nil))
(custom-set-faces
 '(default ((t (:height 120 :family "Menlo"))))
 )
