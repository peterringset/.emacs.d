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

(load-theme 'brin t)


;; --------------------------------------

(load-config "js")
(load-config "css")
(load-config "tern")

;; --------------------------------------


(require 'nodejs-repl)


(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)
