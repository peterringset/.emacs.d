(add-to-list 'load-path (concat +emacs-dir+ "/libs/tern/emacs"))
(autoload 'tern-mode "tern.el" nil t)

(add-hook 'js3-mode-hook (lambda () (tern-mode t)))
(add-hook 'js3-mode-hook (lambda () (auto-complete-mode t)))

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))