(autoload 'js3-mode "js3-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js3-mode))

(add-hook 'js3-mode-hook
	  (lambda ()
	    (setq js3-auto-indent-p t
		  js3-curly-indent-offset 0
		  js3-enter-indents-newline t
		  js3-expr-indent-offset 2
		  js3-lazy-commas t
		  js3-lazy-dots t
		  js3-lazy-operators t
		  js3-paren-indent-offset 2
		  js3-square-indent-offset 4)))
