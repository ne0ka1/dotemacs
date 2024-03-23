;;; init-python.el --- Python development environment configuration

(straight-use-package 'anaconda-mode)

;;; anaconda
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(provide 'init-python)
