;;; init-lisp.el --- Lisp family language config
(straight-use-package 'paredit)

(define-key emacs-lisp-mode-map (kbd "C-c C-x") 'ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)

(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(with-eval-after-load 'eldoc
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))

(provide 'init-lisp)
