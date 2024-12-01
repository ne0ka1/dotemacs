;;; init-lisp.el --- Lisp family language config

(define-key emacs-lisp-mode-map (kbd "C-c C-x") 'ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)

(setq scheme-program-name "mit-scheme")

(provide 'init-lisp)
