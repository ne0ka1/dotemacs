;;; init-shell.el --- config for shells
(straight-use-package 'shell-pop)       ; https://github.com/kyagi/shell-pop-el

;; shell-pop
(require 'shell-pop)
(setq shell-pop-universal-key "C-`")

(provide 'init-shell)
