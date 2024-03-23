;;; init-input.el --- Input for Chinese Language
(straight-use-package 'rime)
(straight-use-package 'sis)
(straight-use-package 'opencc)

;;; Rime
(require 'rime)
(setq rime-librime-root "/usr/")

(setq default-input-method "rime"
      rime-show-preedit nil
      rime-deactivate-when-exit-minibuffer nil
      rime-disable-predicates
      '(rime-predicate-after-alphabet-char-p
	rime-predicate-current-uppercase-letter-p
	rime-predicate-tex-math-or-command-p)
      rime-translate-keybindings '("C-h" "C-j" "C-k" "C-l" "C-g" ";" "'" "C-`"))

;;; Smart Input Source
(require 'sis)
(sis-ism-lazyman-config nil "rime" 'native) ; emacs native

(sis-global-respect-mode) ; conflicts with meow keypad
(sis-global-context-mode)

(provide 'init-input)
