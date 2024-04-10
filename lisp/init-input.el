;;; init-input.el --- Input for Chinese Language
(straight-use-package 'rime) ;; https://github.com/DogLooksGood/emacs-rime
(straight-use-package 'sis)             ; https://github.com/laishulu/emacs-smart-input-source
(straight-use-package 'opencc)

;;; Rime
(require 'rime)
(cond
 (sys/macp 
  (setq rime-librime-root "~/.emacs.d/var/librime/dist"
        rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@29/29.3/include"))
 (sys/linuxp
  (setq rime-librime-root "/usr/")))

(setq default-input-method "rime"
      rime-show-preedit nil
      rime-deactivate-when-exit-minibuffer nil
      rime-disable-predicates
      '(rime-predicate-after-alphabet-char-p
	rime-predicate-current-uppercase-letter-p
	rime-predicate-tex-math-or-command-p)
      rime-translate-keybindings '("C-h" "C-j" "C-k" "C-l" "C-g" ";" "'" "C-`"))

;; compatible with 
(set-face-attribute 'rime-default-face nil :foreground "#000000" :background "#ffffff")

;;; Smart Input Source
(require 'sis)
(sis-ism-lazyman-config nil "rime" 'native)

(sis-global-respect-mode) ; conflicts with meow keypad
(sis-global-context-mode)

(provide 'init-input)
