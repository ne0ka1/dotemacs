;;; init-completion.el --- Completion Suite

;; completion in minibuffer, in region
(straight-use-package 'vertico)         ; https://github.com/minad/vertico
(straight-use-package 'orderless)       ; https://github.com/oantolin/orderless
(straight-use-package 'corfu)           ; https://github.com/minad/corfu
(straight-use-package 'corfu-terminal)
(straight-use-package 'corfu-candidate-overlay)
(straight-use-package 'nerd-icons)	; https://github.com/rainstormstudio/nerd-icons.el
(straight-use-package 'nerd-icons-corfu); https://github.com/LuigiPiucco/nerd-icons-corfu

;;; Minibuffer completion & narrowing

;; Native annotations in favor of Marginalia, which has alignment issue
(setq completions-detailed t)
;; Ignore case
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; Orderless
(setq completion-styles '(substring orderless basic)
      orderless-component-separator 'orderless-escapable-split-on-space)

;; Vertico
(require 'vertico)
(setq vertico-count 7	  ; Maximal number of candidates to show
      vertico-cycle t	  ; Enable cycling for `vertico-next/previous'
      vertico-count-format nil)	    ; No prefix with number of entries
(define-key vertico-map (kbd "C-j") 'vertico-next)
(define-key vertico-map (kbd "C-k") 'vertico-previous)
(define-key vertico-map (kbd "C-<return>") 'vertico-exit-input)
(vertico-mode)

;; Vertico Multiform
(require 'vertico-multiform)
(require 'vertico-flat)
(setq vertico-multiform-commands
        '((execute-extended-command flat)))
(setq vertico-multiform-categories
      '((file flat)))
(vertico-multiform-mode)

;; In-region completion
(setq tab-always-indent 'complete)	; Enable indentation+completion using the TAB key

;;; Corfu
(require 'corfu)

(unless (display-graphic-p)
  (require 'corfu-terminal)
  (corfu-terminal-mode +1))

(setq corfu-cycle t                ; Enable cycling for `corfu-next/previous'
      ; corfu-auto t                 ; Enable auto completion
      corfu-quit-at-boundary nil   ; Never quit at completion boundary
      corfu-quit-no-match t        ; Quit when no match
      corfu-on-exact-match nil     ; Configure handling of exact matches
      corfu-scroll-margin 5)       ; Use scroll margin

(define-key corfu-map (kbd "C-j") 'corfu-next)
(define-key corfu-map (kbd "C-k") 'corfu-previous)

(global-corfu-mode)

;; corfu extensions
(require 'corfu-popupinfo)
(corfu-popupinfo-mode)
(setq corfu-popupinfo-delay 1.0)

(require 'corfu-candidate-overlay)
(corfu-candidate-overlay-mode)

;; nerd-icon
(require 'nerd-icons)
(cond
 (sys/macp
  (setq nerd-icons-font-family "SFMono Nerd Font"))
 (sys/linuxp
  (setq nerd-icons-font-family "SFMono Nerd Font Mono")))

(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

(provide 'init-completion)
