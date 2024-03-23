;;; init-completion.el --- Completion Suite
(straight-use-package 'which-key)
(straight-use-package 'vertico)
(straight-use-package 'corfu)
(straight-use-package 'corfu-terminal)
(straight-use-package 'corfu-candidate-overlay)
(straight-use-package 'kind-icon)

;;; which-key
(add-hook 'emacs-startup-hook 'which-key-mode)

;;; Minibuffer completion & narrowing
;;; Emacs
(setq completion-styles '(basic substring)				    ; Default substring
      completion-category-overrides '((symbol-help (styles flex)))) ; Enhance symbol searching

(setq completions-detailed t)		; Annotations

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)		; Ignore Case

;;; Vertico
(require 'vertico)

(setq vertico-count 7	  ; Maximal number of candidates to show
      vertico-cycle t	  ; Enable cycling for `vertico-next/previous'
      vertico-count-format nil)	    ; No prefix with number of entries

(define-key vertico-map (kbd "C-j") 'vertico-next)
(define-key vertico-map (kbd "C-k") 'vertico-previous)
(define-key vertico-map (kbd "C-<return>") 'vertico-exit-input)

(vertico-mode)

;; Vertico extensions
(add-to-list 'load-path (expand-file-name "straight/build/vertico/extensions" straight-base-dir))

(require 'vertico-multiform)
(require 'vertico-flat)

(setq vertico-multiform-commands
        '((execute-extended-command flat)))

(setq vertico-multiform-categories
      '((buffer flat)
	(file flat)))

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
(add-to-list 'load-path (expand-file-name "straight/build/corfu/extensions" straight-base-dir))

(require 'corfu-popupinfo)
(corfu-popupinfo-mode)
(setq corfu-popupinfo-delay 1.0)

(require 'corfu-candidate-overlay)
(corfu-candidate-overlay-mode)

;; kind-icon
(setq kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

(provide 'init-completion)
