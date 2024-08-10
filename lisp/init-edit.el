;;; init-editing.el --- Editing Tricks
(straight-use-package 'tempel)          ; https://github.com/minad/tempel

;;; which-key
(add-hook 'emacs-startup-hook 'which-key-mode)
(setq-default which-key-idle-delay 1.5)
(global-set-key (kbd "C-h C-h") nil)    ; 'help-for-help
(global-set-key (kbd "C-h C-m") 'which-key-show-major-mode) ; 'view-order-manuals

;;; Nice add-ons
;; Visual Line Mode
(global-visual-line-mode 1)

;; Outline minor mode
(add-hook 'prog-mode-hook #'outline-minor-mode)

;; Line Number
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Parentheses
(electric-pair-mode)

;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; open url/file
(global-set-key (kbd "C-c C-o") 'find-file-at-point)

;;; Check spelling
(add-hook 'text-mode-hook #'flyspell-mode)
(with-eval-after-load 'flyspell
  ;; conflict with iflipb
  (define-key flyspell-mode-map (kbd "C-.") nil) ;flyspell-auto-correct-word
  (define-key flyspell-mode-map (kbd "C-,") nil) ;flyspell-goto-next-error
  ;; conflict with avy
  (define-key flyspell-mode-map (kbd "C-;") nil) ;flyspell-auto-correct-previous-word
)

;;; Tempel
;; Configure Tempel
(setq tempel-path (no-littering-expand-etc-file-name "templates"))
(global-set-key (kbd "M-+") 'tempel-complete) ; undefined
(global-set-key (kbd "M-*") 'tempel-insert) ; undefined

(with-eval-after-load 'tempel
  (keymap-set tempel-map "<tab>" 'tempel-next)
  (keymap-set tempel-map "S-<tab>" 'tempel-previous))

;; Setup completion at point. `tempel-expand' only triggers on exact matches.
(defun tempel-setup-capf ()
  (setq-local completion-at-point-functions
              (cons #'tempel-expand
                    completion-at-point-functions)))

(add-hook 'conf-mode-hook 'tempel-setup-capf)
(add-hook 'prog-mode-hook 'tempel-setup-capf)
(add-hook 'text-mode-hook 'tempel-setup-capf)

(provide 'init-edit)
