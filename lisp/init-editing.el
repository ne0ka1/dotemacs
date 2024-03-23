;;; init-editing.el --- Editing Tricks
(straight-use-package 'tempel)

;;; Nice add-ons
;; Visual Line Mode
(global-visual-line-mode 1)

;; Highlight line
;; (add-hook 'prog-mode-hook #'hl-line-mode)

;; Line Number
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Parentheses
(electric-pair-mode)

;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;;; Check spelling
;; (add-hook 'text-mode-hook #'flyspell-mode)
;; These keybindings conflict with iflipb
(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-.") nil) ;flyspell-auto-correct-word
  (define-key flyspell-mode-map (kbd "C-,") nil) ;flyspell-goto-next-error
  )

;;; Tempel
;; Configure Tempel
(setq tempel-path (no-littering-expand-etc-file-name "templates"))
(global-set-key (kbd "M-+") 'tempel-complete) ; undefined
(global-set-key (kbd "M-*") 'tempel-insert) ; undefined

;; (setq tempel-trigger-prefix "<")
;; Setup completion at point
(defun tempel-setup-capf ()
  ;; Add the Tempel Capf to `completion-at-point-functions'.
  ;; `tempel-expand' only triggers on exact matches. Alternatively use
  ;; `tempel-complete' if you want to see all matches, but then you
  ;; should also configure `tempel-trigger-prefix', such that Tempel
  ;; does not trigger too often when you don't expect it. NOTE: We add
  ;; `tempel-expand' *before* the main programming mode Capf, such
  ;; that it will be tried first.
  (setq-local completion-at-point-functions
              (cons #'tempel-expand
                    completion-at-point-functions)))

(add-hook 'conf-mode-hook 'tempel-setup-capf)
(add-hook 'prog-mode-hook 'tempel-setup-capf)
(add-hook 'text-mode-hook 'tempel-setup-capf)

(provide 'init-editing)
