;;; init-editing.el --- Editing Tricks
(straight-use-package 'tempel)          ; https://github.com/minad/tempel

;;; which-key
(when (version< emacs-version "30.0")
  (straight-use-package 'which-key))

(which-key-mode)
(setq-default which-key-idle-delay 1.5)
(global-set-key (kbd "C-h C-h") nil)    ; 'help-for-help
(global-set-key (kbd "C-h C-m") 'which-key-show-major-mode) ; 'view-order-manuals

;;; Configs
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

;;; Keybindings
(global-set-key (kbd "M-j") #'evil-join)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") #'rename-current-buffer-file) ; 'find-file-read-only

;;; Check spelling
(add-hook 'text-mode-hook #'flyspell-mode)

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
