;;; init-evil.el --- Lightweight Evil

(straight-use-package 'evil)            ; https://github.com/noctuid/evil-guide 

;;; Some variables affect the way Evil is loaded, so set these before Evil is loaded.

(setq evil-want-C-u-scroll nil          ; Preserve C-u
      evil-want-C-u-delete t            ; Use C-u in insert mode
      evil-want-C-w-in-emacs-state t    ; C-w as window management
      evil-want-C-i-jump nil            ; Otherwise TAB is broke
      evil-want-keybinding nil          ; Compatibility with (k)evil-collection
      evil-disable-insert-state-bindings t ; Enable emacs bindings in insert state
      evil-respect-visual-line-mode t
      evil-undo-system 'undo-redo)      ; Use Emacs native undo system

;;; Activation
(require 'evil)
(evil-mode)

;;; Evil normal & motion state map
;; Add scroll keys
(evil-global-set-key 'motion (kbd "SPC") 'scroll-up-command)
(evil-global-set-key 'motion (kbd "DEL") 'scroll-down-command)
(evil-global-set-key 'normal (kbd "SPC") 'scroll-up-command)
(evil-global-set-key 'normal (kbd "DEL") 'scroll-down-command)

;; Free up Emacs bindings
;; C-character
(evil-global-set-key 'motion (kbd "C-f") nil) ; evil-scroll-page-down
(evil-global-set-key 'motion (kbd "C-b") nil) ; evil-scroll-page-up
(evil-global-set-key 'motion (kbd "C-v") nil) ; evil-visual-block
(evil-global-set-key 'motion (kbd "C-y") nil) ; evil-scroll-line-up
(evil-global-set-key 'motion (kbd "C-e") nil) ; evil-scroll-line-down
(evil-global-set-key 'normal (kbd "C-.") nil) ; evil-repeat-pop; for iflipb

;; M-character
(evil-global-set-key 'normal (kbd "M-.") nil) ; evil-repeat-pop-next

;; C-S-o to jump forward (substitute for C-i)
(evil-global-set-key 'normal (kbd "C-S-o") 'evil-jump-forward)

;; Conflict with which-key paging (C-h)
(evil-global-set-key 'motion (kbd "C-w C-h") nil) ; evil-window-left

;;; Evil insert state map.
(evil-global-set-key 'insert (kbd "C-h") 'evil-delete-backward-char-and-join)
(evil-global-set-key 'insert (kbd "C-w") 'evil-delete-backward-word)
(evil-global-set-key 'insert (kbd "C-u") 'evil-delete-back-to-indentation)
(evil-global-set-key 'insert (kbd "M-h") (lookup-key global-map (kbd "C-h"))) ; mark-paragraph

(provide 'init-evil)
