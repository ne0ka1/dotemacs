;;; init-navigation.el --- Convenient navigation

;; Navigation inside and between buffer
(straight-use-package 'rg)       ; https://github.com/dajva/rg.el
(straight-use-package 'avy)      ; https://github.com/abo-abo/avy
(straight-use-package 'consult)  ; https://github.com/minad/consult
(straight-use-package 'iflipb)   ; https://github.com/jrosdahl/iflipb
(straight-use-package 'popper)   ; https://github.com/karthink/popper

;;; avy
<<<<<<< HEAD
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
=======
(global-set-key (kbd "C-;") 'avy-goto-char-2)
(global-set-key (kbd "C-:") 'avy-goto-char)
>>>>>>> a6f3932 (Use popper and shell-pop; refine init-edit)

;;; recentf
(add-hook 'emacs-startup-hook 'recentf-mode)
(require 'recentf)
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-var-directory))
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-etc-directory))
(setq recentf-max-saved-items 200)

;;; Consult
(global-set-key (kbd "C-c m") 'consult-imenu)	   ; i"m"enu
(global-set-key (kbd "C-c T") 'consult-theme)	   ; "T"heme
(global-set-key (kbd "C-h s") 'consult-info)       ; 'describe-syntax
(global-set-key [remap Info-search] 'consult-info) ; key "s" in Info-mode
(global-set-key (kbd "C-x b") 'consult-buffer)     ; 'switch-buffer
(global-set-key (kbd "C-x p b") 'consult-project-buffer)     ; 'project-switch-buffer
(global-set-key (kbd "C-s") 'consult-line)         ; 'i-search
(global-set-key (kbd "M-s") 'consult-ripgrep)      ; isearch related prefix key
(global-set-key (kbd "M-g g") 'consult-goto-line)  ; 'goto-line

;;; Buffer flip
<<<<<<< HEAD
(setq iflipb-wrap-around t)
(global-set-key (kbd "C-,") 'iflipb-next-buffer)
(global-set-key (kbd "C-.") 'iflipb-previous-buffer)
(setq iflipb-ignore-buffers (list (rx line-start "*Backtrace")
                                  (rx line-start "*Help")
                                  (rx line-start "*Messages")
                                  (rx line-start "*Calendar")
                                  (rx line-start "magit-process")
                                  (rx line-start "*" (zero-or-more anything) "output*")
                                  (rx line-start "*straight-process*")))
=======
(setq iflipb-wrap-around t
      iflipb-ignore-buffers "\\*.*\\*")
(global-set-key (kbd "C-,") 'iflipb-next-buffer)
(global-set-key (kbd "C-.") 'iflipb-previous-buffer)
(setq iflipb-ignore-buffers "")
>>>>>>> a6f3932 (Use popper and shell-pop; refine init-edit)

;;; Buffer management using iBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; Window management
(add-hook 'emacs-startup-hook 'winner-mode)
;; use evil to manage window
(evil-global-set-key 'motion (kbd "C-w n") 'evil-window-split)
(evil-global-set-key 'motion (kbd "C-w u") 'winner-undo)
(setq winner-boring-buffers '("*Help*"
                              "*Apropos"
                              "*Compile-Log*"
                              "*Ibuffer*"))

;;; Popper -- pop up windows
(setq popper-display-function #'display-buffer-below-selected
      popper-echo-dispatch-actions t)

<<<<<<< HEAD
(global-set-key (kbd "C-`") 'popper-toggle) 
=======
(global-set-key (kbd "C-h `") 'popper-toggle) 
>>>>>>> a6f3932 (Use popper and shell-pop; refine init-edit)
(global-set-key (kbd "M-`") 'popper-cycle)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)

(setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$"
        "^\\*eldoc.*\\*$"
        "\\*Compile-Log\\*$"
        "\\*Completions\\*$"
        "\\*Warnings\\*$"
        "\\*Async Shell Command\\*$"
        "\\*Apropos\\*$"
        "\\*Backtrace\\*$"
        "\\*Calendar\\*$"
        "\\*Fd\\*$" "\\*Find\\*$" "\\*Finder\\*$"

        ;; supply both the name and major mode to match them consistently
        "^\\*eshell.*\\*$" eshell-mode
        "^\\*shell.*\\*$"  shell-mode
        "^\\*term.*\\*$"   term-mode
        "^\\*vterm.*\\*$"  vterm-mode
        
        help-mode
        compilation-mode
        devdocs-mode
        grep-mode occur-mode rg-mode
        
        flymake-diagnostics-buffer-mode
        flycheck-error-list-mode flycheck-verify-mode))
        
;; use `C-g' to close popper window
(defun popper-close-window-hack (&rest _)
  "Close popper window via `C-g'."
  (when (and (called-interactively-p 'interactive)
             (not (region-active-p))
             popper-open-popup-alist)
    (let ((window (caar popper-open-popup-alist)))
      (when (window-live-p window)
        (delete-window window)))))

(advice-add #'keyboard-quit :before #'popper-close-window-hack)

;; activate popper-mode
(popper-mode +1)
(popper-echo-mode +1)

(provide 'init-navigation)
