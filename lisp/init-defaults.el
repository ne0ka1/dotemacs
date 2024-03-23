;; init-base.el --- Better default configurations.

;;; Modifiers
(when sys/macp
 ;; make Mac keyboard's command key as control key
  (setq ns-command-modifier 'control))

;;; Minimal interface
;; Note that frame has been handled in early-init.el
;; Startup setting
(setq inhibit-startup-screen t		; Inhibit the startup screen
      initial-scratch-message nil)		  ; No *scratch* buffer initial documentation

;; Remove interface cluttor
(setq frame-title-format '("" "%b - Emacs "))
(tooltip-mode -1)
(setq ring-bell-function 'ignore)

;; Cursor
(setq-default cursor-in-non-selected-windows nil) ; No cursor in other windows
(setq cursor-in-non-selected-windows nil
      blink-cursor-mode nil)		; No blinking cursor

;;; Less verbose emacs
;; use y-or-n-p instead of yes-or-no-p
(setq use-short-answers t         ; use y-or-n-p instead of yes-or-no-p
      confirm-kill-processes nil) ; Do not confirm killingn processes on exit

;; Get rid of kill buffer query functions
(setq kill-buffer-query-functions
    (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))

;; No question for killing buffer
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Sane keybindings
(global-set-key (kbd "C-x f") 'find-file)

;; Keep the directory clean (Used no-littering)
(setq create-lockfiles nil)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode)

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; Save what you enter into minibuffer prompts
(setq history-length 25)                ; reduce performance impact
(savehist-mode 1)

;; Remember the last cursor position of opened files
(save-place-mode 1)

;; Environment Variables
(setenv "http_proxy" "http://127.0.0.1:7890")
(setenv "https_proxy" "http://127.0.0.1:7890")
(setenv "all_proxy" "socks5://127.0.0.1:7890")

(provide 'init-defaults)
