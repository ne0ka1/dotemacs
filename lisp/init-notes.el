;;; init-notes.el --- Writing (Markdown) notes in Emacs

(straight-use-package 'olivetti)
(straight-use-package 'deft)
(straight-use-package 'zetteldeft)      ; https://www.eliasstorms.net/zetteldeft/
(straight-use-package 'markdown-mode)   ; https://jblevins.org/projects/markdown-mode/

;;; Olivetti mode
(setq olivetti-body-width 0.67)

;;; Markdown-mode
(customize-set-variable 'markdown-header-scaling t)
;; same with solarized-theme default
(customize-set-variable 'markdown-header-scaling-values '(1.3 1.2 1.15 1.1 1.05 1.0))
(setq markdown-enable-wiki-links t
      markdown-hide-urls t
      markdown-fontify-code-blocks-natively t)

;;; My-writing-mode
(defun my-writing-mode ()
    (interactive)
    (setq line-spacing 5)
    (olivetti-mode)
    (setq buffer-face-mode-face '(:family "iA Writer Duospace" :height 130))
    (buffer-face-mode))

(add-hook 'org-mode-hook 'my-writing-mode)
(add-hook 'markdown-mode-hook 'my-writing-mode)

;;; Deft
(require 'deft)
(setq deft-default-extension "md"
      deft-directory "~/Notes"
      deft-auto-save-interval 0 	; Disable deft autosave
      deft-time-format nil              ; Disable modification time string
      deft-strip-summary-regexp ".*"    ; Disable summary
      )

(define-key deft-mode-map (kbd "C-j") 'next-line)
(define-key deft-mode-map (kbd "C-k") 'previous-line)
(define-key deft-mode-map (kbd "C-d") 'deft-delete-file)
(define-key deft-mode-map (kbd "<return>") 'deft-open-file-other-window)

;; Open deft in side window
(defun deft-side-window ()
  (interactive)
  (let ((dummy-buf (generate-new-buffer deft-buffer)))
    (with-current-buffer dummy-buf
      (deft-mode))
    (display-buffer-in-side-window dummy-buf
                   '((side . left)
                 (slot . 0)
                 (window-width . 0.25)))))


;;; Zetteldeft
(require 'zetteldeft)
;; set up keybindings
(define-prefix-command 'zetteldeft-prefix)
(global-set-key (kbd "C-c d") 'zetteldeft-prefix)
(global-set-key (kbd "C-c d d") 'deft-side-window)
(global-set-key (kbd "C-c d D") 'deft)
(global-set-key (kbd "C-c d R") 'deft-refresh)
(global-set-key (kbd "C-c d .") 'zetteldeft-search-at-point)
(global-set-key (kbd "C-c d s") 'my-zetteldeft-ripgrep)
(global-set-key (kbd "C-c d S") 'zetteldeft-search-current-id)
(global-set-key (kbd "C-c d f") 'zetteldeft-find-file)
(global-set-key (kbd "C-c d o") 'zetteldeft-follow-link)
(global-set-key (kbd "C-c d h") 'zetteldeft-go-home)
(global-set-key (kbd "C-c d t") 'zetteldeft-avy-tag-search)
(global-set-key (kbd "C-c d T") 'zetteldeft-tag-buffer)
(global-set-key (kbd "C-c d /") 'zetteldeft-search-tag)
(global-set-key (kbd "C-c d #") 'zetteldeft-tag-insert)
(global-set-key (kbd "C-c d $") 'zetteldeft-tag-remove)
(global-set-key (kbd "C-c d C-i") 'zetteldeft-full-search-full-title-insert)
(global-set-key (kbd "C-c d i") 'zetteldeft-find-file-full-title-insert)
(global-set-key (kbd "C-c d I") 'zetteldeft-find-file-id-insert)
(global-set-key (kbd "C-c d n") 'zetteldeft-new-file)
(global-set-key (kbd "C-c d N") 'zetteldeft-new-file-and-link)
(global-set-key (kbd "C-c d B") 'zetteldeft-new-file-and-backlink)
(global-set-key (kbd "C-c d b") 'zetteldeft-backlink-add)
(global-set-key (kbd "C-c d r") 'zetteldeft-file-rename)
(global-set-key (kbd "C-c d =") 'zetteldeft-count-words)
(global-set-key (kbd "C-c d w") 'zetteldeft-extract-region-to-note)

;; work with markdown
(setq zetteldeft-link-indicator "[["
      zetteldeft-link-suffix "]]"
      zetteldeft-tag-line-prefix nil
      zetteldeft-title-prefix "# "
      zetteldeft-backlink-prefix "Backlink: ")

;; highlight links
(font-lock-add-keywords 'markdown-mode
                        `((,(concat "\\[\\["
                                    zetteldeft-id-regex
                                    "\\]\\]")
                           . font-lock-warning-face)))

(provide 'init-notes)
