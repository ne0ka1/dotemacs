;;; init-notes.el --- Writing (Markdown) notes in Emacs

(straight-use-package 'markdown-mode)   ; https://jblevins.org/projects/markdown-mode/
(straight-use-package 'olivetti)        ; https://github.com/rnkn/olivetti
(straight-use-package 'zk)              ; https://github.com/localauthor/zk

;;; Markdown-mode
(customize-set-variable 'markdown-header-scaling t)
;; same with solarized-theme default
(customize-set-variable 'markdown-header-scaling-values '(1.3 1.2 1.15 1.1 1.05 1.0))
(setq markdown-enable-wiki-links t
      markdown-hide-urls t
      markdown-fontify-code-blocks-natively t)

;;; My-writing-mode
(setq olivetti-body-width 0.67)

(defun my-writing-mode ()
    (interactive)
    (setq line-spacing 5)
    (olivetti-mode)
    (cond
     (sys/macp (setq buffer-face-mode-face '(:family "iA Writer Duo S" :height 170)))
     (sys/linuxp (setq buffer-face-mode-face '(:family "iA Writer Duospace" :height 130))))
    (buffer-face-mode))

(add-hook 'org-mode-hook 'my-writing-mode)
(add-hook 'markdown-mode-hook 'my-writing-mode)

;;; zk
(require 'zk)
;; (setq zk-directory "~/Notes"
(setq zk-file-extension "md"
      zk-id-time-string-format "%Y-%m-%d-%H%M"
      zk-id-regexp "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{4\\}\\)"
      zk-link-and-title-format "[[%i]] %t"
      zk-new-note-link-insert 'zk)       ; insert a link only in a zk note

;; simplify the new note header
(defun my-zk-new-note-header (title new-id &optional orig-id)
  (insert (format "# %s\n" title))
  (when (featurep 'evil) (evil-insert-state)))
(setq zk-new-note-header-function 'my-zk-new-note-header)

;; set up keybindings
(define-prefix-command 'zk-prefix)
(global-set-key (kbd "C-c k") 'zk-prefix)
(global-set-key (kbd "C-c k n") 'zk-new-note)
(global-set-key (kbd "C-c k f") 'zk-find-file)
(global-set-key (kbd "C-c k o") 'zk-follow-link-at-point)
(global-set-key (kbd "C-c k l") 'zk-links-in-note)
(global-set-key (kbd "C-c k #") 'zk-tag-insert)
(global-set-key (kbd "C-c k t") 'zk-tag-search)
(global-set-key (kbd "C-c k s") 'zk-search)
(global-set-key (kbd "C-c k i") 'zk-insert-link)
(global-set-key (kbd "C-c k b") 'zk-backlinks)

(provide 'init-notes)
