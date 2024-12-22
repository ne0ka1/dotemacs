;;; init-notes.el --- Writing (Markdown) notes in Emacs

(straight-use-package 'obsidian)        ; https://github.com/licht1stein/obsidian.el
(straight-use-package 'consult-notes)   ; https://github.com/mclear-tools/consult-notes

;;; My-writing-mode
(setq olivetti-body-width 0.67)

(defun my-writing-mode ()
    (interactive)
    (setq line-spacing 5)
    (cond
     (sys/macp (setq buffer-face-mode-face '(:family "iA Writer Duo S" :height 170)))
     (sys/linuxp (setq buffer-face-mode-face '(:family "iA Writer Duospace" :height 130))))
    (buffer-face-mode))

(add-hook 'org-mode-hook 'my-writing-mode)
;; (add-hook 'markdown-mode-hook 'my-writing-mode)

;;; Obsidian
(obsidian-specify-path "~/mind")

;; Define obsidian-mode bindings
(add-hook
 'obsidian-mode-hook
 (lambda ()
   ;; Replace standard command with Obsidian.el's in obsidian vault:
   (local-set-key (kbd "C-c C-o") 'obsidian-follow-link-at-point)

   ;; Use either `obsidian-insert-wikilink' or `obsidian-insert-link':
   (local-set-key (kbd "C-c C-l") 'obsidian-insert-wikilink)

   ;; Following backlinks
   (local-set-key (kbd "C-c C-b") 'obsidian-backlink-jump)))

(global-set-key (kbd "C-c o") 'obsidian-jump)
(global-obsidian-mode)

;;; consult-notes
(setq consult-notes-file-dir-sources
      '(("org" ?o "~/org/")
        ("mind" ?m "~/mind")))
(global-set-key (kbd "C-c k") 'consult-notes-search-in-all-notes)

(provide 'init-notes)
