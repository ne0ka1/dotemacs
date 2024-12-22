;;; init-markdown.el --- markdown configurations

(require 'init-evil)

(straight-use-package 'markdown-mode)   ; https://jblevins.org/projects/markdown-mode/
; https://github.com/Somelauw/evil-markdown
(straight-use-package
 '(evil-markdown :type git :host github :repo "Somelauw/evil-markdown"))

;;; Markdown-mode
(setq markdown-enable-wiki-links t
      markdown-asymmetric-header t
      markdown-hide-urls t
      ;; markdown-hide-markup t
      markdown-fontify-code-blocks-natively t)

(customize-set-variable 'markdown-header-scaling t)
; in sync with solarized-theme default
(customize-set-variable 'markdown-header-scaling-values '(1.3 1.2 1.15 1.1 1.05 1.0))

;; preview
(defun my-markdown-preview-file ()
  "open Markdown preview on current file"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked\\ 2.app %s"
           (shell-quote-argument (buffer-file-name)))))

(with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "C-c C-m") #'my-markdown-preview-file))

;;; evil-markdown
(require 'evil-markdown)

(provide 'init-markdown)
