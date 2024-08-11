;;; init-utility.el --- Utilities of Emacs

;; dependency of xwwp-full. should install manually
(straight-use-package 'ctable)          ; https://github.com/kiwanami/emacs-ctable

;;; Webkit browser
(add-to-list 'load-path (expand-file-name "site-lisp/xwwp-full" user-emacs-directory))
(require 'xwwp-full)
(global-set-key (kbd "C-c b") 'xwwp)
(setq browse-url-browser-function #'xwidget-webkit-browse-url)

;; the web page scrolls up while the viewpoint scrolls down
(with-eval-after-load 'xwidget
  (define-key xwidget-webkit-mode-map "j" #'xwidget-webkit-scroll-up-line)
  (define-key xwidget-webkit-mode-map "k" #'xwidget-webkit-scroll-down-line)
  (define-key xwidget-webkit-mode-map "l" #'xwwp-follow-link)
  (define-key xwidget-webkit-mode-map "f" #'xwwp-ace-toggle)
  (define-key xwidget-webkit-mode-map "y" #'xwwp-history-show)
  (define-key xwidget-webkit-mode-map "g" #'xwwp-browse-url-other-window)
)

(setq xwwp-search-prefix "https://duckduckgo.com/?q=")

(when sys/linuxp
  (straight-use-package 'goldendict)    ; https://github.com/emacsmirror/goldendict
  (straight-use-package 'elfeed)        ; https://github.com/skeeto/elfeed
  (straight-use-package 'elfeed-org)    ; https://github.com/remyhonig/elfeed-org

;;; Goldendict
  (require 'goldendict)
  (global-set-key (kbd "C-h w") 'goldendict-dwim) ; where-is

;;; elfeed
  (elfeed-org)
  (setq elfeed-search-title-max-width 60)

  ;; select entries in elfeed-search and remove them
  (defun my-elfeed-db-remove-entry (id)
    "Removes the entry for ID"
    (avl-tree-delete elfeed-db-index id)
    (remhash id elfeed-db-entries))

  (defun my-elfeed-search-remove-selected ()
    "Remove selected entries from database"
    (interactive)
    (let* ((entries (elfeed-search-selected))
	   (count (length entries)))
      (when (y-or-n-p (format "Delete %d entires?" count))      
        (cl-loop for entry in entries
	         do (my-elfeed-db-remove-entry (elfeed-entry-id entry)))))
    (elfeed-search-update--force))
)

(when sys/macp
  (straight-use-package 'nov)           ; https://depp.brause.cc/nov.el/
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(provide 'init-utility)
