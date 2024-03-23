;;; init-utility.el --- Utilities of Emacs

(straight-use-package 'goldendict)
(straight-use-package 'activity-watch-mode)
(straight-use-package 'elfeed)          ; https://github.com/skeeto/elfeed
(straight-use-package 'elfeed-org)     ; https://github.com/remyhonig/elfeed-org

;;; Goldendict
(require 'goldendict)
(global-set-key (kbd "C-h w") 'goldendict-dwim) ; where-is

;;; Activity Watch
(add-hook 'emacs-startup-hook 'global-activity-watch-mode)

;;; elfeed
(setq shr-use-fonts nil)                ; elfeed use shr to display articles
(elfeed-org)

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

(provide 'init-utility)
