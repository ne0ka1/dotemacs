;;; init-package.el

;; Modify load path after straight.el is set up, otherwise Org version crash
(let ((straight-org
       (expand-file-name "straight/build/org" user-emacs-directory)))
  (when (file-exists-p straight-org)
    (add-to-list 'load-path straight-org)))

;; Using straight.el for package management 
(setq straight-check-for-modifications nil) ; Disable checking for speedup

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
"https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; no-littering
(straight-use-package 'no-littering)
(require 'no-littering)
(no-littering-theme-backups)

;; org
(straight-use-package 'org)
(require 'org)

;; emacs profile
(straight-use-package 'esup)

(provide 'init-package)
