;;; init-package.el

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

;; straight config
(setq straight-vc-git-default-protocol 'https
      straight-vc-git-default-clone-depth 1)

;; no-littering
(straight-use-package 'no-littering)
(require 'no-littering)
(no-littering-theme-backups)

;; emacs profile
(straight-use-package 'esup)
(setq esup-depth 0) ;; fix a bug. see https://github.com/jschaf/esup/issues/54

;; exec-path-from-shell
(straight-use-package 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(provide 'init-package)
