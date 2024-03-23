;;; init-prog.el --- IDE experience for programming

(straight-use-package 'treemacs)
(straight-use-package 'treemacs-evil)
(straight-use-package 'treemacs-projectile)
(straight-use-package 'projectile)
(straight-use-package 'rg)

;;; Treemacs
(global-set-key (kbd "C-x t t") 'treemacs)
(global-set-key (kbd "C-x t d") 'treemacs-select-directory)
(global-set-key (kbd "C-x t f") 'treemacs-find-file)
(global-set-key (kbd "C-x t C-t") 'treemacs-find-tag)

(with-eval-after-load 'treemacs
  (require 'treemacs-evil)
  (require 'treemacs-projectile)

  (treemacs-follow-mode)
  (treemacs-filewatch-mode)

  (setq treemacs-width 25
	treemacs-follow-after-init t)
)

;;; Projectile
;; https://docs.projectile.mx/projectile/index.html
(require 'projectile)
(add-hook 'emacs-startup-hook 'projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; dispatcher for grep, rg, ag. always use rg.
(define-key projectile-mode-map (kbd "C-c p s") 'projectile-ripgrep)

(provide 'init-prog)
