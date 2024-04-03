;;; init-prog.el --- IDE experience for programming

(straight-use-package 'devdocs)
(straight-use-package 'magit)
(straight-use-package 'evil-magit)      ; https://github.com/emacs-evil/evil-magit
(straight-use-package 'treemacs)
(straight-use-package 'treemacs-evil)

;;; Devdocs
(global-set-key (kbd "C-h d") 'devdocs-lookup) ; 'apropos-documentation
(setq shr-use-fonts nil)                       ; devdocs use shr to render texts
(add-hook 'c-mode-hook
          (lambda () (setq-local devdocs-current-docs '("c"))))

;;; Magit
(require 'evil-magit)
(setq evil-magit-use-y-for-yank nil)

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

(provide 'init-prog)
