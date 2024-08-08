;;; init-prog.el --- IDE experience for programming

(straight-use-package 'devdocs)         ; https://github.com/astoff/devdocs.el
(straight-use-package 'magit)           ; https://github.com/magit/magit
(straight-use-package 'evil-magit)      ; https://github.com/emacs-evil/evil-magit
(straight-use-package 'treemacs)        ; https://github.com/Alexander-Miller/treemacs
(straight-use-package 'treemacs-evil)   ; included in treemacs
(straight-use-package 'treemacs-magit)  ; included in treemacs

;;; Devdocs
(global-set-key (kbd "C-h d") 'devdocs-lookup) ; 'apropos-documentation
(setq shr-use-fonts nil)                       ; devdocs use shr to render texts
(add-hook 'c-mode-hook
          (lambda () (setq-local devdocs-current-docs '("c"))))

;;; Magit
(require 'evil-magit)
(setq evil-magit-use-y-for-yank nil)

;;; Treemacs
(global-set-key (kbd "M-o") 'treemacs-select-window)
(global-set-key (kbd "C-x t t") 'treemacs)
(global-set-key (kbd "C-x t b") 'treemacs-bookmark)
(global-set-key (kbd "C-x t f") 'treemacs-find-file)
(global-set-key (kbd "C-x t d") 'treemacs-select-directory)

(with-eval-after-load 'treemacs
  (require 'treemacs-evil)
  (require 'treemacs-magit)

  (treemacs-filewatch-mode)
  (treemacs-git-mode 'simple)

  (setq treemacs-width 25
        treemacs-wide-toggle-width 50))

(provide 'init-prog)
