;;; init-prog.el --- IDE experience for programming
;; reference jump, documents, git client, file explorer.

(require 'init-evil)

(straight-use-package 'dumb-jump)       ; https://github.com/jacktasia/dumb-jump
(straight-use-package 'devdocs)         ; https://github.com/astoff/devdocs.el
(straight-use-package 'magit)           ; https://github.com/magit/magit
(straight-use-package 'evil-magit)      ; https://github.com/emacs-evil/evil-magit
(straight-use-package 'treemacs)        ; https://github.com/Alexander-Miller/treemacs
(straight-use-package 'treemacs-evil)   ; included in treemacs
(straight-use-package 'treemacs-magit)  ; included in treemacs
(straight-use-package 'treemacs-nerd-icons)     ; https://github.com/rainstormstudio/treemacs-nerd-icons

;;; Xref and dumb-jump
;; use faster search tool
(setq xref-search-program 'rg
      dumb-jump-prefer-searcher 'rg)
;; use consult to select xref locations with preview
(setq xref-show-definitions-function #'consult-xref
      xref-show-xrefs-function #'consult-xref)
;; use dumb-jump with xref
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;; Devdocs
(global-set-key (kbd "C-h d") 'devdocs-lookup) ; 'apropos-documentation
(setq shr-use-fonts nil)                       ; devdocs use shr to render texts
(add-hook 'c-mode-hook
          (lambda () (setq-local devdocs-current-docs '("c"))))
(add-hook 'c++-mode-hook
          (lambda () (setq-local devdocs-current-docs '("cpp"))))
(add-hook 'java-mode-hook
          (lambda () (setq-local devdocs-current-docs '("openjdk~21"))))
(add-hook 'kotlin-mode-hook
          (lambda () (setq-local devdocs-current-docs '("kotlin~1.9"))))

;;; Magit
(with-eval-after-load 'magit
  (require 'evil-magit)
  (setq evil-magit-use-y-for-yank nil))

;;; Treemacs
(global-set-key (kbd "M-0") 'treemacs-select-window)
(global-set-key (kbd "C-x t t") 'treemacs)
(global-set-key (kbd "C-x t b") 'treemacs-bookmark)
(global-set-key (kbd "C-x t f") 'treemacs-find-file)
(global-set-key (kbd "C-x t d") 'treemacs-select-directory)

(with-eval-after-load 'treemacs
  (require 'treemacs-evil)
  (require 'treemacs-magit)
  (require 'treemacs-nerd-icons)

  (treemacs-load-theme "nerd-icons")
  (treemacs-filewatch-mode)
  (treemacs-git-mode 'simple)

  (setq treemacs-width 25
        treemacs-wide-toggle-width 50))

(provide 'init-prog)
