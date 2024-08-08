;;; init-interface.el --- Minimal and elegant interface for Emacs

;; Font, Theme, Dashboard, Modeline
; https://github.com/purcell/color-theme-sanityinc-tomorrow
(straight-use-package 'color-theme-sanityinc-tomorrow)
(straight-use-package 'dashboard)       ; https://github.com/emacs-dashboard/emacs-dashboard
(straight-use-package 'anzu)            ; https://github.com/emacsorphanage/anzu
(straight-use-package 'evil-anzu)       ; https://github.com/emacsorphanage/evil-anzu
(straight-use-package 'nano-modeline)   ; https://github.com/rougier/nano-modeline
(straight-use-package                   ; https://github.com/manateelazycat/awesome-tray
 '(awesome-tray :type git :host github :repo "manateelazycat/awesome-tray"))

;;; Font
(cond
 (sys/winp
  (set-face-attribute 'default nil :family "Consolas" :height 140))
 (sys/macp
  (set-face-attribute 'default nil :family "SFMono Nerd Font" :height 170)
  (set-face-attribute 'variable-pitch nil :family "Palatino" :height 170)
  (set-fontset-font t 'symbol "Apple Color Emoji" nil 'prepend))
 (sys/linuxp
  (set-face-attribute 'default nil :family "SFMono Nerd Font Mono" :height 130)
  (set-face-attribute 'variable-pitch nil :family "Libertinus Sans" :height 140)
  (set-face-attribute 'fixed-pitch-serif nil :family "SFMono Nerd Font Mono" :height '130)
  ;; Emacs recognize Apple Emoji (font already installed)
  (set-fontset-font t 'symbol "Apple Color Emoji" nil 'prepend)))

;; shortcut for change font size
(defun my-change-font-size (new-size)
  "Change the font size to the given value"
  (interactive "nNew font size (default 17): ")
  (set-face-attribute 'default nil :family "SFMono Nerd Font" :height (* 10 new-size)))
(global-set-key (kbd "C-x -") #'my-change-font-size)

;;; Theme
(load-theme 'sanityinc-tomorrow-night t)

;;; Dashboard
(dashboard-setup-startup-hook)
(setq dashboard-startup-banner 2
      dashboard-items '((agenda . 5)(recents . 5)(projects . 5)))
(setq dashboard-agenda-tags-format nil
      dashboard-agenda-sort-strategy '(time-up)
      dashboard-agenda-time-string-format "%m-%d")
(setq dashboard-footer-messages '("If today were the last day of my life, would I want to do what I am about to do today?" "Your time is limited, so don't waste it living someone else's life."))
(define-key dashboard-mode-map (kbd "SPC") 'dashboard-return)

;;; anzu
(setq anzu-cons-mode-line-p nil)
(add-hook 'emacs-startup-hook 'global-anzu-mode)
(with-eval-after-load 'evil
  (require 'evil-anzu))

;;; nano-modeline
(require 'nano-modeline)

(add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
(add-hook 'text-mode-hook            #'nano-modeline-text-mode)
(add-hook 'org-mode-hook             #'nano-modeline-org-mode)
(add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
(add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
(add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
(add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
(add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
(add-hook 'term-mode-hook            #'nano-modeline-term-mode)
(add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
(add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
(add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
(add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)

(setq nano-modeline-padding '(0.1 . 0.1))
(nano-modeline-text-mode)

;;; awesome-tray
(setq awesome-tray-active-modules
      '("anzu" "clock")
      awesome-tray-date-format "%-H:%-M"
;; already hide the mode line in early-init.el by setting mode-line-format.
;; any packages that mess with the mode-line can re-enable the mode-line.
;; disable them in their own settings, like anzu.
      awesome-tray-hide-mode-line nil)
(add-hook 'emacs-startup-hook 'awesome-tray-mode)

(provide 'init-interface)
