;;; init-navigation.el --- Convenient navigation

;; Navigation inside and between buffer
(straight-use-package 'rg)       ; https://github.com/dajva/rg.el
(straight-use-package 'consult)  ; https://github.com/minad/consult
(straight-use-package 'iflipb)   ; https://github.com/jrosdahl/iflipb
(straight-use-package            ; https://github.com/jpkotta/openwith
  '(open-with :type git :host github :repo "jpkotta/openwith"))

;;; open url/file
(global-set-key (kbd "C-c C-o") 'find-file-at-point)

;;; recentf
(add-hook 'emacs-startup-hook 'recentf-mode)
(require 'recentf)
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-var-directory))
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-etc-directory))
(setq recentf-max-saved-items 200)

;;; Consult
(global-set-key (kbd "C-c m") 'consult-imenu)	   ; i"m"enu
(global-set-key (kbd "C-c T") 'consult-theme)	   ; "T"heme
(global-set-key (kbd "C-h s") 'consult-info)       ; 'describe-syntax
(global-set-key [remap Info-search] 'consult-info) ; key "s" in Info-mode
(global-set-key (kbd "C-x b") 'consult-buffer)     ; 'switch-buffer
(global-set-key (kbd "C-x p b") 'consult-project-buffer)     ; 'project-switch-buffer
(global-set-key (kbd "C-s") 'consult-line)         ; 'i-search
(global-set-key (kbd "M-s") 'consult-ripgrep)      ; isearch related prefix key
(global-set-key (kbd "M-g g") 'consult-goto-line)  ; 'goto-line

;;; Buffer flip
(setq iflipb-wrap-around t
      iflipb-ignore-buffers nil)
(global-set-key (kbd "C-,") 'iflipb-next-buffer)
(global-set-key (kbd "C-.") 'iflipb-previous-buffer)

;;; Buffer Management using iBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; Open-with
;; refer to https://www.emacswiki.org/emacs/OpenWith
(when sys/linuxp
  (require 'openwith)
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4" "flac" "avi" "wmv"
                  "wav" "mov" "flv" "ogm" "ogg" "mkv"))
               "vlc" '(file))
         (list (openwith-make-extension-regexp
                '("doc" "docx" "xls" "ppt" "odt" "ods" "odg" "odp"))
               "libreoffice" '(file))
         (list (openwith-make-extension-regexp
                '("pdf"))
               "okular" '(file))
         (list (openwith-make-extension-regexp
                '("epub"))
               "foliate" '(file))))
  (openwith-mode 1))

(provide 'init-navigation)
