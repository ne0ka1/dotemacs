;;; init-dired.el --- config for dired

(straight-use-package 'diredfl)         ; https://github.com/purcell/diredfl

;;; vanilla dired
(with-eval-after-load 'dired-mode
  (define-key dired-mode-map (kbd "M-s") nil)) ; conflict with consult-ripgrep

;; Guess a default target directory
(setq dired-dwim-target t)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always)

;; use GNU ls as `gls' from `coreutils' if available.
(when sys/macp
  (setq insert-directory-program "gls"
  ls-lisp-use-insert-directory-program t))

;; show directory first
(setq dired-listing-switches "-alh --group-directories-first")

;; colorful dired
(add-hook 'dired-mode-hook 'diredfl-mode)

;;; dired-x
(with-eval-after-load 'dired
  (require 'dired-x)
  (let ((cmd (cond (sys/macp "open")
                   (sys/linuxp "xdg-open")
                   (sys/winp "start")
                   (t ""))))
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.\\(?:doc\\|docx\\|xls\\|ppt\\|pptx\\)\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\|wav\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.epub?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))
  
  
  (setq dired-omit-files
        (concat dired-omit-files
              "\\|^.DS_Store$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

(provide 'init-dired)
