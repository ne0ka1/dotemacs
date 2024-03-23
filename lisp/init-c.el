;;; init-c.el --- C and C++ language config

;; Change the default style for CC mode to "stroustrup".
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "stroustrup")))

;; use TAB to complete
;; (when (equal tab-always-indent 'complete)
;;   (define-key c-mode-base-map [remap c-indent-line-or-region] #'completion-at-point))

(provide 'init-c)
