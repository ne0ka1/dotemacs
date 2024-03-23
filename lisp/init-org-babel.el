;;; init-org-babel.el --- Org Babel config
;; Activating Languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)(C . t)(java . t)(python . t)))

(setq-default org-src-fontify-natively t         ; Fontify code in code blocks.
              org-src-tab-acts-natively t        ; Tab acts as in source editing
              org-confirm-babel-evaluate nil     ; No confirmation before executing code
              org-edit-src-content-indentation 0 ; No relative indentation for code blocks
              org-fontify-whole-block-delimiter-line t) ; Fontify whole block

(provide 'init-org-babel)
