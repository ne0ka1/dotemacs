;;; init.el

;;; system information
(defconst sys/winp
  (eq system-type 'windows-nt)
  "Are we running on a Windows system?")
(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")
(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

;;; update load path
(push (expand-file-name "lisp" user-emacs-directory) load-path)

;;; base
(require 'init-package)
(require 'init-defaults)
(require 'init-evil)
(require 'init-bindings)

;;; core
(require 'init-navigation)
(require 'init-completion)
(require 'init-editing)
;; (require 'init-input)
(require 'init-interface)

;;; extra
(require 'init-prog)
(require 'init-notes)
;; (require 'init-utility)

;;; org
(require 'init-org-base)
(require 'init-org-babel)
(require 'init-org-gtd)
(require 'init-org-journal)

;;; lang
(require 'init-c)
;; (require 'init-lisp)
;; (require 'init-python)
;; (require 'init-latex)

;;; custom file
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file 'noerror 'nomessage)
