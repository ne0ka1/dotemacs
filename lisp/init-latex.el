;;; init-latex.el --- Emacs support for Latex

(straight-use-package 'pdf-tools)
(straight-use-package 'auctex)
(straight-use-package 'auctex-latexmk)

;;; PDF-tools
(pdf-loader-install) ; On demand loading, leads to faster startup time
(with-eval-after-load 'pdf-tools
  (setq-default pdf-view-use-scaling t
		pdf-view-use-imagemagick nil) ; Mac retina display
)

;;; LaTeX and auctex
(with-eval-after-load 'latex
  (setq TeX-auto-save t
	TeX-parse-self t)
  (setq-default TeX-master nil)

  ;; compile to pdf
  (tex-pdf-mode)

  ;; correlate the source and the output
  (TeX-source-correlate-mode)

    ;; set a correct indentation in a few additional environments
    (add-to-list 'LaTeX-indent-environment-list '("lstlisting" current-indentation))
    (add-to-list 'LaTeX-indent-environment-list '("tikzcd" LaTeX-indent-tabular))
    (add-to-list 'LaTeX-indent-environment-list '("tikzpicture" current-indentation))

;;; Use pdfview with auctex
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	Tex-view-program-list '(("PDF Toos" Tex-pdf-tools-sync-view))
	TeX-source-correlate-start-server t)

  ;; Update PDF buffers after successful LaTeX runs
  ;; (add-hook 'TeX-after-compilation-finished-hook
  ;;            #'TeX-revert-document-buffer)

  ;; electric pairs in auctex
  (setq TeX-electric-sub-and-superscript t
	LaTeX-electric-left-right-brace t
	TeX-electric-math (cons "$" "$"))

  ;; prettify
  (add-hook 'Tex-mode-hook 'prettify-symbols-mode)
  (setq prettify-symbols-unprettify-at-point 'right-edge)

  (with-eval-after-load 'auctex-latexmk
    (auctex-latexmk-setup)
    (setq 'auctex-latexmk-inherit-TeX-PDF-mode t)
    (add-hook 'TeX-mode-hook #'(lambda () (setq TeX-command-default "LatexMk")))))

(provide 'init-latex)
