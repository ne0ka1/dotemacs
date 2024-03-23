;;; init-org-journal.el --- Org journal
(straight-use-package 'org-journal)

;;; Org journal Setting
(require 'org-journal)
(add-hook 'org-journal-after-entry-create-hook #'evil-insert-state) ; New journal entry
(setq org-journal-dir "~/org/journal"
      org-journal-file-format "%Y%m.org"
      org-journal-file-type 'monthly
      org-journal-date-format "%A, %Y/%m/%d")

;;; Modify Calendar Keybinding
(with-eval-after-load 'calendar
  (define-key calendar-mode-map "j" 'calendar-forward-week) ; org-journal prefix key
  (define-key calendar-mode-map "k" 'calendar-backward-week)
  (define-key calendar-mode-map "h" 'calendar-backward-day) ; calendar-cursor-holidays
  (define-key calendar-mode-map "l" 'calendar-forward-day)
  (define-key calendar-mode-map "d" 'org-journal-display-entry) ; diary-view-entries
  (define-key calendar-mode-map "m" 'org-journal-mark-entries) ; diary-mark-entries
  (define-key calendar-mode-map "n" 'org-journal-new-date-entry) ; diary-mark-entries
  )

;;; Integrate org-journal with org-capture
(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(add-to-list 'org-capture-templates '("j" "Journal" plain (function org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                               :jump-to-captured t :immediate-finish t))

(provide 'init-org-journal)
