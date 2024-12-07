;;; init-org-journal.el

;; journal.org file for monthly journal

;;; Journal Capture
(defun my-journal-capture-target ()
  (find-file "~/org/journal.org")
  (let ((date (format-time-string "%Y-%m-%d %A")))
    (goto-char (org-find-exact-headline-in-buffer date nil t))))

(add-to-list org-capture-templates
    ("j" "Journal" entry (function my-journal-capture-target)
     "** %<%R> %?"))

;;; Journal helper
;; Run this helper function to populate the journal file for a month
(require 'calendar)

(defun my-insert-journal-entry ()
  "Insert lines like '* YYYY-MM-DD Day' for the actual number of days in the current month."
  (interactive)
  (let* ((current-time (current-time))
         (decoded-time (decode-time current-time))
         (year (nth 5 decoded-time))
         (month (nth 4 decoded-time))
         (days-in-month (calendar-last-day-of-month month year)))
    (dotimes (day days-in-month)
      (let ((date (encode-time 0 0 0 (1+ day) month year)))
        (insert (format "* %s %s\n"
                        (format-time-string "%Y-%m-%d" date)
                        (format-time-string "%A" date)))))))

(provide 'init-org-journal)
