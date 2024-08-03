;;; init-org-gtd.el --- Org mode GTD implementation and Expansion
;;; Items
;; Todos
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "CNCL(c@/!)" "DONE(d)"))
      org-use-fast-todo-selection 'expert)

;; Tags
;; emojis cannot align properly so give up align altogether
(setq org-tags-column 0
      org-auto-align-tags t
      org-fast-tag-selection-single-key t
      ;; place @ before tags which don't have emoji display, to avoid clash
      ;; they are placed before context emojis, because org sort according to last tag
      org-tag-alist '((:startgroup)
                      ("@easy" . ?e)
                      ("@medium" . ?m)
                      ("@difficult" . ?d)
                      ("@relax" . ?r)
                      (:endgroup)
                      (:startgroup)
                      ("cloud" . ?c)              ; :cloud: creative
                      ("fire" . ?f)               ; :fire: urgent
                      ("tv" . ?t)                 ; :tv: watch video
                      ("books" . ?b)              ; :books: read book
                      ("writing_hand" . ?w)       ; :writing_hand: write
                      ("house" . ?h)              ; :house: house
                      ("spy" . ?s)                ; :spy: social
                      ("athletic_shoe" . ?a)      ; :athletic_shoe: errands
                      (:endgroup))
      )

;; Logs
(setq org-log-repeat nil
      org-log-done 'time
      org-log-into-drawer t)

;; Projects
;; if it contains any NEXT keyword item, the project is marked unstuck
(setq org-stuck-projects
      '("+LEVEL=1-CATEGORY=\"inbox\"-CATEGORY=\"calendar\"/-DONE-CNCL-WAIT"
        ("NEXT")
        nil "\[[a-z]+\]"))

;;; Dependencies edna
(setq org-edna-use-inheritance t)
;; (org-edna-mode)

;;; Capture
(setq org-capture-templates
 '(("t" "Todo" entry (file "~/org/inbox.org")
     "* TODO %?\n%U\n\n  %i")
    ("l" "Link" entry (file "~/org/inbox.org")
     "* %?\n%U\n\n  %i\n  %a")
    ("j" "Journal" entry (file+function "~/org/journal.org" (lambda ()(format-time-string "%Y-%m-%d %A")))
     "** %<%R> %?")
))

;;; Journal
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

;;; Refile
(setq org-refile-targets '((nil :maxlevel . 9) ; Headings in the current buffer, up to level 9
                           (org-agenda-files :maxlevel . 2))
      org-refile-use-cache nil			; Don't cache refile targets
      org-refile-use-outline-path t)		; Provide refile targets as paths

;;; Archive
(setq org-archive-location "~/org/archive/log.org::datetree/::"
      org-archive-save-context-info '(file olpath itags))

;;; Time and Clock
;; General settings.
(setq org-read-date-popup-calendar nil ; Do not pop up a calendar when prompting for a date
      org-timer-default-timer 25)      ; The default timer when a timer is set, in minutes 

(setq org-global-properties '(("EFFORT_ALL" . "10min 15min 20min 30min 45min 1h 1.5h 2h 2.5h 3h")))
(setq org-columns-default-format "%25ITEM %TODO %EFFORT %CLOCKSUM_T")

;; Org clock.
(setq org-clock-in-switch-to-state "NEXT" ; Swith the item to NEXT when clocked in
      org-clock-mode-line-total 'current) ; Time displayed in the mode line, current instance

;;; Agenda
;; General
;; Agenda files
(setq org-agenda-files '("~/org/"))

;; Display settings.
(setq org-agenda-window-setup 'current-window
      org-agenda-restore-windows-after-quit t
      ;; remove context tags but not energy tags
      org-agenda-remove-tags nil
      org-agenda-hide-tags-regexp "^[^@]+"
      ;; emojis cannot align properly so give up align altogether
      org-agenda-tags-column 0
      org-agenda-sort-noeffort-is-high nil)

;; add tag in todo types
(setq org-agenda-prefix-format
'((agenda . " %i %-12:c%?-12t% s")
 (todo . " %i %-12:c\:%T\: ")
 (tags . " %i %-12:c")
 (search . " %i %-12:c"))
)

;;; Daily Agenda
;; Daily/weekly agenda items
(setq org-agenda-span 'day     ; Number of days to include in overview
      org-agenda-start-on-weekday nil ; Agenda always start on current day
      
      org-agenda-skip-scheduled-if-done t ; Don't show scheduled items when they are done
      org-agenda-skip-timestamp-if-done t ; Don't show timestamp items when they are done
      org-agenda-skip-deadline-if-done t ; Don't show deadline item when they are done

      org-deadline-warning-days 7       ; Deadline default pre-warning
      org-agenda-skip-deadline-prewarning-if-scheduled nil ; Show scheduled item's prewarning
      org-agenda-skip-additional-timestamps-same-entry t) ; Don't show multiple lines for one item

;; Agenda time grid
(setq org-agenda-use-time-grid nil
      org-agenda-time-grid
      '((daily)(800 1000 1200 1400 1600 1800 2000) "......" "-----------------")
      org-agenda-current-time-string "-----> now <-----")


;;; Agenda Todo List
(setq org-agenda-todo-ignore-scheduled t ; Ignore scheduled todos, which act like "deferred"
      org-agenda-todo-list-sublevels t ; Check sublevels for tasks (default t)
      org-agenda-tags-todo-honors-ignore-options t) ; Honor ignore options in tags-todo search

;;; Custom Agenda
  
(setq org-agenda-block-separator nil)

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda ""
                  ((org-agenda-prefix-format " %i %-12:c%?-12t% s\:%T\: ")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))))
          (tags-todo "PRIORITY=\"A\"|TODO=\"NEXT\"-PRIORITY=\"C\""
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'timestamp))
                 (org-agenda-prefix-format " %i %-12:c\:%T\: ")
                 (org-agenda-overriding-header "\nTasks\n")))
          (agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags "CLOSED>=\"<today>\"/-CNCL"
                ((org-agenda-overriding-header "\nCompleted today\n")))))

	("r" "Review"
	 ((tags "CATEGORY=\"inbox\"/-DONE-CNCL"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "Inbox\n")))
	  (stuck "" ((org-agenda-prefix-format "  %?-12(org-get-title)")
                     (org-agenda-overriding-header "\nStuck Project\n")))
          (todo "WAIT" ((org-agenda-prefix-format " %i %-12:c")
                      (org-agenda-overriding-header "\nWaiting\n"))))) ; No tag

        ("c" "Context"
           ((tags-todo "cloud/-WAIT"
                 ((org-agenda-overriding-header " :cloud: Research/Creative \n")
                  (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'timestamp))))
           (tags-todo "fire/-WAIT"
                 ((org-agenda-overriding-header "\n :fire: Urgent\n")
                  (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'timestamp))))
           (tags-todo "house/-WAIT"
                 ((org-agenda-overriding-header "\n :house: Home\n")
                  (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'timestamp))))
           (tags-todo "books/-WAIT"
                 ((org-agenda-overriding-header "\n :books: Reading")
                  (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'timestamp))))
           (tags-todo "tv/-WAIT"
                 ((org-agenda-overriding-header "\n :tv: Watching\n")
                  (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'timestamp))))
           (tags-todo "keyboard/-WAIT"
                 ((org-agenda-overriding-header "\n :writing_hand: Writing\n")
                  (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'timestamp))))
           (tags-todo "spy/-WAIT"
                 ((org-agenda-overriding-header "\n :spy: Social\n")
                  (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'timestamp))))
           (tags-todo "athletic_shoe/-WAIT"
                 ((org-agenda-overriding-header "\n :athletic_shoe: Errands\n")
                  (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'timestamp))))
           ;; Routines are the last because they typically are scheduled
           ))

        ("E" "Energy"
         ;; display context tags in prefix
         ((tags-todo "@easy/-WAIT"
                     ((org-agenda-overriding-header " 😎 Easy\n")
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format " %i %-12:c\:%T\: ")))
          (tags-todo "@medium/-WAIT"
                     ((org-agenda-overriding-header "\n 🤔 Medium\n")
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format " %i %-12:c\:%T\: ")))
          (tags-todo "@difficult/-WAIT"
                     ((org-agenda-overriding-header "\n 🤯 Difficult\n")
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format " %i %-12:c\:%T\: ")))
           (tags-todo "@relax/-WAIT"
                     ((org-agenda-overriding-header "\n 😌 Relax\n")
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format " %i %-12:c\:%T\: ")))
         ))
))

(provide 'init-org-gtd)
