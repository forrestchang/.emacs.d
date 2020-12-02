;; init-org.el

;; Misc settings
(setq org-directory "~/Dropbox/Org")
(setq org-default-notes-file "~/Dropbox/Org/Roam/inbox.org")
(setq org-export-with-sub-superscripts nil)
(setq org-log-reschedule (quote time))
(add-hook 'org-mode-hook 'org-indent-mode)

;; Set todo keywords
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "MAYBE(m)" "WAITING(w@/!)" "BLOCKED(b@/!)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "PROJ(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              )))

(setq org-todo-keyword-faces
      '(("TODO" . "#008891")
        ("STARTED" . "#ff9642")
        ("MAYBE" . "#cdc9c3")
        ("WAITING" . "#9088d4")
        ("BLOCKED" . "#d7385e")
        ("PROJ" . "#7579e7")
        ))

;; Image
(setq org-image-actual-width '(650))

;; Block tasks when have not done subtasks
(setq org-enforce-todo-dependencies t)

;; Block tasks when have not done checkobx
(setq org-enforce-todo-checkbox-dependencies t)

;; Using unique ID's for links in Org-mode
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
      org-clone-delete-id t)

;; org-agenda-files
(setq org-agenda-files '(
                         "~/Dropbox/Org/Roam/inbox.org"
                         "~/Dropbox/Org/Roam/inbox_beorg.org"
                         "~/Dropbox/Org/Roam/inbox_alfred.org"
                         "~/Dropbox/Org/Roam/todo.org"
                         "~/Dropbox/Org/Roam/project.org"
                         "~/Dropbox/Org/Roam/20200908201236-tiktok.org"
                         "~/Dropbox/Org/Roam/20201127172427-family.org"
                         "~/Dropbox/Org/Roam/20201127172419-personal.org"
                         ))

;; Org agenda misc settings
(setq org-agenda-span '1)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-start-day "0d")
(setq org-log-into-drawer t)
(setq org-agenda-archives-mode t)
(setq org-agenda-time-grid
      (quote
       ((daily today remove-match)
        (0800 1000 1200 1400 1600 1800 2000 2200)
        "......" "----------------")))

;; Log done date
(setq org-log-done t)

;; Non-nil means switching todo states with S-cursor counts as state change.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; Warning 30 days before deadline
(setq org-deadline-warning-days 7)

;; Org refile
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets
      '((org-agenda-files . (:maxlevel . 4))))

;; Exclude completed tasks from refile targets
;; https://michael.englehorn.com/config.html
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Org capture
(setq org-capture-templates nil)
(add-to-list 'org-capture-templates
             '("t" "Todo" entry (file "~/Dropbox/Org/Roam/inbox.org")
               (file "~/.doom.d/templates/new-todo-template.org")))
(add-to-list 'org-capture-templates
             '("q" "Quick note for clocking item" item (clock)
               "Note taken on %U \\\ \n%?" :prepend t))

;; Org clock
;; Include current clocking item
(setq org-clock-report-include-clocking-task t)

;; Reusme clocking task when emacs is restarted
(org-clock-persistence-insinuate)

;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Show lot of clocking history so it's easy to pick items off
(setq org-clock-history-length 25)

;; Separate drawers for clocking and logs
;; (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))

;; Save clock data and state changes to LOGBOOK drawer
;; (setq org-clock-into-drawer t)

;; Remove zero clock records
(setq org-clock-out-remove-zero-time-clocks t)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume t)

;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

;; Use pretty things for the clocktable
(setq org-pretty-entities t)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

;; Change tasks to whatever when clocking in
(setq org-clock-in-switch-to-state "STARTED")

;; Global effort estimate values
(setq org-global-properties
      '(("Effort_ALL" .
         "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00")))

;; Set org clock report time format
(setq org-duration-format (quote h:mm))

;; Clock mode line
(setq org-clock-mode-line-total 'auto)

;; Remove empty logbook drawers
;; https://michael.englehorn.com/config.html
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

;; (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;; Add an effort estimate on the fly when clocking in
(add-hook 'org-clock-in-prepare-hook
          'my-org-mode-ask-effort)

(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

;; Hacks
;; 让中文也可以不加空格就使用行内格式
(setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
(setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
(org-element-update-syntax)
;; 规定上下标必须加 {}，否则中文使用下划线时它会以为是两个连着的下标
(setq org-use-sub-superscripts "{}")

;; Capture images
(use-package org-download
  :init
  (setq org-download-heading-lvl nil)
  (setq org-download-image-dir "~/Dropbox/Org/Roam/Asset")
  (setq org-download-image-org-width 400)
  (setq org-download-image-html-width 400))

;; Org pomodoro
(use-package org-pomodoro
  :init (setq org-pomodoro-length '50
              org-pomodoro-short-break-length '10)
  :config
  (defvar terminal-notifier-command
    (executable-find "terminal-notifier") "/usr/local/bin/terminal-notifier")
  ;; Org notification
  (defun notify-osx (title message)
    (start-process "terminal-notifier"
                   "*terminal-notifier*"
                   terminal-notifier-command
                   "-title" title
                   "-message" message
                   "-sender" "org.gnu.Emacs"
                   "-activate" "org.gnu.Emacs"))

  (add-hook 'org-pomodoro-finished-hook
            (lambda ()
              (notify-osx "Pomodoro completed!" "Time for a break.")))

  (add-hook 'org-pomodoro-break-finished-hook
            (lambda ()
              (notify-osx "Pomodoro Short Break Finished" "Ready for Another?")))

  (add-hook 'org-pomodoro-long-break-finished-hook
            (lambda ()
              (notify-osx "Pomodoro Long Break Finished" "Ready for Another?")))

  (add-hook 'org-pomodoro-killed-hook
            (lambda ()
              (notify-osx "Pomodoro Killed" "One does not simply kill a pomodoro!")))
  )


;; org-ql & org agenda views
(use-package org-ql)
(setq org-agenda-custom-commands
      '(("a" "Cumstom Agenda View"
         ((org-ql-block '(and (todo "TODO" "STARTED")
                              (scheduled :on today)
                              (category "Routine"))
                        ((org-ql-block-header "Routines")))
          (org-ql-block '(and (todo "TODO" "STARTED")
                              (priority "A")
                              (or (scheduled :on today) (deadline :on today)))
                        ((org-ql-block-header "Today MITs")))
          (org-ql-block '(and (todo "TODO" "STARTED")
                              (or (scheduled :on today) (deadline :on today))
                              (not (priority "A"))
                              (not (category "Routine")))
                        ((org-ql-block-header "Today Tasks")))
          (org-ql-block '(and (todo "TODO" "STARTED")
                              (or (scheduled :to today) (deadline :to today))
                              (not (ts :on today)))
                        ((org-ql-block-header "Overscheduled")))
          (org-ql-block '(and (clocked :on today)
                              (not (category "Routine")))
                        ((org-ql-block-header "Today Clocked")))
          (org-ql-block '(and (closed :on today))
                        ((org-ql-block-header "Today Done")))
          (agenda)
          (org-ql-block '(and (todo "BLOCKED"))
                        ((org-ql-block-header "Blocked Tasks")))
          (org-ql-block '(and (todo "PROJ"))
                        ((org-ql-block-header "Projects")))
          (org-ql-block '(and (closed :on -1))
                        ((org-ql-block-header "Yesterday Done")))
          ))
        ))

;; Org roam
(use-package org-roam
  :init
  (setq org-roam-directory (file-truename "~/Dropbox/Org/Roam")
        org-roam-dailies-directory "Daily/"
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-tag-sources '(prop last-directory)
        org-id-link-to-org-use-id t
        org-roam-verbos nil))

(use-package org-roam-server
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8899
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-network-label-truncate-length 60
        org-roam-server-serve-files t
        org-roam-server-network-label-wrap-length 20)
  (defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph"
    (interactive)
    (org-roam-server-mode 1)
    (browse-url-default-browser (format "http://localhost:%d" org-roam-server-port)))
  )

(provide 'init-org)
