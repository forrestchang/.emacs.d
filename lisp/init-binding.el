;; init-binding.el

;; Misc
(map!
 :keymaps 'normal
 "SPC" 'counsel-M-x
 "q R" 'restart-emacs
 "q q" 'kill-emacs
  )
(global-set-key (kbd "C-c i d") 'insert-current-date-time)
(global-set-key (kbd "C-c i t") 'insert-current-time)

;; Buffer
(map!
 :keymaps 'normal
  "TAB" 'zjy/switch-buffer
  "b b" 'counsel-switch-buffer
  "b d" 'kill-this-buffer
  )

;; File
(map!
 :keymaps 'normal
  "f s" 'save-buffer
  "f f" 'counsel-find-file
  "f r" 'counsel-buffer-or-recentf
  )

;; Magit
(map!
  :keymaps 'normal
  "g s" 'magit-status
  )

;; Window
(map!
 :keymaps 'normal
  "w m" 'delete-other-windows
  "w -" 'split-window-below
  "w /" 'split-window-right
  "w w" 'ace-window
  )

;; Search
(map!
 :keymaps 'normal
  "s s" 'swiper
  )

;; Describe
(map!
 :keymaps 'normal
  "h v" 'describe-variable
  "h t" 'describe-theme
  "h f" 'describe-function
  )

;; org-mode
(map!
  :keymaps 'normal
  "A" (lambda () (interactive) (org-agenda nil "a"))
  "I" (lambda () (interactive) (org-capture nil "t"))
  "G" 'org-clock-goto
  )
(localmap!
  :keymaps 'org-mode-map
  :states '(normal visual)
  "r r" 'org-refile
  "i" 'org-clock-in
  "o" 'org-clock-out
  "t" 'org-todo
  "d s" 'org-schedule
  "d d" 'org-deadline
  "p" 'org-priority-up
  )

;; org-roam
(general-define-key
 :keymaps 'org-mode-map
 "C-c I" 'org-roam-insert-immediate
 )

(map!
  :keymaps 'normal
  "T"'org-roam-dailies-find-today
  "n f" 'org-roam-find-file
  "n r r" 'org-roam-buffer-toggle-display
  )

(provide 'init-binding)
