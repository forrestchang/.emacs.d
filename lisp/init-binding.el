;; init-binding.el

;; Misc
(my-leader-def
 :keymaps 'normal
 "SPC" 'execute-extended-command
 "q R" 'restart-emacs
 "q q" 'kill-emacs
  )
(global-set-key (kbd "C-c i d") 'insert-current-date-time)
(global-set-key (kbd "C-c i t") 'insert-current-time)

;; Buffer
(my-leader-def
 :keymaps 'normal
  "TAB" 'zjy/switch-buffer
  "b b" 'counsel-switch-buffer
  )

;; File
(my-leader-def
 :keymaps 'normal
  "f s" 'save-buffer
  "f f" 'counsel-find-file
  "f r" 'counsel-buffer-or-recentf
  )

;; Window
(my-leader-def
 :keymaps 'normal
  "w m" 'delete-other-windows
  "w -" 'split-window-below
  "w /" 'split-window-right
  "w w" 'ace-window
  )

;; Search
(my-leader-def
 :keymaps 'normal
  "s s" 'swiper
  )

;; Describe
(my-leader-def
 :keymaps 'normal
  "h v" 'describe-variable
  "h t" 'describe-theme
  "h f" 'describe-function
  )

;; org-mode
(my-leader-def
  :keymaps 'normal
  "A" (lambda () (interactive) (org-agenda nil "a"))
  "I" (lambda () (interactive) (org-capture nil "t"))
  "G" 'org-clock-goto
  )
(my-local-leader-def
  :states 'normal
  :keymaps 'org-mode-map
  "i" 'org-clock-in
  "o" 'org-clock-out
  "t" 'org-todo
  "n" 'org-add-note
  )

;; org-roam
(general-define-key
 :keymaps 'org-mode-map
 "C-c I" 'org-roam-insert
 )

(my-leader-def
  :keymaps 'normal
  "T"'org-roam-dailies-find-today
  "n f" 'org-roam-find-file
  "n r r" 'org-roam-buffer-toggle-display
  )

(provide 'init-binding)
