;; -*- lexical-binding: t; -*-

;; ----------------------------------------------------------------------
;; Package
;; ----------------------------------------------------------------------

(require 'package)

;; Set ELPA
(setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
                         ("melpa" . "http://elpa.zilongshanren.com/melpa/")))

;; Package list
(setq package-list '(
					 ;; Emacs
					 use-package gcmh exec-path-from-shell
					 ))

(package-initialize)
(setq package-enable-at-startup nil)

;; Install packages that aren't installed
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless
	  (package-installed-p package) (package-install package)))


(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose nil)


;; ----------------------------------------------------------------------
;; Global variables and settings
;; ----------------------------------------------------------------------

(setq *is-macos* (eq system-type 'darwin))

(setq jy/home (concat (getenv "HOME") "/"))
(setq jy/dropbox (concat jy/home "Dropbox/"))

(setq trash-directory (concat jy/home ".Trash"))


;; ----------------------------------------------------------------------
;; Global functions
;; ----------------------------------------------------------------------

(defun string-rtrim (str)
  "Remove trailing white-space from a string."
  (replace-regexp-in-string "[ \t\n]*$" "" str))

(defun jy/split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun jy/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

;; From https://gist.github.com/3402786
(defun jy/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))


;; ----------------------------------------------------------------------
;; Garbage Collections
;; ----------------------------------------------------------------------

(use-package gcmh
  :diminish gcmh-mode
  :config
  (setq gcmh-idle-delay 5
		gcmh-high-cons-threshold (* 16 1024 1024)) ;; 16mb
  (gcmh-mode 1))

(add-hook 'emacs-startup-hook
		  (lambda ()
			(setq gc-cons-percentage 0.1))) ;; default value for `gc-cons-percentage`

(add-hook 'emacs-startup-hook
		  (lambda ()
			(message "Emacs ready in %s with %d garbage collections."
					 (format "%.2f seconds"
							 (float-time
							  (time-subtract after-init-time before-init-time)))
					 gcs-done)))

;; ----------------------------------------------------------------------
;; Load custom lisp files
;; ----------------------------------------------------------------------

;; (load (expand-file-name "private.el" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)


;; ----------------------------------------------------------------------
;; Path
;; ----------------------------------------------------------------------

(use-package exec-path-from-shell
  :config
  (progn
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))))


;; ----------------------------------------------------------------------
;; General configuration
;; ----------------------------------------------------------------------

(winner-mode 1)

;; Replace "yes" or "no" to "y" and "n"
(fset 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs 'yes-or-no-p)

;; Major mode of new buffers
(set initial-major-mode 'org-mode)

;; Don't resize the frames in steps
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)

;; When opening a file on Mac, use an existing frame
(setq ns-pop-up-frames nil)

;; Lines

(setq-default truncate-lines t)
(setq-default tab-width 4)
(setq-default fill-column 80)

(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0.1
		show-paren-highlight-openparen t
		show-paren-when-point-inside-paren t
		show-paren-when-point-in-periphery t)
  (show-paren-mode 1))

(setq sentence-end-double-space nil)

;; Scrolling
(setq scroll-conservatively 101)

(setq
 ;; If the frame contains multiple windows, scroll the one under the cursor
 ;; instead of the one that currently has keyboard focus.
 mouse-wheel-follow-mouse 't
 ;; Completely disable mouse wheel acceleration to avoid speeding away.
 mouse-wheel-progressive-speed nil
 ;; Make each scroll-event move 2 lines at a time. Simply hold down shift to
 ;; move twice as fast, or hold down control to move 3x as fast.
 mouse-wheel-scroll-amount '(2 ((shift) . 4) ((control) . 6))
 )

;; Sane trackpad/mouse scroll settings
(setq mac-redisplay-dont-reset-vscroll t
	  mac-mouse-wheel-smooth-scroll nil)

(setq visible-bell nil)

(setq line-move-visual t) ;; C-p, C-n use visual lines

;; Use system trash
(setq delete-by-moving-to-trash t)

;; Backups/Lockfiles
;; Don't generate backups and lockfiles
(setq create-lockfiles nil
	  make-backup-files nil
	  version-control t
	  backup-by-copying t
	  delete-old-versions t
	  kept-old-versions 5
	  kept-new-versions 5
	  back-directory-alist (list (cons "." (concat user-emacs-directory "backup/"))))

(use-package recentf
  :ensure nil
  :config
  (setq recentf-auto-cleanup 'never
		recentf-max-menu-items 0
		recentf-max-saved-items 200))

;; Encoding
(setq locale-coding-system 'utf-8)

;; Misc optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq save-interprogram-paste-before-kill t
	  apropos-do-all t
	  mouse-yank-at-point t)

;; ----------------------------------------------------------------------
;; Keyboard
;; ----------------------------------------------------------------------

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  (which-key-setup-minibuffer)
  :config
  (setq which-key-idle-delay 0.1))

(use-package evil
  :init
  (setq evil-want-keybinding t
		evil-want-fine-undo t
		evil-want-keybinding nil)

  :config

  ;; (evil-set-initial-state 'dashboard-mode 'motion)

  ;; ---- Keybinding
  (define-key evil-motion-state-map "/" 'swiper)
  (define-key evil-window-map "\C-w" 'evil-delete-buffer)
  (define-key evil-motion-state-map "\C-b" 'evil-scroll-up)

  ;; ---- Settings cursor colors
  (setq evil-emacs-state-cursor    '("#649bce" box))
  (setq evil-normal-state-cursor   '("#ebcb8b" box))
  (setq evil-operator-state-cursor '("#ebcb8b" hollow))
  (setq evil-visual-state-cursor   '("#677691" box))
  (setq evil-insert-state-cursor   '("#eb998b" (bar . 2)))
  (setq evil-replace-state-cursor  '("#eb998b" hbar))
  (setq evil-motion-state-cursor   '("#ad8beb" box))

  ;; Evil-like keybinds for custom-mode-map
  (evil-define-key nil 'custom-mode-map
	;; moion
	(kbd "C-j") 'widget-forward
	(kbd "C-k") 'widget-backward
	"q" 'Custom-buffer-done)

  ;; Kill buffer instead of hide buffer in some of those pesky modes
  (dolist (mode '(help-mode-map
				  calendar-mode-map
				  (evil-define-key 'motion mode "q" 'kill-this-buffer))))

  (evil-mode 1)
  )

(use-package evil-surround
  :defer 2
  :config
  (global-evil-surround-mode 1))

(use-package evil-snipe
  :diminish evil-snipe-mode
  :diminish evil-snipe-local-mode
  :after evil
  :config
  (evil-snipe-mode +1))

(use-package general
  :config

  ;; ---- Space leader key

  ;; Preamble
  (general-define-key
   :states '(normal motion visual)
   :keymaps 'override
   :prefix "SPC"

   ;; Top level functions
   "/" '(counsel-rg :which-key "ripgrep")
   ":" '(projectile-find-file :which-key "project find file")
   "." '(counsel-find-file :which-key "find file")
   "," '(counsel-recentf :which-key "rencet files")
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "SPC" '(counsel-M-x :which-key "M-x")
   "qq" '(save-buffers-kill-emacs :which-key "quick emacs")

   ;; Buffer
   "b" '(nil :which-key "buffer")
   "bb" '(counsel-switch-buffer :which-key "switch buffers")
   "bd" '(evil-delete-buffer :which-key "delete buffer")

   ;; Code
   "c" '(nil :which-key "code")
   "cf" '(format-all-buffer :which-key "format code")
   "cl" '(comment-line :which-key "comment/uncomment line")

   ;; Files
   "f" '(nil :which-key "file")
   "ff" '(counsel-find-file :which-key "find file")
   "fr" '(counsel-recentf :which-key "recent files")
   "fs" '(save-buffer :which-key "save buffer")
   "fS" '(evil-write-all :which-key "save all buffer")
   "fo" '(reveal-in-osx-finder :which-key "reveal in finder")

   ;; Git
   "g" '(nil :which-key "git")
   "gs" '(magit-status :which-key "git status")
   "gfl" '(magit-log-buffer-file :which-key "file git log")

   ;; Help
   "h" '(nil :which-key "help/emacs")
   "hv" '(counsel-describe-variable :which-key "describe variable")
   "hb" '(counsel-descbinds :which-key "describe bindings")
   "hM" '(describe-mode :which-key "describe mode")
   "hf" '(counsel-describe-function :which-key "describe func")
   "hF" '(counsel-describe-face :which-key "describe face")
   "hk" '(describe-key :which-key "describe key")

   ;; Toggles
   "t" '(nil :which-key "toggles")
   "tT" '(toggle-truncate-lines :which-key "truncate lines")
   "tv" '(visual-line-mode :which-key "visual line mode")
   "tn" '(display-line-numbers-mode :which-key "display line numbers")
   "ta" '(mixed-pitch-mode :which-key "variable pitch mode")
   "tc" '(visual-fill-column-mode :which-key "visual fill column mode")
   "tt" '(counsel-load-theme :which-key "load theme")
   "tw" '(writeroom-mode :which-key "writeroom-mode")
   "tR" '(read-only-mode :which-key "read only mode")
   "tr" '(display-fill-column-indicator-mode :which-key "fill column indicator")
   "tm" '(hide-mode-line-mode :which-key "hide modeline mode")

   ;; Windows
   "w" '(nil :which-key "window")
   "ww" '(ace-window :which-key "ace window")
   "wd" '(evil-window-delete :which-key "delete window")
   "wm" '(jy/toggle-maximize-buffer :which-key "maximize buffer")
   "w-" '(jy/split-window-vertically-and-switch :which-key "split below")
   "w/" '(jy/split-window-horizontally-and-switch :which-key "split right")
   "wl" '(evil-window-right :which-key "evil-window-right")
   "wh" '(evil-window-left :which-key "evil-window-left")
   "wj" '(evil-window-down :which-key "evil-window-down")
   "wk" '(evil-window-up :which-key "evil-window-up")
   "wz" '(text-scale-adjust :which-key "text zoom")

   ;; Search
   "s" '(nil :which-key "search")
   "ss" '(swiper :which-key "search buffer")
   ) ;; End leader prefix general.el block

  (general-def
	:prefix ","
	:states 'motion
	:keymaps 'emacs-lisp-mode-map
	"" nil
	"e" '(nil :which-key "eval")
	"es" '(eval-last-sexp :which-key "eval sexp")
	"er" '(eval-region :which-key "eval region")
	"eb" '(eval-buffer :which-key "eval buffer")
	"c" '(check-parens :which-key "check parens")
	)

  (general-def
	:states '(normal visual motion)
	"j" 'evil-next-visual-line ;; visual line instead of actual line
	"k" 'evil-previous-visual-line
	)

  (general-def
	:states '(insert)
	"C-a" 'evil-beginning-of-visual-line
	"C-e" 'evil-end-of-visual-line
	"C-S-a" 'evil-beginning-of-line
	"C-S-e" 'evil-end-of-line
	"C-n" 'evil-next-visual-line
	"C-p" 'evil-previous-visual-line
	)

  ;; Xwidget ------
  ;; (general-define-key :states 'normal :keymaps 'xwidget-webkit-mode-map
  ;; 					"j" 'xwidget-webkit-scroll-up-line
  ;; 					"k" 'xwidget-webkit-scroll-down-line
  ;; 						"gg" 'xwidget-webkit-scroll-top
  ;; 						"G" 'xwidget-webkit-scroll-bottom)

  )


;; More configs about hydra, can refer to
;; https://github.com/jakebox/jake-emacs
(use-package hydra
  :defer t)


;; ----------------------------------------------------------------------
;; Completion
;; ----------------------------------------------------------------------

(use-package company
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (setq compand-minimum-prefix-length 2
		company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not erc-mode message-mode help-mode gud-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
          company-echo-metadata-frontend)  ; show selected candidate docs in echo area
        company-backends '(company-capf company-files company-keywords)
        company-auto-complete nil
        company-auto-complete-chars nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

  :config
  (setq compand-idle-delay 0.1)
  :custom-face
  (company-tooltip ((t (:family "Jetbrains Mono")))))

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-extra-directories nil) ;; hide . and .. directories
  (setq ivy-initial-inputs-alist nil) ;; remove the ^ in ivy searches
  ;; (setq ivy-fixed-height-minibuffer t)
  (ivy-mode 1)

  ;; Shows a preview of the face in counsel-describe-face
  (add-to-list 'ivy-format-functions-alist '(counsel-describe-face . counsel--faces-format-function))

  :general
  (general-define-key
   :keymaps '(ivy-minibuffer-map ivy-switch-buffer-map)
   "C-k" 'ivy-previous-line
   "C-j" 'ivy-next-line
   )
  )

(use-package all-the-icons-ivy-rich
  :init
  (all-the-icons-ivy-rich-mode 1)
  :config
  (setq all-the-icons-ivy-rich-icon-size 1.0))

(use-package ivy-rich
  :after (ivy)
  :init
  (setq ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :config
  (setq counsel-switch-buffer-preview-virtual-buffers nil) ;; Removes recentfiles/bookmarks from counsel-switch-buffer
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; Hides file names beginning with .
         "\\(?:\\`[#.]\\)"))

  ;; Sorts counsel-recentf in order of time last accessed
  (add-to-list 'ivy-sort-functions-alist
               '(counsel-recentf . file-newer-than-file-p))

  (add-to-list 'recentf-exclude
               (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

  (setq-default counsel--fzf-dir jy/home)
  )

(use-package prescient
  :config
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000)
  (prescient-persist-mode +1)
  )

(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode +1))

(use-package company-prescient
  :defer 2
  :after company
  :config
  (company-prescient-mode +1))

;; From Doom Emacs
(use-package smartparens
  :diminish smartparens-mode
  :defer 1
  :config
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  (setq sp-max-prefix-length 25)
  (setq sp-max-pair-length 4)
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)

  (with-eval-after-load 'evil
    (setq sp-show-pair-from-inside t)
    (setq sp-cancel-autoskip-on-backward-movement nil)
    (setq sp-pair-overlay-keymap (make-sparse-keymap)))

  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))

  ;; In lisps ( should open a new form if before another parenthesis
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))


  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             ;; Don't autopair opening braces if before a word character or
             ;; other opening brace. The rationale: it interferes with manual
             ;; balancing of braces, and is odd form to have s-exps with no
             ;; whitespace in between, e.g. ()()(). Insert whitespace if
             ;; genuinely want to start a new form in the middle of a word.
             :unless '(sp-point-before-word-p sp-point-before-same-p)))
  (smartparens-global-mode t))

;; Search and replace
(use-package evil-anzu :defer t)

(use-package simpleclip
  :config
  (simpleclip-mode 1))

(use-package undo-fu
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "U" 'undo-fu-only-redo))

(use-package super-save
  :diminish super-save-mode
  :defer 2
  :config
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration 5) ;; after 5 seconds of not typing autosave
  (setq super-save-triggers ;; Functions after which buffers are saved (switching window, for example)
        '(evil-window-next evil-window-prev balance-windows other-window))
  (super-save-mode +1))

;; After super-save autosaves, wait __ seconds and then clear the buffer. I don't like
;; the save message just sitting in the echo area.
(defun jy-clear-echo-area-timer ()
  (run-at-time "2 sec" nil (lambda () (message " "))))

(advice-add 'super-save-command :after 'jy-clear-echo-area-timer)

;; Saveplace
(use-package saveplace
  :init (setq save-place-limit 100)
  :config (save-place-mode))

;; ----------------------------------------------------------------------
;; UI
;; ----------------------------------------------------------------------

(setq text-scale-mode 1.1)

(setq-default line-spacing 0.00)

(set-face-attribute 'default nil
					:family "Jetbrains Mono"
					:weight 'regular
					:height 180)

(line-number-mode)
(column-number-mode)
(display-time-mode -1)
(size-indication-mode -1)

(use-package doom-modeline
  :init (doom-modeline-mode)
  :config
  (setq doom-modeline-enable-word-count t
		doom-modeline-buffer-encoding nil
		doom-modeline-icon nil
		doom-modeline-bar-width 3)
  )

(use-package mixed-pitch
  :defer t
  :config
  (setq mixed-pitch-set-height t))

(setq jy-doom-modeline-text-height 135)

(use-package doom-themes
  :after mixed-pitch
  :custom-face
  ;; Keep the modeline proper every time I use these themes.
  (mode-line ((t (:height ,jy-doom-modeline-text-height))))
  (mode-line-inactive ((t (:height ,jy-doom-modeline-text-height)))))

(load-theme 'doom-one-light t)

;; Line numbers, fringe, hl-line
(setq-default fringes-outside-margins nil)
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)

(set-face-attribute 'fringe nil :background nil)
(set-face-attribute 'header-line nil :background nil :inherit 'default)

(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
				org-agenda-mode-hook
				term-mode-hook
				shell-mode-hook
				xwidget-webkit-mode-hook
				pdf-view-mode-hook
				eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-hook 'prog-mode-hook 'hl-line-mode)

;; For writing
(use-package visual-fill-column
  :defer t
  :config
  (setq visual-fill-column-width 50
        visual-fill-column-center-text t))

(use-package writeroom-mode
  :defer t
  :config
  (setq writeroom-maximize-window nil
        writeroom-header-line "" ;; Makes sure we have a header line, that's blank
        writeroom-mode-line t
        writeroom-global-effects nil) ;; No need to have Writeroom do any of that silly stuff
  (setq writeroom-width 70)
  ;; (add-hook 'writeroom-mode-hook (lambda () (setq-local line-spacing 10)))
  )

;; ----------------------------------------------------------------------
;; Programming
;; ----------------------------------------------------------------------
(use-package projectile
  :defer t
  :general
  (:keymaps 'projectile-mode-map
            "s-f" 'projectile-find-file
            "s-p" 'projectile-command-map
            "C-c p" 'projectile-command-map
            "s-c" 'projectile-commander)
  :init
  (projectile-mode +1))

(use-package rainbow-mode :defer t)

(use-package hl-todo
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
		'(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF4500")
          ("DEBUG"  . "#A020F0")
          ("WIP"   . "#1E90FF"))))

(use-package magit)

(use-package evil-collection
  :init (evil-collection-init))

(use-package ace-window)

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode +1)

  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added [224]
	nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
	nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
	nil nil 'bottom)
  )

(use-package format-all)
