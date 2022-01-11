;; -*- lexical-binding: t; -*-

;; ----------------------------------------------------------------------
;; Package
;; ----------------------------------------------------------------------

(require 'package)

;; ELPA and NonGNU ELPA are default in Emacs28
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

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
  (setq which-key-idle-delay 0.3))

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

   ;; Files
   "f" '(nil :which-key "file")
   "ff" '(counsel-find-file :which-key "find file")
   "fr" '(counsel-recentf :which-key "recent files")
   "fs" '(save-buffer :which-key "save buffer")
   "fS" '(evil-write-all :which-key "save all buffer")
   "fo" '(reveal-in-osx-finder :which-key "reveal in finder")

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
   )

  )

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
  (setq compand-idle-delay 0.35)
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
  )
