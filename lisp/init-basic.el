;; init-basic.el

;; Debug
(setq debug-on-error t)
(setq-default lexical-binding t)

;; Personal information
(setq user-full-name "Jiayuan")
(setq user-main-address "forrestchang7@gmail.com")


;; Disable lockfiles and backup files
(setq create-lockfiles nil
      make-backup-files nil)

;; Disable auto save
(setq auto-save-default nil)

;; Better defaults for editing
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80
              word-wrap t
              truncate-lines t)

(setq require-final-newline t)

(add-hook 'text-mode-hook #'visual-line-mode)

(defvar show-paren-delay)
(setq show-paren-delay 0.0)
(show-paren-mode t)

(defun zjy/buf-show-trailing-whitespace ()
  (interactive)
    (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'zjy/buf-show-trailing-whitespace)
(custom-set-faces '(trailing-whitespace ((t (:background "dim gray")))))

;; Replace yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; Minimalize UI
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; Open recentf
(recentf-mode 1)
(defvar recentf-max-saved-items)
(setq recentf-max-saved-items 200)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package command-log-mode)

;; Enable `which-key`
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-secondary-delay 0.01))

;; Restart emacs
(use-package restart-emacs
  :ensure t)

;; Switch buffer
(defun zjy/switch-buffer ()
  (interactive)
  (if (evil-alternate-buffer)
      (switch-to-buffer (car (evil-alternate-buffer)))
    (switch-to-buffer (other-buffer (current-buffer) t))))

;; Swipper
(use-package swiper
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
  )

;; Counsel
(use-package counsel)

(provide 'init-basic)
