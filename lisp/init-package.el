;; -*- lexical-binding: t; -*-

(require 'package)

;; Set ELPA
(setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
                         ("melpa" . "http://elpa.zilongshanren.com/melpa/")))

(package-initialize)
(setq package-enable-at-startup nil)

(require 'use-package)

(setq use-package-always-ensure t)
(setq use-package-verbose nil)

;; Custom lisp files
;; (load (expand-file-name "private.el" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(provide 'init-package)
