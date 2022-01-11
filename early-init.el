;; -*- lexical-binding: t; -*-

;; Garbage collections
(setq gc-cons-percentage 0.6)

;; Compile warnings
(setq comp-async-report-warnings-errors nil) ;; native-comp warning
(setq byte-compile-warnings '(not free-wars unresolved noruntime lexical make-locale))

;; Window configuration
(setq frame-inhibit-implied-resize t)

;; Less clutter
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; This makes the Aqua titlebar color the same as Emacs
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))