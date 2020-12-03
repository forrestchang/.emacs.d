;;; init.el

;; Optimize gc
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Keep a ref to the actual file-name-handler
(defvar default-file-name-handler-alist file-name-handler-alist)

;; Set the file-name-handler to nil,
;; because regexing is CPU intensive
(setq file-name-handler-alist nil)

;; Reset file-name-handler-alist after initialization
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)))

;; Increase the amount of data from the process
(setq read-process-output-max (* 1024 1024))

(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                           ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load packages
(require 'init-function)
(require 'init-package)

(require 'init-basic)
(require 'init-ui)
(require 'init-editor)
(require 'init-company)

(require 'init-org)
(require 'init-beancount)

(require 'init-evil)
(require 'init-binding)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
