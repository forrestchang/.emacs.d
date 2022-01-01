(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *is-macos* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Bootstrap config

(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-utils)

