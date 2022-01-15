;; -*- lexical-binding: t; -*-

;; Garbage collection

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

(provide 'init-performance)
