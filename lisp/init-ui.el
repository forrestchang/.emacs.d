;; init-ui.el

(load-theme 'wombat t)

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t
        sml/theme 'respectful)
  (sml/setup))

(set-face-attribute 'default nil :height 200)

(provide 'init-ui)
