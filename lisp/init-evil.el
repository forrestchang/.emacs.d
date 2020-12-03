;; init-evil.el

;; Evil Mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode))

(use-package evil-indent-textobject
  :ensure t)

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  )

(use-package powerline-evil
  :ensure t
  :config
  (powerline-evil-vim-color-theme))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :ensure t
  :config
  (general-create-definer map!
    :prefix "SPC"
    :state '(normal visual))
  (general-create-definer localmap!
    :prefix "SPC m"
    :state '(normal visual))
  )


(provide 'init-evil)
