;; init-evil.el

;; Evil Mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode))

(use-package evil-indent-textobject
  :ensure t)

(use-package evil-org
  :ensure t
  :config
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
  (add-hook 'org-mode-hook (lambda () (evil-org-mode))))

(use-package powerline-evil
  :ensure t
  :config
  (powerline-evil-vim-color-theme))

(use-package general
  :ensure t
  :config
  (general-create-definer my-leader-def
                          :prefix "SPC")
  (general-create-definer my-local-leader-def
                          :prefix "SPC m")
  )

(provide 'init-evil)
