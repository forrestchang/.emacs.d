(use-package solidity-mode
  :straight t
  :bind
  (("C-c C-g" . 'solidity-estimate-gas-at-point))
  :config
  (setq solitidy-comment-style 'slash))

(use-package solidity-flycheck
  :after solidity-mode
  :config
  (setq solidity-flycheck-solium-checker-active t))

(provide 'lang-solidity)
