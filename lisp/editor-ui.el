(use-package hl-todo
  :straight t
  ;; global hook activates hl-todo-mode for prog-mode, text-mode
  ;; mode can be explicitly defined using hl-todo-activate-in-modes variable
  :hook (after-init . global-hl-todo-mode))

(use-package highlight-parentheses
  :straight t
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (setq highlight-parentheses-colors '("Springgreen3"
                                       "IndianRed1"
                                       "IndianRed3"
                                       "IndianRed4"))
  (set-face-attribute 'highlight-parentheses-highlight nil :weight 'ultra-bold))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'editor-ui)
