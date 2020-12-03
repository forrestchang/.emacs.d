;; -*- lexical-binding: t; -*-
;; init-company.el

(use-package company
  :bind (:map company-active-map
              ("C-p" . company-select-previous)
              ("C-n" . company-select-next)
              ("C-k" . company-select-previous)
              ("C-j" . company-select-next)
              ("<tab>" . company-complete-common-or-cycle))
  :hook (after-init . global-company-mode)
  :init
  (setq company-require-match nil)
  )

(provide 'init-company)
