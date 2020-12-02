;; init-beancount.el

(use-package beancount
  :quelpa ((beancount :fetcher github :repo "cnsunyour/beancount.el") :upgrade t)
  :mode
  ("\\.bean\\(?:count\\)?\\'" . beancount-mode)
  :config
  (setq beancount-accounts-files
        (directory-files "~/Dropbox/Org/Beancount/account/"
                         'full
                         (rx ".beancount" eos))))

(provide 'init-beancount)
