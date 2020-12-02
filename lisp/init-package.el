;; init-package.el



(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Bootstrap `quelpa`
(use-package quelpa
  :ensure t
  :custom
  (quelpa-update-melpa-p nil)
  (quelpa-self-upgrade-p nil)
  (quelpa-checkout-melpa-p nil))

(use-package quelpa-use-package
  :ensure t
  :config
  (quelpa-use-package-activate-advice)
  :custom
  (quelpa-use-package-inhibit-loading-quelpa t))

(provide 'init-package)
