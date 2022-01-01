(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")))

;; Bootstrap `straight.el'
(defvar bootstrap-version)
(setq straight-check-for-modifications '(find-when-checking)
      straight-host-usernames '((github . "forrestchang"))
      straight-vc-git-default-clone-depth 1)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(if init-file-debug
    (setq use-package-verbose t
          use-package-minimum-reported-time 0
          use-package-compute-statistics t
          use-package-inject-hooks t
          debug-on-error t)
  (setq use-package-expand-minimally t))
(straight-use-package 'use-package)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :straight t
  :defer t
  :init
  (setq exec-path-from-shell-arguments nil
        exec-path-from-shell-variables '("PATH" "MANPATH"
                                         "GNUPGHOME" "SSH_AUTH_SOCK"
                                         "WAKATIME_HOME"))
  (exec-path-from-shell-initialize))

(use-package no-littering :straight t :defer t)

(use-package restart-emacs
  :straight t
  :commands restart-emacs-debug-init
  :config
  (defun restart-emacs-debug-init (&optional args)
    "Restart emacs and enable debug-init."
    (interactive)
    (restart-emacs (cons "--debug-init" args))))

(provide 'core-packages)
