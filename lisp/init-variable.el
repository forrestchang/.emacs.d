;; -*- lexical-binding: t; -*-

(setq *IS-MACOS* (eq system-type 'darwin))

(setq jy/home (concat (getenv "HOME") "/"))
(setq jy/dropbox (concat jy/home "Dropbox/"))

(setq trash-directory (concat jy/home ".Trash"))

(provide 'init-variable)
