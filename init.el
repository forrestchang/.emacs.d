;;; Load path
;; optimize: force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add 'package-initialize :after #'update-load-path)
(advice-add 'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

(require 'core-packages)
(require 'core-config)
(require 'core-functions)
(require 'core-keybinds)
