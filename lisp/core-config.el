(set-language-environment 'utf-8)

(setq user-full-name "Jiayaun")

(when (memq window-system '(mac ns))
  (setq ns-command-modifier 'hyper
        ns-pop-up-frames nil))

(setq initial-scratch-message nil   ;; "make scratch buffer empty"
      inhibit-startup-message t)    ;; "disable splash screen"

(setq-default indent-tabs-mode nil
              fill-column 80
              standard-indent 2
              tab-width 2)

;; no beep and visual blinking
(setq ring-bell-function 'ignore
      visible-bell nil)

(setq frame-resize-pixelwise t)

;; highlight current line
(global-hl-line-mode 1)
;; no blink
(blink-cursor-mode 0)
;; prettify symbols
(global-prettify-symbols-mode 1)

;; Single space between sentences is more widespread than double
(setq sentence-end-double-space nil)

;; smooth scrolling
(setq scroll-conservatively 101
      scroll-margin 2)

;; draw underline lower
(setq x-underline-at-descent-line t)

;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook #'goto-address-prog-mode)
;; Highlight and follow bug references in comments and strings
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)
;; enable subword-mode in prog-mode
(add-hook 'prog-mode-hook #'subword-mode)

;; scroll compilation to first error or end
(setq compilation-scroll-output 'first-error)

;; Don't try to ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Use system trash for file deletion.
(setq delete-by-moving-to-trash t)

;; autosave each change
(setq bookmark-save-flag 1)

;; don't set a fringe mark at bookmarked lines
(setq bookmark-set-fringe-mark nil)

;; keep focus while navigating help buffers
(setq help-window-select t)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
;; (fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

;; don't load outdated compiled files.
(setq load-prefer-newer t)

;; inhibit annoying warnings
(setq warning-minimum-log-level :error)

(defun server-remove-kill-buffer-hook ()
  "Remove prompt if the file is opened in other clients."
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook #'server-remove-kill-buffer-hook)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode))

(use-package desktop
  :commands restart-emacs-without-desktop
  :init (desktop-save-mode)
  :config
  ;; inhibit no-loaded prompt
  (setq desktop-file-modtime (file-attribute-modification-time
                              (file-attributes
                               (desktop-full-file-name)))
        desktop-lazy-verbose nil
        desktop-load-locked-desktop t
        desktop-restore-eager 1
        desktop-restore-frames nil
        desktop-save t)

  (defun restart-emacs-without-desktop (&optional args)
    "Restart emacs without desktop."
    (interactive)
    (restart-emacs (cons "--no-desktop" args)))

  (defun desktop-read@override (desktop-read)
    "Inhibit `desktop-read' message"
    (let ((inhibit-message t))
      (funcall desktop-read)))
  (advice-add 'desktop-read :around #'desktop-read@override))

(use-package dired
  :defer t
  :config
  (setq dired-auto-revert-buffer t
        dired-create-destination-dirs 'ask
        dired-dwim-target t
        dired-listing-switches "-aBhl --group-directories-first"
        dired-vc-rename-file t))

(use-package doc-view
  :defer t
  :config
  (setq doc-view-resolution 400))

(use-package elec-pair
  :hook (after-init . electric-pair-mode))

(use-package ediff
  :hook ((ediff-quit . winner-undo)
         (ediff-prepare-buffer . outline-show-all))
  :config
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally
                ediff-merge-split-window-function 'split-window-horizontally))

(use-package files
  :init
  (setq make-backup-files nil        ;; don't create backup~ files
        revert-without-query '(".*") ;; disable revert query
        enable-remote-dir-locals t)
  :config
  ;; Prompt to open file literally if large file.
  (defun check-large-file ()
    "Check when opening large files - literal file open."
    (let* ((filename (buffer-file-name))
           (size (nth 7 (file-attributes filename))))
      (when (and
             (not (memq major-mode '(archive-mode tar-mode jka-compr git-commit-mode image-mode
                                                  doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
                                                  pdf-view-mode tags-table-mode fundamental-mode)))
             size (> size (* 1024 1024 1))
             (y-or-n-p (format (concat "%s is a large file, open literally to "
                                       "avoid performance issues?")
                               filename)))
        (setq buffer-read-only t)
        (buffer-disable-undo)
        (fundamental-mode))))
  (add-hook 'find-file-hook #'check-large-file)

  (defun system-move-file-to-trash (filename)
    (process-file-shell-command
     (format "trash %S" (file-local-name filename))))

  (defun make-directory-maybe ()
    "Create parent directory if not exists while visiting file."
    (let ((dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p dir)
        (if (y-or-n-p (format "Directory %s does not exist,do you want you create it? " dir))
            (make-directory dir t)
          (keyboard-quit)))))
  (add-to-list 'find-file-not-found-functions 'make-directory-maybe nil #'eq))

(use-package newcomment
  :commands comment-or-uncomment
  :config
  (defun comment-or-uncomment ()
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region
         (region-beginning) (region-end))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*$"))
          (call-interactively 'comment-dwim)
        (comment-or-uncomment-region
         (line-beginning-position) (line-end-position))))))

(use-package project
  :defer t
  :config
  (setq project-current-inhibit-prompt t
        project-vc-merge-submodules nil
        project-switch-commands '((project-find-file "Find file")
                                  (project-find-regexp "Find regexp")
                                  (project-find-dir "Find directory")
                                  (project-vc-dir "VC-Dir"))
        project-switch-use-entire-map t)

  (defun project-vc-dir ()
    "Run VC-Dir in the current project's root."
    (interactive)
    (let ((project-root (project-root (project-current t))))
      (if (file-exists-p (expand-file-name ".git" project-root))
          (cond ((fboundp 'magit-status-internal)
                 (magit-status-internal project-root))
                ((fboundp 'magit-status)
                 (with-no-warnings (magit-status project-root)))
                (t
                 (vc-dir project-root)))
        (vc-dir project-root)))))

(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 50
        recentf-initialize-file-name-history nil)

  (add-to-list 'recentf-exclude `(recentf-expand-file-name ,(straight--emacs-dir "straight"))))

(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq enable-recursive-minibuffers t ; allow commands in minibuffers
        history-length 100
        savehist-autosave-interval nil
        savehist-additional-variables '(evil-jumps-history
                                        mark-ring global-mark-ring
                                        search-ring regexp-search-ring
                                        extended-command-history))

  (add-hook 'savehist-save-hook
            (defun savehist-unpropertize-variables-h ()
              "Remove text properties from `kill-ring' to reduce savehist cache size."
              (setq kill-ring
                    (mapcar #'substring-no-properties
                            (cl-remove-if-not #'stringp kill-ring))
                    register-alist
                    (cl-loop for (reg . item) in register-alist
                             if (stringp item)
                             collect (cons reg (substring-no-properties item))
                             else collect (cons reg item)))))

  (add-hook 'savehist-save-hook
            (defun savehist-remove-unprintable-registers-h ()
              "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
              ;; Save new value in the temp buffer savehist is running
              ;; `savehist-save-hook' in. We don't want to actually remove the
              ;; unserializable registers in the current session!
              (setq-local register-alist
                          (cl-remove-if-not #'savehist-printable register-alist)))))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package server
  :hook (after-init . server-mode))

(use-package simple
  :hook (before-save . delete-trailing-whitespace)
  :config
  (setq column-number-mode t
        delete-trailing-lines nil
        eval-expression-print-length nil
        eval-expression-print-level nil
        ;; save clipboard contents into kill-ring before replace them
        save-interprogram-paste-before-kill t))

(use-package tramp
  :defer t
  :config
  (setq remote-file-name-inhibit-cache 60
        tramp-default-method "ssh"
        tramp-verbose 1
        tramp-use-ssh-controlmaster-options nil
        vc-handled-backends '(SVN Git))

  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package xref
  :defer t
  :config
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references))

  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function       #'xref-show-definitions-completing-read))

(use-package whitespace
  :hook ((prog-mode . show-trailing-whitespace)
         (diff-mode . whitespace-mode)
         (diff-mode . set-whitespace-style-for-diff))
  :config
  (set-face-attribute 'whitespace-space nil :background nil)
  (set-face-attribute 'whitespace-tab nil :background nil)
  (set-face-attribute 'whitespace-indentation nil :background nil)

  (defun show-trailing-whitespace ()
    (set-face-attribute 'trailing-whitespace nil :background
                        (face-attribute 'font-lock-comment-face
                                        :foreground))
    (setq show-trailing-whitespace t))

  (defun set-whitespace-style-for-diff ()
    "Whitespace configuration for `diff-mode'"
    (setq-local whitespace-style '(face
                                   tabs
                                   tab-mark
                                   spaces
                                   space-mark
                                   trailing
                                   indentation::space
                                   indentation::tab
                                   newline
                                   newline-mark))))

(use-package winner
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init
  (setq winner-dont-bind-my-keys t)
  :config
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*")))


(setq-default custom-file (no-littering-expand-var-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(use-package ivy
  :straight t
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-count-format "%d/%d "))

(use-package counsel
  :straight t
  :bind
  (("M-x" . 'counsel-M-x)
   ("C-s" . 'swiper)
   ("C-x C-f" . 'counsel-find-file)
   ("C-x C-r" . 'counsel-recentf)
   ("C-c g" . 'counsel-git)
   ("C-c j" . 'counsel-git-grep)
   ("C-c /" . 'counsel-ag)
   ("C-x l" . 'counsel-locate)
   ("C-x C-r" . 'ivy-resume)))

(provide 'core-config)
;;; core-config.el ends here