;; init-ui.el

(load-theme 'wombat t)

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t
        sml/theme 'respectful)
  (sml/setup))

;; Better font
;; https://blog.csdn.net/xh_acmagic/article/details/78939246
(defun +zjy/better-font()
  (interactive)
  ;; english font
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :font (format   "%s:pixelsize=%d" "Sarasa Mono SC" 19)) ;; 11 13 17 19 23
        ;; chinese font
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Sarasa Mono SC")))) ;; 14 16 20 22 28
    ))

(defun +zjy/init-font(frame)
  (with-selected-frame frame
    (if (display-graphic-p)
        (+zjy/better-font))))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions #'+zjy/init-font)
  (+zjy/better-font))

(provide 'init-ui)
