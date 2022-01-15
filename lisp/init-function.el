;; -*- lexical-binding: t; -*-

(defun string-rtrim (str)
  "Remove trailing white-space from a string."
  (replace-regexp-in-string "[ \t\n]*$" "" str))

(defun jy/split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun jy/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

;; From https://gist.github.com/3402786
(defun jy/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

(provide 'init-function)
