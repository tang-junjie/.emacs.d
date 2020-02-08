;;; setup-appearance.el --- Emacs UI settings tweaks -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1)
  (column-number-mode t))

(setq font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Default frame size
(when window-system (set-frame-size (selected-frame) 80 42))

;; Smooth scrolling
(setq scroll-step 1
      scroll-margin 6
      auto-window-vscroll nil
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;; Don't beep. Don't visible-bell (fails on el capitan). Just blink the modeline on errors.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; Highlight current line
(global-hl-line-mode 1)

;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))
(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

;; Load default color theme default-black
(load-theme 'default-black)

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; 中文使用微软雅黑字体
(set-fontset-font "fontset-default" 'gb18030 '("Microsoft YaHei" . "unicode-bmp"))

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha '(100 95))
(add-to-list 'default-frame-alist '(alpha 100 95))

(provide 'appearance)
;;; appearance.el ends here
