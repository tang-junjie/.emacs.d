;;; setup-web-mode.el --- Emacs web-mode configurations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; HTML template editing for emacs
(require 'web-mode)

(setq-default indent-tabs-mode nil)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-auto-close-style 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t))
(with-eval-after-load 'web-mode (my-web-mode-hook))

;; Zen coding
(require 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

(provide 'setup-web-mode)
;;; setup-web-mode.el ends here
