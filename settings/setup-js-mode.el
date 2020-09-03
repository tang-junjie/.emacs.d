;;; setup-js-mode.el --- tweak js-mode settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code

;; development environment for Javascript and Typescript
(defun setup-tide-mode ()
  "Setup function for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq indent-tabs-mode nil)
  (setq js-basic-indent 2)
  (setq js-switch-indent-offset 2)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (setq company-tooltip-align-annotations t))
(add-hook 'js-mode-hook #'setup-tide-mode)

;; fix current file using ESLint --fix
(require 'eslint-fix)
(eval-after-load 'js-mode
  '(add-hook 'js-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))

;; formatting with Prettier
(require 'prettier-js)
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(setq prettier-js-args '(
                         "--trailing-comma" "none"
                         "--bracket-spacing" "true"
                         "--single-quote" "true"
                         "--no-semi" "true"
                         "--jsx-single-quote" "true"
                         "--jsx-bracket-same-line" "true"
                         "--print-width" "120"))

(provide 'setup-js-mode)
;;; setup-js-mode.el ends here
