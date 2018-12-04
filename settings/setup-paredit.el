;;; setup-paredit.el --- Minor mode for editing parentheses -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'paredit)

(add-hook 'ielm-mode-hook (lambda () (enable-paredit-mode)))
(add-hook 'lisp-mode-hook (lambda () (enable-paredit-mode)))
(add-hook 'slime-repl-mode-hook (lambda () (enable-paredit-mode)))
(add-hook 'emacs-lisp-mode-hook (lambda () (enable-paredit-mode)))

(require 'paredit-everywhere)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

;; My keybindings for paredit
(define-key paredit-mode-map (kbd "C-j") nil)
(define-key paredit-mode-map (kbd "M-C-<backspace>") 'backward-kill-sexp)

;; don't hijack \ please
(define-key paredit-mode-map (kbd "\\") nil)

;; Enable `paredit-mode' in the minibuffer, during `eval-expression'.
(defun conditionally-enable-paredit-mode ()
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

;; making paredit work with delete-selection-mode
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-newline 'delete-selection t)

(provide 'setup-paredit)
;;; setup-paredit.el ends here
