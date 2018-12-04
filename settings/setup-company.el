;;; setup-company.el --- Modular in-buffer completion framework for Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'company)
(require 'company-jedi)
(require 'company-tern)

;; Python setup (require: pip install jedi epc autopep8 pylint)
(add-hook 'python-mode-hook 'jedi:setup)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;; Javascript setup
(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

;; Use company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)

;; Adjust defaults
(setq company-show-numbers t                        ; show candidates number
      company-dabbrev-downcase nil                  ; remove case replace
      company-idle-delay 0                          ; start completions automatically
      company-echo-delay 0                          ; no delay before autocompletion popup shows
      company-selection-wrap-around t               ; selecting item before first or after last wraps around
      )

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; Don't complete in certain modes to avoid blocking call
(setq company-global-modes '(not gud-mode))
(setq company-global-modes '(not git-commit-mode))

;; Change company-mode nasty default color & behaviors
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; Configurations for restclient
(add-to-list 'company-backends 'company-restclient)

(provide 'setup-company)
;;; setup-company.el ends here
