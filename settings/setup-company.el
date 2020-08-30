;;; setup-company.el --- Modular in-buffer completion framework for Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'company)
(require 'company-box)

;; Use company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'company-mode-hook 'company-box-mode)

;; Adjust defaults
(setq company-show-numbers t          ; show candidates number
      company-dabbrev-downcase nil    ; remove case replace
      company-idle-delay 0            ; start completions automatically
      company-echo-delay 0            ; no delay before autocompletion popup shows
      company-selection-wrap-around t ; selecting item before first or after last wraps around
      )

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; Don't complete in certain modes to avoid blocking call
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

;; Backends configuration
(add-to-list 'company-backends 'company-restclient)

(provide 'setup-company)
;;; setup-company.el ends here
