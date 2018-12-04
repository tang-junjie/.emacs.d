;;; setup-flycheck.el --- Syntax checking for GNU Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'flycheck)

;; Have flycheck override eldoc faster
(setq eldoc-idle-delay 0.1
      flycheck-display-errors-delay 0.2)

(defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.5 30.0)))

;; Each buffer gets its own idle-change-delay because of the
;; buffer-sensitive adjustment above.
(make-variable-buffer-local 'flycheck-idle-change-delay)
(add-hook 'flycheck-after-syntax-check-hook
          'magnars/adjust-flycheck-automatic-syntax-eagerness)

;; Remove newline checks, since they would trigger an immediate check
;; when we want the idle-change-delay to be in effect while editing.
(setq flycheck-check-syntax-automatically '(save
                                            idle-change
                                            mode-enabled))

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here
