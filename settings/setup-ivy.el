;;; setup-ivy.el --- Interactively Do Things -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Ivy - a generic completion frontend for Emacs,
;; Swiper - isearch with an overview, and more. Oh, man!
(require 'ivy)
(ivy-mode t)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Fuzzy matching for Emacs
(require 'flx)

;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
(setq ivy-use-virtual-buffers t)

;; number of result lines to display
(setq ivy-height 10)

;; does not count candidates
(setq ivy-count-format "")

;; no regexp by default
(setq ivy-re-builders-alist
      ;; allow input not in order
      '((t . ivy--regex-ignore-order)))

;; basic configurations
(setq enable-recursive-minibuffers t)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

(provide 'setup-ivy)
;;; setup-ivy.el ends here
