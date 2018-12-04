;;; setup-dired.el --- Emacs dired-mode configuraiton -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dired)
(require 'dired-x) ; dired-jump C-x C-j
(require 'dired-single)

;; Human-readable sizes
(setq dired-listing-switches "-alh")

;; Make dired less verbose
(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))

;; Move files between split panes
(setq dired-dwim-target t)

;; M-up is nicer in dired if it moves to the third line - the first file
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 3))

(define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
(define-key dired-mode-map (vector 'remap 'smart-up) 'dired-back-to-top)
(define-key dired-mode-map (kbd "<M-up>") 'dired-back-to-top)

;; M-down is nicer in dired if it moves to the last file
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
(define-key dired-mode-map (vector 'remap 'smart-down) 'dired-jump-to-bottom)
(define-key dired-mode-map (kbd "<M-down>") 'dired-jump-to-bottom)

(eval-after-load "wdired"
  '(progn
     (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
     (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)))

;; Reuse the current dired buffer to visit another directory
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."
  ;; <add other stuff here>
  (dired-async-mode 1) ; run dired commands asynchronously
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

(provide 'setup-dired)
;;; setup-dired.el ends here
