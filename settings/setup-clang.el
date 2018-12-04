;;; setup-clang.el --- Emacs C Language Settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cc-mode)

;; C default coding style
(setq c-default-style "bsd"
      c-basic-offset 4)

;; Use gdb-many-windows by default
(setq gdb-many-windows t)

;; Non-nil means display source file containing the main routine at startup
(setq gdb-show-main t)

;; Quickly switch between header and implementation
(setq ff-always-in-other-window t)
(add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "C-c M-o") 'ff-find-other-file)))

;; Disassemble C/C++ code under cursor in Emacs
(setq disaster-cc "gcc")
(define-key c-mode-base-map (kbd "C-c M-d") 'disaster)

;; Setup ggtags
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))
(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; Integrate ggtangs with imenu
(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

(provide 'setup-clang)
;;; setup-clang.el ends here
