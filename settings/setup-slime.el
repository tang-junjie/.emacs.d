;;; setup-slime.el --- tweak slime settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'slime-autoloads) ; 注意这里加载的是 slime-autoloads，而不是 slime，要不然C-c C-c等很多功能都没有

;; Default Common Lisp configuration
(setq inferior-lisp-program "sbcl") ; 注：如果此处路径有空格，在M-x slime时会出现问题：apply: Spawning child process: invalid argument
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(slime-setup '(slime-fancy slime-company))

;; Get syntax highlighting for common lisp in SLIME's REPL
(progn
  (defvar slime-repl-font-lock-keywords lisp-font-lock-keywords-2)
  (defun slime-repl-font-lock-setup ()
    (setq font-lock-defaults
          '(slime-repl-font-lock-keywords
            ;; From lisp-mode.el
            nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
            (font-lock-syntactic-face-function . lisp-font-lock-syntactic-face-function))))

  (add-hook 'slime-repl-mode-hook 'slime-repl-font-lock-setup)
  (defadvice slime-repl-insert-prompt (after font-lock-face activate)
    (let ((inhibit-read-only t))
      (add-text-properties
       slime-repl-prompt-start-mark (point)
       '(font-lock-face
         slime-repl-prompt-face
         rear-nonsticky
         (slime-repl-prompt read-only font-lock-face intangible))))))

;; Dimming parentheses
(add-hook 'slime-repl-mode-hook 'paren-face-mode)

;; A nice and quick way to trace/untrace defuns from slime
(define-key slime-mode-map (kbd "C-c t") 'slime-toggle-trace-fdefinition)

;; Set up usual navigation keys to the completion menu for company-mode
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)

(provide 'setup-slime)
;;; setup-slime.el ends here
