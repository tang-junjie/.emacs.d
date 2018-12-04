;;; sai-defuns.el --- My Emacs List Functions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun sai/insert-date ()
  "Insert curent date and time."
  (interactive)
  (insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))

(defun sai/insert-filename ()
  "Insert file name of current buffer at current point"
  (interactive)
  (insert (buffer-file-name (current-buffer))))

(defun sai/toggle-elisp-debugger ()
  "Turn Emacs Lisp debugger on/off."
  (interactive)
  (if (eq debug-on-error t)
      (setq debug-on-error nil)
    (setq debug-on-error t))
  (message (if debug-on-error "Elisp debugger on" "Elisp debugger off")))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;;; sai-defuns.el ends here
