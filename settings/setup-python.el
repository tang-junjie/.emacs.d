;;; setup-python.el --- Emacs Python IDE Settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'elpy)
(require 'py-autopep8)
(require 'highlight-indentation)

(elpy-enable)

(add-hook 'python-mode-hook 'elpy-mode)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'python-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

(set-face-background 'highlight-indentation-face "#3e4451")
(set-face-background 'highlight-indentation-current-column-face "#5f5f5f")

(provide 'setup-python)
;;; setup-python.el ends here
