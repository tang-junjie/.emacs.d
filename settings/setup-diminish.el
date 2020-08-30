;;; setup-diminish.el --- Emacs modeline diminish configuraitons -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Unclutter the modeline
(require 'diminish)
(eval-after-load "ivy" '(diminish 'ivy-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "abbrev" '(diminish 'abbrev-mode))
(eval-after-load "subword" '(diminish 'subword-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "tagedit" '(diminish 'tagedit-mode))
(eval-after-load "eproject" '(diminish 'eproject-mode))
(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "rainbow-mode" '(diminish 'rainbow-mode))
(eval-after-load "drag-stuff" '(diminish 'drag-stuff-mode))
(eval-after-load "Projectile" '(diminish 'projectile-mode))
(eval-after-load "company-box" '(diminish 'company-box-mode))
(eval-after-load "dtrt-indent" '(diminish 'dtrt-indent-mode))
(eval-after-load "git-gutter+" '(diminish 'git-gutter+-mode))
(eval-after-load "EditorConfig" '(diminish 'editorconfig-mode))
(eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
(eval-after-load "paredit-everywhere" '(diminish 'paredit-everywhere-mode))
(eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(provide 'setup-diminish)
;;; setup-diminish.el ends here
