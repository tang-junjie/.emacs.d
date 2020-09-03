;;; mode-mappings.el --- Emacs mode mappings configuraiton -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Emacs lisp
(add-to-list 'auto-mode-alist '("Carton$" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))

;; CSS
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

;; Restclient
(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))

;; JSON
(add-to-list 'auto-mode-alist '("\\.json$'" . json-mode))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html$'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cshtm$'" . web-mode))

;; Web-mode
(add-to-list 'auto-mode-alist '("\\.vue$'" . web-mode))

;; SVG
(add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))

;; Snippets
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; Org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; PlantUML
(add-to-list 'auto-mode-alist '("\\.plantuml$'" . plantuml-mode))

;; YAML
(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; Apache config
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

(provide 'mode-mappings)
;;; mode-mappings.el ends here
