;;; mode-mappings.el --- Emacs mode mappings configuraiton -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; JSON
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

;; CSS
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

;; SVG
(add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))

;; Vue
(add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))

;; Org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; PlantUML
(add-to-list 'auto-mode-alist '("\\.plantuml$" . plantuml-mode))

;; Restclient
(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))

;; Snippets
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;; YAML
(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; Apache
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

(provide 'mode-mappings)
;;; mode-mappings.el ends here
