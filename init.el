;;; init.el --- Live in Emacs! -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Remove security vulnerability
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; Emacs Rocks!")

;; Suppressing ad-handle-definition Warnings when functions are redefined with defadvice
(setq ad-redefinition-action 'accept)

;; Set path to dependencies
(defvar --site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(defvar --settings-dir (expand-file-name "settings" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path --settings-dir)
(add-to-list 'load-path --site-lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file) (customize-save-customized))
(load custom-file)

;; Set up appearance early
(require 'appearance)

;; Settings for currently logged in user
(setq user-settings-dir (concat user-emacs-directory "users/" user-login-name))
(unless (file-exists-p user-settings-dir)
  (make-directory user-settings-dir "users"))
(add-to-list 'load-path user-settings-dir)

;; Add external projects to load path
(dolist (project (directory-files --site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Are we on a mac/linux/windows?
(defvar is-mac (equal system-type 'darwin))
(defvar is-linux (equal system-type 'gnu/linux))
(defvar is-windows (equal system-type 'windows-nt))

;; CommonLisp library for Emacs
(require 'cl-lib)

;; Setup packages
(require 'setup-package)

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup extensions
(eval-after-load 'ivy '(require 'setup-ivy))
(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'shell '(require 'setup-shell))
(require 'setup-ffip)
(require 'setup-rgrep)
(require 'setup-clang)
(require 'setup-hippie)
(require 'setup-python)
(require 'setup-company)
(require 'setup-paredit)
(require 'setup-diminish)
(require 'setup-register)
(require 'setup-web-mode)
(require 'setup-yasnippet)

;; Map files to modes
(require 'mode-mappings)

;; Functions (load all files in defuns-dir)
(defvar defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Site-lisp libraries (load all files in site-lisp)
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(dolist (file (directory-files site-lisp-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Language specific setup files
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

;; Load stuff on demand
(autoload 'slime "setup-slime" nil t)
(autoload 'elnode "setup-elnode" nil t)
(autoload 'skewer-start "setup-skewer" nil t)
(autoload 'skewer-demo "setup-skewer" nil t)
(eval-after-load 'flycheck '(require 'setup-flycheck))

(require 'delsel)
(require 'multifiles)
(require 'expand-region)

;; Misc
(require 'my-misc)
(when is-mac (require 'setup-mac))
(when is-linux (require 'setup-linux))
(when is-windows (require 'setup-windows))

;; Setup key bindings
(require 'key-bindings)

;; Emacs server
(require 'server)
(defvar --server-filename (expand-file-name "server/server" user-emacs-directory))
(if (file-exists-p --server-filename)
    (delete-file --server-filename))
(unless (server-running-p)
  (server-start))

;; Conclude init by setting up specifics for the current user
(when (file-exists-p user-settings-dir)
  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))

;; Saving emacs sessions
(setq desktop-restore-frames nil)
(desktop-save-mode 1)

;;; init.el ends here
