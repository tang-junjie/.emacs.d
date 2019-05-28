;;; my-misc.el --- My miscellaneous settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Some nonstandard editing and utility commands for Emacs
(require 'misc)

;; Various extension functions for Calc
(require 'calc-ext)

;; A bit of misc cargo culting in misc.el
(setq xterm-mouse-mode t)

;; Seed the random-number generator
(random t)

;; Powerline
(powerline-default-theme)

;; Nyan cat
(nyan-mode t)

;; Configure theme-looper
(theme-looper-set-favorite-themes '(leuven solarized-dark))

;; Displays a tab bar at the top
(tabbar-mode t)

;; Displays available keybindings in popup
(which-key-mode t)
(setq projectile-completion-system 'ivy)

;; Display time
(display-time-mode t)

;; Dimming parentheses
(global-paren-face-mode t)

;; An Emacs "jump to definition" package
(dumb-jump-mode t)

;; Persistent scratch
(persistent-scratch-setup-default)

;; Add lorem ipsum filler text to Emacs
(lorem-ipsum-use-default-bindings)

;; A simple-minded way of managing window configs in emacs
(eyebrowse-mode t)
(add-to-list 'window-persistent-parameters '(window-side . writable))
(add-to-list 'window-persistent-parameters '(window-slot . writable))

;; Fold text like in Vim
(vimish-fold-global-mode t)

;; Use cua-mode only for rectangles
(setq cua-enable-cua-keys nil)

;; Don't use expand-region fast keys
(setq expand-region-fast-keys-enabled nil)

;; Show expand-region command used
(setq er--show-expansion-message t)

;; Browse kill ring
(setq browse-kill-ring-quit-action 'save-and-restore)

;; Represent undo-history as an actual tree (visualize with C-x u)
(setq undo-tree-mode-lighter "")
(setq undo-tree-visualizer-diff t)
(global-undo-tree-mode t)

;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

;; Switch from escaped octal character code to escaped HEX
(setq standard-display-table (make-display-table))
(let ((i ?\x80) hex hi low)
  (while (<= i ?\xff)
    (setq hex (format "%x" i))
    (setq hi (elt hex 0))
    (setq low (elt hex 1))
    (aset standard-display-table (unibyte-char-to-multibyte i)
          (vector (make-glyph-code ?\\ 'escape-glyph)
                  (make-glyph-code ?x 'escape-glyph)
                  (make-glyph-code hi 'escape-glyph)
                  (make-glyph-code low 'escape-glyph)))
    (setq i (+ i 1))))

;; Enable the templates for php-mode
(eval-after-load 'php-mode '(require 'php-ext))

;; A *visual* way to choose a window to switch to
(setq switch-window-threshold 2)
(setq switch-window-increase 6)
(setq switch-window-minibuffer-shortcut ?z)
(setq switch-window-shortcut-style 'qwerty)
(setq switch-window-qwerty-shortcuts '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))
(setq switch-window-input-style 'minibuffer)

;; Make zooming affect frame instead of buffers
(require 'zoom-frm)

;; A calendar framework for Emacs
(require 'calfw)

;; EditorConfig plugin for emacs
(editorconfig-mode t)

;; Colorize color names in css-mode
(add-hook 'css-mode-hook 'rainbow-mode)

;; Highlight escape sequences
(hes-mode t)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Sidebar showing a "mini-map" of a buffer
(setq minimap-minimum-width 5)
(setq minimap-window-location 'right)

;; A minor mode that guesses the indentation offset originally used for
;; creating source code files and transparently adjusts the corresponding
;; settings in Emacs
(add-hook 'prog-mode-hook 'dtrt-indent-mode)
(setq global-mode-string (remove 'dtrt-indent-mode-line-info global-mode-string))

;; Whitespace-style
(setq inhibit-compacting-font-caches t)  ; bug fix `https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25148'
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '((space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT ?·?, 46 FULL STOP ?.?
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE ???
        ))

;; Add Urban Dictionary to webjump (C-x g)
(eval-after-load "webjump"
  '(setq webjump-sites
         (append '(("stackoverflow" .
                    [simple-query
                     "http://stackoverflow.com/"
                     "http://stackoverflow.com/search?q="
                     ""])
                   ("(emacs)" .
                    [simple-query
                     "emacs.stackexchange.com"
                     "http://emacs.stackexchange.com/search?q="
                     ""])
                   ("百度" .
                    [simple-query
                     "www.baidu.com"
                     "http://www.baidu.com/s?wd="
                     ""])
                   ("海词" .
                    [simple-query
                     "dict.cn"
                     "http://dict.cn/"
                     ""])
                   ("汉典" .
                    [simple-query
                     "www.zdic.net"
                     "http://www.zdic.net/sousuo/?q="
                     ""])
                   ("法语助手" .
                    [simple-query
                     "www.frdic.com"
                     "http://www.frdic.com/dicts/fr/"
                     ""])
                   ("Urban Dictionary" .
                    [simple-query
                     "www.urbandictionary.com"
                     "http://www.urbandictionary.com/define.php?term="
                     ""]))
                 webjump-sample-sites)))

;; Google translate
(setq google-translate-show-phonetic t)
(setq google-translate-pop-up-buffer-set-focus t)
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "zh-CN")
(setq google-translate-translation-directions-alist
      '(("en" . "zh-CN")
        ("zh-CN" . "en")
        ("fr" . "zh-CN")
        ("zh-CN" . "fr")
        ("en" . "fr")
        ("fr" . "en")))

;; Emacs frontend for weather web service wttr.in
(setq wttrin-default-cities '("Langley"
                              "Surrey"
                              "Vancouver"
                              "Montreal"
                              "Dalian"
                              "Beijing"))

;; Enable the bookmark in eshell
(add-hook 'eshell-mode-hook 'eshell-bookmark-setup)

;; Make occur mode select the window of buffer `*Occur*'
(add-hook 'occur-hook '(lambda () (switch-to-buffer-other-window "*Occur*")))

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t) (eldoc-mode 1)))

;; Fix whitespace on save, but only if the file was clean
(global-whitespace-cleanup-mode t)

;; Use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

;; Make Proced auto update by default
(add-hook 'proced-mode-hook (λ () (proced-toggle-auto-update t)))

;; Edit multiple regions simultaneously in a buffer or a region
(require 'iedit)

;; A emacs tree plugin like NerdTree for Vim
(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq neo-window-fixed-size nil)
(define-key neotree-mode-map (kbd "TAB") 'neotree-enter)
(define-key neotree-mode-map (kbd "RET") 'neotree-enter)
(define-key neotree-mode-map (kbd "q") 'neotree-hide)
(define-key neotree-mode-map (kbd "v") 'neotree-quick-look)
(define-key neotree-mode-map (kbd "h") 'neotree-hidden-file-toggle)

;; View, stage and revert Git changes straight from the buffer
(global-git-gutter+-mode)
(eval-after-load 'git-gutter+
  '(progn
     ;; Jump between hunks
     (define-key git-gutter+-mode-map (kbd "C-x M-g n") 'git-gutter+-next-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x M-g p") 'git-gutter+-previous-hunk)
     ;; Act on hunks
     (define-key git-gutter+-mode-map (kbd "C-x M-g v =") 'git-gutter+-show-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x M-g r") 'git-gutter+-revert-hunks)
     ;; Stage hunk at point.
     ;; If region is active, stage all hunk lines within the region.
     (define-key git-gutter+-mode-map (kbd "C-x M-g t") 'git-gutter+-stage-hunks)
     (define-key git-gutter+-mode-map (kbd "C-x M-g c") 'git-gutter+-commit)
     (define-key git-gutter+-mode-map (kbd "C-x M-g C") 'git-gutter+-stage-and-commit)
     (define-key git-gutter+-mode-map (kbd "C-x M-g C-y") 'git-gutter+-stage-and-commit-whole-buffer)
     (define-key git-gutter+-mode-map (kbd "C-x M-g U") 'git-gutter+-unstage-whole-buffer)))

;; Write backup files to own directory
(defvar --backups-dir (concat user-emacs-directory "backups/"))
(setq backup-directory-alist
      `(("." . ,--backups-dir)
        (,tramp-file-name-regexp nil))
      auto-save-interval 20   ; save every 20 characters typed (this is the minimum)
      backup-by-copying t     ; don't clobber symlinks
      kept-new-versions 10    ; keep 10 latest versions
      kept-old-versions 0     ; don't bother with old versions
      delete-old-versions t   ; don't ask about deleting old versions
      version-control t       ; number backups
      vc-make-backup-files t) ; backup version controlled file

;; Store all backup and autosave files in the temp dir
(defvar --temp-dir (concat user-emacs-directory "temp/"))
(if (not (file-directory-p --temp-dir))
    (make-directory --temp-dir))
(setq abbrev-file-name (expand-file-name "abbrev_defs" --temp-dir)
      tramp-persistency-file-name (expand-file-name "tramp" --temp-dir)
      url-cookie-file (expand-file-name "cookies" url-configuration-directory)
      ac-user-directory --temp-dir
      mc/list-file (expand-file-name ".mc-lists.el" --temp-dir)
      smex-save-file (expand-file-name "smex-items" --temp-dir)
      vimish-fold-dir (expand-file-name "vimish-fold" --temp-dir)
      litable-list-file (expand-file-name ".litable-lists.el" --temp-dir)
      indium-workspace-file (expand-file-name "indium-workspaces.el" --temp-dir)
      auto-save-file-name-transforms `((".*" ,--temp-dir t))
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.auto-saves-" --temp-dir))

;; Make recentf ingore some files
(add-to-list 'recentf-exclude "\\../elpa/*.*\\'")
(add-to-list 'recentf-exclude "\\../temp/*.*\\'")
(add-to-list 'recentf-exclude "\\../backup/*.*\\'")
(add-to-list 'recentf-exclude "\\.elc\\'")

(provide 'my-misc)
;;; my-misc.el ends here
