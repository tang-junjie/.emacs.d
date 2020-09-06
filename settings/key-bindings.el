;;; key-bindings.el --- Emacs keybindings configuraiton -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key
(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super) ; Right Windows key
(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper) ; Menu/App key

;; Map pairs of simultaneously pressed keys to commands
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-one-key-delay 0.16)
(key-chord-define-global ",," 'ibuffer)
(key-chord-define-global ",a" 'ascii-display)
(key-chord-define-global ",b" 'bury-buffer)
(key-chord-define-global ",c" 'cleanup-buffer)
(key-chord-define-global ",d" 'kill-this-buffer-unless-scratch)
(key-chord-define-global ",e" 'eval-buffer)
(key-chord-define-global ",f" 'counsel-recentf)
(key-chord-define-global ",g" 'google-this)
(key-chord-define-global ",l" 'display-line-numbers-mode)
(key-chord-define-global ",m" 'magit-status-fullscreen)
(key-chord-define-global ",n" 'create-scratch-buffer)
(key-chord-define-global ",q" 'read-only-mode)
(key-chord-define-global ",r" 'rgrep-fullscreen)
(key-chord-define-global ",s" 'swiper)
(key-chord-define-global ",t" 'google-translate-at-point)
(key-chord-define-global ",v" 'neotree-toggle)
(key-chord-define-global ",w" 'whitespace-mode)
(key-chord-define-global ",x" 'eshell)
(key-chord-define-global ",z" 'repeat)
(key-chord-define-global "jj" 'ace-jump-char-mode)
(key-chord-define-global "kj" 'ace-jump-mode-pop-mark)
(key-chord-define-global "JJ" 'quick-switch-buffer)

;; More neat bindings for C-x 8
(global-set-key (kbd "C-x 8 t m") (λ (insert "™")))
(global-set-key (kbd "C-x 8 ( c )") (λ (insert "©")))
(global-set-key (kbd "C-x 8 - >") (λ (insert "→")))
(global-set-key (kbd "C-x 8 8") (λ (insert "∞")))
(global-set-key (kbd "C-x 8 ( c )") (λ (insert "©")))
(global-set-key (kbd "C-x 8 v") (λ (insert "✓")))

;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)

;; Copy line above
(require 'misc)
(global-set-key (kbd "s-.") 'copy-from-above-command)

;; Smart M-x: Ivy a generic completion frontend
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "<f1> a") 'counsel-apropos)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-x g") 'counsel-git)
(global-set-key (kbd "C-x G") 'counsel-git-grep)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-x M-a") 'counsel-ag)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-r") 'ivy-resume)
(global-set-key (kbd "C-x C-i") 'ivy-imenu-anywhere)

;; Quickly surround text with delimiters in emacs
(global-set-key (kbd "M-(") 'corral-parentheses-backward)
(global-set-key (kbd "M-)") 'corral-parentheses-forward)
(global-set-key (kbd "M-[") 'corral-brackets-backward)
(global-set-key (kbd "M-]") 'corral-brackets-forward)
(global-set-key (kbd "M-{") 'corral-braces-backward)
(global-set-key (kbd "M-}") 'corral-braces-forward)
(global-set-key (kbd "M-C-\"") 'corral-double-quotes-backward)

;; Expand region (increases selected region by semantic units)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)
(global-set-key (if is-mac (kbd "C-@") (kbd "C-'")) 'er/expand-region)

;; Experimental multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)

;; Mark additional regions matching current region
(global-set-key (kbd "C-|") 'mc/mark-all-dwim)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "M-|") 'mc/mark-all-in-region)

;; Symbol and word specific mark-more
(global-set-key (kbd "s->") 'mc/mark-next-word-like-this)
(global-set-key (kbd "s-<") 'mc/mark-previous-word-like-this)
(global-set-key (kbd "s-|") 'mc/mark-all-words-like-this)
(global-set-key (kbd "M-s->") 'mc/mark-next-symbol-like-this)
(global-set-key (kbd "M-s-<") 'mc/mark-previous-symbol-like-this)
(global-set-key (kbd "M-s-|") 'mc/mark-all-symbols-like-this)

;; Extra multiple cursors stuff
(global-set-key (kbd "C-~") 'mc/reverse-regions)
(global-set-key (kbd "M-~") 'mc/sort-regions)
(global-set-key (kbd "H-~") 'mc/insert-numbers)

(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; Set anchor to start rectangular-region-mode
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; Enable Emacs column selection using mouse
(global-set-key (kbd "M-<down-mouse-1>") 'ignore)
(global-set-key (kbd "M-<down-mouse-1>") 'mouse-start-rectangle)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)

;; M-i for back-to-indentation
(global-set-key (kbd "M-i") 'back-to-indentation)

;; A better Emacs *help* buffer
(global-set-key (kbd "C-c h") 'helpful-at-point)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Like `fill-paragraph' but unfill if used twice
(global-set-key [remap fill-paragraph] #'my-fill-or-unfill)

;; Interactive selective display
(global-set-key (kbd "C-x $") 'inc-selective-display)

;; Change next underscore with a camel case
(global-set-key (kbd "C-c C--") 'replace-next-underscore-with-camel)
(global-set-key (kbd "M-s M--") 'snakeify-current-word)

;; Change word separators
(global-unset-key (kbd "C-x +")) ;; used to be balance-windows
(global-set-key (kbd "C-x + -") (λ (replace-region-by 's-dashed-words)))
(global-set-key (kbd "C-x + _") (λ (replace-region-by 's-snake-case)))
(global-set-key (kbd "C-x + c") (λ (replace-region-by 's-lower-camel-case)))
(global-set-key (kbd "C-x + C") (λ (replace-region-by 's-upper-camel-case)))

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

;; Kill entire line with prefix argument
(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(global-set-key [remap kill-line] (bol-with-prefix kill-line))

;; Killing text
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)
(global-set-key (kbd "M-h") 'kill-region-or-backward-word)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)
(global-set-key (kbd "s-w") 'save-region-or-current-line)
(global-set-key (kbd "M-W") (λ (save-region-or-current-line 1)))

;; Make shell more convenient
(global-set-key (kbd "C-M-z") 'shell)

;; Zap to char
(global-set-key (kbd "M-z") 'ace-jump-zap-to-char)
(global-set-key (kbd "M-Z") 'ace-jump-zap-up-to-char)

;; iy-go-to-char - like f in Vim
(global-set-key (kbd "C-c C-j") 'jump-char-forward)
(global-set-key (kbd "C-c M-j") 'jump-char-backward)

;; vim's ci and co commands
(global-set-key (kbd "M-I") 'change-inner)
(global-set-key (kbd "M-O") 'change-outer)

(global-set-key (kbd "M-s-i") 'copy-inner)
(global-set-key (kbd "M-s-o") 'copy-outer)

;; Create new frame
(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; File finding
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c C-p") 'find-or-create-file-at-point)
(global-set-key (kbd "C-c C-c C-p") 'find-or-create-file-at-point-other-window)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "C-c b") 'quick-switch-buffer)

;; Revert without any fuss
(global-set-key (kbd "s-<escape>") (λ (progn (revert-buffer t t) (message "Buffer reverted."))))

;; Edit file with sudo
(global-set-key (kbd "M-s-e") 'sudo-edit)

;; Copy file path to kill ring
(global-set-key (kbd "C-x M-w") 'copy-current-file-path)

;; Window switching
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x -") 'toggle-window-split)
(global-set-key (kbd "C-x C--") 'rotate-windows)
(global-unset-key (kbd "C-x C-+")) ;; don't zoom like this
(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "M-o") 'ace-window)

;; Resize window easily
(global-set-key (kbd "C-M-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-S-<up>") 'shrink-window)
(global-set-key (kbd "C-M-S-<down>") 'enlarge-window)

;; Zoom frame font size
(global-set-key (kbd "C-S-<wheel-up>") 'zoom-frm-in)
(global-set-key (kbd "C-S-<wheel-down>") 'zoom-frm-out)
(global-set-key (kbd "C-S-<mouse-4>") 'zoom-frm-in)
(global-set-key (kbd "C-S-<mouse-5>") 'zoom-frm-out)

;; Add region to *multifile*
(global-set-key (kbd "C-!") 'mf/mirror-region-in-multifile)

;; Indentation help
(global-set-key (kbd "M-j") (λ (join-line -1)))

;; Help should search more than just commands
(global-set-key (kbd "<f1> a") 'apropos)

;; Navigation bindings
(global-set-key (kbd "C-<prior>") 'beginning-of-buffer)
(global-set-key (kbd "C-<next>") 'end-of-buffer)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; Webjump let's you quickly search google, wikipedia, emacs wiki
(global-set-key (kbd "C-c g") 'webjump)
(global-set-key (kbd "C-c M-g") 'browse-url-at-point)

;; Like isearch, but adds region (if any) to history and deactivates mark
(global-set-key (kbd "C-s") 'isearch-forward-use-region)
(global-set-key (kbd "C-r") 'isearch-backward-use-region)

;; Like isearch-*-use-region, but doesn't fuck with the active region
(global-set-key (kbd "C-S-s") 'isearch-forward)
(global-set-key (kbd "C-S-r") 'isearch-backward)

;; Move more quickly
(global-set-key (kbd "C-S-n") (λ (ignore-errors (forward-line 5))))
(global-set-key (kbd "C-S-p") (λ (ignore-errors (forward-line -5))))
(global-set-key (kbd "C-S-f") (λ (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (λ (ignore-errors (backward-char 5))))

;; Replace string
(global-set-key (kbd "C-c r") 'replace-string)

;; Query replace regex key binding
(global-set-key (kbd "M-&") 'query-replace-regexp)

;; Yank selection in isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-yank-selection)

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Eval buffer
(global-set-key (kbd "C-c C-k") 'eval-buffer)

;; Scratch buffer
(global-set-key (kbd "C-c <tab>") 'goto-scratch)
(global-set-key (kbd "C-c b") 'create-scratch-buffer)

;; Move windows, even in org-mode
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)

;; Magit
(global-set-key (kbd "C-x m") 'magit-status-fullscreen)
(global-set-key (kbd "C-c G") 'git-gutter+-toggle-fringe)
(autoload 'magit-status-fullscreen "magit")

;; Clever newlines
(global-set-key (kbd "C-S-O") 'open-line-above)
(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "<M-return>") 'new-line-dwim)

;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Drag stuff around
(global-set-key (kbd "C-S-<down>") 'drag-stuff-down)
(global-set-key (kbd "C-S-<up>") 'drag-stuff-up)
(global-set-key (kbd "C-S-<left>") 'drag-stuff-left)
(global-set-key (kbd "C-S-<right>") 'drag-stuff-right)

;; Yank and indent
(global-set-key (kbd "C-S-y") 'yank-unindented)

;; Toggle quotes
(global-set-key (kbd "C-c C-\"") 'toggle-quotes)

;; Sorting
(global-set-key (kbd "M-s-l") 'sort-lines)

;; Should be able to eval-and-replace anywhere
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; Shorthand for interactive lambdas
(global-set-key (kbd "C-c M-l") (λ (insert "\u03bb")))

;; emulation of vim's visual line selection
(global-set-key (kbd "C-c C-s") 'select-current-line)

;; Emulation of the vi % command
(global-set-key (kbd "%") 'goto-match-paren)

;; Increase number at point (or other change based on prefix arg)
(global-set-key (kbd "C-c +") 'change-number-at-point)
(global-set-key (kbd "C-c -") 'subtract-number-at-point)

;; Browse the kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

;; Buffer file functions
(global-set-key (kbd "C-x t") 'touch-buffer-file)
(global-set-key (kbd "C-c C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-c C-k") 'delete-current-buffer-file)

;; Jump from file to containing directory
(global-set-key (kbd "C-x C-j") 'dired-jump) (autoload 'dired-jump "dired")

;; Fold text like in Vim
(global-set-key (kbd "C-c f f") 'vimish-fold)
(global-set-key (kbd "C-c f d") 'vimish-fold-delete)
(global-set-key (kbd "C-c f t") 'vimish-fold-toggle)
(global-set-key (kbd "C-c f D") 'vimish-fold-delete)
(global-set-key (kbd "C-c f T") 'vimish-fold-toggle)

;; Find files by name and display results in dired
(global-set-key (kbd "M-s-f") 'find-name-dired)

;; Easy-mode fullscreen rgrep
(global-set-key (kbd "M-s s") 'git-grep-fullscreen)
(global-set-key (kbd "M-s S") 'rgrep-fullscreen)

;; Multi-occur
(global-set-key (kbd "C-c m") 'multi-occur)
(global-set-key (kbd "C-c C-m") 'multi-occur-in-matching-buffers)

;; Display and edit occurances of regexp in buffer
(global-set-key (kbd "C-c o") 'occur)

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

;; Visual regexp
(define-key global-map (kbd "M-&") 'vr/query-replace)
(define-key global-map (kbd "M-/") 'vr/replace)

;; Auto-YASnippet
(global-set-key (kbd "H-w") #'aya-create)
(global-set-key (kbd "H-y") #'aya-expand)

;; Transparent frame
(global-set-key (kbd "C-c .") 'frame-transparency)

;; Cycle themes
(global-set-key (kbd "C-\"") 'theme-looper-enable-next-theme)

;; Toggle displaying image in eww
(require 'eww)
(define-key eww-mode-map (kbd "i") 'endless/toggle-image-display)

;; Find file in project
(global-set-key (kbd "C-x p") 'find-file-in-project)

;; Find file in project, with specific patterns
(global-unset-key (kbd "C-x C-p")) ;; which used to be delete-blank-lines (also bound to C-c C-<return>)
(global-set-key (kbd "C-x C-p js") (ffip-create-pattern-file-finder "*.js"))
(global-set-key (kbd "C-x C-p ts") (ffip-create-pattern-file-finder "*.ts"))
(global-set-key (kbd "C-x C-p vu") (ffip-create-pattern-file-finder "*.jsx"))
(global-set-key (kbd "C-x C-p vu") (ffip-create-pattern-file-finder "*.vue"))
(global-set-key (kbd "C-x C-p jn") (ffip-create-pattern-file-finder "*.json"))
(global-set-key (kbd "C-x C-p ht") (ffip-create-pattern-file-finder "*.html"))
(global-set-key (kbd "C-x C-p cs") (ffip-create-pattern-file-finder "*.css"))
(global-set-key (kbd "C-x C-p el") (ffip-create-pattern-file-finder "*.el"))
(global-set-key (kbd "C-x C-p md") (ffip-create-pattern-file-finder "*.md"))
(global-set-key (kbd "C-x C-p or") (ffip-create-pattern-file-finder "*.org"))
(global-set-key (kbd "C-x C-p tx") (ffip-create-pattern-file-finder "*.txt"))
(global-set-key (kbd "C-x C-p xm") (ffip-create-pattern-file-finder "*.xml"))
(global-set-key (kbd "C-x C-p in") (ffip-create-pattern-file-finder "*.ini"))
(global-set-key (kbd "C-x C-p ss") (ffip-create-pattern-file-finder "*.scss"))
(global-set-key (kbd "C-x C-p co") (ffip-create-pattern-file-finder "*.conf"))
(global-set-key (kbd "C-x C-p sh") (ffip-create-pattern-file-finder "*.sh"))
(global-set-key (kbd "C-x C-p ic") (ffip-create-pattern-file-finder "*.ico"))
(global-set-key (kbd "C-x C-p sv") (ffip-create-pattern-file-finder "*.svg"))
(global-set-key (kbd "C-x C-p c++") (ffip-create-pattern-file-finder "*.cpp"))
(global-set-key (kbd "C-x C-p cc") (ffip-create-pattern-file-finder "*.c"))
(global-set-key (kbd "C-x C-p !") (ffip-create-pattern-file-finder "*"))

;; List Packages
(global-set-key (kbd "<f3>") 'list-packages)

;; Toggle Elisp debugger
(global-set-key (kbd "<f5>") 'sai/toggle-elisp-debugger)

;; Smart compile
(global-set-key (kbd "<s-f5>") 'smart-compile)

;; Open in desktop
(global-set-key (kbd "<f6>") 'xah-open-in-desktop)

;; Open in external app
(global-set-key (kbd "s-<f6>") 'xah-open-in-external-app)

;; Toggle focus-mode
(global-set-key (kbd "<f7>") 'focus-mode)

;; Toggle minimap-mode
(global-set-key (kbd "s-<f7>") 'minimap-mode)

;; Toggle kakapo-mode
(global-set-key (kbd "<f8>") 'kakapo-mode)

;; Toggle litable-mode
(global-set-key (kbd "s-<f8>") 'litable-mode)

;; Toggle elmacro-mode
(global-set-key (kbd "<f9>") 'elmacro-mode)

;; Toggle macrostep-mode
(global-set-key (kbd "s-<f9>") 'macrostep-mode)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "<f10>") 'menu-bar-mode)

;; Toggle tool bar
(global-set-key (kbd "s-<f10>") 'tool-bar-mode)

;; Toggle tabbar-mode
(global-set-key (kbd "<s-f11>") 'tabbar-mode)

;; Restart emacs
(global-set-key (kbd "<f12>") 'restart-emacs)

;; Weather forecast
(global-set-key (kbd "s-<f12>") 'wttrin)

(provide 'key-bindings)
;;; key-bindings.el ends here
