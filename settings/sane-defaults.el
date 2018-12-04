;;; sane-default.el --- Emacs sane default configuraitons -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; All roads lead to home
(setq default-directory user-emacs-directory)

;; Better defaults
(setq visible-bell t
      apropos-do-all t
      load-prefer-newer t
      mouse-yank-at-point t
      auto-image-file-mode t
      comint-process-echoes t
      require-final-newline t
      select-enable-primary nil
      truncate-partial-width-windows nil
      save-interprogram-paste-before-kill t)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)
(setq auto-revert-remote-files t)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8-unix) ; pretty
(set-terminal-coding-system 'utf-8-unix) ; pretty
(set-keyboard-coding-system 'utf-8-unix) ; pretty
(setq default-buffer-file-coding-system 'utf-8-unix) ; please
(prefer-coding-system 'utf-8-unix) ; with sugar on top

;; Enable prettify symbols mode
(global-prettify-symbols-mode)
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Turn on abbrev mode globally
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 1000) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Remember cursor position, for emacs 25.1 or later
(save-place-mode 1)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Try indent first, otherwise try to complete the thing at point
(setq tab-always-indent 'complete)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold (* 100 1024 1024))

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; A saner compile
(setq compilation-ask-about-save nil)         ; I'm not scared of saving everyting
(setq compilation-scroll-output 'next-error)  ; stop on the first error
(setq compilation-skip-threshold 2)           ; don't stop on info or warnings

;; Easily search for non-ASCII characters
(setq search-default-mode #'char-fold-to-regexp)
(setq replace-char-fold t)

;; Make eww the default broswer in Emacs
(setq browse-url-browser-function 'eww-browse-url)
(setq shr-color-visible-luminance-min 90)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Eldoc for lisp
(setq global-eldoc-mode t)

;; Electric indent
(setq electric-indent-mode t)

;; Electric pair: automatically close parenthesis, curly brace etc.
(setq electric-pair-pairs '((?\{ . ?\})))
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Show line numbers in prog-mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

;; Don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

;; Support for ANSI colors in shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Use human readable Size column instead of original one
(require 'ibuffer)
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))

;; No prompt for confirmation unless it's modified buffer
(setq ibuffer-expert t)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(setq set-mark-command-repeat-pop t)

;; Offer to create parent directories if they do not exist
;; `http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/'
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

;; Abort the minibuffer when using the mouse
;; `http://trey-jackson.blogspot.ca/2010/04/emacs-tip-36-abort-minibuffer-when.html'
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; While chmod is interactive, you can of course also use it from elisp
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(provide 'sane-defaults)
;;; sane-defaults ends here
