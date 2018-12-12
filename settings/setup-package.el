;;; setup-package.el --- Emacs package configuraiton -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)

;; Stop Emacs from adding the (package-initialize) to my config
(setq package--init-file-ensured t)

;; Override built-in packages (e.g. org)
(setq package-enable-at-startup nil)

;; Set up package archives
(setq package-check-signature nil)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; Pin package to specific archive
(setq package-pinned-packages '(()))

;; Activate all the packages (in particular autoloads)
(unless package--initialized (package-initialize))

;; Bootstrap `dash'
(unless (package-installed-p 'dash)
  (package-refresh-contents)
  (package-install 'dash)
  (defmacro comment (&rest ignore))
  (defmacro λ (&rest body)
    ;; shorthand for interactive lambdas
    `(lambda ()
       (interactive)
       ,@body)))

;; Font lock dash.el
(require 'dash)
(eval-after-load "dash" '(dash-enable-font-lock))

;;; On-demand installation of packages

(defun packages-install (packages)
  "Install PACKAGES if not already installed."
  (--each packages
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun init--install-packages ()
  "Install extensions if they're missing."
  (packages-install
   '(0xc                                ;* Base conversion made easy
     2048-game                          ;* play 2048 in Emacs
     ace-jump-mode                      ;* a quick cursor location minor mode for emacs
     ace-jump-zap                       ;* Character zapping, `ace-jump-mode` style
     ace-window                         ;* Quickly switch windows.
     ag                                 ;* A front-end for ag ('the silver searcher'), the C ack replacement.
     all-the-icons                      ;* A library for inserting Developer icons
     angular-mode                       ;* Major mode for Angular.js
     angular-snippets                   ;* Yasnippets for AngularJS
     annoying-arrows-mode               ;* Ring the bell if using arrows too much
     apache-mode                        ;* major mode for editing Apache configuration files
     auto-package-update                ;* Automatically update Emacs packages.
     auto-yasnippet                     ;* Quickly create disposable yasnippets
     avy                                ;* tree-based completion
     bash-completion                    ;* BASH completion for the shell buffer
     basic-mode                         ;* major mode for editing BASIC code
     browse-kill-ring                   ;* interactively insert items from kill-ring
     calfw                              ;* Calendar view framework on Emacs
     change-inner                       ;* Change contents based on semantic units
     cider                              ;* Clojure Interactive Development Environment that Rocks
     clj-refactor                       ;* A collection of commands for refactoring Clojure code
     clojure-mode                       ;* Major mode for Clojure code
     clojure-mode-extra-font-locking    ;* Extra font-locking for Clojure mode
     common-lisp-snippets               ;* Yasnippets for Common Lisp
     company                            ;* Modular text completion framework
     company-jedi                       ;* company-mode completion back-end for Python JEDI
     company-restclient                 ;* company-mode completion back-end for restclient-mode
     company-tern                       ;* Tern backend for company-mode
     corral                             ;* Quickly surround text with delimiters
     counsel                            ;* Various completion functions using Ivy
     csharp-mode                        ;* C# mode derived mode
     css-eldoc                          ;* an eldoc-mode plugin for CSS source code
     csv-mode                           ;* Major mode for editing comma/char separated values
     dash                               ;* A modern list library for Emacs
     diminish                           ;* Diminished modes are minor modes with no modeline display
     dired-single                       ;* Reuse the current dired buffer to visit a directory
     disaster                           ;* Disassemble C/C++ code under cursor in Emacs
     dockerfile-mode                    ;* Major mode for editing Docker's Dockerfiles
     drag-stuff                         ;* Drag stuff (lines, words, region, etc...) around
     dtrt-indent                        ;* Adapt to foreign indentation offsets
     dumb-diff                          ;* fast arbitrary diffs
     dumb-jump                          ;* jump to definition for multiple languages without configuration.
     editorconfig                       ;* EditorConfig Emacs Plugin
     edn                                ;* Support for reading and writing the edn data format from elisp
     elisp-slime-nav                    ;* Make M-. and M-, work in elisp like they do in slime
     elmacro                            ;* find callers of elisp functions or macros
     elnode                             ;* The Emacs webserver.
     elpy                               ;* Emacs Python Development Environment
     emmet-mode                         ;* Unofficial Emmet's support for emacs
     eproject                           ;* assign files to projects, programatically
     eshell-bookmark                    ;* Integrate bookmarks with eshell.
     esup                               ;* the Emacs StartUp Profiler (ESUP)
     expand-region                      ;* Increase selected region by semantic units.
     eyebrowse                          ;* Easy window config switching
     f                                  ;* Modern API for working with files and directories
     feature-mode                       ;* Major mode for editing Gherkin (i.e. Cucumber) user stories
     find-file-in-project               ;* Find file/directory and review Diff/Patch/Commit efficiently everywhere
     flx                                ;* fuzzy matching with good sorting
     flycheck                           ;* On-the-fly syntax checking
     flycheck-plantuml                  ;* Integrate plantuml with flycheck
     focus                              ;* Dim the font color of text in surrounding sections
     ggtags                             ;* emacs frontend to GNU Global source code tagging system
     gist                               ;* Emacs integration for gist.github.com
     git-gutter-fringe+                 ;* Fringe version of git-gutter+.el
     google-this                        ;* A set of functions and bindings to google under point.
     google-translate                   ;* Emacs interface to Google Translate.
     helpful                            ;* a better *help* buffer
     highlight-escape-sequences         ;* Highlight escape sequences
     highlight-indent-guides            ;* Minor mode to highlight indentation
     htmlize                            ;* Convert buffer text and decorations to HTML.
     hydra                              ;* Make bindings that stick around.
     iedit                              ;* Edit multiple regions in the same way simultaneously.
     imenu-anywhere                     ;* ido/ivy/helm imenu across same mode/project/etc buffers
     impatient-mode                     ;* Serve buffers live over HTTP
     indium                             ;* JavaScript Awesome Development Environment
     inflections                        ;* convert english words between singular and plural
     js2-mode                           ;* Improved JavaScript editing mode
     js2-refactor                       ;* A JavaScript refactoring library for emacs.
     json-mode                          ;* Major mode for editing JSON files.
     jump-char                          ;* navigation by char
     kaesar-mode                        ;* Another AES algorithm encrypt/decrypt string with password.
     kakapo-mode                        ;* TABS (hard or soft) for indentation (leading whitespace), and SPACES for alignment.
     key-chord                          ;* map pairs of simultaneously pressed keys to commands
     less-css-mode                      ;* Major mode for editing LESS CSS files (lesscss.org)
     litable                            ;* dynamic evaluation replacement with emacs
     lorem-ipsum                        ;* Insert dummy pseudo Latin text.
     macrostep                          ;* interactive macro expander
     magit                              ;* A Git porcelain inside Emacs
     markdown-mode                      ;* Major mode for Markdown-formatted text
     minimap                            ;* Sidebar showing a "mini-map" of a buffer
     mouse-slider-mode                  ;* scale numbers dragged under the mouse
     multifiles                         ;* View and edit parts of multiple files in one buffer
     multiple-cursors                   ;* Multiple cursors for Emacs.
     nasm-mode                          ;* NASM x86 assembly major mode
     neotree                            ;* A tree plugin like NerdTree for Vim
     nodejs-repl                        ;* Run Node.js REPL
     nyan-mode                          ;* Nyan Cat shows position in current buffer in mode-line.
     ob-restclient                      ;* org-babel functions for restclient-mode
     org                                ;* Outline-based notes management and organizer
     org-page                           ;* a static site generator based on org mode
     paredit                            ;* minor mode for editing parentheses
     paredit-everywhere                 ;* Enable some paredit features in non-lisp buffers
     paren-face                         ;* a face for parentheses in lisp modes
     pcre2el                            ;* regexp syntax converter
     persistent-scratch                 ;* Preserve the scratch buffer across Emacs sessions
     php-mode                           ;* Major mode for editing PHP code
     plantuml-mode                      ;* Major mode for PlantUML
     powerline                          ;* Rewrite of Powerline
     py-autopep8                        ;* Use autopep8 to beautify a Python buffer
     python-mode                        ;* Python major mode
     rainbow-mode                       ;* Colorize color names in buffers
     restart-emacs                      ;* Restart emacs from within emacs
     restclient                         ;* An interactive HTTP client for Emacs
     s                                  ;* The long lost Emacs string manipulation library.
     shell-command                      ;* enables tab-completion for `shell-command'
     simple-httpd                       ;* pure elisp HTTP server
     simplezen                          ;* A simple subset of zencoding-mode for Emacs.
     skewer-mode                        ;* live browser JavaScript, CSS, and HTML interaction
     slime                              ;* Superior Lisp Interaction Mode for Emacs
     slime-company                      ;* slime completion backend for company mode
     smart-compile                      ;* an interface to `compile'
     smex                               ;* M-x interface with Ido-style fuzzy matching.
     solarized-theme                    ;* The Solarized color theme, ported to Emacs.
     string-edit                        ;* Avoid escape nightmares by editing string in separate buffer
     suggest                            ;* suggest elisp functions that give the output requested
     switch-window                      ;* A *visual* way to choose a window to switch to
     tabbar                             ;* Display a tab bar in the header line
     theme-looper                       ;* Loop thru the available color-themes
     try                                ;* Try out Emacs packages.
     undo-tree                          ;* Treat undo history as a tree
     use-package                        ;* A configuration macro for simplifying your .emacs
     use-package-chords                 ;* key-chord keyword for use-package
     vimish-fold                        ;* Fold text like in Vim
     visual-regexp                      ;* A regexp/replace command for Emacs with interactive visual feedback
     vlf                                ;* View Large Files
     vue-html-mode                      ;* Major mode for editing Vue.js templates
     vue-mode                           ;* Major mode for vue component based on mmm-mode
     web-mode                           ;* major mode for editing web templates
     wgrep                              ;* Writable grep buffer and apply the changes to files
     which-key                          ;* Display available keybindings in popup
     whitespace-cleanup-mode            ;* Intelligently call whitespace-cleanup on save
     wttrin                             ;* Emacs frontend for weather web service wttr.in
     xref-js2                           ;* Jump to references/definitions using ag & js2-mode's AST
     yasnippet                          ;* Yet another snippet extension for Emacs.
     zerodark-theme                     ;* A dark, medium contrast theme for Emacs
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; install org-plus-contrib after initial startup only if not already installed
(add-hook 'after-init-hook #'(lambda() (unless (package-installed-p 'org-plus-contrib)
                                    (package-refresh-contents)
                                    (package-install 'org-plus-contrib))))

(provide 'setup-package)
;;; setup-package.el ends here
