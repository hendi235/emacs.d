;;; init.el --- Emacs configuration of me, based on config by Sebastian Wiesner <https://gihub.com/lunaryorn/.emacs.d> -*- lexical-binding: t; -*-

;;; Commentary:

;;
;; User key prefixes:
;;
;; - C-c A: Align
;; - C-c a: Ag
;; - C-c b: Helm commands (b for "browse")
;; - C-c d: Data stuff
;; - C-c e: Edit commands, general and mode specific
;; - C-c f: Files
;; - C-c h: Help and documentation
;; - C-c j: Jumping and navigation
;; - C-c l: List things
;; - C-c m: Multiple cursors
;; - C-c s: Symbol commands
;; - C-c t: Skeletons and templates
;; - C-c u: Miscellaneous utilities, including minor modes
;; - C-c v: Version control
;; - C-c w: Web stuff

;;; Code:

;;; Debugging
(setq message-log-max 10000)

;;; ==== PACKAGE MGT ====
;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Requires
(eval-when-compile
  (require 'use-package))

;; use-package dependencies, but don't know yet if being disabled has any consequences
(require 'bind-key)

;; diminish minor mode text from modeline
(require 'diminish)

(require 'subr-x)
(require 'rx)
(require 'time-date)


;;; ==== INITIALIZATION ====
;; And disable the site default settings
(setq inhibit-default-init t)

;;; ==== ENVIRONMENT ====
;;; Environment fixup
(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      ;; Use a non-interactive shell.  We use a login shell, even though we have
      ;; our paths setup in .zshenv.  However, OS X adds global settings to the
      ;; login profile.  Notably, this affects /usr/texbin from MacTeX
      (setq exec-path-from-shell-arguments '("-l")))

    (dolist (var '("EMAIL" "PYTHONPATH" "INFOPATH" "SBT_OPTS"))
      (add-to-list 'exec-path-from-shell-variables var))

    (exec-path-from-shell-initialize)

    (setq user-mail-address (getenv "EMAIL"))

    ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
    ;; already initializes info, we need to explicitly add the $INFOPATH
    ;; directories to `Info-directory-list'.
    (with-eval-after-load 'info
      (dolist (dir (parse-colon-path (getenv "INFOPATH")))
        (when dir
          (add-to-list 'Info-directory-list dir))))))

;;; Customization, init file and package management
(defconst lunaryorn-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :defer t
  :config
  (setq custom-file lunaryorn-custom-file
        custom-buffer-done-kill nil            ; Kill when existing
        custom-buffer-verbose-help nil         ; Remove redundant help text
        ;; Show me the real variable name
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil)
  :init (load lunaryorn-custom-file 'no-error 'no-message))

(use-package paradox                    ; Better package menu
  :ensure t
  :bind (("C-c l p" . paradox-list-packages)
         ("C-c l P" . package-list-packages-no-fetch))
  :config
  ;; Don't ask for a token, please, and don't bug me about asynchronous updates
  (setq paradox-github-token t
        paradox-execute-asynchronously nil))

(use-package bug-hunter                 ; Search init file for bugs
  :ensure t)

(use-package server                     ; The server of `emacsclient'
  :defer t
  :init (server-mode)
  :diminish server-buffer-clients)

;;; ==== UI ====

;;; Font setup
(set-frame-font "Consolas" nil t)   ; Default font


;; Select best available font
;; dynamic-fonts is depend on this package
;(use-package font-utils
;  :ensure t)

;; Get rid of tool bar, menu bar and scroll bars.  On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
;  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No blinking and beeping, no startup screen, no scratch message and short
;; Yes/No questions.
(blink-cursor-mode -1)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      initial-scratch-message "Hello there!\n")
(fset 'yes-or-no-p #'y-or-n-p)
;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

(use-package dynamic-fonts              ; Select best available font
  :disabled t           
  :ensure t
  :config
  (progn
    (setq dynamic-fonts-preferred-monospace-fonts
          '(
            ;; Best fonts
            ;"Source Code Pro"   ; https://github.com/adobe-fonts/source-code-pro
            ;"Anonymous Pro" ; http://www.marksimonson.com/fonts/view/anonymous-pro
            ;; Consolas and its free alternative.  Ok, but not my preference
            ;"Inconsolata"
            "Consolas"
            ;; Also still kind of ok
            ;"Fira Mono"
            ;; System fonts, as last resort
            "Menlo"
            ;"DejaVu Sans Mono"
            "Bitstream Vera Mono"
            "Courier New")
          ;dynamic-fonts-preferred-monospace-point-size (pcase system-type
          ;                                               (`darwin 13)
          ;                                               (_ 11))
          dynamic-fonts-preferred-monospace-point-size 11

          dynamic-fonts-preferred-proportional-fonts
          '(
            ;; Best, from
            ;; https://www.mozilla.org/en-US/styleguide/products/firefox-os/typeface/
            ;"Fira Sans"
            ;; System fonts, as last resort
            ;"Helvetica"
            "Segoe UI"
            ;"DejaVu Sans"
            ;"Bitstream Vera"
            "Tahoma"
            ;"Verdana"
            ;"Arial Unicode MS"
            "Arial")
         ;;dynamic-fonts-preferred-proportional-point-size (pcase system-type
         ;;                                                   (`darwin 13)
         ;;                                                   (_ 11))
         dynamic-fonts-preferred-proportional-point-size 11
          )

    (dynamic-fonts-setup)))

(use-package unicode-fonts              ; Map Unicode blocks to fonts
  :disabled t
  :ensure t
  ;; Enable emoticon mappings
  :config (progn (setq unicode-fonts-skip-font-groups '(low-quality-glyphs)
                       unicode-fonts-use-prepend t)

                 (unicode-fonts-setup)))

;;; THEMES
(use-package spacegray   ; current active theme
  :ensure spacegray-theme
  :defer t
  :init (load-theme 'spacegray 'no-confirm))

(use-package solarized
  :disabled t
  :ensure solarized-theme
  :defer t
  :init (load-theme 'solarized-dark 'no-confirm)
  :config
  ;; Disable variable pitch fonts in Solarized theme
  (setq solarized-use-variable-pitch nil
        ;; Don't add too much colours to the fringe
        solarized-emphasize-indicators nil
        ;; I find different font sizes irritating.
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0))

(use-package subatomic
  :disabled t
  :ensure subatomic-theme
  :defer t
  :init (load-theme 'subatomic 'no-confirm))

(bind-key "C-c u v" #'variable-pitch-mode)

;; show line and column number
(line-number-mode t)
(column-number-mode t)





;;; ==== BEHAVIOR ====

;;; MiniBuffer and helm
(setq history-length 1000)              ; Store more history

(use-package savehist                   ; Save minibuffer history
  :init (savehist-mode t)
  :config (setq savehist-save-minibuffer-history t
                savehist-autosave-interval 180))

(use-package helm                       ; Powerful minibuffer input framework
  :ensure t
  :bind (("C-c b b" . helm-resume))
  :init (progn (helm-mode 1)
               (with-eval-after-load 'helm-config
                 (warn "`helm-config' loaded! Get rid of it ASAP!")))
  :config (setq helm-split-window-in-side-p t)
  :diminish helm-mode)

(use-package helm-misc                  ; Misc helm commands
  :ensure helm
  :bind (([remap switch-to-buffer] . helm-mini)))

(use-package helm-command               ; M-x in Helm
  :ensure helm
  :bind (([remap execute-extended-command] . helm-M-x)))

(use-package helm-eval                  ; Evaluate expressions with Helm
  :ensure helm
  :bind (("C-c b M-:" . helm-eval-expression-with-eldoc)
         ("C-c b *"   . helm-calcul-expression)))

(use-package helm-color                 ; Input colors with Helm
  :ensure helm
  :bind (("C-c b c" . helm-colors)))

(use-package helm-unicode               ; Unicode input with Helm
  :ensure t
  :bind ("C-c b 8" . helm-unicode))

;;; Buffer, Windows and Frames
(setq frame-resize-pixelwise t          ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))
(setq-default line-spacing 0.1)         ; A bit more spacing between lines

;; Configure `display-buffer' behaviour for some special buffers.
(setq display-buffer-alist
      `(
        ;; Put REPLs and error lists into the bottom side window
        (,(rx bos (or "*Flycheck errors*" ; Flycheck error list
                      "*compilation"      ; Compilation buffers
                      "*sbt"              ; SBT REPL and compilation buffer
                      "*SQL"              ; SQL REPL
                      "*shell"            ; Shell window
                      ))
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (side            . bottom)
         (reusable-frames . visible)
         (window-height   . 0.33))
        ;; Let `display-buffer' reuse visible frames for all buffers.  This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; later entry with more specific actions.
        ("." nil (reusable-frames . visible))))

(use-package frame                      ; Frames
  :bind (("C-c u F" . toggle-frame-fullscreen))
  :init (progn
          ;; Kill `suspend-frame'
          (global-set-key (kbd "C-z") nil)
          (global-set-key (kbd "C-x C-z") nil))
  :config (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(use-package focus-autosave-mode        ; Save buffers when focus is lost
  :ensure t
  :init (focus-autosave-mode)
  :diminish (focus-autosave-mode))

(use-package uniquify                   ; Make buffer names unique
  :config (setq uniquify-buffer-name-style 'forward))

(use-package helm-buffers               ; Helm for buffer management
  :ensure helm
  :defer t
  :config (setq helm-buffers-fuzzy-matching t))

(use-package ibuffer                    ; Better buffer list
  :bind (([remap list-buffers] . ibuffer))
  ;; Show VC Status in ibuffer
  :config (setq ibuffer-formats
                '((mark modified read-only vc-status-mini " "
                        (name 18 18 :left :elide)
                        " "
                        (size 9 -1 :right)
                        " "
                        (mode 16 16 :left :elide)
                        " "
                        (vc-status 16 16 :left)
                        " "
                        filename-and-process)
                  (mark modified read-only " "
                        (name 18 18 :left :elide)
                        " "
                        (size 9 -1 :right)
                        " "
                        (mode 16 16 :left :elide)
                        " " filename-and-process)
                  (mark " "
                        (name 16 -1)
                        " " filename))))

(use-package ibuffer-vc                 ; Group buffers by VC project and status
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-projectile         ; Group buffers by Projectile project
  :ensure t
  :defer t)

(use-package windmove                   ; Move between windows with Shift+Arrow
  :bind (("S-<left>"  . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<up>"    . windmove-up)
         ("S-<down>"  . windmove-down)))

(use-package winner                     ; Undo and redo window configurations
  :init (winner-mode))

(use-package ediff-wind                 ; Ediff window management
  :defer t
  :config
  ;; Prevent Ediff from spamming the frame
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

(use-package desktop                    ; Save buffers, windows and frames
  :init (desktop-save-mode)
  :config (progn
            ;; Save desktops a minute after Emacs was idle.
            (setq desktop-auto-save-timeout 60)

            (dolist (mode '(magit-mode git-commit-mode))
              (add-to-list 'desktop-modes-not-to-save mode))))


;;; FILE HANDLING

;; warn when opening files bigger than 200MB
(setq large-file-warning-threshold 200000000)

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Delete files to trash
(setq delete-by-moving-to-trash t)

(use-package files                      ; Core commands for files
  :bind (("C-c f z" . revert-buffer)
         ("C-c f /" . revert-buffer))
  :config
  ;; Use GNU ls for Emacs
  (when-let (gnu-ls (and (eq system-type 'darwin) (executable-find "gls")))
    (setq insert-directory-program gnu-ls)))

(use-package ffap                       ; Find files at point
  :defer t
  ;; Please stop pinging random hosts!  See
  ;; https://github.com/technomancy/emacs-starter-kit/issues/39
  :config (setq ffap-machine-p-known 'reject))

(use-package tramp                      ; Access remote files
  :defer t
  :config
  ;; Store auto-save files locally
  (setq tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save")))

(use-package dired                      ; Edit directories
  :defer t
  :config
  (progn
    (require 'dired-x)

    (setq dired-auto-revert-buffer t    ; Revert on re-visiting
          ;; Better dired flags: `-l' is mandatory, `-a' shows all files, `-h'
          ;; uses human-readable sizes, and `-F' appends file-type classifiers
          ;; to file names (for better highlighting)
          dired-listing-switches "-alhF"
          dired-ls-F-marks-symlinks t   ; -F marks links with @
          ;; Inhibit prompts for simple recursive operations
          dired-recursive-copies 'always)

    (when (or (memq system-type '(gnu gnu/linux))
              (string= (file-name-nondirectory insert-directory-program) "gls"))
      ;; If we are on a GNU system or have GNU ls, add some more `ls' switches:
      ;; `--group-directories-first' lists directories before files, and `-v'
      ;; sorts numbers in file names naturally, i.e. "image1" goes before
      ;; "image02"
      (setq dired-listing-switches
            (concat dired-listing-switches " --group-directories-first -v")))))

(use-package dired-x                    ; Additional tools for Dired
  :bind (("C-x C-j" . dired-jump))
  :init (add-hook 'dired-mode-hook #'dired-omit-mode)
  :config
  (progn
    (setq dired-omit-verbose nil)        ; Shut up, dired

    (when (eq system-type 'darwin)
      ;; OS X bsdtar is mostly compatible with GNU Tar
      (setq dired-guess-shell-gnutar "tar"))

    ;; Diminish dired-omit-mode. We need this hack, because Dired Omit Mode has
    ;; a very peculiar way of registering its lighter explicitly in
    ;; `dired-omit-startup'.  We can't just use `:diminish' because the lighter
    ;; isn't there yet after dired-omit-mode is loaded.
    (add-function :after (symbol-function 'dired-omit-startup)
                  (lambda () (diminish 'dired-omit-mode))
                  '((name . dired-omit-mode-diminish)))))

(use-package helm-files                 ; Helm for file finding
  :ensure helm
  :defer t
  :bind (([remap find-file] . helm-find-files)
         ("C-c f r"         . helm-recentf))
  :config (setq helm-recentf-fuzzy-match t
                ;; Use recentf to find recent files
                helm-ff-file-name-history-use-recentf t
                ;; Find library from `require', `declare-function' and friends
                helm-ff-search-library-in-sexp t))

(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :ensure t
  :config (progn (dolist (name '(".cask"
                                 ".vagrant"
                                 ".ensime_cache" ".ensime"))
                   ;; Ignore some additional directories
                   (add-to-list 'ignoramus-file-basename-exact-names name))
                 (ignoramus-setup)))

(use-package bookmark                   ; Bookmarks for Emacs buffers
  :bind (("C-c l b" . list-bookmarks))
  ;; Save bookmarks immediately after a bookmark was added
  :config (setq bookmark-save-flag 1))

(use-package recentf                    ; Save recently visited files
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'" ; Package files
                              "/itsalltext/" ; It's all text temp files
                              ;; And all other kinds of boring files
                              #'ignoramus-boring-p)))

(use-package saveplace                  ; Save point position in files
  :config (setq-default save-place t))

(setq view-read-only t)                 ; View read-only files

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :config (setq auto-revert-verbose nil ; Shut up, please!
                ;; Revert Dired buffers, too
                global-auto-revert-non-file-buffers t))

(use-package image-file                 ; Visit images as images
  :init (auto-image-file-mode))

(use-package launch                     ; Open files in external programs
  :ensure t
  :defer t)

(use-package reveal-in-finder           ; Reveal current buffer in finder
  :ensure t
  :bind (("C-c f f" . reveal-in-finder)))

;;; Additional bindings for built-ins
(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)


;;; NAVIGATION AND SCROLL

(setq scroll-margin 0                   ; Drag the point along while scrolling
      scroll-conservatively 1000        ; Never recenter the screen while scrolling
      scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
      ;; These settings make trackpad scrolling on OS X much more predictable
      ;; and smooth
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

(use-package avy-jump                   ; Jump to characters in buffers
  :ensure avy
  :bind (("C-c j s" . avy-isearch)
         ("C-c j j" . avy-goto-char-2)
         ("C-c j w" . avy-goto-word-1)))

(use-package ace-link                   ; Fast link jumping
  :ensure t
  :defer t
  :init (progn (with-eval-after-load 'info
                 (bind-key "C-c j l" #'ace-link-info Info-mode-map))

               (with-eval-after-load 'help-mode
                 (defvar help-mode-map)  ; Silence the byte compiler
                 (bind-key "C-c j l" #'ace-link-help help-mode-map))))

(use-package ace-window                 ; Fast window switching
  :ensure t
  :bind (("C-x o" . ace-window)
         ("C-c o" . ace-window)))

(use-package page-break-lines           ; Turn page breaks into lines
  :ensure t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package outline                    ; Navigate outlines in buffers
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'outline-minor-mode))
  :diminish (outline-minor-mode . "📑"))

(use-package nlinum                     ; Line numbers in display margin
  :ensure t
  :bind (("C-c u l" . nlinum-mode)))

(use-package helm-imenu                 ; Helm frontend for imenu
  :ensure helm
  :bind (("C-c i" . helm-imenu-in-all-buffers)))


;;; BASIC EDITING

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 8)
;; Make Tab complete if the line is indented
(setq tab-always-indent 'complete)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

(setq kill-ring-max 200                 ; More killed items
      ;; Save the contents of the clipboard to kill ring before killing
      save-interprogram-paste-before-kill t)


(use-package helm-ring                  ; Helm commands for rings
  :ensure helm
  :bind (([remap yank-pop]        . helm-show-kill-ring)
         ([remap insert-register] . helm-register)))

(use-package delsel                     ; Delete the selection instead of insert
  :defer t
  :init (delete-selection-mode))

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :bind (("C-c u w c" . whitespace-cleanup-mode)
         ("C-c e w" . whitespace-cleanup))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish (whitespace-cleanup-mode . "⌫"))

(use-package subword                    ; Subword/superword editing
  :defer t
  :diminish subword-mode)

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-regexp              ; Regexp replace with in-buffer display
  :ensure t
  :bind (("C-c r" . vr/query-replace)
         ("C-c R" . vr/replace)))

(use-package zop-to-char                ; Better zapping
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(use-package easy-kill                  ; Easy killing and marking on C-w
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp]      . easy-mark)))

(use-package align                      ; Align text in buffers
  :bind (("C-c e a" . align)
         ("C-c e c" . align-current)
         ("C-c e r" . align-regexp)))

(use-package multiple-cursors           ; Edit text with multiple cursors
  :ensure t
  :bind (("C-c m <SPC>" . mc/vertical-align-with-space)
         ("C-c m a"     . mc/vertical-align)
         ("C-c m e"     . mc/mark-more-like-this-extended)
         ("C-c m h"     . mc/mark-all-like-this-dwim)
         ("C-c m l"     . mc/edit-lines)
         ("C-c m n"     . mc/mark-next-like-this)
         ("C-c m p"     . mc/mark-previous-like-this)
         ("C-c m r"     . vr/mc-mark)
         ("C-c m C-a"   . mc/edit-beginnings-of-lines)
         ("C-c m C-e"   . mc/edit-ends-of-lines)
         ("C-c m C-s"   . mc/mark-all-in-region))
  :config
  (setq mc/mode-line
        ;; Simplify the MC mode line indicator
        '(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
                      face font-lock-warning-face)))

(use-package expand-region              ; Expand region by semantic units
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Same for region casing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package auto-insert                ; Automatic insertion into new files
  :defer t
  :bind (("C-c e i" . auto-insert)))

(use-package copyright                  ; Deal with copyright notices
  :defer t
  :bind (("C-c e C" . copyright-update))
  ;; Update copyright when visiting files
  :init (add-hook 'find-file-hook #'copyright-update)
  ;; Use ranges to denote consecutive years
  :config (setq copyright-year-ranges t
                copyright-names-regexp (regexp-quote user-full-name)))

;; Additional keybindings
(bind-key [remap just-one-space] #'cycle-spacing)

;;; Paired delimiters
(use-package elec-pair                  ; Electric pairs
  :disabled t
  :init (electric-pair-mode))

(use-package paren                      ; Highlight paired delimiters
  :disabled t
  :init (show-paren-mode)
  :config (setq show-paren-when-point-inside-paren t
                show-paren-when-point-in-periphery t))

(use-package smartparens                ; Parenthesis editing and balancing
  :ensure t
  :init (progn (smartparens-global-mode)
               (show-smartparens-global-mode)

               (dolist (hook '(inferior-emacs-lisp-mode-hook
                               emacs-lisp-mode-hook))
                 (add-hook hook #'smartparens-strict-mode)))
  :config (progn
            (setq sp-autoskip-closing-pair 'always
                  ;; Don't kill entire symbol on C-k
                  sp-hybrid-kill-entire-symbol nil)

            (use-package myconfig-smartparens ; Personal Smartparens extensions
              :load-path "lisp/"
              :config (hmaulana-smartparens-bind-keys)))
  :diminish smartparens-mode)

;;; HIGHLIGHT AND FONTIFICATION

(defun lunaryorn-whitespace-style-no-long-lines ()
  "Configure `whitespace-mode' for Org.

Disable the highlighting of overlong lines."
  (setq-local whitespace-style (-difference whitespace-style
                                            '(lines lines-tail))))

(defun lunaryorn-whitespace-mode-local ()
  "Enable `whitespace-mode' after local variables where set up."
  (add-hook 'hack-local-variables-hook #'whitespace-mode nil 'local))

(use-package whitespace                 ; Highlight bad whitespace
  :bind (("C-c u w w" . whitespace-mode))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'lunaryorn-whitespace-mode-local))
  :config
  ;; Highlight tabs, empty lines at beg/end, trailing whitespaces and overlong
  ;; portions of lines via faces.  Also indicate tabs via characters
  (setq whitespace-style '(face indentation space-after-tab space-before-tab
                                tab-mark empty trailing lines-tail)
        whitespace-line-column nil)     ; Use `fill-column' for overlong lines
  :diminish (whitespace-mode . "▢"))

(use-package hl-line                    ; Highlight the current line
  :init (global-hl-line-mode 1))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'rainbow-delimiters-mode)))

(use-package hi-lock                    ; Custom regexp highlights
  :init (global-hi-lock-mode))

(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package rainbow-mode               ; Fontify color values in code
  :ensure t
  :bind (("C-c u r" . rainbow-mode))
  :config (add-hook 'css-mode-hook #'rainbow-mode))

(use-package highlight-symbol           ; Highlighting and commands for symbols
  :ensure t
  :defer t
  :bind
  (("C-c s %" . highlight-symbol-query-replace)
   ("C-c s n" . highlight-symbol-next-in-defun)
   ("C-c s o" . highlight-symbol-occur)
   ("C-c s p" . highlight-symbol-prev-in-defun))
  ;; Navigate occurrences of the symbol under point with M-n and M-p, and
  ;; highlight symbol occurrences
  :init (progn (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
               (add-hook 'prog-mode-hook #'highlight-symbol-mode))
  :config
  (setq highlight-symbol-idle-delay 0.4     ; Highlight almost immediately
        highlight-symbol-on-navigation-p t) ; Highlight immediately after
                                        ; navigation
  :diminish highlight-symbol-mode)


;;; SKELETON, COMPLETION, AND EXPANSION

;; In `completion-at-point', do not pop up silly completion buffers for less
;; than five candidates.  Cycle instead.
(setq completion-cycle-threshold 5)

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (progn
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :init (global-company-mode)
  :config
  (progn
    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t))
  :diminish company-mode)

(use-package company-statistics         ; Sort company candidates by statistics
  :ensure t
  :defer t
  :init (company-statistics-mode))

(use-package company-math               ; Completion for Math symbols
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          ;; Add backends for math characters
          (add-to-list 'company-backends 'company-math-symbols-unicode)
          (add-to-list 'company-backends 'company-math-symbols-latex)))

(use-package helm-company               ; Helm frontend for company
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          ;; Use Company for completion
          (bind-key [remap completion-at-point] #'helm-company company-mode-map)
          (bind-key "C-:" #'helm-company company-mode-map)
          (bind-key "C-:" #'helm-company company-active-map)))


;;; Spelling and syntax checking
(use-package ispell                     ; Spell checking
  :defer t
  :config
  (progn
    (setq ispell-program-name (if (eq system-type 'darwin)
                                  (executable-find "aspell")
                                (executable-find "hunspell"))
          ispell-dictionary "en_GB"     ; Default dictionnary
          ispell-silently-savep t       ; Don't ask when saving the private dict
          ;; Increase the height of the choices window to take our header line
          ;; into account.
          ispell-choices-win-default-height 5)

    (unless ispell-program-name
      (warn "No spell checker available.  Install Hunspell or ASpell for OS X."))))

(use-package flyspell                   ; On-the-fly spell checking
  :disabled t
  :bind (("C-c u f s" . flyspell-mode))
  :init (progn (dolist (hook '(text-mode-hook message-mode-hook))
                 (add-hook hook 'turn-on-flyspell))
               (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (progn
    (setq flyspell-use-meta-tab nil
          ;; Make Flyspell less chatty
          flyspell-issue-welcome-flag nil
          flyspell-issue-message-flag nil)

    ;; Free C-M-i for completion
    (define-key flyspell-mode-map "\M-\t" nil)
    ;; Undefine mouse buttons which get in the way
    (define-key flyspell-mouse-map [down-mouse-2] nil)
    (define-key flyspell-mouse-map [mouse-2] nil))
  :diminish (flyspell-mode . "✓"))

(use-package flycheck                   ; On-the-fly syntax checking
  :disabled t
  :ensure t
  :bind (("C-c l e" . list-flycheck-errors)
         ("C-c u f c" . flycheck-mode))
  :init (global-flycheck-mode)
  :config (progn
            (setq flycheck-display-errors-function
                  #'flycheck-display-error-messages-unless-error-list)

            ;; Use italic face for checker name
            (set-face-attribute 'flycheck-error-list-checker-name nil
                                :inherit 'italic)

            (use-package lunaryorn-flycheck ; Personal Flycheck extensions
              :load-path "lisp/"
              :config (progn
                        ;; Don't highlight undesired errors from html tidy
                        (add-hook 'flycheck-process-error-functions
                                  #'lunaryorn-discard-undesired-html-tidy-error)

                        (setq flycheck-mode-line
                            '(:eval (lunaryorn-flycheck-mode-line-status))))))
  :diminish flycheck-mode)

(use-package helm-flycheck              ; Helm frontend for Flycheck errors
  :disabled t
  :ensure t
  :bind (("C-c ! L" . helm-flycheck)))

;;; TEXT EDITING

;;; Other markup languages
(use-package rst                        ; ReStructuredText
  :defer t
  :config
  ;; Indent with 3 spaces after all kinds of literal blocks
  (setq rst-indent-literal-minimized 3
        rst-indent-literal-normal 3)

  (bind-key "C-=" nil rst-mode-map)
  ;; For similarity with AUCTeX
  (bind-key "C-c C-j" #'rst-insert-list rst-mode-map)
  ;; …and with Markdown Mode
  (bind-key "M-RET" #'rst-insert-list rst-mode-map))

(use-package markdown-mode              ; Markdown
  :ensure t
  ;; Just no, dear Markdown Mode.  Don't force that bastard Github dialect upon
  ;; me!
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (progn
    ;; Process Markdown with Pandoc, using a custom stylesheet for nice output
    (let ((stylesheet (expand-file-name
                       (locate-user-emacs-file "etc/pandoc.css"))))
      (setq markdown-command
            (mapconcat #'shell-quote-argument
                       `("pandoc" "--toc" "--section-divs"
                         "--css" ,(concat "file://" stylesheet)
                         "--standalone" "-f" "markdown" "-t" "html5")
                       " ")))

    ;; No filling in GFM, because line breaks are significant.
    (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
    ;; Use visual lines instead
    (add-hook 'gfm-mode-hook #'visual-line-mode)
    (add-hook 'gfm-mode-hook #'lunaryorn-whitespace-style-no-long-lines)

    (bind-key "C-c C-s C" #'markdown-insert-gfm-code-block markdown-mode-map)
    (bind-key "C-c C-s P" #'markdown-insert-gfm-code-block markdown-mode-map)

    ;; Fight my habit of constantly pressing M-q.  We should not fill in GFM
    ;; Mode.
    (bind-key "M-q" #'ignore gfm-mode-map)

    (use-package lunaryorn-markdown     ; Personal Markdown extensions
      :load-path "lisp/"
      :commands (lunaryorn-markdown-post-header)
      :config (bind-key "C-c t h" #'lunaryorn-markdown-post-header
                        markdown-mode-map))))

(use-package jira-markup-mode           ; Jira markup
  :ensure t
  :defer t)

(use-package graphviz-dot-mode          ; Graphviz
  :ensure t
  :defer t
  :config
  (setq graphviz-dot-indent-width 4))

;;; Programming utilities
(use-package prog-mode                  ; Prog Mode
  :bind (("C-c u p" . prettify-symbols-mode)))

(use-package eldoc                      ; Documentation in minibuffer
  :defer t
  ;; Enable Eldoc for `eval-expression', too
  :init (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  :config
  (setq-default eldoc-documentation-function #'describe-char-eldoc)
  :diminish (eldoc-mode . "⁇"))

(use-package restclient                 ; ReST REPL for Emacs
  :ensure t
  :defer t)

(use-package company-restclient         ; Company support for restclient
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-restclient)))

;;; Emacs Lisp
(bind-key "C-c u d" #'toggle-debug-on-error)

(use-package helm-elisp                 ; Helm commands for Emacs Lisp
  :ensure helm
  :bind (("C-c f l" . helm-locate-library)
         ("C-c h a" . helm-apropos)))

(use-package elisp-slime-nav            ; Jump to definition of symbol at point
  :ensure t
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :diminish elisp-slime-nav-mode)

(use-package ielm                       ; Emacs Lisp REPL
  :bind (("C-c z" . ielm)))

(use-package elisp-mode                 ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode ("/Cask\\'" . emacs-lisp-mode)
  :config (progn
            (require 'ert)

;            (use-package lunaryorn-elisp ; Personal tools for Emacs Lisp
;              :load-path "lisp/"
;              :init (progn
;                      (add-hook 'emacs-lisp-mode-hook
;                                #'lunaryorn-add-use-package-to-imenu)
;
;                      (bind-key "C-c f c" #'lunaryorn-find-cask-file
;                                emacs-lisp-mode-map)))
  ))

;;; Python
(use-package python                     ; Python editing
  :defer t
  :config
  (progn
    ;; PEP 8 compliant filling rules, 79 chars maximum
    (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))
    (add-hook 'python-mode-hook #'subword-mode)

    (let ((ipython (executable-find "ipython")))

      (if ipython
          (setq python-shell-interpreter ipython)
        (warn "IPython is missing, falling back to default python")))))

(use-package anaconda-mode              ; Powerful Python backend for Emacs
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook #'anaconda-mode))

(use-package company-anaconda           ; Python backend for Company
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-anaconda)))

(use-package pip-requirements           ; requirements.txt files
  :ensure t
  :defer t)

;;; Ruby
(use-package inf-ruby                   ; Ruby REPL
  :ensure t
  :defer t
  :init (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  :config
  ;; Easily switch to Inf Ruby from compilation modes to Inf Ruby
  (inf-ruby-switch-setup))

(use-package robe                       ; Ruby backend for Emacs
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-robe)))

;;; Web languages

(use-package web-mode                   ; Template editing
  :ensure t
  :defer t
  :mode "/templates?/.*\\.\\(php\\|html\\)\\'"
  :config
  (setq web-mode-markup-indent-offset 2))

(use-package js2-mode                   ; Javascript editing
  :ensure t
  :mode "\\.js\\'"
  :config (progn (setq-default js2-basic-offset 2)
                 (setq js2-global-externs '("angular"))

                 (add-hook 'js2-mode-hook #'js2-highlight-unused-variables-mode)))

(use-package css-mode                   ; CSS editing
  :defer t
  :config
  (progn
    ;; Run Prog Mode hooks, because for whatever reason CSS Mode derives from
    ;; `fundamental-mode'.
    (add-hook 'css-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

    ;; Mark css-indent-offset as safe local variable.  TODO: Report upstream
    (put 'css-indent-offset 'safe-local-variable #'integerp)))

(use-package css-eldoc                  ; Basic Eldoc for CSS
  :ensure t
  :commands (turn-on-css-eldoc)
  :init (add-hook 'css-mode-hook #'turn-on-css-eldoc))

;;; Misc programming languages
(use-package sh-script                  ; Shell scripts
  :mode ("\\.zsh\\'" . sh-mode)
  :config
  ;; Use two spaces in shell scripts.
  (setq sh-indentation 2                ; The basic indentation
        sh-basic-offset 2               ; The offset for nested indentation
        ))

(use-package puppet-mode                ; Puppet manifests
  :disabled t
  :ensure t
  :defer t
  :config
  ;; Fontify variables in Puppet comments
  (setq puppet-fontify-variables-in-comments t))

(use-package nxml-mode                  ; XML editing
  :defer t
  ;; Complete closing tags, and insert XML declarations into empty files
  :config (setq nxml-slash-auto-complete-flag t
                nxml-auto-insert-xml-declaration-flag t))

;;; Databases
(use-package sql                        ; SQL editing and REPL
  :bind (("C-c d c" . sql-connect)
         ("C-c d m" . sql-mysql)))

;;; Version control
(use-package vc-hooks                   ; Simple version control
  :defer t
  :config
  ;; Always follow symlinks to files in VCS repos
  (setq vc-follow-symlinks t))

(use-package diff-hl                    ; Highlight hunks in fringe
  :ensure t
  :defer t
  :init (progn
          ;; Highlight changes to the current file in the fringe
          (global-diff-hl-mode)
          ;; Highlight changed files in the fringe of Dired
          (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

          ;; Fall back to the display margin, if the fringe is unavailable
          (unless (display-graphic-p)
            (diff-hl-margin-mode))))

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind (("C-c g"   . magit-status)
         ("C-c v c" . magit-clone)
         ("C-c v v" . magit-status)
         ("C-c v g" . magit-blame)
         ("C-c v l" . magit-log-buffer-file))
  :config
  (progn
    (setq magit-revert-buffers 'silent
          magit-save-repository-buffers 'dontask
          magit-refs-show-commit-count 'all
          ;; For some reason this doesn't work :(
          ;; magit-completing-read-function
          ;; #'helm-completing-read-with-cands-in-buffer
          )

    ;; Set Magit's repo dirs for `magit-status' from Projectile's known
    ;; projects.  Initialize the `magit-repository-directories' immediately
    ;; after Projectile was loaded, and update it every time we switched
    ;; projects, because the new project might have been unknown before
    (defun lunaryorn-magit-set-repo-dirs-from-projectile ()
      "Set `magit-repo-dirs' from known Projectile projects."
      (let ((project-dirs (bound-and-true-p projectile-known-projects)))
        ;; Remove trailing slashes from project directories, because Magit adds
        ;; trailing slashes again, which breaks the presentation in the Magit
        ;; prompt.
        (setq magit-repository-directories
              (mapcar #'directory-file-name project-dirs))))

    (with-eval-after-load 'projectile
      (lunaryorn-magit-set-repo-dirs-from-projectile))

    (add-hook 'projectile-switch-project-hook
              #'lunaryorn-magit-set-repo-dirs-from-projectile)))

(use-package magit-gh-pulls             ; Show Github PRs in Magit
  :ensure t
  :defer t
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(use-package git-commit                 ; Git commit message mode
  :ensure t
  :defer t
  :config
  ;; Oh, really?  Come on… I know what I'm doing…
  (remove-hook 'git-commit-finish-query-functions
               #'git-commit-check-style-conventions))

(use-package gitconfig-mode             ; Git configuration mode
  :ensure t
  :defer t)

(use-package gitignore-mode             ; .gitignore mode
  :ensure t
  :defer t)

(use-package gitattributes-mode         ; Git attributes mode
  :ensure t
  :defer t)

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :bind (("C-c v t" . git-timemachine)))

;;; SEARCH

(use-package isearch                    ; Search buffers
  :bind (("C-c s s" . isearch-forward-symbol-at-point))
  ;; `:diminish' doesn't work for isearch, because it uses eval-after-load on
  ;; the feature name, but isearch.el does not provide any feature.  For the
  ;; same reason we have to use `:init', but isearch is always loaded anyways.
  :init (progn (diminish 'isearch-mode)

               ;; Please, isearch, let me scroll during search
               (setq isearch-allow-scroll t)))

(use-package helm-regex                 ; Helm regex tools
  :ensure helm
  :bind (([remap occur] . helm-occur)
         ("C-c e o"     . helm-multi-occur)))

(use-package grep                       ; Control grep from Emacs
  :defer t
  :config
  (progn
    (when-let (gnu-find (and (eq system-type 'darwin)
                             (executable-find "gfind")))
      (setq find-program gnu-find))

    (when-let (gnu-xargs (and (eq system-type 'darwin)
                              (executable-find "gxargs")))
      (setq xargs-program gnu-xargs))))

(use-package locate                     ; Search files on the system
  :defer t
  :config
  ;; Use mdfind as locate substitute on OS X, to utilize the Spotlight database
  (when-let (mdfind (and (eq system-type 'darwin) (executable-find "mdfind")))
    (setq locate-command mdfind)))

(use-package ag                         ; Search code in files/projects
  :ensure t
  :bind (("C-c a d" . ag-dired-regexp)
         ("C-c a D" . ag-dired)
         ("C-c a f" . ag-files)
         ("C-c a k" . ag-kill-other-buffers)
         ("C-c a K" . ag-kill-buffers))
  :config
  (setq ag-reuse-buffers t            ; Don't spam buffer list with ag buffers
        ag-highlight-search t         ; A little fanciness
        ;; Use Projectile to find the project root
        ag-project-root-function (lambda (d) (let ((default-directory d))
                                               (projectile-project-root)))))

(use-package wgrep                      ; Edit grep/occur/ag results in-place
  :ensure t
  :defer t)

(use-package wgrep-ag                   ; Wgrep for ag
  :ensure t
  :defer t)

(use-package helm-ag                    ; Helm frontend for Ag
  :ensure t
  :bind (("C-c a a" . helm-do-ag)
         ("C-c a A" . helm-ag))
  :config (setq helm-ag-fuzzy-match t
                helm-ag-insert-at-point 'symbol
                helm-ag-source-type 'file-line))

;;; PROJECT MANAGEMENT

(use-package projectile                 ; Project management for Emacs
  :ensure t
  :init (projectile-global-mode)
  :config
  (progn
    ;; Remove dead projects when Emacs is idle
    (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

    (setq projectile-completion-system 'helm
          projectile-find-dir-includes-top-level t
          projectile-mode-line '(:propertize
                                 (:eval (concat " " (projectile-project-name)))
                                 face font-lock-constant-face))

    (projectile-register-project-type 'haskell-stack '("stack.yml")
                                      "stack build" "stack test"))
  :diminish projectile-mode)

(use-package helm-projectile            ; Helm frontend for Projectile
  :ensure t
  :defer t
  :init (with-eval-after-load 'projectile (helm-projectile-on))
  :config (setq projectile-switch-project-action #'helm-projectile))

;;; Date and time
(use-package calendar                   ; Built-in calendar
  :bind ("C-c u c" . calendar)
  :config
  ;; In Europe we start on Monday
  (setq calendar-week-start-day 1))

(use-package time                       ; Show current time
  :bind (("C-c u i" . emacs-init-time)
         ("C-c u t" . display-time-world))
  :config
  (setq display-time-world-time-format "%H:%M %Z, %d. %b"
        display-time-world-list '(("Europe/Berlin"    "Berlin")
                                  ;("Europe/London"    "London")
                                  ;("Europe/Istanbul"  "Istanbul")
                                  ("America/Winnipeg" "Winnipeg (CA)")
                                  ;("America/New_York" "New York (USA)")                                
                                  ("Asia/Tokyo"       "Tokyo (JP)"))))

;;; Terminal emulation and shells
(use-package shell                      ; Dump shell in Emacs
  :bind ("C-c u s" . shell))

(use-package term                       ; Terminal emulator in Emacs
  :bind ("C-c u S" . ansi-term))

;;; Documents
(use-package doc-view
  :defer t
  :config (progn
            ;; Render PDFs at 300dpi
            (setq doc-view-resolution 300)

            ;; Warn if Doc View falls back to Ghostscript for rendering
            (unless (eq doc-view-pdf->png-converter-function
                        'doc-view-pdf->png-converter-mupdf)
              (warn "Doc View is not using mupdf.
Install mudraw with brew install mupdf-tools"))))

;;; Net & Web
(use-package browse-url                 ; Browse URLs
  :bind (("C-c w u" . browse-url)))

(use-package bug-reference              ; Turn bug refs into browsable buttons
  :defer t
  :init (progn (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
               (add-hook 'text-mode-hook #'bug-reference-mode)))

(use-package eww                        ; Emacs' built-in web browser
  :bind (("C-c w b" . eww-list-bookmarks)
         ("C-c w w" . eww)))

(use-package sendmail                   ; Send mails from Emacs
  :defer t
  :config (setq send-mail-function 'smtpmail-send-it))

(use-package message                    ; Compose mails from Emacs
  :defer t
  :config (setq message-send-mail-function 'smtpmail-send-it
                ;; Don't keep message buffers around
                message-kill-buffer-on-exit t))

;;; ONLINE HELP

(use-package find-func                  ; Find function/variable definitions
  :bind (("C-x F"   . find-function)
         ("C-x 4 F" . find-function-other-window)
         ("C-x K"   . find-function-on-key)
         ("C-x V"   . find-variable)
         ("C-x 4 V" . find-variable-other-window)))

(use-package info                       ; Info manual viewer
  :defer t
  :config
  ;; Fix the stupid `Info-quoted' face.  Courier is an abysmal face, so go back
  ;; to the default face.
  (set-face-attribute 'Info-quoted nil :family 'unspecified
                      :inherit font-lock-type-face))

(use-package helm-info                  ; Helm tools for Info
  :ensure helm
  :bind (("C-c h e" . helm-info-emacs)
         ("C-c h i" . helm-info-at-point)))

(use-package helm-man                   ; Browse manpages with Heml
  :ensure helm
  :bind (("C-c h m" . helm-man-woman)))

(use-package helm-descbinds             ; Describe key bindings with Helm
  :ensure t
  :init (helm-descbinds-mode))

(bind-key "C-c h b" #'describe-personal-keybindings)

;;; MODES

;; sl flex syntax highlight
(use-package slflex-mode
    :load-path "lisp/"
    :config (add-to-list 'auto-mode-alist 
                         '("\\.sl\\>" . slflex-mode))
    )

;;(use-package sltrace-mode
;;    :load-path "lisp/")

;; ==== GLOBAL KEYBINDING ====
(global-set-key (kbd "C-;") 'unhighlight-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
;;
;;; ==== MY CUSTOM FUNCTION ====
;; Keep all your matches highlighted after searching, ala VIM. (see http://www.emacswiki.org/emacs/HighlightFromIsearch)
;(defun isearch-highlight-phrase ()
;        "Invoke `highligh-phrase' from within isearch."
;        (interactive)
;        (let ((case-fold-search isearch-case-fold-search))
;          (highlight-phrase (if isearch-regexp
;                                isearch-string
;                              (regexp-quote isearch-string)))))

;; Use C-TAB to keep the highlight
;(define-key isearch-mode-map (kbd "C-<tab>") 'isearch-highlight-phrase)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here