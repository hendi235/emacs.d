;;; ==== PACKAGE MGT ====
;; Please don't load outdated byte code
(setq load-prefer-newer t)
;; separate file for Customize generated settings
(setq custom-file "~/.emacs-customize")
(when (file-exists-p custom-file)
  (load custom-file))
;;
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(package-initialize)
;;
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;;;
;;; Requires
(eval-when-compile
  (require 'use-package))
;; use-package dependencies, but don't know yet if being disabled has any consequences
(require 'bind-key)
(require 'diminish)
;;
;; Search init file for bugs
;; usage: M-x bug-hunter-init-file RET RET
(use-package bug-hunter
  :ensure t)
;;
;;; ==== UI ====
;; Select best available font
(use-package dynamic-fonts
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
            "Fira Mono"
            ;; System fonts, as last resort
            ;"Menlo"
            "DejaVu Sans Mono"
            "Bitstream Vera Mono"
            "Courier New")
          dynamic-fonts-preferred-monospace-point-size (pcase system-type
                                                         (`darwin 13)
                                                         (_ 10))
          dynamic-fonts-preferred-proportional-fonts
          '(
            ;; Best, from
            ;; https://www.mozilla.org/en-US/styleguide/products/firefox-os/typeface/
            ;"Fira Sans"
            ;; System fonts, as last resort
            "Helvetica"
            "Segoe UI"
            "DejaVu Sans"
            "Bitstream Vera"
            "Tahoma"
            "Verdana"
            "Arial Unicode MS"
            "Arial")
          dynamic-fonts-preferred-proportional-point-size (pcase system-type
                                                            (`darwin 13)
                                                            (_ 10)))
    (dynamic-fonts-setup)))
;;; THEMES
(use-package spacegray   ; current active theme
  ;:disabled t
  :ensure spacegray-theme
  :defer t
  :init (load-theme 'spacegray 'no-confirm))
;;
(use-package solarized
  :disabled t
  :ensure solarized-theme
  :defer t
  :init (load-theme 'solarized-dark 'no-confirm)
  :config
  ;; Disable variable pitch fonts in Solarized theme
  (setq solarized-use-variable-pitch nil
        ;; Don't add too much colours to the fringe
        ;solarized-emphasize-indicators nil
        ;; I find different font sizes irritating.
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0))
;;
(use-package subatomic
  :disabled t
  :ensure subatomic-theme
  :defer t
  :init (load-theme 'subatomic 'no-confirm))
;;
;; Make buffer name unique
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))
;;
;; show column number
(column-number-mode t)
;; no beep sound
(setq visible-bell t)
;; no blinking cursor
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode nil))
;; Get rid of tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;;; ==== BEHAVIOR ====
;; Move between windows with Shift+Arrow
(use-package windmove
  :bind (("S-<left>"  . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<up>"    . windmove-up)
         ("S-<down>"  . windmove-down)))
;; Helm
(use-package helm
  :ensure t
  :bind (
         ;; Replace some standard bindings with Helm equivalents
         ([remap execute-extended-command] . helm-M-x)
         ([remap find-file]                . helm-find-files)
         ([remap switch-to-buffer]         . helm-mini)
         ([remap yank-pop]                 . helm-show-kill-ring)
         ([remap insert-register]          . helm-register)
         ([remap occur]                    . helm-occur)
         ;; Special helm bindings
         ("C-c b b"                        . helm-resume)
         ("C-c b C"                        . helm-colors)
         ("C-c b *"                        . helm-calcul-expression)
         ("C-c b 8"                        . helm-ucs)
         ("C-c b M-:"                      . helm-eval-expression-with-eldoc)
         ;; Helm features in other maps
         ("C-c i"                          . helm-semantic-or-imenu)
         ("C-c h a"                        . helm-apropos)
         ("C-c h e"                        . helm-info-emacs)
         ("C-c h i"                        . helm-info-at-point)
         ("C-c h m"                        . helm-man-woman)
         ("C-c f r"                        . helm-recentf)
         ("C-c f l"                        . helm-locate-library))
  :init (progn (helm-mode 1)

               (with-eval-after-load 'helm-config
                 (warn "`helm-config' loaded! Get rid of it ASAP!")))
  :config (setq helm-split-window-in-side-p t)
  :diminish (helm-mode))
;;
(use-package helm-buffers
  :ensure helm
  :defer t
  :config (setq helm-buffers-fuzzy-matching t))

;;; FILE HANDLING
;; warn when opening files bigger than 200MB
(setq large-file-warning-threshold 200000000)
;;
;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
;;
;; Access remote files
(use-package tramp
  :defer t
  :config
  ;; Store auto-save files locally
  (setq tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save")))
;;
;; Edit directories
(use-package dired
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
;;
;; Additional tools for Dired
(use-package dired-x
  :bind (("C-x C-j" . dired-jump))
  :config
  (progn
    (setq dired-omit-verbose nil)        ; Shut up, dired

    (when (eq system-type 'darwin)
      ;; OS X bsdtar is mostly compatible with GNU Tar
      (setq dired-guess-shell-gnutar "tar"))))
;;
(use-package helm-files
  :ensure helm
  :defer t
  :config (setq helm-recentf-fuzzy-match t
                ;; Use recentf to find recent files
                helm-ff-file-name-history-use-recentf t
                ;; Find library from `require', `declare-function' and friends
                helm-ff-search-library-in-sexp t))
;;
;; Bookmarks for Emacs buffers
(use-package bookmark
  :bind (("C-c l b" . list-bookmarks))
  ;; Save bookmarks immediately after a bookmark was added
  :config (setq bookmark-save-flag 1))
;;
;; Ignore uninteresting files everywhere
(use-package ignoramus
  :ensure t
  :init (ignoramus-setup))
;;
;; Save recently visited files
(use-package recentf
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
;;
;; Save point position in files
(use-package saveplace
  :config (setq-default save-place t))
;;
;; View read-only files
(setq view-read-only t)
;;
;; Auto-revert buffers of changed files
(use-package autorevert
  :init (global-auto-revert-mode))
;;
;; Visit images as images
(use-package image-file
  :init (auto-image-file-mode))
;;
;;; NAVIGATION AND SCROLL
;;
;; Never recenter the screen while scrolling
(setq scroll-conservatively 1000)
;;
;; This package is required by ace-window
(use-package avy
    :ensure t)
;;
;; Jump to characters in buffers.
;(use-package avy-jump
;  :ensure avy
;  :bind (("C-c j s" . avy-isearch)
;         ("C-c j j" . avy-goto-char-2)
;         ("C-c j w" . avy-goto-word-1)))
;;
;; Fast window switching
(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)
         ("C-c o" . ace-window)))
;;
;; Line number in display margin
(use-package nlinum
  :ensure t
  :bind (("C-c t l" . nlinum-mode)))
;;
;;; BASIC EDITING
;;
;; Cleanup whitespace in buffers
(use-package whitespace-cleanup-mode
  :ensure t
  :bind (("C-c t c" . whitespace-cleanup-mode))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish whitespace-cleanup-mode)

;;; HIGHLIGHT AND FONTIFICATION
;; Custom regexp highlights
(use-package hi-lock
  :init (global-hi-lock-mode))
;;
;;; SKELETON, COMPLETION, AND EXPANSION
;; Powerful expansion and completion
(use-package hippie-exp
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
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
;;
;;; SEARCHING
;; Search buffers
(use-package isearch
  :bind (("C-c s s" . isearch-forward-symbol-at-point)))
;;
;; Search code in files/projects
;; usage:  M-x pt-regexp  or  M-x projectile-pt
(use-package pt
  :ensure t
  :bind (("C-c a d" . pt-dired-regexp)
         ("C-c a D" . pt-dired)
         ("C-c a f" . pt-files)
         ("C-c a k" . pt-kill-other-buffers)
         ("C-c a K" . pt-kill-buffers))
  :config
  (setq pt-reuse-buffers t            ; Don't spam buffer list with ag buffers
        pt-highlight-search t         ; A little fanciness
        ;; Use Projectile to find the project root
        pt-project-root-function (lambda (d) (let ((default-directory d))
                                               (projectile-project-root)))))
;;
(use-package helm-pt
  :ensure t
  :bind (("C-c a a" . helm-do-pt)
         ("C-c a A" . helm-pt))
  :config (setq helm-pt-fuzzy-match t
                helm-pt-insert-at-point 'symbol
                helm-pt-source-type 'file-line))
;;
;;; PROJECT MANAGEMENT
;;; Project management with Projectile
(use-package projectile
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
                                 face font-lock-constant-face)))
  :diminish projectile-mode)
;;
;; Projectile with steroids
(use-package helm-projectile
  :ensure t
  :defer t
  :init (with-eval-after-load 'projectile (helm-projectile-on))
  :config (setq projectile-switch-project-action #'helm-projectile))
;;
;;; ONLINE HELP
;; Find function/variable definitions
(use-package find-func
  :bind (("C-x F"   . find-function)
         ("C-x 4 F" . find-function-other-window)
         ("C-x K"   . find-function-on-key)
         ("C-x V"   . find-variable)
         ("C-x 4 V" . find-variable-other-window)))
;;
;; Info manual viewer
(use-package info
  :defer t
  :config
  ;; Fix the stupid `Info-quoted' face.  Courier is an abysmal face, so go back
  ;; to the default face.
  (set-face-attribute 'Info-quoted nil :family 'unspecified
                      :inherit font-lock-type-face))
;;
(use-package helm-descbinds
  :ensure t
  :init (helm-descbinds-mode))
;;; ==== COMMON SETTING ====
;;
(bind-key "C-c h b" #'describe-personal-keybindings)
;;
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
;; avoid accidental quit
(setq confirm-kill-emacs 'y-or-n-p)
;;
;; ==== GLOBAL KEYBINDING ====
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
