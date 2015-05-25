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
;;; Requires
(eval-when-compile
  (require 'use-package))
;; use-package dependencies, but don't know yet if being disabled has any consequences
;(require 'bind-key)
;(require 'diminish)
;;
;; Search init file for bugs
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
            "Anonymous Pro" ; http://www.marksimonson.com/fonts/view/anonymous-pro
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
                                                         (_ 12))
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
                                                            (_ 12)))
    (dynamic-fonts-setup)))
;;; Themes
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
        ;solarized-height-minus-1 1.0
        ;solarized-height-plus-1 1.0
        ;solarized-height-plus-2 1.0
        ;solarized-height-plus-3 1.0
        ;solarized-height-plus-4 1.0
        ))
;;
(use-package subatomic
  :disabled t
  :ensure subatomic-theme
  :defer t
  :init (load-theme 'subatomic 'no-confirm))
;;; ==== COMMON SETTING ====
(column-number-mode t)
;; no beep sound
(setq visible-bell t)
;; no blinking cursor
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode nil))
;; Get rid of tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;;
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
;; avoid accidental quit
(setq confirm-kill-emacs 'y-or-n-p)
;; warn when opening files bigger than 200MB
(setq large-file-warning-threshold 200000000)
;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
;;
;; ==== KEYBINDING ====
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
