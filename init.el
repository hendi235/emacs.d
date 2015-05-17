;;; init.el --- Bootstrap Emacs configuration
;;;
;;; ;;; Commentary:
;;;
;;; PACKAGE REPO
(when (>= emacs-major-version 24)
  (require 'package)
  ;;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize))
;;;
;;;
;;; The absolute minimal package I want to have, loaded automatically if not installed yet
(defvar my-packages '(magit atom-dark-theme nlinum))
(package-initialize)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
;;;
;;; CUSTOM LOAD
(load-file "slflex-mode.el")
;;;
;;;
;;;
;;; CUSTOM SET
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(ansi-term-color-vector
   [unspecified "#151718" "#CE4045" "#9FCA56" "#DCCD69" "#55B5DB" "#A074C4" "#55B5DB" "#D4D7D6"])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (atom-dark)))
 '(custom-safe-themes
   (quote
    ("90d329edc17c6f4e43dbc67709067ccd6c0a3caa355f305de2041755986548f2" "108b3724e0d684027c713703f663358779cc6544075bc8fd16ae71470497304f" "94ba29363bfb7e06105f68d72b268f85981f7fba2ddef89331660033101eb5e5" "f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" default)))
 '(fci-rule-color "#2D2D2D")
 '(horizontal-scroll-bar-mode t)
 '(package-selected-packages
   (quote
    (seti-theme pkg-info nlinum magit ido-vertical-mode ido-ubiquitous helm-themes helm-package helm-mode-manager helm-ag hc-zenburn-theme grandshell-theme csv-mode color-theme-solarized atom-dark-theme)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#202020")
 '(vc-annotate-color-map
   (quote
    ((20 . "#C99090")
     (40 . "#D9A0A0")
     (60 . "#ECBC9C")
     (80 . "#DDCC9C")
     (100 . "#EDDCAC")
     (120 . "#FDECBC")
     (140 . "#6C8C6C")
     (160 . "#8CAC8C")
     (180 . "#9CBF9C")
     (200 . "#ACD2AC")
     (220 . "#BCE5BC")
     (240 . "#CCF8CC")
     (260 . "#A0EDF0")
     (280 . "#79ADB0")
     (300 . "#89C5C8")
     (320 . "#99DDE0")
     (340 . "#9CC7FB")
     (360 . "#E090C7"))))
 '(vc-annotate-very-old-color "#E090C7"))
;;;
;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 113 :width normal)))))
;;;
;;;
;;; Global key rebinding
;;(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key [f12] 'toggle-truncate-lines)
;;;
;;; No more scattering backup file(s)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
;;;
