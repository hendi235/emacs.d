;;; init.el --- Bootstrap Emacs configuration
;;;
;;; ;;; Commentary:
;;;
;;; PACKAGE REPO
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize))
;;;
;;;
;;; The absolute minimal package I want to have, loaded automatically if not installed yet
(defvar my-packages '(magit atom-dark-theme))
(package-initialize)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
;;;
;;;
;;; CUSTOM SET
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(ansi-term-color-vector
   [unspecified "#151718" "#CE4045" "#9FCA56" "#DCCD69" "#55B5DB" "#A074C4" "#55B5DB" "#D4D7D6"])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (atom-dark)))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "94ba29363bfb7e06105f68d72b268f85981f7fba2ddef89331660033101eb5e5" "0251780e8e79d2a5e75eec7ee3b6c646b882495cb884d9dd32f30c60f9d65db6" default)))
 '(display-time-24hr-format t)
 '(frame-background-mode (quote dark))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(mode-require-final-newline t)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(show-trailing-whitespace nil)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(truncate-lines t))
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
