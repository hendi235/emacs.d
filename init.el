;;  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;;  (package-initialize))
;;;
;;;
;;; The absolute minimal package I want to have, loaded automatically if not installed yet
;;(defvar my-packages '(magit atom-dark-theme nlinum))
;;(package-initialize)
;;(dolist (p my-packages)
;;  (when (not (package-installed-p p))
;;    (package-install p)))
;;;
;;; CUSTOM LOAD
(load-file "slflex-mode.el")
;;;
;;;
;;;
;;; CUSTOM SET
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 113 :width normal)))))
;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wombat)))
 '(horizontal-scroll-bar-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(package-archives
   (quote
    (("melpa-stable" . "http://stable.melpa.org/packages/")
;;     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (smex seti-theme pkg-info nlinum magit ido-vertical-mode ido-ubiquitous helm-package helm-mode-manager helm-ag dash csv-mode color-theme-sanityinc-solarized atom-dark-theme)))
 '(save-place t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
;;;
;;; ==== MY CUSTOM SETTINGS MUST START HERE ====
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
