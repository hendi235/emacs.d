;;  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;;  (package-initialize))
;;;
;;;
;;; The absolute minimal package I want to have, loaded automatically if not installed yet
;;(defvar my-packages '(magit helm-package helm-mode-manager helm-ag nlinum))
;;(package-initialize)
;;(dolist (p my-packages)
;;  (when (not (package-installed-p p))
;;    (package-install p)))
;;;
;;; CUSTOM LOAD
;;(load-file "slflex-mode.el")
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
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("0f63b94e366a6a9cd3ac12b3f5e7b88ba214fd592a99fb5bc55af33fb2280c7f" "21d9280256d9d3cf79cbcf62c3e7f3f243209e6251b215aede5026e0c5ad853f" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "90d329edc17c6f4e43dbc67709067ccd6c0a3caa355f305de2041755986548f2" "084fb2f46f170f79b68db062e7d47df904739e4846bf4cea742dbf66ceef672a" "fcecf5757b909acbacc4fea2fa6a5fb9a63776be43cbff1dc0dffc9c02674478" default)))
 '(horizontal-scroll-bar-mode t)
 '(package-archives
   (quote
    (("melpa-stable" . "http://stable.melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
;; '(package-selected-packages
;;   (quote
;;    (smex pkg-info nlinum magit ido-vertical-mode ido-ubiquitous dash csv-mode color-theme-sanityinc-solarized)))
 '(save-place t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
;;;
;;; ==== MY CUSTOM SETTINGS MUST START HERE ====
;;;
;;; No more scattering backup file(s)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
;;;
;;;
;;; HELM configuration
(add-to-list 'load-path "~/.emacs.d/elpa/helm-1.7.1/")
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;;; Somehow below command are error, so comment them
;;(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ;; rebind tab to run persistent action
;;(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ;; make TAB works in terminal
;;(define-key helm-map (kbd "C-z")  'helm-select-action) ;; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ;; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ;; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ;; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ;; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

;; optional fuzzy matching for helm-M-x
(setq helm-M-x-fuzzy-match t)

;; fuzzy matching for helm-mini, a list-buffer replacement
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(helm-mode 1)
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
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-x b") 'helm-mini)
