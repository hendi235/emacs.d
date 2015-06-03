;;; myconfig-smartparens.el --- Personal smartparens configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal smartparens configuration.
;;
;; Extends the default `smartparens-config' with my custom key bindings and
;; additional pairs.

(require 'smartparens-config)

;;; Custom pair for slflex
  ;; these pairs do not have global definitions, only appropriate in slflex-mode
  (sp-local-pair 'slflex-mode "SWITCH" "END SWITCH")
  (sp-local-pair 'slflex-mode "IF" "END IF")
  (sp-local-pair 'slflex-mode "CASE" "END CASE")
  (sp-local-pair 'slflex-mode "WHILE" "END WHILE")
  
(provide 'myconfig-smartparens)

;;; myconfig-smartparens.el ends here
