;;; sltrace-mode.el --- sltrace major mode

 ;;; Commentary:
 ;; Based on sample.el by Author StefanMonnier
 ;; 

 ;;; Code:

;; (defvar sltrace-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [foo] 'sltrace-do-foo)
;;     map)
;;   "Keymap for `sltrace-mode'.")

 (defvar sltrace-mode-syntax-table
   (let ((st (make-syntax-table)))
;;     (modify-syntax-entry ?# "<" st)
;;     (modify-syntax-entry ?\n ">" st)
     st)
   "Syntax table for `sltrace-mode'.")

 (defvar sltrace-font-lock-defaults
   '(("flex>\\.\\*in\\.sub\\.\\*" (1 font-lock-function-name-face)))
   )

;; (defvar sltrace-imenu-generic-expression
;;   ...)

;; (defvar sltrace-outline-regexp
;;   ...)

 ;;;###autoload
 (define-derived-mode sltrace-mode fundamental-mode "sltrace mode"
   :syntax-table sltrace-mode-syntax-table
;;   (setq-local comment-start "# ")
;;   (setq-local comment-start-skip "#+\\s-*")
   (setq-local font-lock-defaults
	'(sltrace-font-lock-defaults))
;;   (setq-local indent-line-function 'sltrace-indent-line)
;;   (setq-local imenu-generic-expression
;;	sltrace-imenu-generic-expression)
;;   (setq-local outline-regexp sltrace-outline-regexp)
   )

 ;;; Indentation

 ;;(defun sltrace-indent-line ()
 ;;  "Indent current line of sltrace code."
 ;;  (interactive)
 ;;  (let ((savep (> (current-column) (current-indentation)))
;;	 (indent (condition-case nil (max (sltrace-calculate-indentation) 0)
;;		   (error 0))))
;;     (if savep
;;	 (save-excursion (indent-line-to indent))
;;       (indent-line-to indent))))

;; (defun sltrace-calculate-indentation ()
;;   "Return the column to which the current line should be indented."
;;   ...)


 (provide 'sltrace-mode)
 ;;; sltrace-mode.el ends here
