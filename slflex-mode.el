;;; slflex.el --- major mode for editing slflex source in Emacs -*- lexical-binding: t -*-

;;; Code:

;;(defgroup slflex nil
 ;; "Major mode for editing slflex source in Emacs."
;;  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
;;  :group 'languages)

(defconst slflex-syntax-table
  (let ((table (make-syntax-table)))
    ;; ' is a string delimiter
    ;;(modify-syntax-entry ?' "\"" table)
    ;; " is a string delimiter too
    ;;(modify-syntax-entry ?\" "\"" table)

    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 12" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(define-derived-mode slflex-mode prog-mode "SL Flex Mode"
  :syntax-table slflex-syntax-table
  (font-lock-fontify-buffer))

(provide 'slflex-mode)

;;; slflex.el ends here
