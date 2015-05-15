;;; slflex.el --- major mode for editing slflex source in Emacs -*- lexical-binding: t -*-

;;; Code:

;;(defgroup slflex nil
 ;; "Major mode for editing slflex source in Emacs."
;;  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
;;  :group 'languages)
;;

(defvar slflex-events
  '("BOOLEAN" "INTEGER" "FLOAT" "STRING" "LONG" "DATE" "DATE_AND_TIME" "ARRAY" "TRUE" "FALSE" "NULL" "ENUM" "BYTE"))

(defvar slflex-keywords
    '("IF" "END IF" "ELSIF" "ELSE" "WHILE" "END WHILE"  "CASE" "END CASE" "RESULTS" "END RESULTS" "PARAMETERS" "END PARAMETERS" "SWITCH" "END SWITCH" "DEFAULT" "END DEFAULT" 
      "OR" "AND" "LEAVE" "THEN" "DO" "OF" "SIZE" "MANDATORY" "RETURN" "EXISTS" "NOT" "INSTANCE" "CALL" "ADMIN" "INTERFACE"
      "DIV" "MOD"))

(defvar slflex-functions
  '("TypeCast"))

(defvar slflex-gacs
  '("theGenericAccess" "CallSupervision"))

  (defvar slflex-font-lock-defaults
    `((
       ;; ; : , ; { } =>  @ $ = are all special elements
       ;;(":\\|,\\|;\\|{\\|}\\|=>\\|@\\|$\\|=" . font-lock-keyword-face)
       ( ,(regexp-opt slflex-keywords 'words) . font-lock-builtin-face)
       ( ,(regexp-opt slflex-events 'words) . font-lock-constant-face)
       ( ,(regexp-opt slflex-functions 'words) . font-lock-function-name-face)
       ( ,(regexp-opt slflex-gacs 'words) . font-lock-preprocessor-face)
       )))

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
  (setq font-lock-defaults slflex-font-lock-defaults)
  (font-lock-fontify-buffer))

(provide 'slflex-mode)

;;; slflex.el ends here
