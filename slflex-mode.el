;;; slflex.el --- major mode for editing slflex source in Emacs -*- lexical-binding: t -*-

;; Copyright ....

;; Author: 
;; Keywords: languages

;;; Commentary: This mode is based on emacs pascal-mode

;; USAGE
;; =====

;; Emacs should enter slflex mode when you find a slflex source file.
;; When you have entered slflex mode, you can get more info by pressing
;; C-h m.  You can also get help describing various functions by:
;; C-h f <Name of function you want described>

;; If you want to customize slflex mode to fit you better, you may add
;; these lines (the values of the variables presented here are the defaults):
;;
;; ;; User customization for slflex mode
;; (setq slflex-indent-level       3
;;       slflex-case-indent        2
;;       slflex-auto-newline       nil
;;       slflex-tab-always-indent  t
;;       slflex-auto-endcomments   t
;;       slflex-auto-lineup        '(all)
;;       slflex-type-keywords      '("array" "file" "packed" "char"
;; 				     "integer" "real" "string" "record")
;;       slflex-start-keywords     '("begin" "end" "function" "procedure"
;; 				     "repeat" "until" "while" "read" "readln"
;; 				     "reset" "rewrite" "write" "writeln")
;;       slflex-separator-keywords '("downto" "else" "mod" "div" "then"))

;;; Code:

(defgroup slflex nil
  "Major mode for editing slflex source in Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defvar slflex-mode-abbrev-table nil
  "Abbrev table in use in slflex mode buffers.")
(define-abbrev-table 'slflex-mode-abbrev-table ())

;; define several class of keywords
(setq slflex-keywords '("PARAMETERS" "RESULTS" "DEFAULT" "RETURN" "INTERFACE" "MANDATORY" "BREAK" "DO" "ELSE" "ELSIF" "FOR" "IF" "CASES" "WHILE" "SWITCH" "END" "EXISTS" "NOT" "CALL" "ADMIN" "INSTANCE" "THEN" ) )
(setq slflex-types '("BOOLEAN" "CONST" "ENUM" "BYTE" "NULL" "TRUE" "FALSE" "FLOAT" "INTEGER" "ARRAY" "LONG" "DATE" "DATE_AND_TIME" "STRING" "CHAR"))
;; use constants coloring for GAC variable
(setq slflex-constants '("theGenericAccess."))
(setq slflex-events '("at_rot_target" "at_target" "attach"))
(setq slflex-functions '("GetOnTouchDateForSubscription" "Abort" "AccessStatisticsCounter" "Addition" "AddNumberToList" "AppendAnnoForGroupCall" "Trace" "TypeCast" "WasSubscriptionInsertedViaBatch" "WriteConfirmationTicket"))

;; create the regex string for each class of keywords
(setq slflex-keywords-regexp (regexp-opt slflex-keywords 'words))
(setq slflex-type-regexp (regexp-opt slflex-types 'words))
(setq slflex-constant-regexp (regexp-opt slflex-constants 'words))
(setq slflex-event-regexp (regexp-opt slflex-events 'words))
(setq slflex-functions-regexp (regexp-opt slflex-functions 'words))

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq slflex-font-lock-keywords
  `(
    (,slflex-type-regexp . font-lock-type-face)
    (,slflex-constant-regexp . font-lock-constant-face)
    (,slflex-event-regexp . font-lock-builtin-face)
    (,slflex-functions-regexp . font-lock-function-name-face)
    (,slflex-keywords-regexp . font-lock-keyword-face)
    ;; note: order above matters. “slflex-keywords-regexp” goes last because
    ;; otherwise the keyword “state” in the function “state_entry”
    ;; would be highlighted.
))

;; syntax table. Needed for e.g: correct comment syntax coloring
(defvar slflex-syntax-table nil "Syntax table for `slflex-mode'.")
(setq slflex-syntax-table
      (let ((synTable (make-syntax-table)))

        ;; style comment: “// …” 
        (modify-syntax-entry ?/ "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)

        synTable))


;;;###autoload
(define-derived-mode slflex-mode prog-mode "slflex mode"
  "Major mode for editing slflex code.\\<slflex-mode-map>"
  (setq-local local-abbrev-table slflex-mode-abbrev-table)
  (setq-local local-syntax-table slflex-syntax-table)
  ;; Font lock support, for syntax highlighting
  (setq-local font-lock-defaults '(slflex-font-lock-keywords nil t))
  ;;(setq-local syntax-propertize-function slflex--syntax-propertize)
  ;; Imenu support
;;  (setq-local imenu-generic-expression slflex-imenu-generic-expression)
;;  (setq-local imenu-case-fold-search t)
  ;; clear memory
  (setq slflex-keywords-regexp nil)
  (setq slflex-types-regexp nil)
  (setq slflex-constants-regexp nil)
  (setq slflex-events-regexp nil)
  (setq slflex-functions-regexp nil)
  ;; slflex-mode's own hide/show support.
  (add-to-invisibility-spec '(slflex . t)))

(provide 'slflex)

;;; slflex.el ends here
