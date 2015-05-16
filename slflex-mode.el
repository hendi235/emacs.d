;;; slflex.el --- major mode for editing slflex source in Emacs -*- lexical-binding: t -*-

;;; Code:

;;(defgroup slflex nil
 ;; "Major mode for editing slflex source in Emacs."
;;  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
;;  :group 'languages)
;;

(defvar slflex-events
  '("BOOLEAN" "INTEGER" "FLOAT" "STRING" "LONG" "DATE" "TIME" "ARRAY" "TRUE" "FALSE" "NULL" "ENUM" "BYTE"))

(defvar slflex-keywords
    '("IF" "END IF" "ELSIF" "ELSE" "WHILE" "END WHILE" "CASE" "END CASE" "RESULTS" "END RESULTS" "PARAMETERS" "END PARAMETERS"
      "SWITCH" "END SWITCH" "DEFAULT" "END DEFAULT" 
      "OR" "AND" "LEAVE" "THEN" "DO" "OF" "SIZE" "MANDATORY" "RETURN" "EXISTS" "NOT" "INSTANCE" "CALL" "ADMIN" "INTERFACE"
      "DIV" "MOD"))

(defvar slflex-functions
  '("Abort" "AccessStatisticsCounter" "Addition" "AddNumberToList" "AppendAnnoForGroupCall"
    "AppendAnnouncement" "AppendClassification" "AppendMenuItem" "AppendMenuItemSelCodeList"
    "AppendValue" "ArrayCopy" "AsciiToChar" "Assignment" "Charge" "DeleteBalObject" "ReadBalance"
    "Recharge" "ReleaseReservation" "Reserve" "SetSessionCreditLimit" "SetExpirationDate" "SetSessionValues"
    "BitwiseOps" "BooleanTerm" "CallSubroutine" "ChangeNumberAtPositionInList" "CheckPin" "Condition" "Connect"
    "CounterCalculateNextDate" "CounterCheckAccumulators" "CounterGetPeriodFactor"
    "CounterGetSubscriberValue" "CounterInitialize" "CounterRemove" "CounterSetBalances"
    "CounterSetSubscriberValue" "CreateUniqueId" "CutPrefix" "DecodeString" "DecrementUseCaseCounter"
    "DeleteNumberFromList" "DeleteSubscription" "DynamicAliasGenAcc" "ExistsVariable"
    "FindDiscountsForContract" "FindInDataModel" "FindMandatoryPackagesForContract" "FindMnpData"
    "FindPeriodicForContract" "FindProductForContract" "FindSubscriptionsForContract" "FloatDivision"
    "FlushNotifications" "Format" "genMsgAddLeafs" "genMsgCreateNewMsg" "genMsgGetEnumValue"
    "genMsgGetEventType" "genMsgReadEventAttribute" "genMsgSendAndFlush" "GetCalendarField"
    "GetCellCongestion" "GetCurrentTime" "GetGroupMembership" "GetHandleOfCursor" "GetLogicId"
    "GetNumberFromList" "GetNumberOfReferencedEntities" "GetOnTouchDateForSubscription"
    "GetRoamingZone" "GetRoamingZoneByDomain" "GetRoamingZoneByIPAddress" "GetRoamingZoneByMccMnc"
    "GetSize" "GetSubscriptions" "GetSubscriptionState" "GetSuppliers" "GetTimeId" "GetTimeZoneInfo"
    "GetValue" "GroupCall" "HttpRequest" "IfEqual" "IfGreater" "IfLess" "IncrementUseCaseCounter"
    "IndexOf" "InsertNewSubscription" "IntegerDivision" "IsClassificationAvailable" "IsInRange"
    "IsNumberInList" "IsPerformanceTimerRunning" "IsSubscriptionActive" "IsSubscriptionInfoSet"
    "IsSubscriptionMandatory" "IsValueAvailable" "LoadSubscriberData" "BalanceIdToName"
    "BalanceNameToId" "Matrix" "Maximum" "MaximumFromList" "Minimum" "MinimumFromList" "ModifyDate"
    "ModifyNumberInList" "Modulo" "Multiplication" "Multiplex" "NavigateCursor" "NavigateCursorInList"
    "NoOperation" "Notify" "NotifyImmediately" "NormalizeNumber" "PatternMatch" "PlayAnnouncement"
    "PlayMenu" "Prompt" "ReadCIBMessage" "ReadESXMessage" "ReadTicket" "RemoveGenericAccessVariable"
    "RemoveOptionalGDMAttribute" "RequestNotifyImmediately" "ResetMenuAndAnnos" "SendAtiMessage"
    "SendAtmMessage" "SendAtsiMessage" "SendCIBMessage" "SendESXMessage" "SendSmsViaMap" "SendSms"
    "SendUSSDNotification" "SendUSSDPrompt" "SendUSSDResponse" "SetBalance" "SetCallTicketFields"
    "SetCursorByHandle" "SetLocalPushFields" "SetOnTouchDateForSubscription" "SetPushFields"
    "SetSubscriptionActive" "SetSubscriptionInfo" "Sort" "StandardVoucherRecharging" "StartPerformanceTimer"
    "StopPerformanceTimer" "StringCase" "StringConcatenation" "StringLength" "StringReplace"
    "StringStartsWith" "StringTokenizer" "SubString" "Subtraction" "Switch" "TimeIntervals" "TimeZone"
    "Trace" "TypeCast" "WasSubscriptionInsertedViaBatch" "WriteConfirmationTicket"))

;;(defvar slflex-gacs
;;  '("theGenericAccess[-_\.A-Za-z0-9]+"))

(defvar slflex-font-lock-defaults
  `((
     ;; ; : , ; { } =>  @ $ = are all special elements
     ;;(":\\|,\\|;\\|{\\|}\\|=>\\|@\\|$\\|=" . font-lock-keyword-face)
     ( ,(regexp-opt slflex-keywords 'words) . font-lock-builtin-face)
     ( ,(regexp-opt slflex-events 'words) . font-lock-constant-face)
     ( ,(regexp-opt slflex-functions 'words) . font-lock-function-name-face)
     ("\\(theGenericAccess\\|\\<InputParameters\\|\\<tga\\|\\<CCS\\|\\<AdditionalResult\\|\\<Counters\\|\\<CheapSpot\\|\\<FBC\\|\\<HotCharge\\|\\<OnTouch\\|\\<QoS\\|\\<RatingInformation\\|\\<S[sS]7\\|\\<DTMF\\|\\<convChargingGA\\|\\<Const\\|\\<CallSupervision\\)[-_\\.A-Za-z0-9]+" . font-lock-preprocessor-face)
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
