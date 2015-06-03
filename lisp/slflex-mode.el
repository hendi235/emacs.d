;;; slflex.el --- major mode for editing slflex source in Emacs -*- lexical-binding: t -*-

;;; Code:

(defgroup slflex nil
  "Major mode for editing slflex source in Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "slflex-"
  :group 'languages)

(defconst slflex-events
  '("BOOLEAN" "INTEGER" "FLOAT" "STRING" "LONG" "DATE" "TIME" "ARRAY" "TRUE" "FALSE" "NULL" "ENUM" "BYTE"))

(defconst slflex-keywords
    ;; Remember, order of word is important! Make sure shortest word is put on the last.
    '("END IF" "ELSIF" "END WHILE" "END CASE" "END RESULTS" "END PARAMETERS" "END SWITCH" "END DEFAULT"
      "NOT EXISTS" "MANDATORY" "RETURN" "EXISTS" "INSTANCE" "ADMIN" "INTERFACE"
      "LEAVE" "DEFAULT" "SWITCH" "PARAMETERS" "RESULTS" "CASE" "WHILE" "ELSE" 
      "CALL" "SIZE" "THEN" "DIV" "MOD" "AND" "NOT" "IF" "OR" "DO" "OF"))

(defconst slflex-actions
  '("Abort" "AccessStatisticsCounter" "Addition" "AddNumberToList" "AppendAnnoForGroupCall"
    "AppendAnnouncement" "AppendClassification" "AppendMenuItem" "AppendMenuItemSelCodeList"
    "AppendValue" "ArrayCopy" "AsciiToChar" "Assignment" "Charge" "DeleteBalObject" "ReadBalance"
    "Recharge" "ReleaseReservation" "Reserve" "SetSessionCreditLimit" "SetExpirationDate"
    "BitwiseOps" "BooleanTerm" "CallSubroutine" "ChangeNumberAtPositionInList" "CheckPin"
    "CounterCalculateNextDate" "CounterCheckAccumulators" "CounterGetPeriodFactor" "SetSessionValues"
    "CounterGetSubscriberValue" "CounterInitialize" "CounterRemove" "CounterSetBalances"
    "CounterSetSubscriberValue" "CreateUniqueId" "CutPrefix" "DecodeString" "DecrementUseCaseCounter"
    "DeleteNumberFromList" "DeleteSubscription" "DynamicAliasGenAcc" "ExistsVariable" "Connect"
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
    "IsSubscriptionMandatory" "IsValueAvailable" "LoadSubscriberData" "BalanceIdToName" "Condition"
    "BalanceNameToId" "Matrix" "Maximum" "MaximumFromList" "Minimum" "MinimumFromList" "ModifyDate"
    "ModifyNumberInList" "Modulo" "Multiplication" "Multiplex" "NavigateCursor" "NavigateCursorInList"
    "NoOperation" "Notify" "NotifyImmediately" "NormalizeNumber" "PatternMatch" "PlayAnnouncement"
    "PlayMenu" "Prompt" "ReadCIBMessage" "ReadESXMessage" "ReadTicket" "RemoveGenericAccessVariable"
    "RemoveOptionalGDMAttribute" "RequestNotifyImmediately" "ResetMenuAndAnnos" "SendAtiMessage"
    "SendAtmMessage" "SendAtsiMessage" "SendCIBMessage" "SendESXMessage" "SendSmsViaMap" "SendSms"
    "SendUSSDNotification" "SendUSSDPrompt" "SendUSSDResponse" "SetBalance" "SetCallTicketFields"
    "SetCursorByHandle" "SetLocalPushFields" "SetOnTouchDateForSubscription" "SetPushFields"
    "SetSubscriptionActive" "SetSubscriptionInfo" "Sort" "StandardVoucherRecharging" "TimeZone"
    "StopPerformanceTimer" "StringCase" "StringConcatenation" "StringLength" "StringReplace"
    "StringStartsWith" "StringTokenizer" "SubString" "Subtraction" "Switch" "TimeIntervals"
    "Trace" "TypeCast" "WasSubscriptionInsertedViaBatch" "WriteConfirmationTicket" "StartPerformanceTimer"
    ))

(defvar slflex-font-lock-defaults
  `((
     ;; ; : , ; { } =>  @ $ = are all special elements
     ;;(":\\|,\\|;\\|{\\|}\\|=>\\|@\\|$\\|=" . font-lock-keyword-face)
     ("<-\\|->\\|#\\|(\\|)\\|:=\\|/\\|<\\|>\\|=\\|*\\|-\\|+" . font-lock-builtin-face)
     ( ,(regexp-opt slflex-keywords 'words) . font-lock-builtin-face)
     ( ,(regexp-opt slflex-events 'words) . font-lock-constant-face)
     ( ,(regexp-opt slflex-actions 'words) . font-lock-function-name-face)
     ;; Define generic acces pattern
     (,(concat "\\(theGenericAccess\\|\\<InputParameter\\|\\<tga\\|\\<CCS\\|\\<AdditionalResult\\|"
                "\\<Counters\\|\\<CheapSpot\\|\\<FBC\\|\\<HotCharge\\|\\<OnTouch\\|\\<QoS\\|"
                "\\<RatingInformation\\|\\<S[sS]7\\|\\<DTMF\\|\\<convChargingGA\\|\\<Const\\|"
                "\\<CallSupervision\\|\\<SubsDM\\(FUS\\|DB\\)\\)[-_\\.A-Za-z0-9]+") . font-lock-warning-face)
     )))

(defvar slflex-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; ' is a string delimiter
    ;;(modify-syntax-entry ?' "\"" st)
    ;; " is a string delimiter too
    ;;(modify-syntax-entry ?\" "\"" st)

    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 12" st)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" st)
    st))

(define-derived-mode slflex-mode fundamental-mode "SL Flex Mode"
  :syntax-table slflex-mode-syntax-table
  (setq font-lock-defaults slflex-font-lock-defaults)
  (font-lock-fontify-buffer))

(provide 'slflex-mode)

;;; slflex.el ends here
