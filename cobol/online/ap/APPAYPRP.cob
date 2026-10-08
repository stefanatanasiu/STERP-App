      *>================================================================
      *> PROGRAM-ID : APPAYPRP
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : PAYMENT PROPOSAL PREPARATION.  THE PROPOSAL
      *>              STATES ITS SETTLEMENT DATE, PAYING BANK,
      *>              CURRENCY AND SELECTION RULES, AND SHOWS DUE
      *>              ITEMS, USABLE CREDITS, DISCOUNTS, EXCLUSIONS
      *>              WITH REASONS AND THE TOTAL CASH REQUIREMENT.
      *>              SELECTION AND RESERVATION ARE DONE BY
      *>              ap.usp_ReserveProposal IN ONE SERIALISED
      *>              OPERATION SO THAT TWO PROPOSALS CANNOT CLAIM THE
      *>              SAME RESIDUAL (AP-011).
      *>              THE PAY DATE MUST BE A VALID PROCESSING DAY FOR
      *>              THE CHOSEN METHOD, AND A SINGLE PAYMENT ABOVE
      *>              THE CHAPS THRESHOLD IS ROUTED TO CHAPS (D-16).
      *> CALLED BY  : .NET PAYMENT RUN FORM, APBACS
      *> INTERFACE  : CALL "APPAYPRP" USING WS-APPAYPRP-CA
      *>              COPYBOOK APPAYPRPCA
      *> CALLS      : SQLIO, APSVC, APHASH
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1992-11-16 DJM  ORIGINAL BACS PAYMENT SELECTION RUN
      *> 1998-02-09 RJH  CR0790 MINIMUM PAYMENT AND EXCLUSION REASONS
      *> 2008-05-27 KMP  CR2310 FASTER PAYMENTS ROUTING
      *> 2026-10-05 STERP RESERVATION PROCEDURE, CHAPS THRESHOLD
      *>================================================================
      *> REQ: AP-011, AP-012, AP-017, AP-020, AP-026, D-16
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APPAYPRP.
       AUTHOR. D J MILLER.
       DATE-WRITTEN. 16/11/1992.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APSVCARE.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
           05  WS-EOF-SW               PIC X VALUE "N".
               88  WS-EOF                        VALUE "Y".
       01  WS-SUM-ROW.
           05  WS-M-SELECTED           PIC X(24).
           05  WS-M-EXCLUDED           PIC X(24).
           05  WS-M-CASH               PIC X(24).
           05  WS-M-CREDITS            PIC X(24).
           05  WS-M-DISCOUNTS          PIC X(24).
           05  WS-M-RESERVED           PIC X(24).
           05  WS-M-OUTFLOW            PIC X(24).
       01  WS-LINE-ROW.
           05  WS-L-NUM                PIC X(8).
           05  WS-L-SUPPLIER           PIC X(20).
           05  WS-L-ITEMID             PIC X(24).
           05  WS-L-DOCUMENT           PIC X(30).
           05  WS-L-TYPE               PIC X(12).
           05  WS-L-DUE                PIC X(8).
           05  WS-L-CURRENCY           PIC X(3).
           05  WS-L-GROSS              PIC X(24).
           05  WS-L-GROSSGBP           PIC X(24).
           05  WS-L-DISCOUNT           PIC X(24).
           05  WS-L-CREDIT             PIC X(24).
           05  WS-L-RESERVED           PIC X(24).
           05  WS-L-NETPAY             PIC X(24).
           05  WS-L-BANKID             PIC X(24).
           05  WS-L-EXCLUDED           PIC X(1).
           05  WS-L-REASON             PIC X(60).
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(40).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-MAX-SINGLE           PIC S9(13)V99 VALUE 0.
           05  WS-HASH-TEXT            PIC X(300) VALUE SPACES.
           05  WS-MONEY-ED             PIC -(11)9.99.
           05  WS-COUNT-ED             PIC Z(5)9.
           05  WS-DATE-ED              PIC 9(8).
       LINKAGE SECTION.
       COPY APPAYPRPCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "METHOD"  PERFORM 2000-METHOD THRU 2000-EXIT
               WHEN "SELECT"  PERFORM 3000-BUILD THRU 3000-EXIT
               WHEN "RESERVE" PERFORM 3000-BUILD THRU 3000-EXIT
               WHEN "RELEASE" PERFORM 3000-BUILD THRU 3000-EXIT
               WHEN "TOTALS"  PERFORM 5000-TOTALS THRU 5000-EXIT
               WHEN OTHER     PERFORM 9100-BAD-FUNC THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "APPAYPRP" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "AP-011" TO CA-REQ-REF
           MOVE SPACES TO PP-NOTE
           MOVE "N" TO PP-DAY-ADJUSTED
           MOVE "N" TO PP-METHOD-CHANGED
           MOVE "N" TO PP-OVER-LIMIT
           MOVE 0 TO PP-LINE-CNT
           MOVE FUNCTION UPPER-CASE(CA-COMPANY) TO CA-COMPANY
           IF PP-METHOD = SPACES OR PP-METHOD = LOW-VALUES
               MOVE "BACS" TO PP-METHOD
           END-IF
           IF PP-CURRENCY = SPACES OR PP-CURRENCY = LOW-VALUES
               MOVE "GBP" TO PP-CURRENCY
           END-IF
           IF PP-VERSION = 0
               MOVE 1 TO PP-VERSION
           END-IF
           IF CA-COMPANY = SPACES
               MOVE "APP1001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           IF PP-PAY-DATE = 0
               MOVE CA-BUSINESS-DATE TO PP-PAY-DATE
           END-IF
           IF PP-CUTOFF-DATE = 0
               MOVE PP-PAY-DATE TO PP-CUTOFF-DATE
           END-IF
           IF PP-CHAPS-THRESHOLD = 0
               INITIALIZE WS-APSVC
               MOVE "PARMNUM" TO SVC-FUNC
               PERFORM 8900-SVC-HEADER THRU 8900-EXIT
               MOVE "AP.CHAPS.THRESHOLD" TO SVC-TEXT-1
               MOVE 250000 TO SVC-NUM-1
               CALL "APSVC" USING WS-APSVC
               MOVE SVC-NUM-OUT TO PP-CHAPS-THRESHOLD
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AP-026 / D-16.  BACS WORKS ON A THREE DAY CYCLE: INPUT ON DAY
      *> ONE, PROCESSING ON DAY TWO AND THE ENTRY DATE ON DAY THREE.
      *> THE PROPOSAL'S PAY DATE IS THE ENTRY DATE, SO IT MUST ITSELF
      *> BE A BACS PROCESSING DAY; A BANK HOLIDAY MOVES IT ON BY THE
      *> APPROVED RULE BEFORE APPROVAL, NOT AFTER.
      *>----------------------------------------------------------------
       2000-METHOD SECTION.
       2000-START.
           MOVE PP-PAY-DATE TO PP-PAY-DATE-RAW
           EVALUATE TRUE
               WHEN PP-BACS
                   PERFORM 2100-BACS-DATES THRU 2100-EXIT
               WHEN PP-CHAPS OR PP-FPS
                   PERFORM 2200-SAMEDAY THRU 2200-EXIT
               WHEN OTHER
                   PERFORM 2200-SAMEDAY THRU 2200-EXIT
           END-EVALUATE
           PERFORM 2300-LIMITS THRU 2300-EXIT.
       2000-EXIT.
           EXIT.

       2100-BACS-DATES SECTION.
       2100-START.
           INITIALIZE WS-APSVC
           MOVE "BUSDAY" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE PP-PAY-DATE TO SVC-DATE-1
           MOVE "BACS" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-FLAG-1 TO PP-IS-PROC-DAY
           IF PP-IS-PROC-DAY NOT = "Y"
               INITIALIZE WS-APSVC
               MOVE "ADJUST" TO SVC-FUNC
               PERFORM 8900-SVC-HEADER THRU 8900-EXIT
               MOVE PP-PAY-DATE TO SVC-DATE-1
               MOVE "BACS" TO SVC-TEXT-1
      *>       DATECALC KNOWS NEXT / PREV / NONE ONLY.  "FWD" WAS
      *>       REJECTED AND LEFT A ZERO RESULT DATE, WHICH REACHED
      *>       SQL SERVER AS THE TEXT "0" AND FAILED THE INSERT WITH
      *>       A DATE CONVERSION ERROR (AP-011, AC-034).
               MOVE "NEXT" TO SVC-TEXT-2
               CALL "APSVC" USING WS-APSVC
               IF SVC-RC > 4 OR SVC-DATE-OUT = 0
                   PERFORM 2190-NO-CALENDAR THRU 2190-EXIT
                   GO TO 2100-EXIT
               END-IF
               MOVE SVC-DATE-OUT TO PP-PAY-DATE
               MOVE "Y" TO PP-DAY-ADJUSTED
               MOVE "Y" TO PP-IS-PROC-DAY
           END-IF
           INITIALIZE WS-APSVC
           MOVE "ADDBUS" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE PP-PAY-DATE TO SVC-DATE-1
           MOVE -1 TO SVC-INT-1
           MOVE "BACS" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-RC > 4 OR SVC-DATE-OUT = 0
               PERFORM 2190-NO-CALENDAR THRU 2190-EXIT
               GO TO 2100-EXIT
           END-IF
           MOVE SVC-DATE-OUT TO PP-BACS-PROC-DATE
           INITIALIZE WS-APSVC
           MOVE "ADDBUS" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE PP-PAY-DATE TO SVC-DATE-1
           MOVE -2 TO SVC-INT-1
           MOVE "BACS" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-RC > 4 OR SVC-DATE-OUT = 0
               PERFORM 2190-NO-CALENDAR THRU 2190-EXIT
               GO TO 2100-EXIT
           END-IF
           MOVE SVC-DATE-OUT TO PP-BACS-INPUT-DATE
           IF PP-DAY-ADJUSTED = "Y"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APP1010" TO CA-REASON-CODE
               MOVE PP-PAY-DATE TO WS-DATE-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "The entry date is not a Bacs processing day; "
                      "it has been moved to "
                      WS-DATE-ED
                      " under the approved next processing day rule"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> THE BACS CALENDAR COULD NOT ANSWER.  REFUSE THE RUN RATHER
      *> THAN CARRY A ZERO DATE INTO THE PROPOSAL HEADER (AP-011).
      *>----------------------------------------------------------------
       2190-NO-CALENDAR SECTION.
       2190-START.
           MOVE 8 TO CA-RETURN-CODE
           MOVE "APP1014" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "The Bacs processing calendar could not supply a "
                  "working entry date for this payment run"
               DELIMITED BY SIZE INTO CA-MESSAGE
           ADD 1 TO CA-MSG-COUNT
           MOVE "Y" TO WS-FATAL-SW.
       2190-EXIT.
           EXIT.

       2200-SAMEDAY SECTION.
       2200-START.
           INITIALIZE WS-APSVC
           MOVE "BUSDAY" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE PP-PAY-DATE TO SVC-DATE-1
           MOVE "ENG" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-FLAG-1 TO PP-IS-PROC-DAY
           MOVE PP-PAY-DATE TO PP-BACS-INPUT-DATE
           MOVE PP-PAY-DATE TO PP-BACS-PROC-DATE
           IF PP-IS-PROC-DAY NOT = "Y"
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APP1011" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The chosen payment date is not a working day "
                      "for this payment method"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
           END-IF.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> D-16: CHAPS FOR SAME DAY OR OVER THE THRESHOLD, FASTER
      *> PAYMENTS ONLY INSIDE THE BANK LIMIT.  AN OVER LIMIT FASTER
      *> PAYMENT IS REFUSED ON THAT METHOD RATHER THAN SILENTLY SENT.
      *>----------------------------------------------------------------
       2300-LIMITS SECTION.
       2300-START.
           IF PP-SINGLE-LIMIT = 0
               MOVE 1000000 TO WS-MAX-SINGLE
           ELSE
               MOVE PP-SINGLE-LIMIT TO WS-MAX-SINGLE
           END-IF
           IF PP-FPS AND PP-TOTAL-OUTFLOW > WS-MAX-SINGLE
               MOVE "Y" TO PP-OVER-LIMIT
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APP1012" TO CA-REASON-CODE
               MOVE WS-MAX-SINGLE TO WS-MONEY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "A Faster Payment above the bank limit of GBP "
                      FUNCTION TRIM(WS-MONEY-ED)
                      " cannot be submitted on that method"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 2300-EXIT
           END-IF
           IF NOT PP-CHAPS AND PP-TOTAL-OUTFLOW > PP-CHAPS-THRESHOLD
               MOVE "Y" TO PP-METHOD-CHANGED
               MOVE "CHAPS" TO PP-METHOD
               MOVE PP-CHAPS-THRESHOLD TO WS-MONEY-ED
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APP1013" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The run exceeds GBP "
                      FUNCTION TRIM(WS-MONEY-ED)
                      " and has been routed to CHAPS"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       2300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3000-BUILD SECTION.
       3000-START.
           INITIALIZE WS-APSVC
           MOVE "PERM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "AP.PAYMENT.PROPOSE" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-FLAG-1 NOT = "Y"
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APP1002" TO CA-REASON-CODE
               MOVE SVC-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 3000-EXIT
           END-IF
           PERFORM 2000-METHOD THRU 2000-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           IF PP-PROPOSAL-ID = 0
               PERFORM 3100-CREATE-HEADER THRU 3100-EXIT
               IF WS-FATAL
                   GO TO 3000-EXIT
               END-IF
           ELSE
               PERFORM 3150-UPDATE-HEADER THRU 3150-EXIT
           END-IF
           PERFORM 4000-RESERVE THRU 4000-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           PERFORM 5000-TOTALS THRU 5000-EXIT
           PERFORM 2300-LIMITS THRU 2300-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           PERFORM 6000-HASH THRU 6000-EXIT
           IF CA-RETURN-CODE < 4
               MOVE PP-TOTAL-OUTFLOW TO WS-MONEY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Proposal "
                      FUNCTION TRIM(PP-PROPOSAL-REF)
                      " requires GBP "
                      FUNCTION TRIM(WS-MONEY-ED)
                      " of cash"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       3000-EXIT.
           EXIT.

       3100-CREATE-HEADER SECTION.
       3100-START.
           INITIALIZE WS-APSVC
           MOVE "NUMBER" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "APPP" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-RC > 4
               MOVE 12 TO CA-RETURN-CODE
               MOVE SVC-REASON TO CA-REASON-CODE
               MOVE SVC-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 3100-EXIT
           END-IF
           MOVE SVC-TEXT-OUT(1:30) TO PP-PROPOSAL-REF
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; "
                  "INSERT ap.PaymentProposal (CompanyCode, "
                  "ProposalRef, Version, SelectionCutoffDate, "
                  "PayDate, PaymentMethod, BankAccountCode, "
                  "CurrencyCode, IsBacsProcessingDay, BacsInputDate, "
                  "Status, PreparedBy, CreatedBy) SELECT ?, ?, ?, "
                  "CAST(? AS date), CAST(? AS date), ?, ?, ?, ?, "
                  "CAST(? AS date), 'DRAFT', ?, ?; "
                  "SELECT CAST(SCOPE_IDENTITY() AS varchar(24))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE PP-PROPOSAL-REF TO SQLIO-PARM-VAL(2)
           MOVE PP-VERSION TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE PP-CUTOFF-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE PP-PAY-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE PP-METHOD TO SQLIO-PARM-VAL(6)
           IF PP-BANK-ACCOUNT = SPACES
               MOVE "GBP1" TO PP-BANK-ACCOUNT
           END-IF
           MOVE PP-BANK-ACCOUNT TO SQLIO-PARM-VAL(7)
           MOVE PP-CURRENCY TO SQLIO-PARM-VAL(8)
           IF PP-IS-PROC-DAY = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(9)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(9)
           END-IF
           MOVE PP-BACS-INPUT-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE CA-USER TO SQLIO-PARM-VAL(11)
           MOVE CA-USER TO SQLIO-PARM-VAL(12)
           MOVE 12 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 12
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3100-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-C-VALUE)
           MOVE WS-NUM TO PP-PROPOSAL-ID
           MOVE CA-USER TO PP-PREPARED-BY.
       3100-EXIT.
           EXIT.

       3150-UPDATE-HEADER SECTION.
       3150-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PaymentProposal SET "
                  "SelectionCutoffDate = CAST(? AS date), PayDate = "
                  "CAST(? AS date), PaymentMethod = ?, "
                  "BankAccountCode = ?, CurrencyCode = ?, "
                  "IsBacsProcessingDay = ?, BacsInputDate = "
                  "CAST(? AS date), ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE ProposalId = ? "
                  "AND Status IN ('DRAFT','PROPOSED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE PP-CUTOFF-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE PP-PAY-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE PP-METHOD TO SQLIO-PARM-VAL(3)
           MOVE PP-BANK-ACCOUNT TO SQLIO-PARM-VAL(4)
           MOVE PP-CURRENCY TO SQLIO-PARM-VAL(5)
           IF PP-IS-PROC-DAY = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(6)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(6)
           END-IF
           MOVE PP-BACS-INPUT-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE CA-USER TO SQLIO-PARM-VAL(8)
           MOVE PP-PROPOSAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE 9 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       3150-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4000-RESERVE SECTION.
       4000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC ap.usp_ReserveProposal ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE PP-PROPOSAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           EVALUATE CA-FUNCTION
               WHEN "SELECT"  MOVE "SELECT" TO SQLIO-PARM-VAL(4)
               WHEN "RELEASE" MOVE "RELEASE" TO SQLIO-PARM-VAL(4)
               WHEN OTHER     MOVE "RESERVE" TO SQLIO-PARM-VAL(4)
           END-EVALUATE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 7 TO SQLIO-COL-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 7
               MOVE 24 TO SQLIO-COL-WIDTH(WS-I)
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUM-ROW
           IF SQLIO-ERROR
               MOVE "Y" TO WS-FATAL-SW
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APP1014" TO CA-REASON-CODE
               MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "CLOS" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-SUM-ROW
               GO TO 4000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 4000-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-M-SELECTED)
           MOVE WS-NUM TO PP-SELECTED-CNT
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-M-EXCLUDED)
           MOVE WS-NUM TO PP-EXCLUDED-CNT
           COMPUTE PP-CASH-PRINCIPAL = FUNCTION NUMVAL(WS-M-CASH)
           COMPUTE PP-CREDITS-APPLIED =
               FUNCTION NUMVAL(WS-M-CREDITS)
           COMPUTE PP-DISCOUNTS-TAKEN =
               FUNCTION NUMVAL(WS-M-DISCOUNTS)
           COMPUTE PP-RESERVED-TOTAL = FUNCTION NUMVAL(WS-M-RESERVED)
           COMPUTE PP-TOTAL-OUTFLOW = FUNCTION NUMVAL(WS-M-OUTFLOW)
           IF PP-EXCLUDED-CNT > 0
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APP1015" TO CA-REASON-CODE
               MOVE PP-EXCLUDED-CNT TO WS-COUNT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-COUNT-ED)
                      " item(s) were excluded; each exclusion states "
                      "its reason" DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       5000-TOTALS SECTION.
       5000-START.
           MOVE 0 TO PP-LINE-CNT
           MOVE 0 TO PP-SUPPLIER-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(l.LineNum AS varchar(8)), "
                  "l.SupplierCode, CAST(ISNULL(l.OpenItemId,0) AS "
                  "varchar(24)), ISNULL(l.DocumentNumber,''), "
                  "ISNULL(oi.ItemType,'INVOICE'), "
                  "ISNULL(CONVERT(char(8), oi.DueDate, 112),"
                  "'00000000'), l.CurrencyCode, "
                  "CAST(l.GrossAmount AS varchar(24)), "
                  "CAST(l.GrossAmountGBP AS varchar(24)), "
                  "CAST(l.DiscountGBP AS varchar(24)), "
                  "CAST(l.CreditAppliedGBP AS varchar(24)), "
                  "CAST(l.ReservedAmountGBP AS varchar(24)), "
                  "CAST(l.NetPayGBP AS varchar(24)), "
                  "CAST(ISNULL(l.SupplierBankId,0) AS varchar(24)), "
                  "CAST(l.IsExcluded AS char(1)), "
                  "ISNULL(LEFT(l.ExclusionReason,60),'') "
                  "FROM ap.PaymentProposalLine l LEFT JOIN "
                  "ap.OpenItem oi ON oi.OpenItemId = l.OpenItemId "
                  "WHERE l.ProposalId = ? ORDER BY l.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE PP-PROPOSAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 16 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 30 TO SQLIO-COL-WIDTH(4)
           MOVE 12 TO SQLIO-COL-WIDTH(5)
           MOVE 8 TO SQLIO-COL-WIDTH(6)
           MOVE 3 TO SQLIO-COL-WIDTH(7)
           PERFORM VARYING WS-I FROM 8 BY 1 UNTIL WS-I > 14
               MOVE 24 TO SQLIO-COL-WIDTH(WS-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-WIDTH(15)
           MOVE 60 TO SQLIO-COL-WIDTH(16)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE "N" TO WS-EOF-SW
           PERFORM UNTIL WS-EOF OR PP-LINE-CNT >= 60
               MOVE 2 TO SQLIO-CURSOR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-EOF-SW
               ELSE
                   ADD 1 TO PP-LINE-CNT
                   MOVE PP-LINE-CNT TO WS-I
                   PERFORM 5100-MOVE-LINE THRU 5100-EXIT
               END-IF
           END-PERFORM
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW.
       5000-EXIT.
           EXIT.

       5100-MOVE-LINE SECTION.
       5100-START.
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-L-NUM)
           MOVE WS-NUM TO PP-L-NUM(WS-I)
           MOVE WS-L-SUPPLIER TO PP-L-SUPPLIER(WS-I)
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-L-ITEMID)
           MOVE WS-NUM TO PP-L-ITEM-ID(WS-I)
           MOVE WS-L-DOCUMENT TO PP-L-DOCUMENT(WS-I)
           MOVE WS-L-TYPE TO PP-L-TYPE(WS-I)
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-L-DUE)
           MOVE WS-NUM TO PP-L-DUE-DATE(WS-I)
           MOVE WS-L-CURRENCY TO PP-L-CURRENCY(WS-I)
           COMPUTE PP-L-GROSS(WS-I) = FUNCTION NUMVAL(WS-L-GROSS)
           COMPUTE PP-L-GROSS-GBP(WS-I) =
               FUNCTION NUMVAL(WS-L-GROSSGBP)
           COMPUTE PP-L-DISCOUNT(WS-I) =
               FUNCTION NUMVAL(WS-L-DISCOUNT)
           COMPUTE PP-L-CREDIT(WS-I) = FUNCTION NUMVAL(WS-L-CREDIT)
           COMPUTE PP-L-RESERVED(WS-I) =
               FUNCTION NUMVAL(WS-L-RESERVED)
           COMPUTE PP-L-NET-PAY(WS-I) = FUNCTION NUMVAL(WS-L-NETPAY)
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-L-BANKID)
           MOVE WS-NUM TO PP-L-BANK-ID(WS-I)
           IF WS-L-EXCLUDED = "1"
               MOVE "Y" TO PP-L-EXCLUDED(WS-I)
           ELSE
               MOVE "N" TO PP-L-EXCLUDED(WS-I)
           END-IF
           MOVE WS-L-REASON TO PP-L-REASON(WS-I).
       5100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AP-012: THE APPROVAL IS GIVEN FOR EXACT CONTENT.  THE HASH
      *> COVERS THE SUPPLIER, BENEFICIARY, CURRENCY, AMOUNTS, DATE AND
      *> METHOD, SO ANY MATERIAL AMENDMENT MAKES THE EARLIER APPROVAL
      *> INAPPLICABLE.
      *>----------------------------------------------------------------
       6000-HASH SECTION.
       6000-START.
           MOVE SPACES TO WS-HASH-TEXT
           MOVE PP-TOTAL-OUTFLOW TO WS-MONEY-ED
           MOVE PP-SELECTED-CNT TO WS-COUNT-ED
           MOVE PP-PAY-DATE TO WS-DATE-ED
           STRING FUNCTION TRIM(PP-PROPOSAL-REF) "|"
                  FUNCTION TRIM(CA-COMPANY) "|"
                  FUNCTION TRIM(PP-METHOD) "|"
                  FUNCTION TRIM(PP-BANK-ACCOUNT) "|"
                  FUNCTION TRIM(PP-CURRENCY) "|"
                  WS-DATE-ED "|"
                  FUNCTION TRIM(WS-MONEY-ED) "|"
                  FUNCTION TRIM(WS-COUNT-ED)
               DELIMITED BY SIZE INTO WS-HASH-TEXT
           CALL "APHASH" USING WS-HASH-TEXT PP-CONTENT-HASH
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PaymentProposal SET ContentHash = ? "
                  "WHERE ProposalId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE PP-CONTENT-HASH TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE PP-PROPOSAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       6000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8500-EXEC-UPDATE SECTION.
       8500-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 8500-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       8500-EXIT.
           EXIT.

       8900-SVC-HEADER SECTION.
       8900-START.
           MOVE CA-COMPANY TO SVC-COMPANY
           MOVE CA-SITE TO SVC-SITE
           MOVE CA-USER TO SVC-USER
           MOVE CA-BUSINESS-DATE TO SVC-DATE
           MOVE CA-CORRELATION-ID TO SVC-CORRELATION.
       8900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-FATAL-SW
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNC SECTION.
       9100-START.
           MOVE "APP1099" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "APPAYPRP does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE "Y" TO WS-FATAL-SW
           MOVE 12 TO CA-RETURN-CODE
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in APPAYPRP" TO CA-MESSAGE
           END-IF
           ADD 1 TO CA-MSG-COUNT
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       9300-EXIT.
           EXIT.
