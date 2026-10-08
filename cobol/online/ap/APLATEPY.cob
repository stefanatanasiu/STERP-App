      *>================================================================
      *> PROGRAM-ID : APLATEPY
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : A SUPPLIER'S CLAIM FOR STATUTORY LATE PAYMENT
      *>              INTEREST AND FIXED COMPENSATION UNDER THE LATE
      *>              PAYMENT OF COMMERCIAL DEBTS (INTEREST) ACT 1998.
      *>              THE CLAIM IS A FINANCE COST LINKED TO THE LATE
      *>              INVOICE - IT IS NEVER TREATED AS COST OF GOODS
      *>              (AP-030).  AGREED PAYMENT TERMS ARE ALSO CHECKED
      *>              AGAINST THE STATUTORY AND POLICY MAXIMUM FOR THE
      *>              SUPPLIER'S SIZE; TERMS BEYOND THE APPROVED
      *>              MAXIMUM FOR A SMALL SUPPLIER NEED AN AUTHORISED
      *>              EXCEPTION (D-25).
      *>                interest = debt x (base rate + margin) / 100
      *>                           x days late / 365
      *>                compensation = 40 / 70 / 100 by debt band
      *> CALLED BY  : .NET REGISTER FORM, APPPR
      *> INTERFACE  : CALL "APLATEPY" USING WS-APLATEPY-CA
      *>              COPYBOOK APLATEPYCA
      *> CALLS      : SQLIO, APSVC, APPOST
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1999-04-06 KMP  ORIGINAL - LATE PAYMENT ACT 1998 COMMENCEMENT
      *> 2013-03-16 AJS  CR3010 COMPENSATION BANDS AND RECOVERY COSTS
      *> 2026-10-05 STERP FINANCE COST ACCOUNT 7200, TERM LIMIT CHECK
      *>================================================================
      *> REQ: AP-030, AP-009, D-25, D-28
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APLATEPY.
       AUTHOR. K M PATEL.
       DATE-WRITTEN. 06/04/1999.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APSVCARE.
       COPY APPOSTWK.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-INV-ROW.
           05  WS-R-ID                 PIC X(24).
           05  WS-R-REGISTER           PIC X(30).
           05  WS-R-SUPPLIER           PIC X(20).
           05  WS-R-DOCREF             PIC X(40).
           05  WS-R-DUEDATE            PIC X(8).
           05  WS-R-GROSS              PIC X(24).
           05  WS-R-TERMS              PIC X(12).
           05  WS-R-SIZE               PIC X(8).
           05  WS-R-SETTLE             PIC X(12).
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(40).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-RAW                  PIC S9(15)V9(6) VALUE 0.
           05  WS-MONEY-ED             PIC -(11)9.99.
           05  WS-DAYS-ED              PIC -(6)9.
       LINKAGE SECTION.
       COPY APLATEPYCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "CALC"    PERFORM 2000-CALC THRU 2000-EXIT
               WHEN "CLAIM"   PERFORM 3000-CLAIM THRU 3000-EXIT
               WHEN "AGREE"   PERFORM 4000-AGREE THRU 4000-EXIT
               WHEN "TERMCHK" PERFORM 5000-TERMCHK THRU 5000-EXIT
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
           MOVE "APLATEPY" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "AP-030" TO CA-REQ-REF
           MOVE SPACES TO LP-NOTE
           MOVE "N" TO LP-TERM-EXCEPTION
           MOVE FUNCTION UPPER-CASE(CA-COMPANY) TO CA-COMPANY
           IF LP-CLAIM-DATE = 0
               MOVE CA-BUSINESS-DATE TO LP-CLAIM-DATE
           END-IF
           IF LP-FINANCE-ACCOUNT = SPACES
               MOVE "7200" TO LP-FINANCE-ACCOUNT
           END-IF
           PERFORM 1100-PARAMETERS THRU 1100-EXIT
           IF CA-COMPANY = SPACES
               MOVE "APT0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1000-EXIT.
           EXIT.

       1100-PARAMETERS SECTION.
       1100-START.
           IF LP-MARGIN-PCT = 0
               MOVE "LATEPAY.MARGIN.PCT" TO SVC-TEXT-1
               MOVE 8 TO SVC-NUM-1
               PERFORM 8100-PARMNUM THRU 8100-EXIT
               MOVE SVC-NUM-OUT TO LP-MARGIN-PCT
           END-IF
           IF LP-BAND1-GBP = 0
               MOVE "LATEPAY.COMP.BAND1" TO SVC-TEXT-1
               MOVE 40 TO SVC-NUM-1
               PERFORM 8100-PARMNUM THRU 8100-EXIT
               MOVE SVC-NUM-OUT TO LP-BAND1-GBP
           END-IF
           IF LP-BAND2-GBP = 0
               MOVE "LATEPAY.COMP.BAND2" TO SVC-TEXT-1
               MOVE 70 TO SVC-NUM-1
               PERFORM 8100-PARMNUM THRU 8100-EXIT
               MOVE SVC-NUM-OUT TO LP-BAND2-GBP
           END-IF
           IF LP-BAND3-GBP = 0
               MOVE "LATEPAY.COMP.BAND3" TO SVC-TEXT-1
               MOVE 100 TO SVC-NUM-1
               PERFORM 8100-PARMNUM THRU 8100-EXIT
               MOVE SVC-NUM-OUT TO LP-BAND3-GBP
           END-IF
           IF LP-STATUTE-REF = SPACES
               MOVE "LATEPAY.STATUTE" TO SVC-TEXT-1
               MOVE "LPCDIA1998" TO SVC-TEXT-2
               INITIALIZE WS-APSVC
               MOVE "PARMTXT" TO SVC-FUNC
               PERFORM 8900-SVC-HEADER THRU 8900-EXIT
               MOVE "LATEPAY.STATUTE" TO SVC-TEXT-1
               MOVE "LPCDIA1998" TO SVC-TEXT-2
               CALL "APSVC" USING WS-APSVC
               MOVE SVC-TEXT-OUT(1:40) TO LP-STATUTE-REF
           END-IF.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE STATUTORY RATE IS THE BANK OF ENGLAND BASE RATE PLUS THE
      *> MARGIN.  INTEREST RUNS FROM THE DAY AFTER THE DEBT FELL DUE
      *> TO THE DAY IT WAS PAID.  COMPENSATION IS A FIXED SUM BY DEBT
      *> BAND AND DOES NOT ACCRUE DAILY.
      *>----------------------------------------------------------------
       2000-CALC SECTION.
       2000-START.
           IF LP-INVOICE-ID NOT = 0 OR LP-REGISTER-NUMBER NOT = SPACES
               PERFORM 7000-READ-INVOICE THRU 7000-EXIT
               IF WS-FATAL
                   GO TO 2000-EXIT
               END-IF
           END-IF
           IF LP-DUE-DATE = 0 OR LP-DEBT-GBP = 0
               MOVE "APT0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A due date and a debt amount are required to "
                      "calculate a late payment claim"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF LP-PAID-DATE = 0
               MOVE CA-BUSINESS-DATE TO LP-PAID-DATE
           END-IF
           INITIALIZE WS-APSVC
           MOVE "DAYSBTWN" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE LP-DUE-DATE TO SVC-DATE-1
           MOVE LP-PAID-DATE TO SVC-DATE-2
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-INT-OUT TO LP-DAYS-LATE
           IF LP-DAYS-LATE < 0
               MOVE 0 TO LP-DAYS-LATE
           END-IF
           COMPUTE LP-STATUTORY-PCT =
               LP-BASE-RATE-PCT + LP-MARGIN-PCT
           IF LP-DAYS-LATE = 0
               MOVE 0 TO LP-INTEREST-GBP
               MOVE 0 TO LP-COMPENSATION-GBP
               MOVE 0 TO LP-CLAIMED-GBP
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The debt was not paid late; no statutory "
                      "claim arises" DELIMITED BY SIZE
                   INTO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF
           COMPUTE WS-RAW =
               ((LP-DEBT-GBP * LP-STATUTORY-PCT) / 100)
               * (LP-DAYS-LATE / 365)
           INITIALIZE WS-APSVC
           MOVE "ROUND" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE WS-RAW TO SVC-NUM-1
           MOVE 2 TO SVC-INT-1
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-NUM-OUT TO LP-INTEREST-GBP
           EVALUATE TRUE
               WHEN LP-DEBT-GBP < 1000
                   MOVE LP-BAND1-GBP TO LP-COMPENSATION-GBP
               WHEN LP-DEBT-GBP < 10000
                   MOVE LP-BAND2-GBP TO LP-COMPENSATION-GBP
               WHEN OTHER
                   MOVE LP-BAND3-GBP TO LP-COMPENSATION-GBP
           END-EVALUATE
           COMPUTE LP-CLAIMED-GBP =
               LP-INTEREST-GBP + LP-COMPENSATION-GBP
           MOVE LP-CLAIMED-GBP TO WS-MONEY-ED
           MOVE LP-DAYS-LATE TO WS-DAYS-ED
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "GBP " FUNCTION TRIM(WS-MONEY-ED)
                  " claimable for "
                  FUNCTION TRIM(WS-DAYS-ED)
                  " day(s) late, as a finance cost"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3000-CLAIM SECTION.
       3000-START.
           PERFORM 2000-CALC THRU 2000-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           IF LP-CLAIMED-GBP = 0
               GO TO 3000-EXIT
           END-IF
           IF LP-CLAIM-REF = SPACES
               INITIALIZE WS-APSVC
               MOVE "NUMBER" TO SVC-FUNC
               PERFORM 8900-SVC-HEADER THRU 8900-EXIT
               MOVE "ARLP" TO SVC-TEXT-1
               CALL "APSVC" USING WS-APSVC
               MOVE SVC-TEXT-OUT(1:30) TO LP-CLAIM-REF
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; "
                  "INSERT ap.LatePaymentClaim (CompanyCode, ClaimRef,"
                  " SupplierCode, PurchaseInvoiceId, DocumentNumber, "
                  "SupplierDocRef, ClaimDate, DueDate, PaidDate, "
                  "DaysLate, DebtAmountGBP, BaseRatePct, MarginPct, "
                  "StatutoryRatePct, InterestGBP, CompensationGBP, "
                  "ClaimedTotalGBP, StatuteRef, IsDisputed, "
                  "DisputeReason, Status, FinanceCostAccount, Note, "
                  "CreatedBy) SELECT ?, ?, ?, NULLIF(?,0), "
                  "NULLIF(?,''), NULLIF(?,''), CAST(? AS date), "
                  "CAST(? AS date), CAST(? AS date), ?, ?, ?, ?, ?, "
                  "?, ?, ?, ?, ?, NULLIF(?,''), 'CLAIMED', ?, "
                  "NULLIF(?,''), ? WHERE NOT EXISTS (SELECT 1 FROM "
                  "ap.LatePaymentClaim c WHERE c.CompanyCode = ? AND "
                  "c.ClaimRef = ?); "
                  "SELECT CAST(SCOPE_IDENTITY() AS varchar(24))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE LP-CLAIM-REF TO SQLIO-PARM-VAL(2)
           MOVE LP-SUPPLIER TO SQLIO-PARM-VAL(3)
           MOVE LP-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE LP-DOCUMENT-NUMBER TO SQLIO-PARM-VAL(5)
           MOVE LP-SUPPLIER-REF TO SQLIO-PARM-VAL(6)
           MOVE LP-CLAIM-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE LP-DUE-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE LP-PAID-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE LP-DAYS-LATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE LP-DEBT-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE LP-BASE-RATE-PCT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE LP-MARGIN-PCT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE LP-STATUTORY-PCT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(14)
           MOVE LP-INTEREST-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(15)
           MOVE LP-COMPENSATION-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(16)
           MOVE LP-CLAIMED-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(17)
           MOVE LP-STATUTE-REF TO SQLIO-PARM-VAL(18)
           IF LP-IS-DISPUTED = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(19)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(19)
           END-IF
           MOVE LP-DISPUTE-REASON TO SQLIO-PARM-VAL(20)
           MOVE LP-FINANCE-ACCOUNT TO SQLIO-PARM-VAL(21)
           MOVE LP-NOTE TO SQLIO-PARM-VAL(22)
           MOVE CA-USER TO SQLIO-PARM-VAL(23)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(24)
           MOVE LP-CLAIM-REF TO SQLIO-PARM-VAL(25)
           MOVE 25 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 25
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3000-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-C-VALUE)
           MOVE WS-NUM TO LP-CLAIM-ID
           MOVE "CLAIMED" TO LP-STATUS
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PurchaseInvoice SET "
                  "LateInterestClaimedGBP = LateInterestClaimedGBP + "
                  "?, LateCompensationGBP = LateCompensationGBP + ?, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE PurchaseInvoiceId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE LP-INTEREST-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE LP-COMPENSATION-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE LP-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           MOVE LP-CLAIMED-GBP TO WS-MONEY-ED
           MOVE 4 TO CA-RETURN-CODE
           MOVE "APT0010" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Claim " FUNCTION TRIM(LP-CLAIM-REF)
                  " for GBP " FUNCTION TRIM(WS-MONEY-ED)
                  " recorded against the late invoice; it awaits "
                  "agreement" DELIMITED BY SIZE INTO CA-MESSAGE
           ADD 1 TO CA-MSG-COUNT.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AGREEING THE CLAIM POSTS IT AS A FINANCE COST (ACCOUNT 7200),
      *> NOT TO PURCHASES, AND CREATES THE PAYABLE FOR IT.
      *>----------------------------------------------------------------
       4000-AGREE SECTION.
       4000-START.
           INITIALIZE WS-APSVC
           MOVE "PERM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "AP.INVOICE.APPROVE" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-FLAG-1 NOT = "Y"
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APT0003" TO CA-REASON-CODE
               MOVE SVC-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 4000-EXIT
           END-IF
           IF LP-DEC-WAIVE
             AND (LP-DISPUTE-REASON = SPACES
                  OR LP-DISPUTE-REASON = LOW-VALUES)
               MOVE "APT0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A waiver of a statutory claim needs a "
                      "recorded reason" DELIMITED BY SIZE
                   INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF LP-AGREED-GBP = 0 AND LP-DEC-AGREE
               MOVE LP-CLAIMED-GBP TO LP-AGREED-GBP
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.LatePaymentClaim SET Status = ?, "
                  "AgreedTotalGBP = ?, ApprovedBy = ?, ApprovedAt = "
                  "SYSDATETIME(), DisputeReason = "
                  "COALESCE(NULLIF(?,''), DisputeReason), ModifiedBy "
                  "= ?, ModifiedAt = SYSDATETIME() WHERE CompanyCode "
                  "= ? AND ((? <> 0 AND ClaimId = ?) OR (? <> '' AND "
                  "ClaimRef = ?)) AND Status = 'CLAIMED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           IF LP-DECISION = SPACES
               MOVE "AGREED" TO LP-DECISION
           END-IF
           MOVE LP-DECISION TO SQLIO-PARM-VAL(1)
           MOVE LP-AGREED-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE LP-DISPUTE-REASON(1:200) TO SQLIO-PARM-VAL(4)
           MOVE CA-USER TO SQLIO-PARM-VAL(5)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(6)
           MOVE LP-CLAIM-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE LP-CLAIM-REF TO SQLIO-PARM-VAL(9)
           MOVE LP-CLAIM-REF TO SQLIO-PARM-VAL(10)
           MOVE 10 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           MOVE LP-DECISION TO LP-STATUS
           IF LP-DEC-AGREE AND LP-AGREED-GBP > 0
               PERFORM 4100-POST-COST THRU 4100-EXIT
           END-IF
           MOVE LP-AGREED-GBP TO WS-MONEY-ED
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-MESSAGE
           EVALUATE TRUE
               WHEN LP-DEC-AGREE
                   STRING "Claim agreed at GBP "
                          FUNCTION TRIM(WS-MONEY-ED)
                          " and charged to finance costs"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               WHEN LP-DEC-WAIVE
                   STRING "Claim waived with a recorded reason"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               WHEN OTHER
                   STRING "Claim rejected with a recorded reason"
                       DELIMITED BY SIZE INTO CA-MESSAGE
           END-EVALUATE.
       4000-EXIT.
           EXIT.

       4100-POST-COST SECTION.
       4100-START.
           IF CA-CORRELATION-ID = SPACES
               GO TO 4100-EXIT
           END-IF
           INITIALIZE WS-APPOST
           MOVE "POST" TO PJ-FUNC
           MOVE CA-COMPANY TO PJ-COMPANY
           MOVE CA-SITE TO PJ-SITE
           MOVE CA-USER TO PJ-USER
           MOVE CA-BUSINESS-DATE TO PJ-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO PJ-CORRELATION
           MOVE "AP.LATEPAY" TO PJ-EVENT-CODE
           MOVE "AP" TO PJ-JOURNAL-TYPE
           MOVE LP-CLAIM-REF TO PJ-SOURCE-REF
           MOVE SPACES TO PJ-DESCRIPTION
           STRING "Statutory late payment claim "
                  FUNCTION TRIM(LP-CLAIM-REF) " from "
                  FUNCTION TRIM(LP-SUPPLIER)
               DELIMITED BY SIZE INTO PJ-DESCRIPTION
           MOVE LP-CLAIM-DATE TO PJ-POSTING-DATE
           MOVE LP-CLAIM-DATE TO PJ-DOCUMENT-DATE
           MOVE "GBP" TO PJ-CURRENCY
           MOVE 1 TO PJ-FX-RATE
           MOVE "JNL" TO PJ-SERIES-CODE
           MOVE "AP" TO PJ-SUBLEDGER-TYPE
           MOVE LP-SUPPLIER TO PJ-SUBLEDGER-REF
           MOVE 2 TO PJ-LINE-CNT
           MOVE LP-FINANCE-ACCOUNT TO PJ-L-ACCOUNT(1)
           MOVE LP-AGREED-GBP TO PJ-L-DR(1)
           MOVE "ADMIN" TO PJ-L-COST-CTR(1)
           MOVE SPACES TO PJ-L-NARRATIVE(1)
           STRING "Late payment interest and compensation"
               DELIMITED BY SIZE INTO PJ-L-NARRATIVE(1)
           MOVE "2000" TO PJ-L-ACCOUNT(2)
           MOVE LP-AGREED-GBP TO PJ-L-CR(2)
           MOVE "Trade creditors control" TO PJ-L-NARRATIVE(2)
           CALL "APPOST" USING WS-APPOST
           IF PJ-RC < 8
               MOVE PJ-JOURNAL-NUMBER TO LP-JOURNAL-NUMBER
           ELSE
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APT0011" TO CA-REASON-CODE
               MOVE PJ-MESSAGE TO LP-NOTE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       4100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> D-25.  THE MAXIMUM TERM IS A PARAMETER.  FOR A SMALL SUPPLIER
      *> ANYTHING BEYOND IT NEEDS AN AUTHORISED EXCEPTION, WHICH IS A
      *> HOLD (06) RATHER THAN A WARNING.
      *>----------------------------------------------------------------
       5000-TERMCHK SECTION.
       5000-START.
           IF LP-MAX-TERM-DAYS = 0
               MOVE "TERMS.MAX.DAYS" TO SVC-TEXT-1
               MOVE 60 TO SVC-NUM-1
               PERFORM 8100-PARMNUM THRU 8100-EXIT
               MOVE SVC-NUM-OUT TO LP-MAX-TERM-DAYS
           END-IF
           IF LP-TERM-DAYS = 0 AND LP-TERMS-CODE NOT = SPACES
               INITIALIZE WS-APSVC
               MOVE "TERMS" TO SVC-FUNC
               PERFORM 8900-SVC-HEADER THRU 8900-EXIT
               MOVE LP-TERMS-CODE TO SVC-TEXT-1
               MOVE CA-BUSINESS-DATE TO SVC-DATE-1
               CALL "APSVC" USING WS-APSVC
               MOVE SVC-INT-OUT TO LP-TERM-DAYS
           END-IF
           MOVE "N" TO LP-TERM-EXCEPTION
           IF LP-TERM-DAYS > LP-MAX-TERM-DAYS
               MOVE "Y" TO LP-TERM-EXCEPTION
               MOVE LP-TERM-DAYS TO WS-DAYS-ED
               MOVE SPACES TO CA-MESSAGE
               IF LP-BUSINESS-SIZE = "SMALL"
                   MOVE 6 TO CA-RETURN-CODE
                   MOVE "APT0012" TO CA-REASON-CODE
                   STRING "Terms of " FUNCTION TRIM(WS-DAYS-ED)
                          " days exceed the approved maximum for a "
                          "small supplier and need an authorised "
                          "exception" DELIMITED BY SIZE
                       INTO CA-MESSAGE
               ELSE
                   MOVE 4 TO CA-RETURN-CODE
                   MOVE "APT0013" TO CA-REASON-CODE
                   STRING "Terms of " FUNCTION TRIM(WS-DAYS-ED)
                          " days exceed the approved maximum "
                          "payment term" DELIMITED BY SIZE
                       INTO CA-MESSAGE
               END-IF
               ADD 1 TO CA-MSG-COUNT
           ELSE
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Agreed terms are within the statutory and "
                      "policy maximum" DELIMITED BY SIZE
                   INTO CA-MESSAGE
           END-IF.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7000-READ-INVOICE SECTION.
       7000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(p.PurchaseInvoiceId AS "
                  "varchar(24)), p.RegisterNumber, p.SupplierCode, "
                  "p.SupplierDocRef, ISNULL(CONVERT(char(8), "
                  "p.DueDate, 112),'00000000'), "
                  "CAST(p.GrossAmountGBP AS varchar(24)), "
                  "ISNULL(p.TermsCode,''), s.BusinessSize, "
                  "p.SettlementState FROM ap.PurchaseInvoice p "
                  "JOIN ap.Supplier s ON s.CompanyCode = p.CompanyCode"
                  " AND s.SupplierCode = p.SupplierCode "
                  "WHERE p.CompanyCode = ? AND ((? <> 0 AND "
                  "p.PurchaseInvoiceId = ?) OR (? <> '' AND "
                  "p.RegisterNumber = ?))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE LP-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE LP-REGISTER-NUMBER TO SQLIO-PARM-VAL(4)
           MOVE LP-REGISTER-NUMBER TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 9 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 20 TO SQLIO-COL-WIDTH(3)
           MOVE 40 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 12 TO SQLIO-COL-WIDTH(7)
           MOVE 8 TO SQLIO-COL-WIDTH(8)
           MOVE 12 TO SQLIO-COL-WIDTH(9)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-INV-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 7000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "APT0005" TO CA-REASON-CODE
               MOVE "That registered document was not found"
                   TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 7000-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R-ID)
           MOVE WS-NUM TO LP-INVOICE-ID
           MOVE WS-R-REGISTER TO LP-REGISTER-NUMBER
           MOVE WS-R-SUPPLIER TO LP-SUPPLIER
           MOVE WS-R-DOCREF TO LP-SUPPLIER-REF
           IF LP-DUE-DATE = 0
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R-DUEDATE)
               MOVE WS-NUM TO LP-DUE-DATE
           END-IF
           IF LP-DEBT-GBP = 0
               COMPUTE LP-DEBT-GBP = FUNCTION NUMVAL(WS-R-GROSS)
           END-IF
           IF LP-TERMS-CODE = SPACES
               MOVE WS-R-TERMS TO LP-TERMS-CODE
           END-IF
           MOVE WS-R-SIZE TO LP-BUSINESS-SIZE.
       7000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8100-PARMNUM SECTION.
       8100-START.
           MOVE SVC-TEXT-1 TO WS-ONE-COL
           MOVE SVC-NUM-1 TO WS-NUM
           INITIALIZE WS-APSVC
           MOVE "PARMNUM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE WS-ONE-COL TO SVC-TEXT-1
           MOVE WS-NUM TO SVC-NUM-1
           CALL "APSVC" USING WS-APSVC.
       8100-EXIT.
           EXIT.

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
           MOVE "APT0099" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "APLATEPY does not support function "
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
               MOVE "SQL failure in APLATEPY" TO CA-MESSAGE
           END-IF
           ADD 1 TO CA-MSG-COUNT
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       9300-EXIT.
           EXIT.
