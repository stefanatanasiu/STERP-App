      *>================================================================
      *> PROGRAM-ID : ARADJUST
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : CUSTOMER BALANCE ADJUSTMENTS.  THE SALES LEDGER
      *>              HAS NO BALANCE FIELD TO EDIT: A BALANCE IS THE
      *>              SUM OF ITS OPEN ITEMS, SO EVERY MOVEMENT IS A
      *>              REASONED DOCUMENT WITH A BALANCING FINANCIAL
      *>              TREATMENT POSTED THROUGH ARPOSTGL (AR-020).
      *>              HANDLES WRITE OFFS, MINOR/SMALL DIFFERENCES
      *>              (D-22 GBP 5.00), SETTLEMENT DISCOUNT
      *>              CLASSIFICATION (AR-012), GOODWILL CREDITS,
      *>              ACCOUNT TO ACCOUNT TRANSFERS, CORRECTIONS AND
      *>              DOUBTFUL DEBT PROVISION MOVEMENTS.
      *> CALLED BY  : RCPTALOC, ARCTL (.NET), ARSTMT BATCH
      *> INTERFACE  : CALL "ARADJUST" USING WS-ADJUST-CA
      *>              COPYBOOK ARADJUSTCA
      *> CALLS      : SQLIO, PARMGET, PERMCHK, NUMGEN, DATECALC,
      *>              ARPOSTGL, AUDITLOG, EXCPRAIS
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-04-16 RJH  ORIGINAL SALES LEDGER PORT (MVS)
      *> 1992-11-30 DJM  CR0233 ADJUSTMENT TYPES AND REASON CODES
      *> 1994-02-11 KMP  CR0362 CLERK TOLERANCE TABLE (WAS HARD 2.50)
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS THROUGHOUT
      *> 2004-04-01 DJM  CR1702 SETTLEMENT DISCOUNT RECLASSIFICATION
      *> 2026-10-05 STERP APPROVAL MATRIX, TRANSFER LEG, D-22 LIMITS
      *>================================================================
      *> REQ: AR-012, AR-020, AR-021
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARADJUST.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 16/04/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ARWORK.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARM-CA==.
       COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERM-CA==.
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATE-CA==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDIT-CA==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXC-CA==.
       COPY ARPOSTGLCA REPLACING ==DFHCOMMAREA== BY ==WS-POSTGL-CA==.
      *>   target open item as it stands before the adjustment
       01  WS-ITM.
           05  WS-IT-ID                PIC X(24).
           05  WS-IT-TYPE              PIC X(12).
           05  WS-IT-CUST              PIC X(20).
           05  WS-IT-DOCNO             PIC X(30).
           05  WS-IT-CCY               PIC X(4).
           05  WS-IT-RESID             PIC X(24).
           05  WS-IT-RESGBP            PIC X(24).
           05  WS-IT-ORIG              PIC X(24).
           05  WS-IT-DUE               PIC X(10).
           05  WS-IT-DISCDUE           PIC X(10).
           05  WS-IT-DISCAMT           PIC X(24).
           05  WS-IT-DISP              PIC X(4).
           05  WS-IT-WOFF              PIC X(4).
           05  WS-IT-SETST             PIC X(12).
       01  WS-CUS.
           05  WS-CU-NAME              PIC X(120).
           05  WS-CU-STATUS            PIC X(12).
           05  WS-CU-CTL               PIC X(12).
           05  WS-CU-CCY               PIC X(4).
       01  WS-TOC.
           05  WS-TC-NAME              PIC X(120).
           05  WS-TC-STATUS            PIC X(12).
           05  WS-TC-CTL               PIC X(12).
       01  WS-ID-ROW.
           05  WS-NEW-ID               PIC X(24).
           05  WS-ID2                  PIC X(24).
      *>   switches and small working fields (names kept from the
      *>   MVS source so the old desk checks still read across)
       01  WS-SW.
           05  WS-TRAN-OPEN            PIC X VALUE "N".
           05  WS-ITEM-FOUND           PIC X VALUE "N".
           05  WS-DISC-OK              PIC X VALUE "N".
           05  WS-HITS                 PIC S9(4) VALUE 0.
           05  WS-KEYLEN               PIC S9(4) VALUE 0.
           05  WS-TOL                  PIC S9(11)V99 VALUE 5.
           05  WS-LIMIT                PIC S9(11)V99 VALUE 5.
           05  WS-GRACE                PIC S9(4) VALUE 2.
           05  WS-ABS                  PIC S9(11)V99 VALUE 0.
           05  WS-SIGN                 PIC S9(1) VALUE 1.
           05  WS-NEWRES               PIC S9(11)V99 VALUE 0.
           05  WS-CONTRA-ID            PIC S9(9) COMP-5 VALUE 0.
           05  WS-DISC-LAST            PIC 9(8) VALUE 0.
           05  WS-EVT-ROWS             PIC S9(9) COMP-5 VALUE 0.
           05  WS-FALLBACK             PIC X(30) VALUE SPACES.
       01  WS-RSN-LIST                 PIC X(256) VALUE SPACES.
       01  WS-RSN-KEY                  PIC X(24) VALUE SPACES.
       01  WS-CNT-ROW.
           05  WS-CNT-TXT              PIC X(24).
       LINKAGE SECTION.
       COPY ARADJUSTCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           PERFORM 0200-VALIDATE THRU 0200-EXIT
           IF AR-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0300-READ-CUSTOMER THRU 0300-EXIT
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0400-READ-TARGET THRU 0400-EXIT
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0500-REASON-APPROVED THRU 0500-EXIT
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE AR-FUNC
               WHEN "ASSESS"   PERFORM 1000-ASSESS THRU 1000-EXIT
               WHEN "POST"     PERFORM 1000-ASSESS THRU 1000-EXIT
                               PERFORM 2000-POST THRU 2000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           PERFORM 9900-SET-RESULT
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE AR-WORK
           INITIALIZE AR-CTL
           INITIALIZE WS-ITM
           INITIALIZE WS-CUS
           INITIALIZE WS-TOC
           MOVE "N" TO WS-TRAN-OPEN WS-ITEM-FOUND WS-DISC-OK
           MOVE CA-FUNCTION OF DFHCOMMAREA TO AR-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO AR-CO
           MOVE CA-SITE OF DFHCOMMAREA TO AR-SITE
           MOVE CA-USER OF DFHCOMMAREA TO AR-USERID
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO AR-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO AR-CORREL
           MOVE "ARADJUST" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO AR-RC AR-MSG-CNT
           MOVE SPACES TO AR-REASON AR-MSG
           MOVE "AR-020" TO AR-REQ-REF
           MOVE FUNCTION UPPER-CASE(AJ-ADJ-TYPE) TO AJ-ADJ-TYPE
           MOVE FUNCTION UPPER-CASE(AJ-REASON-CODE) TO AJ-REASON-CODE
           MOVE SPACES TO AJ-REQUIRED-PERM
           MOVE "N" TO AJ-WITHIN-LIMIT
           MOVE 0 TO AJ-RESIDUAL-BEFORE AJ-RESIDUAL-AFTER
           IF AJ-DOC-DATE = 0
               MOVE AR-BUSDATE TO AJ-DOC-DATE
           END-IF
           IF AJ-POSTING-DATE = 0
               MOVE AJ-DOC-DATE TO AJ-POSTING-DATE
           END-IF
           PERFORM 0150-TOLERANCES THRU 0150-EXIT.
       0100-EXIT.
           EXIT.

      *>   D-22: up to GBP 5.00 per receipt may be written off by the
      *>   AR clerk with a reason; larger write offs follow D-07.  The
      *>   grace on the discount window is two working days.
      *>   REQ: AR-012
       0150-TOLERANCES SECTION.
       0150-START.
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "AR.WRITEOFF.CLERK.LIMIT" TO PM-PARAM-CODE
           MOVE "5.00" TO PM-DEFAULT-TEXT
           MOVE "Y" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARM-CA
           IF CA-RETURN-CODE OF WS-PARM-CA < 8
               MOVE PM-VALUE-NUM TO WS-TOL
           ELSE
               MOVE 5 TO WS-TOL
           END-IF
           MOVE WS-TOL TO WS-LIMIT
           IF AJ-CLERK-LIMIT NOT = 0
               MOVE AJ-CLERK-LIMIT TO WS-LIMIT
           END-IF
           MOVE WS-LIMIT TO AJ-CLERK-LIMIT
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "AR.DISCOUNT.GRACE.DAYS" TO PM-PARAM-CODE
           MOVE "2" TO PM-DEFAULT-TEXT
           MOVE "Y" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARM-CA
           IF CA-RETURN-CODE OF WS-PARM-CA < 8
               MOVE PM-VALUE-NUM TO WS-GRACE
           ELSE
               MOVE 2 TO WS-GRACE
           END-IF.
       0150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0200-VALIDATE SECTION.
       0200-START.
           IF AR-CO = SPACES
               MOVE "ADJ0001" TO AR-REASON
               MOVE "A company code is required" TO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AJ-CUSTOMER = SPACES OR AJ-CUSTOMER = LOW-VALUES
               MOVE "ADJ0002" TO AR-REASON
               MOVE "A customer account is required" TO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF NOT (AJ-IS-WRITEOFF OR AJ-IS-CREDIT OR AJ-IS-TRANSFER
                   OR AJ-IS-CORRECTION OR AJ-IS-PROVISION)
               MOVE "ADJ0003" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Adjustment type [" FUNCTION TRIM(AJ-ADJ-TYPE)
                      "] is not one of WRITEOFF, CREDIT, TRANSFER, "
                      "CORRECTION or PROVISION"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AJ-AMOUNT-GBP = 0
               MOVE "ADJ0004" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "An adjustment of nil has no effect and is "
                      "not accepted"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
      *>   AR-020 - an approved reason, not a balance edit
           IF AJ-REASON-CODE = SPACES OR AJ-REASON = SPACES
               MOVE "ADJ0005" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Every adjustment needs an approved reason "
                      "code and a written reason"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AJ-IS-TRANSFER
              AND (AJ-TO-CUSTOMER = SPACES
                   OR AJ-TO-CUSTOMER = AJ-CUSTOMER)
               MOVE "ADJ0006" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A transfer needs a different receiving "
                      "customer account"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF (AJ-IS-WRITEOFF OR AJ-IS-CORRECTION OR AJ-IS-TRANSFER)
              AND AJ-TARGET-DOC = SPACES AND AJ-TARGET-ITEM-ID = 0
               MOVE "ADJ0007" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A write off, correction or transfer must "
                      "name the open item it adjusts"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "POST" AND AR-CORREL = SPACES
               MOVE "ADJ0008" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A correlation id is required so that a "
                      "repeated instruction has no second effect"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0300-READ-CUSTOMER SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT c.LegalName, c.Status, "
                  "ISNULL(c.ArControlAccount,''), c.CurrencyCode "
                  "FROM ar.Customer c WHERE c.CompanyCode = ? "
                  "AND c.CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE AJ-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 120 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 4 TO SQLIO-COL-WIDTH(4)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CUS
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "ADJ0010" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Customer " FUNCTION TRIM(AJ-CUSTOMER)
                      " is not on the sales ledger"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 0300-EXIT
           END-IF
      *>   AR-021 - a retired customer still owes what it owes, so the
      *>   account may be adjusted; only the reason must be approved
           IF WS-CU-STATUS = "RETIRED"
               MOVE 4 TO AR-RC
               MOVE "ADJ0011" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Customer " FUNCTION TRIM(AJ-CUSTOMER)
                      " is retired but still carries open items"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF
           IF AJ-IS-TRANSFER
               PERFORM 0350-READ-TO-CUSTOMER THRU 0350-EXIT
           END-IF.
       0300-EXIT.
           EXIT.

       0350-READ-TO-CUSTOMER SECTION.
       0350-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT c.LegalName, c.Status, "
                  "ISNULL(c.ArControlAccount,'') "
                  "FROM ar.Customer c WHERE c.CompanyCode = ? "
                  "AND c.CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE AJ-TO-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 120 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TOC
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0350-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "ADJ0012" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Receiving customer "
                      FUNCTION TRIM(AJ-TO-CUSTOMER)
                      " is not on the sales ledger"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       0350-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   The open item carries the residual.  Nothing here updates a
      *>   balance column, because there is not one (AR-020).
       0400-READ-TARGET SECTION.
       0400-START.
           IF AJ-TARGET-DOC = SPACES AND AJ-TARGET-ITEM-ID = 0
               GO TO 0400-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), o.OpenItemId), "
                  "o.ItemType, o.CustomerCode, o.DocumentNumber, "
                  "o.CurrencyCode, "
                  "CONVERT(varchar(24), o.ResidualAmount), "
                  "CONVERT(varchar(24), o.ResidualAmountGBP), "
                  "CONVERT(varchar(24), o.OriginalAmountGBP), "
                  "CONVERT(varchar(10), o.DueDate, 112), "
                  "ISNULL(CONVERT(varchar(10), o.DiscountDueDate, "
                  "112),''), CONVERT(varchar(24), "
                  "ISNULL(o.DiscountAmount,0)), "
                  "CONVERT(varchar(4), o.IsDisputed), "
                  "CONVERT(varchar(4), o.WrittenOff), "
                  "o.SettlementStatus FROM ar.OpenItem o "
                  "WHERE o.CompanyCode = ? AND o.CustomerCode = ? "
                  "AND (o.OpenItemId = NULLIF(?,0) "
                  "OR o.DocumentNumber = NULLIF(?,''))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE AJ-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE AJ-TARGET-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE AJ-TARGET-DOC TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 14 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 20 TO SQLIO-COL-WIDTH(3)
           MOVE 30 TO SQLIO-COL-WIDTH(4)
           MOVE 4 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 24 TO SQLIO-COL-WIDTH(8)
           MOVE 10 TO SQLIO-COL-WIDTH(9)
           MOVE 10 TO SQLIO-COL-WIDTH(10)
           MOVE 24 TO SQLIO-COL-WIDTH(11)
           MOVE 4 TO SQLIO-COL-WIDTH(12)
           MOVE 4 TO SQLIO-COL-WIDTH(13)
           MOVE 12 TO SQLIO-COL-WIDTH(14)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ITM
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0400-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "ADJ0013" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Open item " FUNCTION TRIM(AJ-TARGET-DOC)
                      " does not exist on account "
                      FUNCTION TRIM(AJ-CUSTOMER)
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 0400-EXIT
           END-IF
           MOVE "Y" TO WS-ITEM-FOUND
           COMPUTE AJ-OPEN-ITEM-ID = FUNCTION NUMVAL(WS-IT-ID)
           COMPUTE AJ-RESIDUAL-BEFORE = FUNCTION NUMVAL(WS-IT-RESGBP)
           MOVE WS-IT-DOCNO TO AJ-TARGET-DOC
           IF WS-IT-WOFF(1:1) = "1"
               MOVE 8 TO AR-RC
               MOVE "ADJ0014" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Open item " FUNCTION TRIM(WS-IT-DOCNO)
                      " has already been written off"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       0400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   AR-020 - the reason code must be one the business has
      *>   approved for that adjustment type.  The approved lists live
      *>   in the parameter file so that Finance may extend them
      *>   without a program change.
       0500-REASON-APPROVED SECTION.
       0500-START.
           MOVE SPACES TO PM-PARAM-CODE
           INITIALIZE WS-PARM-CA
           MOVE "GET" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE SPACES TO PM-PARAM-CODE
           STRING "AR.ADJUST.REASONS." FUNCTION TRIM(AJ-ADJ-TYPE)
               DELIMITED BY SIZE INTO PM-PARAM-CODE
           MOVE SPACES TO PM-DEFAULT-TEXT
           STRING "SMALLDIFF,SETTLEDISC,GOODWILL,BADDEBT,"
                  "UNRECOVERABLE,INSOLVENT,REBILL,MISPOST,"
                  "TRANSFER,PROVISION,AGENCY,STATUTEBARRED"
               DELIMITED BY SIZE INTO PM-DEFAULT-TEXT
           MOVE "Y" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARM-CA
           MOVE SPACES TO WS-RSN-LIST
           STRING "," FUNCTION TRIM(PM-VALUE-TEXT) ","
               DELIMITED BY SIZE INTO WS-RSN-LIST
           MOVE SPACES TO WS-RSN-KEY
           STRING "," FUNCTION TRIM(AJ-REASON-CODE) ","
               DELIMITED BY SIZE INTO WS-RSN-KEY
           COMPUTE WS-KEYLEN =
               FUNCTION LENGTH(FUNCTION TRIM(AJ-REASON-CODE)) + 2
           IF WS-KEYLEN > 24
               MOVE 24 TO WS-KEYLEN
           END-IF
           MOVE 0 TO WS-HITS
           INSPECT WS-RSN-LIST TALLYING WS-HITS
               FOR ALL WS-RSN-KEY(1:WS-KEYLEN)
           IF WS-HITS = 0
               MOVE 8 TO AR-RC
               MOVE "ADJ0015" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Reason code " FUNCTION TRIM(AJ-REASON-CODE)
                      " is not approved for a "
                      FUNCTION TRIM(AJ-ADJ-TYPE) " adjustment"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       0500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   ASSESS - classify the movement, say what authority it needs
      *>   and what the residual becomes.  No side effects.
       1000-ASSESS SECTION.
       1000-START.
           COMPUTE WS-ABS = FUNCTION ABS(AJ-AMOUNT-GBP)
           IF AJ-AMOUNT-GBP < 0
               MOVE -1 TO WS-SIGN
           ELSE
               MOVE 1 TO WS-SIGN
           END-IF
           PERFORM 1100-OVER-ADJUST THRU 1100-EXIT
           IF AR-RC-BAD
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-DISCOUNT-TEST THRU 1200-EXIT
           PERFORM 1300-LIMIT-TEST THRU 1300-EXIT
           PERFORM 1400-RESOLVE-EVENT THRU 1400-EXIT
           IF WS-ITEM-FOUND = "Y" AND NOT AJ-IS-PROVISION
               COMPUTE AJ-RESIDUAL-AFTER =
                   AJ-RESIDUAL-BEFORE - (WS-ABS * WS-SIGN)
           ELSE
               IF AJ-IS-PROVISION
                   MOVE AJ-RESIDUAL-BEFORE TO AJ-RESIDUAL-AFTER
               ELSE
                   COMPUTE AJ-RESIDUAL-AFTER = 0 - (WS-ABS * WS-SIGN)
               END-IF
           END-IF
           IF AR-RC-OK
               MOVE SPACES TO AR-MSG
               MOVE AJ-AMOUNT-GBP TO AR-MONEY-UK
               STRING FUNCTION TRIM(AJ-ADJ-TYPE) " of GBP "
                      FUNCTION TRIM(AR-MONEY-UK) " on account "
                      FUNCTION TRIM(AJ-CUSTOMER)
                      " needs " FUNCTION TRIM(AJ-REQUIRED-PERM)
                   DELIMITED BY SIZE INTO AR-MSG
           END-IF.
       1000-EXIT.
           EXIT.

      *>   a short payment stays outstanding unless a valid discount
      *>   or an approved write off explains it (AR-012)
       1100-OVER-ADJUST SECTION.
       1100-START.
           IF WS-ITEM-FOUND NOT = "Y"
               GO TO 1100-EXIT
           END-IF
           IF AJ-IS-PROVISION
               GO TO 1100-EXIT
           END-IF
           IF WS-ABS > FUNCTION ABS(AJ-RESIDUAL-BEFORE) + 0.004
               MOVE 8 TO AR-RC
               MOVE "ADJ0016" TO AR-REASON
               MOVE AJ-RESIDUAL-BEFORE TO AR-MONEY-UK
               MOVE SPACES TO AR-MSG
               STRING "The adjustment exceeds the residual of GBP "
                      FUNCTION TRIM(AR-MONEY-UK) " on item "
                      FUNCTION TRIM(AJ-TARGET-DOC)
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       1100-EXIT.
           EXIT.

      *>   AR-012 - settlement (prompt payment) discount is separately
      *>   classified from a write off.  A short payment inside the
      *>   stated terms, plus the D-22 two working day receipt timing
      *>   grace, is a discount and goes to the discount account; VAT
      *>   follows the invoice terms or a credit note, never a silent
      *>   reduction here.
      *>   REQ: AR-012
       1200-DISCOUNT-TEST SECTION.
       1200-START.
           MOVE "N" TO WS-DISC-OK
           IF WS-ITEM-FOUND NOT = "Y"
               GO TO 1200-EXIT
           END-IF
           IF NOT (AJ-IS-WRITEOFF OR AJ-IS-CORRECTION)
               GO TO 1200-EXIT
           END-IF
           IF WS-IT-DISCDUE = SPACES OR WS-IT-DISCDUE = "0"
               GO TO 1200-EXIT
           END-IF
           MOVE WS-IT-DISCDUE(1:8) TO AR-NUM-DATE
           INITIALIZE WS-DATE-CA
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DATE-CA
           MOVE AR-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE AR-USERID TO CA-USER OF WS-DATE-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-DATE-CA
           MOVE AR-NUM-DATE TO DC-DATE-1
           MOVE WS-GRACE TO DC-NUMBER
           MOVE "ENG" TO DC-CALENDAR
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA > 4
               MOVE AR-NUM-DATE TO WS-DISC-LAST
           ELSE
               MOVE DC-RESULT-DATE TO WS-DISC-LAST
           END-IF
           COMPUTE AR-AMT1 = FUNCTION NUMVAL(WS-IT-DISCAMT)
           IF AJ-POSTING-DATE <= WS-DISC-LAST
              AND AR-AMT1 > 0
              AND WS-ABS <= AR-AMT1 + 0.004
               MOVE "Y" TO WS-DISC-OK
               MOVE "SETTLEDISC" TO AJ-REASON-CODE
               MOVE 4 TO AR-RC
               MOVE "ADJ0020" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Classified as a settlement discount within "
                      "the stated terms, not as a write off"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       1200-EXIT.
           EXIT.

      *>   D-22 - minor difference inside the clerk tolerance, or an
      *>   amount that needs the D-07 approval matrix.  The hold is
      *>   return code 06, never a silent refusal.
      *>   REQ: AR-012, AR-020
       1300-LIMIT-TEST SECTION.
       1300-START.
           MOVE "N" TO AJ-WITHIN-LIMIT
           EVALUATE TRUE
               WHEN WS-DISC-OK = "Y"
                   MOVE "AR.ADJUST.DISCOUNT" TO AJ-REQUIRED-PERM
                   MOVE "Y" TO AJ-WITHIN-LIMIT
               WHEN AJ-IS-WRITEOFF AND WS-ABS <= WS-LIMIT
                   MOVE "AR.ADJUST.WRITEOFF.MINOR"
                       TO AJ-REQUIRED-PERM
                   MOVE "Y" TO AJ-WITHIN-LIMIT
                   IF AJ-REASON-CODE = SPACES
                       MOVE "SMALLDIFF" TO AJ-REASON-CODE
                   END-IF
               WHEN AJ-IS-WRITEOFF
                   MOVE "AR.ADJUST.WRITEOFF.APPROVE"
                       TO AJ-REQUIRED-PERM
               WHEN AJ-IS-CREDIT
                   MOVE "AR.ADJUST.CREDIT" TO AJ-REQUIRED-PERM
               WHEN AJ-IS-TRANSFER
                   MOVE "AR.ADJUST.TRANSFER" TO AJ-REQUIRED-PERM
               WHEN AJ-IS-PROVISION
                   MOVE "AR.ADJUST.PROVISION" TO AJ-REQUIRED-PERM
               WHEN OTHER
                   MOVE "AR.ADJUST.CORRECTION" TO AJ-REQUIRED-PERM
           END-EVALUATE
           IF AJ-WITHIN-LIMIT NOT = "Y" AND AJ-APPROVAL-ID = 0
              AND AJ-APPROVED-BY = SPACES
               MOVE 6 TO AR-RC
               MOVE "ADJ0021" TO AR-REASON
               MOVE WS-LIMIT TO AR-MONEY-UK
               MOVE SPACES TO AR-MSG
               STRING "Above the clerk tolerance of GBP "
                      FUNCTION TRIM(AR-MONEY-UK)
                      " this adjustment needs a named approver"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF
           IF AJ-APPROVED-BY NOT = SPACES
              AND AJ-APPROVED-BY = AR-USERID
               MOVE 6 TO AR-RC
               MOVE "ADJ0022" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The approver of an adjustment must not be "
                      "the person who raised it"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       1300-EXIT.
           EXIT.

      *>   choose the approved business event for the posting.  Where
      *>   the preferred event has no account mapping yet the generic
      *>   AR.ROUNDING map is used, which is what the 1992 conversion
      *>   did for the types it had no nominal analysis for.
       1400-RESOLVE-EVENT SECTION.
       1400-START.
           IF AJ-EVENT-CODE NOT = SPACES
               GO TO 1450-CHECK-MAP
           END-IF
           EVALUATE TRUE
               WHEN WS-DISC-OK = "Y"  MOVE "AR.DISCOUNT"
                                          TO AJ-EVENT-CODE
               WHEN AJ-IS-WRITEOFF    MOVE "AR.WRITEOFF"
                                          TO AJ-EVENT-CODE
               WHEN AJ-IS-CREDIT      MOVE "AR.CREDITNOTE"
                                          TO AJ-EVENT-CODE
               WHEN AJ-IS-PROVISION   MOVE "AR.PROVISION"
                                          TO AJ-EVENT-CODE
               WHEN AJ-IS-TRANSFER    MOVE "AR.ADJUST"
                                          TO AJ-EVENT-CODE
               WHEN OTHER             MOVE "AR.ADJUST"
                                          TO AJ-EVENT-CODE
           END-EVALUATE.
       1450-CHECK-MAP.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), COUNT(*)) "
                  "FROM fin.EventAccountMap m "
                  "WHERE m.CompanyCode = ? AND m.EventCode = ? "
                  "AND m.IsActive = 1"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE AJ-EVENT-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CNT-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1400-EXIT
           END-IF
           COMPUTE WS-EVT-ROWS = FUNCTION NUMVAL(WS-CNT-TXT)
           IF WS-EVT-ROWS = 0
               MOVE "AR.ROUNDING" TO AJ-EVENT-CODE
           END-IF.
       1400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   POST - one reasoned document, one balancing journal
       2000-POST SECTION.
       2000-START.
           IF AR-RC-BAD OR AR-RC-HOLD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2050-PERMISSION THRU 2050-EXIT
           IF AR-RC-BAD OR AR-RC-HOLD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2060-DOCUMENT-NO THRU 2060-EXIT
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-OPEN
      *>   a doubtful debt provision is a balance sheet movement; it
      *>   does not reduce the customer's balance, so no open item
      *>   and no allocation are written for it (AR-021)
           IF NOT AJ-IS-PROVISION
               PERFORM 2100-WRITE-ITEM THRU 2100-EXIT
               IF AR-RC-BAD
                   PERFORM 9500-ROLLBACK
                   GO TO 2000-EXIT
               END-IF
           END-IF
           IF WS-ITEM-FOUND = "Y" AND NOT AJ-IS-PROVISION
               PERFORM 2200-APPLY-TO-TARGET THRU 2200-EXIT
               IF AR-RC-BAD
                   PERFORM 9500-ROLLBACK
                   GO TO 2000-EXIT
               END-IF
           END-IF
           IF AJ-IS-TRANSFER
               PERFORM 2300-TRANSFER-LEG THRU 2300-EXIT
               IF AR-RC-BAD
                   PERFORM 9500-ROLLBACK
                   GO TO 2000-EXIT
               END-IF
           END-IF
           PERFORM 2400-POST-GL THRU 2400-EXIT
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 2000-EXIT
           END-IF
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "N" TO WS-TRAN-OPEN
           PERFORM 2500-AUDIT THRU 2500-EXIT
           IF AJ-WITHIN-LIMIT NOT = "Y"
               PERFORM 2600-EXCEPTION THRU 2600-EXIT
           END-IF
           MOVE SPACES TO AR-MSG
           MOVE AJ-AMOUNT-GBP TO AR-MONEY-UK
           STRING FUNCTION TRIM(AJ-ADJ-TYPE) " "
                  FUNCTION TRIM(AJ-DOCUMENT-NO) " of GBP "
                  FUNCTION TRIM(AR-MONEY-UK) " posted as journal "
                  FUNCTION TRIM(AJ-JOURNAL-NO)
               DELIMITED BY SIZE INTO AR-MSG.
       2000-EXIT.
           EXIT.

       2050-PERMISSION SECTION.
       2050-START.
           INITIALIZE WS-PERM-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PERM-CA
           MOVE AR-SITE TO CA-SITE OF WS-PERM-CA
           MOVE AR-USERID TO CA-USER OF WS-PERM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-PERM-CA
           MOVE AJ-REQUIRED-PERM TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERM-CA
           IF CA-RETURN-CODE OF WS-PERM-CA > 4
               MOVE 6 TO AR-RC
               MOVE "ADJ0030" TO AR-REASON
               MOVE CA-MESSAGE OF WS-PERM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       2050-EXIT.
           EXIT.

       2060-DOCUMENT-NO SECTION.
       2060-START.
           IF AJ-DOCUMENT-NO NOT = SPACES
               GO TO 2060-EXIT
           END-IF
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-NUM-CA
           MOVE AR-USERID TO CA-USER OF WS-NUM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-NUM-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-NUM-CA
           MOVE "ARADJ" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF CA-RETURN-CODE OF WS-NUM-CA > 4
               MOVE CA-RETURN-CODE OF WS-NUM-CA TO AR-RC
               MOVE "ADJ0031" TO AR-REASON
               MOVE CA-MESSAGE OF WS-NUM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 2060-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO AJ-DOCUMENT-NO.
       2060-EXIT.
           EXIT.

      *>   the adjustment itself is an open item, so the history keeps
      *>   credits, write offs, transfers and corrections apart
      *>   REQ: AR-020, AR-021
       2100-WRITE-ITEM SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ar.OpenItem (CompanyCode, CustomerCode, "
                  "ItemType, DocumentNumber, DocumentDate, DueDate, "
                  "PostingDate, CurrencyCode, FxRate, "
                  "OriginalAmount, OriginalAmountGBP, "
                  "ResidualAmount, ResidualAmountGBP, "
                  "SettlementStatus, SourceInvoiceId, CreatedBy) "
                  "OUTPUT CONVERT(varchar(24), INSERTED.OpenItemId) "
                  "VALUES (?, ?, ?, ?, ?, ?, ?, 'GBP', 1, ?, ?, "
                  "?, ?, ?, NULLIF(?,0), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 14 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE AJ-CUSTOMER TO SQLIO-PARM-VAL(2)
           IF AJ-IS-WRITEOFF
               MOVE "WRITEOFF" TO SQLIO-PARM-VAL(3)
           ELSE
               MOVE "ADJUSTMENT" TO SQLIO-PARM-VAL(3)
           END-IF
           MOVE AJ-DOCUMENT-NO TO SQLIO-PARM-VAL(4)
           MOVE AJ-DOC-DATE TO SQLIO-PARM-VAL(5)
           MOVE AJ-DOC-DATE TO SQLIO-PARM-VAL(6)
           MOVE AJ-POSTING-DATE TO SQLIO-PARM-VAL(7)
           COMPUTE AR-AMT1 = 0 - (WS-ABS * WS-SIGN)
           MOVE AR-AMT1 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(8)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(9)
      *>   an adjustment aimed at an open item is consumed at once by
      *>   the allocation written next; one raised on the account as
      *>   a whole stays open until it is applied or refunded
           IF WS-ITEM-FOUND = "Y"
               MOVE "0" TO SQLIO-PARM-VAL(10)
               MOVE "0" TO SQLIO-PARM-VAL(11)
               MOVE "SETTLED" TO SQLIO-PARM-VAL(12)
           ELSE
               MOVE FUNCTION TRIM(AR-MONEY-ED)
                   TO SQLIO-PARM-VAL(10)
               MOVE FUNCTION TRIM(AR-MONEY-ED)
                   TO SQLIO-PARM-VAL(11)
               MOVE "OPEN" TO SQLIO-PARM-VAL(12)
           END-IF
           MOVE AJ-OPEN-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(13)
           MOVE AR-USERID TO SQLIO-PARM-VAL(14)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 14
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2100-EXIT
           END-IF
           COMPUTE WS-CONTRA-ID = FUNCTION NUMVAL(WS-NEW-ID).
       2100-EXIT.
           EXIT.

      *>   apply the adjustment to the target item through a recorded
      *>   allocation; the residual falls out of the application
       2200-APPLY-TO-TARGET SECTION.
       2200-START.
           COMPUTE WS-NEWRES =
               AJ-RESIDUAL-BEFORE - (WS-ABS * WS-SIGN)
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ar.Allocation (CompanyCode, CustomerCode, "
                  "SourceType, SourceOpenItemId, TargetOpenItemId, "
                  "EffectiveDate, CurrencyCode, AppliedAmount, "
                  "AppliedAmountGBP, DiscountAmountGBP, "
                  "WriteOffAmountGBP, WriteOffReason, "
                  "WriteOffApprovalId, CreatedBy) VALUES "
                  "(?, ?, 'ADJUST', ?, ?, ?, 'GBP', ?, ?, ?, ?, "
                  "?, NULLIF(?,0), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 12 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE AJ-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE WS-CONTRA-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE AJ-OPEN-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE AJ-POSTING-DATE TO SQLIO-PARM-VAL(5)
           COMPUTE AR-AMT1 = WS-ABS * WS-SIGN
           MOVE AR-AMT1 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(6)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(7)
           IF WS-DISC-OK = "Y"
               MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(8)
               MOVE "0" TO SQLIO-PARM-VAL(9)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(8)
               MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(9)
           END-IF
           MOVE AJ-REASON TO SQLIO-PARM-VAL(10)
           MOVE AJ-APPROVAL-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(11)
           MOVE AR-USERID TO SQLIO-PARM-VAL(12)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 12
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2200-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.OpenItem SET ResidualAmount = ?, "
                  "ResidualAmountGBP = ?, SettlementStatus = "
                  "CASE WHEN ABS(?) < 0.005 THEN 'SETTLED' "
                  "ELSE 'PARTIAL' END, WrittenOff = "
                  "CASE WHEN ? = 'Y' AND ABS(?) < 0.005 THEN 1 "
                  "ELSE WrittenOff END, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE OpenItemId = ? "
                  "AND CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 8 TO SQLIO-PARM-CNT
           MOVE WS-NEWRES TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(1)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(3)
           IF AJ-IS-WRITEOFF
               MOVE "Y" TO SQLIO-PARM-VAL(4)
           ELSE
               MOVE "N" TO SQLIO-PARM-VAL(4)
           END-IF
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(5)
           MOVE AR-USERID TO SQLIO-PARM-VAL(6)
           MOVE AJ-OPEN-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(7)
           MOVE AR-CO TO SQLIO-PARM-VAL(8)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 8
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2200-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE WS-NEWRES TO AJ-RESIDUAL-AFTER.
       2200-EXIT.
           EXIT.

      *>   a transfer is two documents, one on each account, so the
      *>   two histories stay readable (AR-020)
       2300-TRANSFER-LEG SECTION.
       2300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ar.OpenItem (CompanyCode, CustomerCode, "
                  "ItemType, DocumentNumber, DocumentDate, DueDate, "
                  "PostingDate, CurrencyCode, FxRate, "
                  "OriginalAmount, OriginalAmountGBP, "
                  "ResidualAmount, ResidualAmountGBP, "
                  "SettlementStatus, CreatedBy) VALUES "
                  "(?, ?, 'ADJUSTMENT', ?, ?, ?, ?, 'GBP', 1, "
                  "?, ?, ?, ?, 'OPEN', ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 11 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE AJ-TO-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE SPACES TO AR-TEXT
           STRING FUNCTION TRIM(AJ-DOCUMENT-NO) "-T"
               DELIMITED BY SIZE INTO AR-TEXT
           MOVE AR-TEXT TO SQLIO-PARM-VAL(3)
           MOVE AJ-DOC-DATE TO SQLIO-PARM-VAL(4)
           MOVE AJ-DOC-DATE TO SQLIO-PARM-VAL(5)
           MOVE AJ-POSTING-DATE TO SQLIO-PARM-VAL(6)
           COMPUTE AR-AMT1 = WS-ABS * WS-SIGN
           MOVE AR-AMT1 TO AR-MONEY-ED
           PERFORM VARYING AR-I FROM 7 BY 1 UNTIL AR-I > 10
               MOVE FUNCTION TRIM(AR-MONEY-ED)
                   TO SQLIO-PARM-VAL(AR-I)
           END-PERFORM
           MOVE AR-USERID TO SQLIO-PARM-VAL(11)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 11
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2300-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW.
       2300-EXIT.
           EXIT.

      *>   AR-020 - the balancing financial treatment.  ARPOSTGL owns
      *>   the mapping, fin.usp_PostJournal owns the period and the
      *>   balance rules.  Nothing here touches a nominal balance.
       2400-POST-GL SECTION.
       2400-START.
           INITIALIZE WS-POSTGL-CA
           MOVE "POST" TO CA-FUNCTION OF WS-POSTGL-CA
           MOVE AR-CO TO CA-COMPANY OF WS-POSTGL-CA
           MOVE AR-USERID TO CA-USER OF WS-POSTGL-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-POSTGL-CA
           MOVE AJ-POSTING-DATE TO CA-BUSINESS-DATE OF WS-POSTGL-CA
           MOVE AJ-EVENT-CODE TO PG-EVENT-CODE
           MOVE "AR" TO PG-JOURNAL-TYPE
           MOVE "AR" TO PG-SOURCE-MODULE
           MOVE AJ-DOCUMENT-NO TO PG-SOURCE-REF
           MOVE AJ-DOC-DATE TO PG-DOC-DATE
           MOVE AJ-POSTING-DATE TO PG-POSTING-DATE
           MOVE "GBP" TO PG-CURRENCY
           MOVE 1 TO PG-FX-RATE
           MOVE "N" TO PG-OWN-TRAN
           MOVE AJ-REASON TO PG-EVIDENCE-REF
           MOVE SPACES TO PG-DESCRIPTION
           STRING FUNCTION TRIM(AJ-ADJ-TYPE) " "
                  FUNCTION TRIM(AJ-DOCUMENT-NO) " "
                  FUNCTION TRIM(AJ-CUSTOMER) " "
                  FUNCTION TRIM(AJ-REASON-CODE)
               DELIMITED BY SIZE INTO PG-DESCRIPTION
           MOVE 2 TO PG-LEG-CNT
           COMPUTE AR-AMT1 = WS-ABS
           EVALUATE TRUE
               WHEN WS-DISC-OK = "Y"
                   MOVE "DISCOUNT" TO PG-G-LEG-CODE(1)
               WHEN AJ-IS-WRITEOFF
                   MOVE "BADDEBT" TO PG-G-LEG-CODE(1)
               WHEN AJ-IS-CREDIT
                   MOVE "RETURNS" TO PG-G-LEG-CODE(1)
               WHEN AJ-IS-PROVISION
                   MOVE "CHARGE" TO PG-G-LEG-CODE(1)
               WHEN OTHER
                   MOVE "ROUNDING" TO PG-G-LEG-CODE(1)
           END-EVALUATE
           IF AJ-IS-PROVISION
               MOVE "PROVISION" TO PG-G-LEG-CODE(2)
           ELSE
               MOVE "DEBTOR" TO PG-G-LEG-CODE(2)
           END-IF
           IF AJ-ACCOUNT-CODE NOT = SPACES
               MOVE AJ-ACCOUNT-CODE TO PG-G-ACCOUNT(1)
           END-IF
           IF WS-CU-CTL NOT = SPACES AND NOT AJ-IS-PROVISION
               MOVE WS-CU-CTL TO PG-G-ACCOUNT(2)
           END-IF
           MOVE AJ-COST-CENTRE TO PG-G-COST-CENTRE(1)
           IF WS-SIGN > 0
               MOVE AR-AMT1 TO PG-G-DEBIT(1)
               MOVE AR-AMT1 TO PG-G-CREDIT(2)
           ELSE
               MOVE AR-AMT1 TO PG-G-CREDIT(1)
               MOVE AR-AMT1 TO PG-G-DEBIT(2)
           END-IF
           MOVE AJ-REASON TO PG-G-NARRATIVE(1)
           MOVE AJ-CUSTOMER TO PG-G-NARRATIVE(2)
           MOVE "AR" TO PG-G-SUBLEDGER(2)
           MOVE AJ-CUSTOMER TO PG-G-SUBREF(2)
           CALL "ARPOSTGL" USING WS-POSTGL-CA
           IF CA-RETURN-CODE OF WS-POSTGL-CA > 4
               MOVE CA-RETURN-CODE OF WS-POSTGL-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-POSTGL-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-POSTGL-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 2400-EXIT
           END-IF
           MOVE PG-JOURNAL-NO TO AJ-JOURNAL-NO.
       2400-EXIT.
           EXIT.

       2500-AUDIT SECTION.
       2500-START.
           INITIALIZE WS-AUDIT-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUDIT-CA
           MOVE AR-CO TO CA-COMPANY OF WS-AUDIT-CA
           MOVE AR-USERID TO CA-USER OF WS-AUDIT-CA
           MOVE AJ-POSTING-DATE TO CA-BUSINESS-DATE OF WS-AUDIT-CA
           MOVE "ar.OpenItem" TO AU-ENTITY-TYPE
           MOVE AJ-DOCUMENT-NO TO AU-ENTITY-KEY
           MOVE "ADJUST" TO AU-ACTION
           MOVE "ResidualAmountGBP" TO AU-FIELD-NAME
           MOVE AJ-RESIDUAL-BEFORE TO AR-MONEY-UK
           MOVE AR-MONEY-UK TO AU-OLD-VALUE
           MOVE AJ-RESIDUAL-AFTER TO AR-MONEY-UK
           MOVE AR-MONEY-UK TO AU-NEW-VALUE
           MOVE SPACES TO AU-REASON
           STRING FUNCTION TRIM(AJ-REASON-CODE) " - "
                  FUNCTION TRIM(AJ-REASON)
               DELIMITED BY SIZE INTO AU-REASON
           MOVE AJ-APPROVED-BY TO AU-AUTHORITY-REF
           MOVE "COBOL" TO AU-SOURCE
           CALL "AUDITLOG" USING WS-AUDIT-CA.
       2500-EXIT.
           EXIT.

      *>   an approved adjustment above the clerk tolerance is left on
      *>   the worklist for the monthly review (D-07 compensating
      *>   control)
       2600-EXCEPTION SECTION.
       2600-START.
           INITIALIZE WS-EXC-CA
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXC-CA
           MOVE AR-CO TO CA-COMPANY OF WS-EXC-CA
           MOVE AR-SITE TO CA-SITE OF WS-EXC-CA
           MOVE AR-USERID TO CA-USER OF WS-EXC-CA
           MOVE AJ-POSTING-DATE TO CA-BUSINESS-DATE OF WS-EXC-CA
           MOVE "AR" TO EX-MODULE
           MOVE "WARNING" TO EX-SEVERITY
           MOVE "AR.ADJUSTMENT.ABOVE.TOLERANCE" TO EX-CATEGORY
           MOVE SPACES TO EX-MESSAGE-TEXT
           MOVE AJ-AMOUNT-GBP TO AR-MONEY-UK
           STRING "Adjustment " FUNCTION TRIM(AJ-DOCUMENT-NO)
                  " of GBP " FUNCTION TRIM(AR-MONEY-UK)
                  " was approved by "
                  FUNCTION TRIM(AJ-APPROVED-BY)
               DELIMITED BY SIZE INTO EX-MESSAGE-TEXT
           MOVE "ARADJUST" TO EX-DOCUMENT-TYPE
           MOVE AJ-DOCUMENT-NO TO EX-DOCUMENT-REF
           MOVE "CREDITCONTROL" TO EX-OWNER-ROLE
           MOVE +30 TO EX-PRIORITY
           MOVE AJ-AMOUNT-GBP TO EX-AMOUNT-GBP
           CALL "EXCPRAIS" USING WS-EXC-CA.
       2600-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO AR-RC
           MOVE "Y" TO AR-ERROR
           ADD 1 TO AR-MSG-CNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE 8 TO AR-RC
           MOVE "ADJ0009" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "ARADJUST does not support function ["
                  FUNCTION TRIM(AR-FUNC) "]"
               DELIMITED BY SIZE INTO AR-MSG
           ADD 1 TO AR-MSG-CNT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE 12 TO AR-RC
           MOVE "SQL0001" TO AR-REASON
           MOVE SQLIO-ERRMSG TO AR-MSG
           ADD 1 TO AR-MSG-CNT.
       9300-EXIT.
           EXIT.

       9500-ROLLBACK SECTION.
       9500-START.
           IF WS-TRAN-OPEN = "Y"
               MOVE "ROLB" TO SQLIO-FUNC
               MOVE 0 TO SQLIO-COL-CNT
               CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
               MOVE "N" TO WS-TRAN-OPEN
           END-IF.
       9500-EXIT.
           EXIT.

       9900-SET-RESULT SECTION.
       9900-START.
           MOVE AR-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE AR-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE AR-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE AR-MSG-CNT TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE AR-REQ-REF TO CA-REQ-REF OF DFHCOMMAREA.
       9900-EXIT.
           EXIT.
