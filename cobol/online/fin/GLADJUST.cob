      *>================================================================
      *> PROGRAM-ID : GLADJUST
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER
      *> PURPOSE    : WRITE-OFFS, IMPAIRMENTS AND OTHER VALUE ONLY
      *>              FINANCIAL CORRECTIONS (FIN-022).
      *>              EACH ADJUSTMENT CARRIES AN APPROVED REASON, AN
      *>              EVIDENCE REFERENCE AND AN APPROVAL LEVEL DRAWN
      *>              FROM A THRESHOLD LADDER.  A VALUE ADJUSTMENT
      *>              NEVER CHANGES A PHYSICAL QUANTITY: AC-017 LEAVES
      *>              100 UNITS AND A NET CARRYING VALUE OF 900.00
      *>              AFTER A 100.00 IMPAIRMENT ON 1,000.00.
      *> CALLED BY  : frmGenericMaint, AR, ITM, .NET CLIENT
      *> INTERFACE  : CALL "GLADJUST" USING WS-GLADJUST-CA
      *>              COPYBOOK GLADJUSTCA
      *> CALLS      : PARMGET, PERMCHK, JNLPOST, AUDITLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1999-02-18 RJH CR1050 ORIGINAL - MICRO FOCUS COBOL/2
      *> 2026-10-05 STERP THRESHOLD LADDER AND EVIDENCE
      *>================================================================
      *> REQ: FIN-022
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GLADJUST.
       AUTHOR. R J HOLBROOK.
       DATE-WRITTEN. 18/02/1999.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARMGET-CA==
                      LEADING ==CA-== BY ==MH-==.
       COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERMCHK-CA==
                      LEADING ==CA-== BY ==KH-==.
       COPY JNLPOSTCA REPLACING ==DFHCOMMAREA== BY ==WS-JNLPOST-CA==
                      LEADING ==CA-== BY ==JH-==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDITLOG-CA==
                      LEADING ==CA-== BY ==AH-==.
       01  WS-SWITCHES.
           05  WS-REJECTED             PIC X VALUE "N".
               88  WS-IS-REJECTED                VALUE "Y".
       01  WS-WORK.
           05  WS-IDX                  PIC S9(4) COMP-5 VALUE 0.
           05  WS-LINE-NO              PIC 9(4) VALUE 0.
           05  WS-CLERK-LIMIT          PIC S9(11)V99 VALUE 500.
           05  WS-MGR-LIMIT            PIC S9(11)V99 VALUE 5000.
           05  WS-ABS-AMT              PIC S9(13)V99 VALUE 0.
           05  WS-CORREL               PIC X(36).
           05  WS-MONEY-ED             PIC -(11)9.99.
       01  WS-ROW.
           05  WS-W-ID                 PIC X(18).
           05  WS-W-STATUS             PIC X(12).
           05  WS-W-AMOUNT             PIC X(22).
           05  WS-W-ACCOUNT            PIC X(12).
           05  WS-W-CONTRA             PIC X(12).
           05  WS-W-APPROVER           PIC X(20).
       LINKAGE SECTION.
       COPY GLADJUSTCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           IF CA-COMPANY = SPACES
               MOVE "ADJ0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "ASSESS"   PERFORM 1000-ASSESS THRU 1000-EXIT
               WHEN "REQUEST"  PERFORM 2000-REQUEST THRU 2000-EXIT
               WHEN "APPROVE"  PERFORM 3000-APPROVE THRU 3000-EXIT
               WHEN "POST"     PERFORM 4000-POST-ADJ THRU 4000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.

       0100-INITIALISE.
           MOVE "N" TO WS-REJECTED
           MOVE "GLADJUST" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "FIN-022" TO CA-REQ-REF
           MOVE 0 TO GA-THRESHOLD
           MOVE 0 TO GA-NET-CARRYING
           MOVE 0 TO GA-QTY-AFTER
           MOVE SPACES TO GA-APPROVAL-LEVEL
           MOVE SPACES TO GA-PERMISSION
           MOVE SPACES TO GA-JOURNAL-NUMBER
           MOVE "N" TO GA-NEEDS-APPROVAL
           MOVE "N" TO GA-EVIDENCE-OK
           IF GA-POSTING-DATE = 0
               MOVE CA-BUSINESS-DATE TO GA-POSTING-DATE
           END-IF
           IF GA-TYPE = SPACES
               MOVE "BALANCE" TO GA-TYPE
           END-IF
           MOVE FUNCTION UPPER-CASE(GA-TYPE) TO GA-TYPE
           MOVE FUNCTION UPPER-CASE(GA-WRITEOFF-REF)
               TO GA-WRITEOFF-REF.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000 - THRESHOLD AND APPROVAL LEVEL.  A VALUE ADJUSTMENT
      *>        LEAVES THE PHYSICAL QUANTITY WHERE IT WAS.
      *>----------------------------------------------------------------
       1000-ASSESS.
           PERFORM 1100-READ-LIMITS THRU 1100-EXIT
           COMPUTE WS-ABS-AMT = FUNCTION ABS(GA-AMOUNT)
           COMPUTE GA-NET-CARRYING = GA-CARRYING-AMOUNT - GA-AMOUNT
           MOVE GA-QUANTITY TO GA-QTY-AFTER
           IF WS-ABS-AMT NOT > WS-CLERK-LIMIT
               MOVE "CLERK" TO GA-APPROVAL-LEVEL
               MOVE WS-CLERK-LIMIT TO GA-THRESHOLD
               MOVE "FIN.JOURNAL.ENTER" TO GA-PERMISSION
               MOVE "N" TO GA-NEEDS-APPROVAL
           ELSE
               IF WS-ABS-AMT NOT > WS-MGR-LIMIT
                   MOVE "MANAGER" TO GA-APPROVAL-LEVEL
                   MOVE WS-MGR-LIMIT TO GA-THRESHOLD
                   MOVE "FIN.JOURNAL.APPROVE" TO GA-PERMISSION
                   MOVE "Y" TO GA-NEEDS-APPROVAL
               ELSE
                   MOVE "CONTROLLER" TO GA-APPROVAL-LEVEL
                   MOVE WS-MGR-LIMIT TO GA-THRESHOLD
                   MOVE "FIN.PERIOD.CLOSE" TO GA-PERMISSION
                   MOVE "Y" TO GA-NEEDS-APPROVAL
               END-IF
           END-IF
           IF GA-EVIDENCE-REF NOT = SPACES
               MOVE "Y" TO GA-EVIDENCE-OK
           END-IF
           MOVE SPACES TO CA-MESSAGE
           MOVE WS-ABS-AMT TO WS-MONEY-ED
           STRING "An adjustment of " FUNCTION TRIM(WS-MONEY-ED)
                  " GBP needs "
                  FUNCTION TRIM(FUNCTION LOWER-CASE(GA-APPROVAL-LEVEL))
                  " authority (" FUNCTION TRIM(GA-PERMISSION) ")"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       1000-EXIT.
           EXIT.

       1100-READ-LIMITS.
           INITIALIZE WS-PARMGET-CA
           MOVE "GETNUM" TO MH-FUNCTION
           MOVE CA-COMPANY TO MH-COMPANY
           MOVE CA-USER TO MH-USER
           MOVE CA-BUSINESS-DATE TO MH-BUSINESS-DATE
           MOVE "FIN.WRITEOFF.CLERK" TO PM-PARAM-CODE
           MOVE "N" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARMGET-CA
           IF PM-FOUND = "Y" AND PM-VALUE-NUM > 0
               MOVE PM-VALUE-NUM TO WS-CLERK-LIMIT
           END-IF
           INITIALIZE WS-PARMGET-CA
           MOVE "GETNUM" TO MH-FUNCTION
           MOVE CA-COMPANY TO MH-COMPANY
           MOVE CA-USER TO MH-USER
           MOVE CA-BUSINESS-DATE TO MH-BUSINESS-DATE
           MOVE "FIN.WRITEOFF.MANAGER" TO PM-PARAM-CODE
           MOVE "N" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARMGET-CA
           IF PM-FOUND = "Y" AND PM-VALUE-NUM > 0
               MOVE PM-VALUE-NUM TO WS-MGR-LIMIT
           END-IF.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000 - RECORD THE REQUEST
      *>----------------------------------------------------------------
       2000-REQUEST.
           PERFORM 1000-ASSESS THRU 1000-EXIT
           IF CA-RETURN-CODE = 12
               GO TO 2000-EXIT
           END-IF
           IF GA-REASON = SPACES OR GA-REASON-CODE = SPACES
               MOVE "ADJ0010" TO CA-REASON-CODE
               MOVE "A write-off needs an approved reason code and an "
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF GA-EVIDENCE-OK NOT = "Y"
               MOVE "ADJ0011" TO CA-REASON-CODE
               MOVE "Supporting evidence must be referenced before an "
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF GA-ACCOUNT = SPACES OR GA-CONTRA-ACCOUNT = SPACES
               MOVE "ADJ0012" TO CA-REASON-CODE
               MOVE "Both the adjusted account and its contra are requ"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF GA-QUANTITY NOT = 0
               PERFORM 2050-CHECK-MOVEMENT THRU 2050-EXIT
               IF CA-FAILED
                   GO TO 2000-EXIT
               END-IF
           END-IF
           IF GA-WRITEOFF-REF = SPACES
               MOVE SPACES TO GA-WRITEOFF-REF
               STRING "WO-" FUNCTION TRIM(CA-COMPANY) "-"
                      GA-POSTING-DATE "-"
                      FUNCTION TRIM(GA-REASON-CODE)
                   DELIMITED BY SIZE INTO GA-WRITEOFF-REF
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.WriteOff (CompanyCode, WriteOffRef, "
                  "WriteOffType, ReasonCode, Reason, AccountCode, "
                  "ContraAccountCode, SubledgerType, SubledgerRef, "
                  "CarryingAmountGBP, AmountGBP, QuantityAffected, "
                  "ThresholdGBP, ApprovalLevel, EvidenceRef, "
                  "PostingDate, Status, RequestedBy) OUTPUT "
                  "inserted.WriteOffId, inserted.Status, '', '', '', "
                  "'' VALUES (?, ?, ?, ?, ?, ?, ?, NULLIF(?,''), "
                  "NULLIF(?,''), ?, ?, ?, ?, ?, ?, ?, ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 18 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE GA-WRITEOFF-REF TO SQLIO-PARM-VAL(2)
           MOVE GA-TYPE TO SQLIO-PARM-VAL(3)
           MOVE GA-REASON-CODE TO SQLIO-PARM-VAL(4)
           MOVE GA-REASON TO SQLIO-PARM-VAL(5)
           MOVE GA-ACCOUNT TO SQLIO-PARM-VAL(6)
           MOVE GA-CONTRA-ACCOUNT TO SQLIO-PARM-VAL(7)
           MOVE GA-SUBLEDGER-TYPE TO SQLIO-PARM-VAL(8)
           MOVE GA-SUBLEDGER-REF TO SQLIO-PARM-VAL(9)
           MOVE GA-CARRYING-AMOUNT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE GA-AMOUNT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE GA-QUANTITY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE GA-THRESHOLD TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE GA-APPROVAL-LEVEL TO SQLIO-PARM-VAL(14)
           MOVE GA-EVIDENCE-REF TO SQLIO-PARM-VAL(15)
           MOVE GA-POSTING-DATE TO SQLIO-PARM-VAL(16)
           IF GA-NEEDS-APPROVAL = "Y"
               MOVE "PENDING" TO SQLIO-PARM-VAL(17)
           ELSE
               MOVE "APPROVED" TO SQLIO-PARM-VAL(17)
           END-IF
           MOVE CA-USER TO SQLIO-PARM-VAL(18)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 18 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 22 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 12 TO SQLIO-COL-WIDTH(5)
           MOVE 20 TO SQLIO-COL-WIDTH(6)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2000-EXIT
           END-IF
           COMPUTE GA-WRITEOFF-ID = FUNCTION NUMVAL(WS-W-ID)
           MOVE WS-W-STATUS TO GA-STATUS
           PERFORM 8200-AUDIT THRU 8200-EXIT
           MOVE SPACES TO CA-MESSAGE
           STRING "Adjustment " FUNCTION TRIM(GA-WRITEOFF-REF)
                  " recorded with status " FUNCTION TRIM(GA-STATUS)
                  ". Physical quantities are unchanged"
               DELIMITED BY SIZE INTO CA-MESSAGE
           IF GA-NEEDS-APPROVAL = "Y"
               MOVE 6 TO CA-RETURN-CODE
               MOVE "ADJ0013" TO CA-REASON-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2050 - A CORRECTION MAY NOT MOVE PHYSICAL QUANTITY ON ITS
      *>        OWN.  WHEN AN ADJUSTMENT CLAIMS A QUANTITY EFFECT THE
      *>        AUTHORISED STOCK MOVEMENT THAT ACTUALLY MOVED THE
      *>        GOODS MUST BE NAMED AND MUST EXIST (FIN-022).
      *>----------------------------------------------------------------
       2050-CHECK-MOVEMENT.
           IF GA-SUBLEDGER-TYPE NOT = "STOCK"
              OR GA-SUBLEDGER-REF = SPACES
               MOVE "ADJ0014" TO CA-REASON-CODE
               MOVE "A quantity affecting adjustment must name the aut"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2050-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(MovementId AS varchar(18)), "
                  "'', '', '', '', '' FROM inv.StockMovement WHERE "
                  "CompanyCode = ? AND CAST(MovementId AS "
                  "varchar(40)) = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE GA-SUBLEDGER-REF TO SQLIO-PARM-VAL(2)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 18 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 22 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 12 TO SQLIO-COL-WIDTH(5)
           MOVE 20 TO SQLIO-COL-WIDTH(6)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2050-EXIT
           END-IF
           IF SQLIO-NOTFOUND OR WS-W-ID = SPACES
               MOVE "ADJ0014" TO CA-REASON-CODE
               MOVE "The stock movement quoted by this adjustment is n"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       2050-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000 - APPROVE AT THE REQUIRED LEVEL
      *>----------------------------------------------------------------
       3000-APPROVE.
           PERFORM 3100-READ-WRITEOFF THRU 3100-EXIT
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           PERFORM 1000-ASSESS THRU 1000-EXIT
           INITIALIZE WS-PERMCHK-CA
           MOVE "CHECK" TO KH-FUNCTION
           MOVE CA-COMPANY TO KH-COMPANY
           MOVE CA-SITE TO KH-SITE
           MOVE CA-USER TO KH-USER
           MOVE CA-BUSINESS-DATE TO KH-BUSINESS-DATE
           MOVE GA-PERMISSION TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERMCHK-CA
           IF KH-RETURN-CODE > 4
               MOVE KH-REASON-CODE TO CA-REASON-CODE
               MOVE KH-MESSAGE TO CA-MESSAGE
               MOVE KH-RETURN-CODE TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.WriteOff SET Status = 'APPROVED', "
                  "ApprovedBy = ?, ApprovedAt = SYSDATETIME() "
                  "WHERE WriteOffId = ? AND CompanyCode = ? "
                  "AND Status = 'PENDING' AND RequestedBy <> ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE GA-WRITEOFF-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3000-EXIT
           END-IF
           IF SQLIO-ROWCOUNT = 0
               MOVE "ADJ0020" TO CA-REASON-CODE
               MOVE "Nothing was approved. The adjustment may already "
                   TO CA-MESSAGE
               MOVE 6 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 3000-EXIT
           END-IF
           MOVE "APPROVED" TO GA-STATUS
           PERFORM 8200-AUDIT THRU 8200-EXIT
           MOVE "The adjustment is approved and may now be posted"
               TO CA-MESSAGE.
       3000-EXIT.
           EXIT.

       3100-READ-WRITEOFF.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(w.WriteOffId AS varchar(18)), w.Status,"
                  " w.AmountGBP, w.AccountCode, w.ContraAccountCode, "
                  "ISNULL(w.ApprovedBy,'') FROM fin.WriteOff w "
                  "WHERE w.CompanyCode = ? AND (w.WriteOffId = "
                  "NULLIF(CAST(? AS int),0) OR w.WriteOffRef = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE GA-WRITEOFF-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE GA-WRITEOFF-REF TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 18 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 22 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 12 TO SQLIO-COL-WIDTH(5)
           MOVE 20 TO SQLIO-COL-WIDTH(6)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "ADJ0021" TO CA-REASON-CODE
               MOVE "That adjustment is not held for the company"
                   TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 3100-EXIT
           END-IF
           COMPUTE GA-WRITEOFF-ID = FUNCTION NUMVAL(WS-W-ID)
           MOVE WS-W-STATUS TO GA-STATUS
           COMPUTE GA-AMOUNT = FUNCTION NUMVAL(WS-W-AMOUNT)
           MOVE WS-W-ACCOUNT TO GA-ACCOUNT
           MOVE WS-W-CONTRA TO GA-CONTRA-ACCOUNT
           MOVE WS-W-APPROVER TO GA-APPROVER.
       3100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000 - POST THE APPROVED ADJUSTMENT
      *>----------------------------------------------------------------
       4000-POST-ADJ.
           PERFORM 3100-READ-WRITEOFF THRU 3100-EXIT
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           IF GA-STATUS NOT = "APPROVED"
               MOVE "ADJ0030" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Only an approved adjustment can be posted. This"
                      " one is " FUNCTION TRIM(GA-STATUS)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO WS-CORREL
           STRING "WOF-" FUNCTION TRIM(CA-COMPANY) "-"
                  FUNCTION TRIM(GA-WRITEOFF-REF)
               DELIMITED BY SIZE INTO WS-CORREL
           MOVE 1 TO WS-LINE-NO
           PERFORM 4100-STAGE-LINE THRU 4100-EXIT
           MOVE 2 TO WS-LINE-NO
           PERFORM 4100-STAGE-LINE THRU 4100-EXIT
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           INITIALIZE WS-JNLPOST-CA
           MOVE "POST" TO JH-FUNCTION
           MOVE CA-COMPANY TO JH-COMPANY
           MOVE CA-SITE TO JH-SITE
           MOVE CA-USER TO JH-USER
           MOVE CA-BUSINESS-DATE TO JH-BUSINESS-DATE
           MOVE WS-CORREL TO JH-CORRELATION-ID
           MOVE GA-POSTING-DATE TO JP-POSTING-DATE
           MOVE GA-POSTING-DATE TO JP-DOCUMENT-DATE
           MOVE "GBP" TO JP-CURRENCY
           MOVE "FIN" TO JP-SOURCE-MODULE
           MOVE GA-WRITEOFF-REF TO JP-SOURCE-REF
           MOVE "FIN.WRITEOFF" TO JP-EVENT-CODE
           MOVE "ADJUST" TO JP-JOURNAL-TYPE
           MOVE "JNL" TO JP-SERIES-CODE
           MOVE GA-REASON TO JP-DESCRIPTION
           MOVE GA-EVIDENCE-REF TO JP-EVIDENCE-REF
           CALL "JNLPOST" USING WS-JNLPOST-CA
           MOVE JP-NEW-JOURNAL-NO TO GA-JOURNAL-NUMBER
           IF JH-RETURN-CODE > 4
               MOVE JH-REASON-CODE TO CA-REASON-CODE
               MOVE JH-MESSAGE TO CA-MESSAGE
               MOVE JH-RETURN-CODE TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.WriteOff SET Status = 'POSTED', "
                  "JournalNumber = ?, CorrelationId = ? "
                  "WHERE WriteOffId = ? AND Status = 'APPROVED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE GA-JOURNAL-NUMBER TO SQLIO-PARM-VAL(1)
           MOVE WS-CORREL TO SQLIO-PARM-VAL(2)
           MOVE GA-WRITEOFF-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "POSTED" TO GA-STATUS
           COMPUTE GA-NET-CARRYING = GA-CARRYING-AMOUNT - GA-AMOUNT
           MOVE GA-QUANTITY TO GA-QTY-AFTER
           PERFORM 8200-AUDIT THRU 8200-EXIT
           MOVE SPACES TO CA-MESSAGE
           MOVE GA-NET-CARRYING TO WS-MONEY-ED
           STRING "Adjustment posted as journal "
                  FUNCTION TRIM(GA-JOURNAL-NUMBER)
                  ". Net carrying value is now "
                  FUNCTION TRIM(WS-MONEY-ED)
                  " GBP and the physical quantity is unchanged"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       4000-EXIT.
           EXIT.

       4100-STAGE-LINE.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, CurrencyCode, DimCostCentre, "
                  "Narrative, SubledgerType, SubledgerRef, CreatedBy) "
                  "SELECT ?, ?, ?, ?, ?, ?, 'GBP', NULLIF(?,''), ?, "
                  "NULLIF(?,''), NULLIF(?,''), ? WHERE NOT EXISTS "
                  "(SELECT 1 FROM fin.JournalStage s WHERE "
                  "s.CorrelationId = ? AND s.LineNum = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 13 TO SQLIO-PARM-CNT
           MOVE WS-CORREL TO SQLIO-PARM-VAL(1)
           MOVE WS-LINE-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE GA-AMOUNT TO SQLIO-NUM-EDIT
           IF WS-LINE-NO = 1
               MOVE GA-ACCOUNT TO SQLIO-PARM-VAL(4)
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
               MOVE "0" TO SQLIO-PARM-VAL(6)
               MOVE GA-DIM-CC TO SQLIO-PARM-VAL(7)
           ELSE
               MOVE GA-CONTRA-ACCOUNT TO SQLIO-PARM-VAL(4)
               MOVE "0" TO SQLIO-PARM-VAL(5)
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
               MOVE SPACES TO SQLIO-PARM-VAL(7)
           END-IF
           MOVE GA-REASON(1:80) TO SQLIO-PARM-VAL(8)
           MOVE GA-SUBLEDGER-TYPE TO SQLIO-PARM-VAL(9)
           MOVE GA-SUBLEDGER-REF TO SQLIO-PARM-VAL(10)
           MOVE CA-USER TO SQLIO-PARM-VAL(11)
           MOVE WS-CORREL TO SQLIO-PARM-VAL(12)
           MOVE WS-LINE-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(13)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       4100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-NOT-NULL.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-IDX)
           END-PERFORM.
       8000-EXIT.
           EXIT.

       8200-AUDIT.
           INITIALIZE WS-AUDITLOG-CA
           MOVE "WRITE" TO AH-FUNCTION
           MOVE CA-COMPANY TO AH-COMPANY
           MOVE CA-USER TO AH-USER
           MOVE CA-BUSINESS-DATE TO AH-BUSINESS-DATE
           MOVE "fin.WriteOff" TO AU-ENTITY-TYPE
           MOVE GA-WRITEOFF-REF TO AU-ENTITY-KEY
           MOVE CA-FUNCTION TO AU-ACTION
           MOVE GA-AMOUNT TO WS-MONEY-ED
           MOVE WS-MONEY-ED TO AU-NEW-VALUE
           MOVE GA-REASON TO AU-REASON
           MOVE GA-EVIDENCE-REF(1:60) TO AU-AUTHORITY-REF
           MOVE "COBOL" TO AU-SOURCE
           CALL "AUDITLOG" USING WS-AUDITLOG-CA.
       8200-EXIT.
           EXIT.

       9000-REJECT.
           MOVE "Y" TO WS-REJECTED
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE "ADJ0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "GLADJUST does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in GLADJUST" TO CA-MESSAGE
           END-IF
           MOVE 12 TO CA-RETURN-CODE
           MOVE "Y" TO WS-REJECTED
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
