      *>================================================================
      *> PROGRAM-ID : ARDISPUT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : THE QUERY LOG.  DISPUTES, PROMISES TO PAY AND
      *>              COLLECTION OWNERSHIP AT INVOICE OR CUSTOMER
      *>              LEVEL (AR-015), AND THE RULES THAT KEEP THE
      *>              REMINDER CYCLE AWAY FROM DISPUTED, SETTLED AND
      *>              CONTACT RESTRICTED ACCOUNTS (AR-016).
      *>              A DISPUTE DOES NOT ERASE DEBT.  UNDER D-06
      *>              DISPUTED AMOUNTS STAY IN CREDIT EXPOSURE; THEY
      *>              ARE ONLY FLAGGED OUT OF THE DUNNING CYCLE.
      *> CALLED BY  : ARCTL (.NET), ARDUNBAT, ARWORKQ
      *> INTERFACE  : CALL "ARDISPUT" USING WS-DISPUT-CA
      *>              COPYBOOK ARDISPUTCA
      *> CALLS      : SQLIO, PERMCHK, NUMGEN, DATECALC, AUDITLOG,
      *>              EXCPRAIS
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-04-16 RJH  ORIGINAL SALES LEDGER PORT (MVS)
      *> 1995-09-04 DJM  CR0501 QUERY LOG ADDED TO SALES LEDGER
      *> 1998-06-22 KMP  CR0702 PROMISE TO PAY AND BROKEN PROMISE SWEEP
      *> 2002-01-14 DJM  CR1455 PARTIAL DISPUTES - CHASE THE BALANCE
      *> 2026-10-05 STERP OWNERSHIP, CONTACT RESTRICTION, D-06
      *>================================================================
      *> REQ: AR-015, AR-016
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARDISPUT.
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
       COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERM-CA==.
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATE-CA==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDIT-CA==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXC-CA==.
       01  WS-CUS.
           05  WS-CU-NAME              PIC X(120).
           05  WS-CU-STATUS            PIC X(12).
           05  WS-CU-RESTRICT          PIC X(100).
       01  WS-ITM.
           05  WS-IT-ID                PIC X(24).
           05  WS-IT-DOCNO             PIC X(30).
           05  WS-IT-RESID             PIC X(24).
           05  WS-IT-DISPAMT           PIC X(24).
           05  WS-IT-DISP              PIC X(4).
           05  WS-IT-COLST             PIC X(12).
           05  WS-IT-SETST             PIC X(12).
       01  WS-CSE.
           05  WS-CS-ID                PIC X(24).
           05  WS-CS-REF               PIC X(30).
           05  WS-CS-TYPE              PIC X(12).
           05  WS-CS-STATUS            PIC X(12).
           05  WS-CS-STEP              PIC X(4).
           05  WS-CS-DISPAMT           PIC X(24).
           05  WS-CS-OWNER             PIC X(20).
       01  WS-PRM.
           05  WS-PR-ID                PIC X(24).
           05  WS-PR-AMT               PIC X(24).
           05  WS-PR-DATE              PIC X(10).
           05  WS-PR-CASE              PIC X(24).
           05  WS-PR-SEQ               PIC X(24).
       01  WS-ID-ROW.
           05  WS-NEW-ID               PIC X(24).
       01  WS-SW.
           05  WS-TRAN-OPEN            PIC X VALUE "N".
           05  WS-ITEM-FOUND           PIC X VALUE "N".
           05  WS-CASE-FOUND           PIC X VALUE "N".
           05  WS-IS-CONTACT           PIC X VALUE "N".
           05  WS-SEQ                  PIC S9(9) VALUE 0.
           05  WS-SWEPT                PIC S9(9) VALUE 0.
           05  WS-RESID                PIC S9(11)V99 VALUE 0.
      *>   the chase cycle of specification 03 section 8, D-06
       01  WS-STEP-TAB.
           05  FILLER                  PIC X(14) VALUE "STATEMENT".
           05  FILLER                  PIC X(14) VALUE "REMINDER1".
           05  FILLER                  PIC X(14) VALUE "REMINDER2".
           05  FILLER                  PIC X(14) VALUE "STOP".
           05  FILLER                  PIC X(14) VALUE "DEMAND".
           05  FILLER                  PIC X(14) VALUE "HANDOFF".
       01  FILLER REDEFINES WS-STEP-TAB.
           05  WS-STEP-NAME            PIC X(14) OCCURS 6 TIMES.
       LINKAGE SECTION.
       COPY ARDISPUTCA.
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
           PERFORM 0400-READ-ITEM THRU 0400-EXIT
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0500-FIND-CASE THRU 0500-EXIT
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE AR-FUNC
               WHEN "DISPUTE"  PERFORM 1000-DISPUTE THRU 1000-EXIT
               WHEN "PROMISE"  PERFORM 2000-PROMISE THRU 2000-EXIT
               WHEN "ACTION"   PERFORM 3000-ACTION THRU 3000-EXIT
               WHEN "RESOLVE"  PERFORM 4000-RESOLVE THRU 4000-EXIT
               WHEN "REVIEW"   PERFORM 5000-REVIEW THRU 5000-EXIT
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
           INITIALIZE WS-CUS
           INITIALIZE WS-ITM
           INITIALIZE WS-CSE
           MOVE "N" TO WS-TRAN-OPEN WS-ITEM-FOUND WS-CASE-FOUND
           MOVE "N" TO WS-IS-CONTACT
           MOVE 0 TO WS-SEQ WS-SWEPT
           MOVE CA-FUNCTION OF DFHCOMMAREA TO AR-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO AR-CO
           MOVE CA-SITE OF DFHCOMMAREA TO AR-SITE
           MOVE CA-USER OF DFHCOMMAREA TO AR-USERID
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO AR-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO AR-CORREL
           MOVE "ARDISPUT" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO AR-RC AR-MSG-CNT
           MOVE SPACES TO AR-REASON AR-MSG
           MOVE "AR-015" TO AR-REQ-REF
           MOVE FUNCTION UPPER-CASE(DS-CASE-TYPE) TO DS-CASE-TYPE
           MOVE FUNCTION UPPER-CASE(DS-ACTION-TYPE) TO DS-ACTION-TYPE
           IF DS-CASE-TYPE = SPACES
               MOVE "DISPUTE" TO DS-CASE-TYPE
           END-IF
           IF DS-ACTION-DATE = 0
               MOVE AR-BUSDATE TO DS-ACTION-DATE
           END-IF
           IF DS-OWNER-USER = SPACES
               MOVE AR-USERID TO DS-OWNER-USER
           END-IF
           MOVE 0 TO DS-BROKEN-COUNT
           MOVE SPACES TO DS-SUPPRESS-REASON
           EVALUATE DS-ACTION-TYPE
               WHEN "CALL"       MOVE "Y" TO WS-IS-CONTACT
               WHEN "EMAIL"      MOVE "Y" TO WS-IS-CONTACT
               WHEN "REMINDER1"  MOVE "Y" TO WS-IS-CONTACT
               WHEN "REMINDER2"  MOVE "Y" TO WS-IS-CONTACT
               WHEN "DEMAND"     MOVE "Y" TO WS-IS-CONTACT
               WHEN "STATEMENT"  MOVE "Y" TO WS-IS-CONTACT
               WHEN OTHER        MOVE "N" TO WS-IS-CONTACT
           END-EVALUATE.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF AR-CO = SPACES
               MOVE "DSP0001" TO AR-REASON
               MOVE "A company code is required" TO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF DS-CUSTOMER = SPACES AND DS-CASE-REF = SPACES
              AND DS-CASE-ID = 0
               MOVE "DSP0002" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A customer account or an existing case "
                      "reference is required"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF NOT (DS-IS-DISPUTE OR DS-IS-COLLECTION
                   OR DS-IS-INSOLVENCY OR DS-IS-LEGAL)
               MOVE "DSP0003" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Case type [" FUNCTION TRIM(DS-CASE-TYPE)
                      "] is not DISPUTE, COLLECTION, INSOLVENCY "
                      "or LEGAL"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "DISPUTE"
              AND (DS-REASON-CODE = SPACES OR DS-REASON = SPACES)
               MOVE "DSP0004" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A dispute needs a reason code and the "
                      "evidence the customer has given"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "DISPUTE" AND DS-DISPUTED-AMT <= 0
               MOVE "DSP0005" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The amount in dispute must be greater "
                      "than nil"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "PROMISE"
              AND (DS-PROMISE-AMT <= 0 OR DS-PROMISE-DATE = 0)
               MOVE "DSP0006" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A promise to pay records an amount, a date "
                      "and an owner"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "ACTION" AND DS-ACTION-TYPE = SPACES
               MOVE "DSP0007" TO AR-REASON
               MOVE "An action type is required" TO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "RESOLVE" AND DS-RESOLUTION = SPACES
               MOVE "DSP0008" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A case is closed with a written resolution, "
                      "not silently"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   AR-016 - the approved communication rules live on the
      *>   customer; a recorded contact restriction is honoured by
      *>   every chase action.
       0300-READ-CUSTOMER SECTION.
       0300-START.
           IF DS-CUSTOMER = SPACES
               GO TO 0300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT c.LegalName, c.Status, "
                  "ISNULL(c.ContactRestriction,'') "
                  "FROM ar.Customer c WHERE c.CompanyCode = ? "
                  "AND c.CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE DS-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 120 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 100 TO SQLIO-COL-WIDTH(3)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CUS
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "DSP0010" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Customer " FUNCTION TRIM(DS-CUSTOMER)
                      " is not on the sales ledger"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 0300-EXIT
           END-IF
           IF DS-CONTACT-RESTRICT = SPACES
               MOVE WS-CU-RESTRICT TO DS-CONTACT-RESTRICT
           END-IF.
       0300-EXIT.
           EXIT.

       0400-READ-ITEM SECTION.
       0400-START.
           IF DS-DOCUMENT-NO = SPACES AND DS-OPEN-ITEM-ID = 0
               GO TO 0400-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), o.OpenItemId), "
                  "o.DocumentNumber, "
                  "CONVERT(varchar(24), o.ResidualAmountGBP), "
                  "CONVERT(varchar(24), o.DisputedAmountGBP), "
                  "CONVERT(varchar(4), o.IsDisputed), "
                  "o.CollectionStatus, o.SettlementStatus "
                  "FROM ar.OpenItem o WHERE o.CompanyCode = ? "
                  "AND (o.OpenItemId = NULLIF(?,0) "
                  "OR (o.DocumentNumber = NULLIF(?,'') "
                  "AND o.CustomerCode = ?))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE DS-OPEN-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE DS-DOCUMENT-NO TO SQLIO-PARM-VAL(3)
           MOVE DS-CUSTOMER TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 4 TO SQLIO-COL-WIDTH(5)
           MOVE 12 TO SQLIO-COL-WIDTH(6)
           MOVE 12 TO SQLIO-COL-WIDTH(7)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ITM
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0400-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "DSP0011" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Open item " FUNCTION TRIM(DS-DOCUMENT-NO)
                      " does not exist for that customer"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 0400-EXIT
           END-IF
           MOVE "Y" TO WS-ITEM-FOUND
           COMPUTE DS-OPEN-ITEM-ID = FUNCTION NUMVAL(WS-IT-ID)
           MOVE WS-IT-DOCNO TO DS-DOCUMENT-NO
           COMPUTE WS-RESID = FUNCTION NUMVAL(WS-IT-RESID)
           MOVE WS-RESID TO DS-RESIDUAL
      *>   AR-015 - a dispute may be partial; the undisputed balance
      *>   stays collectable under policy
           COMPUTE DS-UNDISPUTED-AMT = WS-RESID - DS-DISPUTED-AMT
           IF DS-UNDISPUTED-AMT < 0
               MOVE 0 TO DS-UNDISPUTED-AMT
           END-IF
           IF AR-FUNC = "DISPUTE"
              AND DS-DISPUTED-AMT > WS-RESID + 0.004
               MOVE 8 TO AR-RC
               MOVE "DSP0012" TO AR-REASON
               MOVE WS-RESID TO AR-MONEY-UK
               MOVE SPACES TO AR-MSG
               STRING "The amount in dispute exceeds the residual "
                      "of GBP " FUNCTION TRIM(AR-MONEY-UK)
                      " on that item"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       0400-EXIT.
           EXIT.

      *>   find the open case for this customer, item and type
       0500-FIND-CASE SECTION.
       0500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), k.CaseId), k.CaseRef, "
                  "k.CaseType, k.Status, "
                  "CONVERT(varchar(4), k.CollectionStep), "
                  "CONVERT(varchar(24), k.DisputedAmountGBP), "
                  "ISNULL(k.OwnerUser,'') FROM ar.CollectionCase k "
                  "WHERE k.CompanyCode = ? AND (k.CaseId = "
                  "NULLIF(?,0) OR k.CaseRef = NULLIF(?,'') "
                  "OR (k.CustomerCode = ? AND k.CaseType = ? "
                  "AND k.Status IN ('OPEN','ESCALATED') "
                  "AND (k.OpenItemId = NULLIF(?,0) "
                  "OR (k.OpenItemId IS NULL AND ? = 0))))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE DS-CASE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE DS-CASE-REF TO SQLIO-PARM-VAL(3)
           MOVE DS-CUSTOMER TO SQLIO-PARM-VAL(4)
           MOVE DS-CASE-TYPE TO SQLIO-PARM-VAL(5)
           MOVE DS-OPEN-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(6)
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(7)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 7
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 4 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 20 TO SQLIO-COL-WIDTH(7)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CSE
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0500-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 0500-EXIT
           END-IF
           MOVE "Y" TO WS-CASE-FOUND
           COMPUTE DS-CASE-ID = FUNCTION NUMVAL(WS-CS-ID)
           MOVE WS-CS-REF TO DS-CASE-REF
           MOVE WS-CS-TYPE TO DS-CASE-TYPE
           MOVE WS-CS-STATUS TO DS-STATUS
           COMPUTE DS-COLLECTION-STEP = FUNCTION NUMVAL(WS-CS-STEP)
           IF DS-OWNER-USER = AR-USERID AND WS-CS-OWNER NOT = SPACES
               MOVE WS-CS-OWNER TO DS-OWNER-USER
           END-IF.
       0500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   DISPUTE - raise or amend the disputed amount.  The debt is
      *>   untouched: only the chase flag and the dispute value move.
      *>   REQ: AR-015
       1000-DISPUTE SECTION.
       1000-START.
           MOVE "AR.DISPUTE.RAISE" TO PK-PERMISSION
           PERFORM 8000-PERMISSION THRU 8000-EXIT
           IF AR-RC-BAD OR AR-RC-HOLD
               GO TO 1000-EXIT
           END-IF
           IF WS-CASE-FOUND NOT = "Y"
               PERFORM 8100-CASE-REF THRU 8100-EXIT
               IF AR-RC-BAD
                   GO TO 1000-EXIT
               END-IF
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-OPEN
           IF WS-CASE-FOUND = "Y"
               PERFORM 1100-AMEND-CASE THRU 1100-EXIT
           ELSE
               PERFORM 1200-OPEN-CASE THRU 1200-EXIT
           END-IF
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           IF WS-ITEM-FOUND = "Y"
               PERFORM 1300-FLAG-ITEM THRU 1300-EXIT
               IF AR-RC-BAD
                   PERFORM 9500-ROLLBACK
                   GO TO 1000-EXIT
               END-IF
           END-IF
           MOVE "DISPUTE" TO DS-ACTION-TYPE
           MOVE DS-REASON TO DS-NARRATIVE
           PERFORM 3100-WRITE-ACTION THRU 3100-EXIT
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 9400-COMMIT
           PERFORM 8200-AUDIT-CASE THRU 8200-EXIT
           MOVE "OPEN" TO DS-STATUS
           MOVE SPACES TO AR-MSG
           MOVE DS-DISPUTED-AMT TO AR-MONEY-UK
           STRING "Dispute " FUNCTION TRIM(DS-CASE-REF)
                  " records GBP " FUNCTION TRIM(AR-MONEY-UK)
                  " in dispute; the debt remains payable and stays "
                  "in credit exposure"
               DELIMITED BY SIZE INTO AR-MSG.
       1000-EXIT.
           EXIT.

       1100-AMEND-CASE SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.CollectionCase SET "
                  "DisputedAmountGBP = ?, ReasonCode = ?, "
                  "Reason = ?, OwnerUser = ?, "
                  "NextActionDate = NULLIF(?,'19000101'), "
                  "NextAction = NULLIF(?,''), Status = 'OPEN', "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE CaseId = ? AND CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 8 TO SQLIO-PARM-CNT
           MOVE DS-DISPUTED-AMT TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(1)
           MOVE DS-REASON-CODE TO SQLIO-PARM-VAL(2)
           MOVE DS-REASON TO SQLIO-PARM-VAL(3)
           MOVE DS-OWNER-USER TO SQLIO-PARM-VAL(4)
           IF DS-NEXT-ACTION-DT = 0
               MOVE "19000101" TO SQLIO-PARM-VAL(5)
           ELSE
               MOVE DS-NEXT-ACTION-DT TO SQLIO-PARM-VAL(5)
           END-IF
           MOVE DS-NEXT-ACTION TO SQLIO-PARM-VAL(6)
           MOVE AR-USERID TO SQLIO-PARM-VAL(7)
           MOVE DS-CASE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(8)
           MOVE 9 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(9)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 9
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT.
       1100-EXIT.
           EXIT.

       1200-OPEN-CASE SECTION.
       1200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ar.CollectionCase (CompanyCode, "
                  "CustomerCode, CaseRef, CaseType, OpenItemId, "
                  "DocumentNumber, ReasonCode, Reason, "
                  "DisputedAmountGBP, OwnerUser, CollectionStep, "
                  "NextActionDate, NextAction, Status, CreatedBy) "
                  "OUTPUT CONVERT(varchar(24), INSERTED.CaseId) "
                  "VALUES (?, ?, ?, ?, NULLIF(?,0), NULLIF(?,''), "
                  "?, ?, ?, ?, ?, NULLIF(?,'19000101'), "
                  "NULLIF(?,''), 'OPEN', ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 13 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE DS-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE DS-CASE-REF TO SQLIO-PARM-VAL(3)
           MOVE DS-CASE-TYPE TO SQLIO-PARM-VAL(4)
           MOVE DS-OPEN-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(5)
           MOVE DS-DOCUMENT-NO TO SQLIO-PARM-VAL(6)
           MOVE DS-REASON-CODE TO SQLIO-PARM-VAL(7)
           MOVE DS-REASON TO SQLIO-PARM-VAL(8)
           MOVE DS-DISPUTED-AMT TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(9)
           MOVE DS-OWNER-USER TO SQLIO-PARM-VAL(10)
           MOVE DS-COLLECTION-STEP TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(11)
           IF DS-NEXT-ACTION-DT = 0
               MOVE "19000101" TO SQLIO-PARM-VAL(12)
           ELSE
               MOVE DS-NEXT-ACTION-DT TO SQLIO-PARM-VAL(12)
           END-IF
           MOVE DS-NEXT-ACTION TO SQLIO-PARM-VAL(13)
           MOVE 14 TO SQLIO-PARM-CNT
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
               GO TO 1200-EXIT
           END-IF
           COMPUTE DS-CASE-ID = FUNCTION NUMVAL(WS-NEW-ID).
       1200-EXIT.
           EXIT.

      *>   AR-016 - the item is flagged out of the dunning cycle, but
      *>   it stays on the ledger and in exposure (D-06)
      *>   REQ: AR-015, AR-016
       1300-FLAG-ITEM SECTION.
       1300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.OpenItem SET IsDisputed = 1, "
                  "DisputedAmountGBP = ?, "
                  "CollectionStatus = 'DISPUTED', ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE OpenItemId = ? "
                  "AND CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE DS-DISPUTED-AMT TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(1)
           MOVE AR-USERID TO SQLIO-PARM-VAL(2)
           MOVE DS-OPEN-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE AR-CO TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT.
       1300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   PROMISE - amount, date and owner.  Missing it produces a
      *>   follow up action, never a bank receipt.
      *>   REQ: AR-015
       2000-PROMISE SECTION.
       2000-START.
           MOVE "AR.COLLECTION.PROMISE" TO PK-PERMISSION
           PERFORM 8000-PERMISSION THRU 8000-EXIT
           IF AR-RC-BAD OR AR-RC-HOLD
               GO TO 2000-EXIT
           END-IF
           IF DS-PROMISE-DATE < AR-BUSDATE
               MOVE 8 TO AR-RC
               MOVE "DSP0020" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A promise to pay cannot be dated before "
                      "today"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 2000-EXIT
           END-IF
           IF WS-ITEM-FOUND = "Y"
              AND DS-PROMISE-AMT > WS-RESID + 0.004
               MOVE 4 TO AR-RC
               MOVE "DSP0021" TO AR-REASON
               MOVE WS-RESID TO AR-MONEY-UK
               MOVE SPACES TO AR-MSG
               STRING "The promise exceeds the residual of GBP "
                      FUNCTION TRIM(AR-MONEY-UK) " on that item"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF
           IF WS-CASE-FOUND NOT = "Y"
               MOVE "COLLECTION" TO DS-CASE-TYPE
               PERFORM 8100-CASE-REF THRU 8100-EXIT
               IF AR-RC-BAD
                   GO TO 2000-EXIT
               END-IF
               PERFORM 1200-OPEN-CASE THRU 1200-EXIT
               IF AR-RC-BAD
                   GO TO 2000-EXIT
               END-IF
           END-IF
           MOVE "OPEN" TO DS-PROMISE-STATUS
           MOVE "PROMISE" TO DS-ACTION-TYPE
           PERFORM 2100-FOLLOW-UP THRU 2100-EXIT
           PERFORM 3100-WRITE-ACTION THRU 3100-EXIT
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-SET-NEXT-ACTION THRU 2200-EXIT
           MOVE SPACES TO AR-MSG
           MOVE DS-PROMISE-AMT TO AR-MONEY-UK
           MOVE DS-PROMISE-DATE TO AR-DATE-ED
           STRING "Promise of GBP " FUNCTION TRIM(AR-MONEY-UK)
                  " on " AR-DATE-ED(7:2) "/" AR-DATE-ED(5:2) "/"
                  AR-DATE-ED(1:4) " recorded for "
                  FUNCTION TRIM(DS-OWNER-USER)
               DELIMITED BY SIZE INTO AR-MSG.
       2000-EXIT.
           EXIT.

      *>   the follow up falls one working day after the promise
       2100-FOLLOW-UP SECTION.
       2100-START.
           INITIALIZE WS-DATE-CA
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DATE-CA
           MOVE AR-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE AR-USERID TO CA-USER OF WS-DATE-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-DATE-CA
           MOVE DS-PROMISE-DATE TO DC-DATE-1
           MOVE 1 TO DC-NUMBER
           MOVE "ENG" TO DC-CALENDAR
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA > 4
               MOVE DS-PROMISE-DATE TO DS-NEXT-ACTION-DT
           ELSE
               MOVE DC-RESULT-DATE TO DS-NEXT-ACTION-DT
           END-IF
           MOVE SPACES TO DS-NEXT-ACTION
           STRING "Check the promised payment has been received"
               DELIMITED BY SIZE INTO DS-NEXT-ACTION.
       2100-EXIT.
           EXIT.

       2200-SET-NEXT-ACTION SECTION.
       2200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.CollectionCase SET NextActionDate = ?, "
                  "NextAction = ?, OwnerUser = ?, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE CaseId = ? AND CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE DS-NEXT-ACTION-DT TO SQLIO-PARM-VAL(1)
           MOVE DS-NEXT-ACTION TO SQLIO-PARM-VAL(2)
           MOVE DS-OWNER-USER TO SQLIO-PARM-VAL(3)
           MOVE AR-USERID TO SQLIO-PARM-VAL(4)
           MOVE DS-CASE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(5)
           MOVE AR-CO TO SQLIO-PARM-VAL(6)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 6
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT.
       2200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   ACTION - a contact or a deliberate suppression.  AR-016:
      *>   a chase is not sent for a disputed amount, for an amount
      *>   already settled, or against a recorded contact restriction.
      *>   REQ: AR-016
       3000-ACTION SECTION.
       3000-START.
           MOVE "AR.COLLECTION.ACTION" TO PK-PERMISSION
           PERFORM 8000-PERMISSION THRU 8000-EXIT
           IF AR-RC-BAD OR AR-RC-HOLD
               GO TO 3000-EXIT
           END-IF
           IF WS-CASE-FOUND NOT = "Y"
               MOVE "COLLECTION" TO DS-CASE-TYPE
               PERFORM 8100-CASE-REF THRU 8100-EXIT
               IF AR-RC-BAD
                   GO TO 3000-EXIT
               END-IF
               PERFORM 1200-OPEN-CASE THRU 1200-EXIT
               IF AR-RC-BAD
                   GO TO 3000-EXIT
               END-IF
           END-IF
           PERFORM 3050-SUPPRESSION THRU 3050-EXIT
           PERFORM 3100-WRITE-ACTION THRU 3100-EXIT
           IF AR-RC-BAD
               GO TO 3000-EXIT
           END-IF
           IF DS-SUPPRESS-REASON = SPACES
               PERFORM 3200-ADVANCE-STEP THRU 3200-EXIT
               MOVE SPACES TO AR-MSG
               STRING FUNCTION TRIM(DS-ACTION-TYPE)
                      " recorded on case "
                      FUNCTION TRIM(DS-CASE-REF)
                   DELIMITED BY SIZE INTO AR-MSG
           END-IF.
       3000-EXIT.
           EXIT.

       3050-SUPPRESSION SECTION.
       3050-START.
           MOVE SPACES TO DS-SUPPRESS-REASON
           IF WS-IS-CONTACT NOT = "Y"
               GO TO 3050-EXIT
           END-IF
           IF DS-CONTACT-RESTRICT NOT = SPACES
               MOVE SPACES TO DS-SUPPRESS-REASON
               STRING "Contact restriction on the account: "
                      FUNCTION TRIM(DS-CONTACT-RESTRICT)
                   DELIMITED BY SIZE INTO DS-SUPPRESS-REASON
               MOVE 6 TO AR-RC
               MOVE "DSP0030" TO AR-REASON
               MOVE DS-SUPPRESS-REASON TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 3050-EXIT
           END-IF
           IF WS-ITEM-FOUND = "Y" AND WS-IT-SETST = "SETTLED"
               MOVE SPACES TO DS-SUPPRESS-REASON
               STRING "Item " FUNCTION TRIM(DS-DOCUMENT-NO)
                      " was settled before the reminder was issued"
                   DELIMITED BY SIZE INTO DS-SUPPRESS-REASON
               MOVE 4 TO AR-RC
               MOVE "DSP0031" TO AR-REASON
               MOVE DS-SUPPRESS-REASON TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 3050-EXIT
           END-IF
           IF WS-ITEM-FOUND = "Y" AND WS-IT-DISP(1:1) = "1"
               MOVE SPACES TO DS-SUPPRESS-REASON
               STRING "Item " FUNCTION TRIM(DS-DOCUMENT-NO)
                      " is in dispute; only the undisputed balance "
                      "may be chased"
                   DELIMITED BY SIZE INTO DS-SUPPRESS-REASON
               MOVE 4 TO AR-RC
               MOVE "DSP0032" TO AR-REASON
               MOVE DS-SUPPRESS-REASON TO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       3050-EXIT.
           EXIT.

       3100-WRITE-ACTION SECTION.
       3100-START.
           PERFORM 3150-NEXT-SEQ THRU 3150-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ar.CollectionAction (CaseId, ActionSeq, "
                  "ActionType, ActionDate, PerformedBy, "
                  "ContactName, Narrative, PromiseAmountGBP, "
                  "PromiseDate, PromiseStatus, SuppressedReason) "
                  "VALUES (?, ?, ?, ?, ?, NULLIF(?,''), "
      *>           the promise amount arrives as edited money ("0.00"),
      *>           so it is cast before NULLIF - an int zero made SQL
      *>           SERVER convert "0.00" to int and fail the insert
                  "NULLIF(?,''), "
                  "NULLIF(CAST(? AS decimal(18,2)), 0), "
                  "NULLIF(?,'19000101'), NULLIF(?,''), "
                  "NULLIF(?,''))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 11 TO SQLIO-PARM-CNT
           MOVE DS-CASE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE WS-SEQ TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(2)
           IF DS-ACTION-TYPE = "DISPUTE"
               MOVE "NOTE" TO SQLIO-PARM-VAL(3)
           ELSE
               MOVE DS-ACTION-TYPE TO SQLIO-PARM-VAL(3)
           END-IF
           MOVE DS-ACTION-DATE TO SQLIO-PARM-VAL(4)
           MOVE AR-USERID TO SQLIO-PARM-VAL(5)
           MOVE DS-CONTACT-NAME TO SQLIO-PARM-VAL(6)
           MOVE DS-NARRATIVE TO SQLIO-PARM-VAL(7)
           MOVE DS-PROMISE-AMT TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(8)
           IF DS-PROMISE-DATE = 0
               MOVE "19000101" TO SQLIO-PARM-VAL(9)
           ELSE
               MOVE DS-PROMISE-DATE TO SQLIO-PARM-VAL(9)
           END-IF
           MOVE DS-PROMISE-STATUS TO SQLIO-PARM-VAL(10)
           MOVE DS-SUPPRESS-REASON TO SQLIO-PARM-VAL(11)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 11
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT
           MOVE WS-SEQ TO DS-ACTION-SEQ.
       3100-EXIT.
           EXIT.

      *>   the sequence is taken inside the unit of work; the old
      *>   source used a read-add-one which is kept here on purpose
       3150-NEXT-SEQ SECTION.
       3150-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), "
                  "ISNULL(MAX(a.ActionSeq),0) + 1) "
                  "FROM ar.CollectionAction a WHERE a.CaseId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE DS-CASE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3150-EXIT
           END-IF
           COMPUTE WS-SEQ = FUNCTION NUMVAL(WS-NEW-ID).
       3150-EXIT.
           EXIT.

      *>   the escalation schedule of specification 03 section 8
       3200-ADVANCE-STEP SECTION.
       3200-START.
           IF WS-IS-CONTACT NOT = "Y"
               GO TO 3200-EXIT
           END-IF
           IF DS-COLLECTION-STEP < 6
               ADD 1 TO DS-COLLECTION-STEP
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.CollectionCase SET CollectionStep = ?, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE CaseId = ? AND CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE DS-COLLECTION-STEP TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE AR-USERID TO SQLIO-PARM-VAL(2)
           MOVE DS-CASE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE AR-CO TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT
           IF DS-COLLECTION-STEP > 0 AND DS-COLLECTION-STEP < 7
               MOVE WS-STEP-NAME(DS-COLLECTION-STEP)
                   TO DS-NEXT-ACTION
           END-IF.
       3200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   RESOLVE - release the dispute and put the item back into
      *>   the chase cycle.  The money never moved.
      *>   REQ: AR-015, AR-016
       4000-RESOLVE SECTION.
       4000-START.
           MOVE "AR.DISPUTE.RESOLVE" TO PK-PERMISSION
           PERFORM 8000-PERMISSION THRU 8000-EXIT
           IF AR-RC-BAD OR AR-RC-HOLD
               GO TO 4000-EXIT
           END-IF
           IF WS-CASE-FOUND NOT = "Y"
               MOVE 16 TO AR-RC
               MOVE "DSP0040" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "No open case was found to resolve"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 4000-EXIT
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-OPEN
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.CollectionCase SET Status = 'RESOLVED', "
                  "ResolvedAt = SYSDATETIME(), ResolutionNote = ?, "
                  "DisputedAmountGBP = 0, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE CaseId = ? "
                  "AND CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE DS-RESOLUTION TO SQLIO-PARM-VAL(1)
           MOVE AR-USERID TO SQLIO-PARM-VAL(2)
           MOVE DS-CASE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE AR-CO TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 4000-EXIT
           END-IF
           PERFORM 4100-RELEASE-ITEM THRU 4100-EXIT
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 4000-EXIT
           END-IF
           MOVE "NOTE" TO DS-ACTION-TYPE
           MOVE DS-RESOLUTION TO DS-NARRATIVE
           MOVE 0 TO DS-PROMISE-AMT DS-PROMISE-DATE
           MOVE SPACES TO DS-PROMISE-STATUS
           PERFORM 3100-WRITE-ACTION THRU 3100-EXIT
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 4000-EXIT
           END-IF
           PERFORM 9400-COMMIT
           PERFORM 8200-AUDIT-CASE THRU 8200-EXIT
           MOVE "RESOLVED" TO DS-STATUS
           MOVE 0 TO DS-DISPUTED-AMT
           MOVE SPACES TO AR-MSG
           STRING "Case " FUNCTION TRIM(DS-CASE-REF)
                  " resolved; the item returns to the normal "
                  "collection cycle"
               DELIMITED BY SIZE INTO AR-MSG.
       4000-EXIT.
           EXIT.

       4100-RELEASE-ITEM SECTION.
       4100-START.
           IF WS-ITEM-FOUND NOT = "Y"
               GO TO 4100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.OpenItem SET IsDisputed = 0, "
                  "DisputedAmountGBP = 0, "
                  "CollectionStatus = 'NORMAL', ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE OpenItemId = ? "
                  "AND CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE AR-USERID TO SQLIO-PARM-VAL(1)
           MOVE DS-OPEN-ITEM-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE AR-CO TO SQLIO-PARM-VAL(3)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 3
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT.
       4100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   REVIEW - sweep the promises that fell due and were not
      *>   kept.  A broken promise is a follow up, not a receipt.
      *>   REQ: AR-015, AR-016
       5000-REVIEW SECTION.
       5000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), a.ActionId), "
                  "CONVERT(varchar(24), a.PromiseAmountGBP), "
                  "CONVERT(varchar(10), a.PromiseDate, 112), "
                  "CONVERT(varchar(24), a.CaseId), "
                  "CONVERT(varchar(24), a.ActionSeq) "
                  "FROM ar.CollectionAction a "
                  "INNER JOIN ar.CollectionCase k "
                  "ON k.CaseId = a.CaseId "
                  "WHERE k.CompanyCode = ? "
                  "AND (k.CustomerCode = ? OR ? = '') "
                  "AND a.PromiseStatus = 'OPEN' "
                  "AND a.PromiseDate < ? "
                  "ORDER BY a.PromiseDate"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE DS-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE DS-CUSTOMER TO SQLIO-PARM-VAL(3)
           MOVE AR-BUSDATE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PRM
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 5000-EXIT
           END-IF
           MOVE 0 TO WS-SWEPT
           PERFORM UNTIL AR-FINISHED OR AR-RC-BAD
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-PRM
               IF SQLIO-ERROR
                   PERFORM 9300-SQL-FAILED
               ELSE
                   IF SQLIO-NOTFOUND
                       MOVE "Y" TO AR-DONE
                   ELSE
                       ADD 1 TO WS-SWEPT
                       IF WS-SWEPT > 500
                           MOVE "Y" TO AR-DONE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PRM
           IF AR-RC-BAD
               GO TO 5000-EXIT
           END-IF
           IF WS-SWEPT = 0
               MOVE SPACES TO AR-MSG
               STRING "No promise fell due for review"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 5000-EXIT
           END-IF
           PERFORM 5100-BREAK-PROMISES THRU 5100-EXIT
           MOVE WS-SWEPT TO DS-BROKEN-COUNT
           IF AR-RC-OK
               PERFORM 5200-EXCEPTION THRU 5200-EXIT
               MOVE SPACES TO AR-MSG
               MOVE WS-SWEPT TO AR-INT-ED
               STRING FUNCTION TRIM(AR-INT-ED)
                      " promise(s) were not kept and now need a "
                      "follow up"
                   DELIMITED BY SIZE INTO AR-MSG
               MOVE 4 TO AR-RC
               MOVE "DSP0050" TO AR-REASON
               ADD 1 TO AR-MSG-CNT
           END-IF.
       5000-EXIT.
           EXIT.

      *>   a promise is kept only when the item it covers has been
      *>   settled or reduced below the promised amount
       5100-BREAK-PROMISES SECTION.
       5100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE a SET a.PromiseStatus = CASE WHEN EXISTS "
                  "(SELECT 1 FROM ar.OpenItem o WHERE "
                  "o.OpenItemId = k.OpenItemId "
                  "AND o.ResidualAmountGBP < a.PromiseAmountGBP) "
                  "THEN 'KEPT' ELSE 'BROKEN' END "
                  "FROM ar.CollectionAction a "
                  "INNER JOIN ar.CollectionCase k "
                  "ON k.CaseId = a.CaseId "
                  "WHERE k.CompanyCode = ? "
                  "AND (k.CustomerCode = ? OR ? = '') "
                  "AND a.PromiseStatus = 'OPEN' "
                  "AND a.PromiseDate < ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE DS-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE DS-CUSTOMER TO SQLIO-PARM-VAL(3)
           MOVE AR-BUSDATE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           PERFORM 8500-EXEC THRU 8500-EXIT.
       5100-EXIT.
           EXIT.

       5200-EXCEPTION SECTION.
       5200-START.
           INITIALIZE WS-EXC-CA
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXC-CA
           MOVE AR-CO TO CA-COMPANY OF WS-EXC-CA
           MOVE AR-SITE TO CA-SITE OF WS-EXC-CA
           MOVE AR-USERID TO CA-USER OF WS-EXC-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-EXC-CA
           MOVE "AR" TO EX-MODULE
           MOVE "WARNING" TO EX-SEVERITY
           MOVE "AR.PROMISE.BROKEN" TO EX-CATEGORY
           MOVE SPACES TO EX-MESSAGE-TEXT
           MOVE WS-SWEPT TO AR-INT-ED
           STRING FUNCTION TRIM(AR-INT-ED)
                  " promise(s) to pay passed their date without "
                  "the cash arriving"
               DELIMITED BY SIZE INTO EX-MESSAGE-TEXT
           MOVE "COLLECTIONCASE" TO EX-DOCUMENT-TYPE
           MOVE DS-CASE-REF TO EX-DOCUMENT-REF
           MOVE DS-OWNER-USER TO EX-OWNER-USER
           MOVE "CREDITCONTROL" TO EX-OWNER-ROLE
           MOVE +20 TO EX-PRIORITY
           MOVE AR-BUSDATE TO EX-DUE-AT
           CALL "EXCPRAIS" USING WS-EXC-CA
           MOVE EX-EXCEPTION-ID TO DS-EXCEPTION-ID.
       5200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-PERMISSION SECTION.
       8000-START.
           MOVE PK-PERMISSION TO AR-TEXT
           INITIALIZE WS-PERM-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PERM-CA
           MOVE AR-SITE TO CA-SITE OF WS-PERM-CA
           MOVE AR-USERID TO CA-USER OF WS-PERM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-PERM-CA
           MOVE AR-TEXT TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERM-CA
           IF CA-RETURN-CODE OF WS-PERM-CA > 4
               MOVE 6 TO AR-RC
               MOVE "DSP0060" TO AR-REASON
               MOVE CA-MESSAGE OF WS-PERM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       8000-EXIT.
           EXIT.

       8100-CASE-REF SECTION.
       8100-START.
           IF DS-CASE-REF NOT = SPACES
               GO TO 8100-EXIT
           END-IF
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-NUM-CA
           MOVE AR-USERID TO CA-USER OF WS-NUM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-NUM-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-NUM-CA
           MOVE "ARCASE" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF CA-RETURN-CODE OF WS-NUM-CA > 4
               MOVE CA-RETURN-CODE OF WS-NUM-CA TO AR-RC
               MOVE "DSP0061" TO AR-REASON
               MOVE CA-MESSAGE OF WS-NUM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 8100-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO DS-CASE-REF.
       8100-EXIT.
           EXIT.

       8200-AUDIT-CASE SECTION.
       8200-START.
           INITIALIZE WS-AUDIT-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUDIT-CA
           MOVE AR-CO TO CA-COMPANY OF WS-AUDIT-CA
           MOVE AR-USERID TO CA-USER OF WS-AUDIT-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-AUDIT-CA
           MOVE "ar.CollectionCase" TO AU-ENTITY-TYPE
           MOVE DS-CASE-REF TO AU-ENTITY-KEY
           MOVE AR-FUNC TO AU-ACTION
           MOVE "DisputedAmountGBP" TO AU-FIELD-NAME
           MOVE DS-DISPUTED-AMT TO AR-MONEY-UK
           MOVE AR-MONEY-UK TO AU-NEW-VALUE
           MOVE DS-REASON TO AU-REASON
           MOVE DS-OWNER-USER TO AU-AUTHORITY-REF
           MOVE "COBOL" TO AU-SOURCE
           CALL "AUDITLOG" USING WS-AUDIT-CA.
       8200-EXIT.
           EXIT.

       8500-EXEC SECTION.
       8500-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 8500-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW.
       8500-EXIT.
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
           MOVE "DSP0009" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "ARDISPUT does not support function ["
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

       9400-COMMIT SECTION.
       9400-START.
           IF WS-TRAN-OPEN = "Y"
               MOVE "COMT" TO SQLIO-FUNC
               MOVE 0 TO SQLIO-COL-CNT
               CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
               MOVE "N" TO WS-TRAN-OPEN
           END-IF.
       9400-EXIT.
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
