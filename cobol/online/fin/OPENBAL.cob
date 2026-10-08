      *>================================================================
      *> PROGRAM-ID : OPENBAL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER
      *> PURPOSE    : OPENING LEDGER BALANCES AT CUT OVER (FIN-024).
      *>              OPENING SUBLEDGER DETAIL ESTABLISHES THE CONTROL
      *>              BALANCE ONCE.  LOADING AN OPENING CUSTOMER
      *>              INVOICE CANNOT DUPLICATE ITS VALUE IN THE OPENING
      *>              JOURNAL, SO AC-018 OPENING TRADE DEBTORS ARE
      *>              500.00 AND NOT 1,000.00.
      *>              THE OPENING EQUITY TREATMENT IS IDENTIFIED
      *>              SEPARATELY ON ITS OWN RESERVE ACCOUNT, AND THE
      *>              BATCH IS POSTED AS ONE BALANCED JOURNAL.
      *> CALLED BY  : frmGenericMaint, FINDEMO, .NET CLIENT
      *> INTERFACE  : CALL "OPENBAL" USING WS-OPENBAL-CA
      *>              COPYBOOK OPENBALCA
      *> CALLS      : GLVALID, JNLPOST, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2000-03-27 RJH CR1150 ORIGINAL - CUT OVER FROM THE OLD SYSTEM
      *> 2026-10-05 STERP CONTROL DERIVED FROM DETAIL, NO DOUBLE COUNT
      *>================================================================
      *> REQ: FIN-024
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPENBAL.
       AUTHOR. R J HOLBROOK.
       DATE-WRITTEN. 27/03/2000.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY GLVALIDCA REPLACING ==DFHCOMMAREA== BY ==WS-GLVALID-CA==
                      LEADING ==CA-== BY ==GH-==.
       COPY JNLPOSTCA REPLACING ==DFHCOMMAREA== BY ==WS-JNLPOST-CA==
                      LEADING ==CA-== BY ==JH-==.
       01  WS-SWITCHES.
           05  WS-REJECTED             PIC X VALUE "N".
               88  WS-IS-REJECTED                VALUE "Y".
       01  WS-WORK.
           05  WS-IDX                  PIC S9(4) COMP-5 VALUE 0.
           05  WS-LINE-NO              PIC 9(4) VALUE 0.
           05  WS-CORREL               PIC X(36).
           05  WS-MONEY-ED             PIC -(11)9.99.
       01  WS-SUM-ROW.
           05  WS-S-ROWS               PIC X(12).
           05  WS-S-DEBIT              PIC X(22).
           05  WS-S-CREDIT             PIC X(22).
           05  WS-S-SUBLEDGER          PIC X(22).
           05  WS-S-EQUITY             PIC X(22).
           05  WS-S-REJECTS            PIC X(12).
       01  WS-ID-ROW.
           05  WS-I-ID                 PIC X(18).
           05  WS-I-COUNT              PIC X(12).
       LINKAGE SECTION.
       COPY OPENBALCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           PERFORM 0200-EDIT-REQUEST THRU 0200-EXIT
           IF WS-IS-REJECTED
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "LOAD"     PERFORM 1000-LOAD THRU 1000-EXIT
               WHEN "VALIDATE" PERFORM 2000-VALIDATE THRU 2000-EXIT
               WHEN "POST"     PERFORM 3000-POST-BATCH THRU 3000-EXIT
               WHEN "STATUS"   PERFORM 2000-VALIDATE THRU 2000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.

       0100-INITIALISE.
           MOVE "N" TO WS-REJECTED
           MOVE "OPENBAL" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "FIN-024" TO CA-REQ-REF
           MOVE 0 TO OB-TOTAL-DEBIT
           MOVE 0 TO OB-TOTAL-CREDIT
           MOVE 0 TO OB-CONTROL-GBP
           MOVE 0 TO OB-SUBLEDGER-GBP
           MOVE 0 TO OB-EQUITY-GBP
           MOVE 0 TO OB-DIFFERENCE
           MOVE 0 TO OB-ROW-CNT
           MOVE 0 TO OB-LOADED-CNT
           MOVE 0 TO OB-REJECT-CNT
           MOVE "N" TO OB-DUPLICATE
           MOVE "N" TO OB-BALANCED
           MOVE SPACES TO OB-STATUS
           MOVE SPACES TO OB-REJECT-REASON
           MOVE SPACES TO OB-JOURNAL-NUMBER
           IF OB-SOURCE-TYPE = SPACES
               MOVE "LEDGER" TO OB-SOURCE-TYPE
           END-IF
           IF OB-EQUITY-ACCOUNT = SPACES
               MOVE "3400" TO OB-EQUITY-ACCOUNT
           END-IF
           MOVE FUNCTION UPPER-CASE(OB-BATCH-REF) TO OB-BATCH-REF.
       0100-EXIT.
           EXIT.

       0200-EDIT-REQUEST.
           IF CA-COMPANY = SPACES
               MOVE "OPB0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF OB-BATCH-REF = SPACES
               MOVE "OPB0002" TO CA-REASON-CODE
               MOVE "An opening balance batch reference is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF OB-AS-AT-DATE = 0
               MOVE "OPB0003" TO CA-REASON-CODE
               MOVE "The cut over date is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000 - LOAD ONE OPENING DETAIL ROW
      *>----------------------------------------------------------------
       1000-LOAD.
           IF OB-ACCOUNT = SPACES
               MOVE "OPB0010" TO CA-REASON-CODE
               MOVE "An account code is required on every opening row"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           IF OB-DEBIT = 0 AND OB-CREDIT = 0
               MOVE "OPB0011" TO CA-REASON-CODE
               MOVE "An opening row with no value cannot be loaded"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           IF OB-DEBIT NOT = 0 AND OB-CREDIT NOT = 0
               MOVE "OPB0012" TO CA-REASON-CODE
               MOVE "An opening row carries a debit or a credit, not b"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           PERFORM 1100-CHECK-ACCOUNT THRU 1100-EXIT
           IF WS-IS-REJECTED
               PERFORM 1900-REJECT-ROW THRU 1900-EXIT
               GO TO 1000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.OpeningBalance (CompanyCode, BatchRef, "
                  "AsAtDate, AccountCode, SourceType, SubledgerType, "
                  "SubledgerRef, DocumentRef, DocumentDate, "
                  "CurrencyCode, DebitAmountGBP, CreditAmountGBP, "
                  "DimCostCentre, DimProject, Narrative, Status, "
                  "LoadedBy) OUTPUT inserted.OpeningId, '1' SELECT ?, "
                  "?, ?, ?, ?, NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(CAST(? AS date),'19000101'), "
                  "'GBP', ?, ?, NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), 'LOADED', ? WHERE NOT EXISTS (SELECT "
                  "1 FROM fin.OpeningBalance x WHERE x.CompanyCode = ?"
                  " AND x.BatchRef = ? AND x.AccountCode = ? "
                  "AND x.SourceType = ? AND ISNULL(x.SubledgerRef,'')"
                  " = ? AND ISNULL(x.DocumentRef,'') = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 22 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE OB-BATCH-REF TO SQLIO-PARM-VAL(2)
           MOVE OB-AS-AT-DATE TO SQLIO-PARM-VAL(3)
           MOVE OB-ACCOUNT TO SQLIO-PARM-VAL(4)
           MOVE OB-SOURCE-TYPE TO SQLIO-PARM-VAL(5)
           MOVE OB-SUBLEDGER-TYPE TO SQLIO-PARM-VAL(6)
           MOVE OB-SUBLEDGER-REF TO SQLIO-PARM-VAL(7)
           MOVE OB-DOCUMENT-REF TO SQLIO-PARM-VAL(8)
           IF OB-DOCUMENT-DATE = 0
               MOVE "19000101" TO SQLIO-PARM-VAL(9)
           ELSE
               MOVE OB-DOCUMENT-DATE TO SQLIO-PARM-VAL(9)
           END-IF
           MOVE OB-DEBIT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE OB-CREDIT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE OB-DIM-CC TO SQLIO-PARM-VAL(12)
           MOVE OB-DIM-PROJ TO SQLIO-PARM-VAL(13)
           MOVE OB-NARRATIVE TO SQLIO-PARM-VAL(14)
           MOVE CA-USER TO SQLIO-PARM-VAL(15)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(16)
           MOVE OB-BATCH-REF TO SQLIO-PARM-VAL(17)
           MOVE OB-ACCOUNT TO SQLIO-PARM-VAL(18)
           MOVE OB-SOURCE-TYPE TO SQLIO-PARM-VAL(19)
           MOVE OB-SUBLEDGER-REF TO SQLIO-PARM-VAL(20)
           MOVE OB-DOCUMENT-REF TO SQLIO-PARM-VAL(21)
           MOVE CA-USER TO SQLIO-PARM-VAL(22)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 18 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
      *>       The row is already loaded.  An accepted entry is never
      *>       duplicated on a retry (COM-018, AC-008).
               MOVE "Y" TO OB-DUPLICATE
               MOVE 4 TO CA-RETURN-CODE
               MOVE "OPB0013" TO CA-REASON-CODE
               MOVE "That opening row is already loaded in this batch "
                   TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 1000-EXIT
           END-IF
           ADD 1 TO OB-LOADED-CNT
           MOVE "LOADED" TO OB-STATUS
           MOVE "Opening row accepted" TO CA-MESSAGE.
       1000-EXIT.
           EXIT.

       1100-CHECK-ACCOUNT.
           INITIALIZE WS-GLVALID-CA
           MOVE "ACCOUNT" TO GH-FUNCTION
           MOVE CA-COMPANY TO GH-COMPANY
           MOVE CA-USER TO GH-USER
           MOVE CA-BUSINESS-DATE TO GH-BUSINESS-DATE
           MOVE OB-ACCOUNT TO GV-ACCOUNT
           MOVE OB-AS-AT-DATE TO GV-POSTING-DATE
           MOVE "N" TO GV-MANUAL-ENTRY
           CALL "GLVALID" USING WS-GLVALID-CA
           IF GH-RETURN-CODE > 4
               MOVE GH-REASON-CODE TO CA-REASON-CODE
               MOVE GH-MESSAGE TO CA-MESSAGE
               MOVE GH-MESSAGE TO OB-REJECT-REASON
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1100-EXIT.
           EXIT.

       1900-REJECT-ROW.
           ADD 1 TO OB-REJECT-CNT
           MOVE "REJECTED" TO OB-STATUS
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.OpeningBalance (CompanyCode, BatchRef, "
                  "AsAtDate, AccountCode, SourceType, DebitAmountGBP, "
                  "CreditAmountGBP, Status, RejectReason, LoadedBy) "
                  "VALUES (?, ?, ?, ?, ?, ?, ?, 'REJECTED', ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 9 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE OB-BATCH-REF TO SQLIO-PARM-VAL(2)
           MOVE OB-AS-AT-DATE TO SQLIO-PARM-VAL(3)
           MOVE OB-ACCOUNT TO SQLIO-PARM-VAL(4)
           MOVE OB-SOURCE-TYPE TO SQLIO-PARM-VAL(5)
           MOVE OB-DEBIT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE OB-CREDIT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE OB-REJECT-REASON TO SQLIO-PARM-VAL(8)
           MOVE CA-USER TO SQLIO-PARM-VAL(9)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW.
       1900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000 - TOTAL AND VALIDATE THE BATCH.  THE CONTROL BALANCE IS
      *>        THE SUBLEDGER DETAIL, NOT A SECOND KEYED FIGURE.
      *>----------------------------------------------------------------
       2000-VALIDATE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(12)), "
                  "ISNULL(SUM(CASE WHEN o.Status = 'LOADED' THEN "
                  "o.DebitAmountGBP END),0), ISNULL(SUM(CASE WHEN "
                  "o.Status = 'LOADED' THEN o.CreditAmountGBP END),0),"
                  " ISNULL(SUM(CASE WHEN o.Status = 'LOADED' AND "
                  "o.SourceType = 'SUBLEDGER' THEN o.DebitAmountGBP - "
                  "o.CreditAmountGBP END),0), ISNULL(SUM(CASE WHEN "
                  "o.Status = 'LOADED' AND o.SourceType = 'EQUITY' "
                  "THEN o.CreditAmountGBP - o.DebitAmountGBP END),0), "
                  "CAST(SUM(CASE WHEN o.Status = 'REJECTED' THEN 1 "
                  "ELSE 0 END) AS varchar(12)) FROM fin.OpeningBalance"
                  " o WHERE o.CompanyCode = ? AND o.BatchRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE OB-BATCH-REF TO SQLIO-PARM-VAL(2)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 22 TO SQLIO-COL-WIDTH(2)
           MOVE 22 TO SQLIO-COL-WIDTH(3)
           MOVE 22 TO SQLIO-COL-WIDTH(4)
           MOVE 22 TO SQLIO-COL-WIDTH(5)
           MOVE 12 TO SQLIO-COL-WIDTH(6)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2000-EXIT
           END-IF
           COMPUTE OB-ROW-CNT = FUNCTION NUMVAL(WS-S-ROWS)
           COMPUTE OB-TOTAL-DEBIT = FUNCTION NUMVAL(WS-S-DEBIT)
           COMPUTE OB-TOTAL-CREDIT = FUNCTION NUMVAL(WS-S-CREDIT)
           COMPUTE OB-SUBLEDGER-GBP = FUNCTION NUMVAL(WS-S-SUBLEDGER)
           COMPUTE OB-EQUITY-GBP = FUNCTION NUMVAL(WS-S-EQUITY)
           COMPUTE OB-REJECT-CNT = FUNCTION NUMVAL(WS-S-REJECTS)
           MOVE OB-SUBLEDGER-GBP TO OB-CONTROL-GBP
           COMPUTE OB-DIFFERENCE = OB-TOTAL-DEBIT - OB-TOTAL-CREDIT
           IF OB-DIFFERENCE = 0 AND OB-ROW-CNT > 0
               MOVE "Y" TO OB-BALANCED
               MOVE SPACES TO CA-MESSAGE
               MOVE OB-CONTROL-GBP TO WS-MONEY-ED
               STRING "Opening batch balances. Subledger detail "
                      "establishes a control balance of "
                      FUNCTION TRIM(WS-MONEY-ED)
                      " GBP, counted once"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE "N" TO OB-BALANCED
               MOVE 4 TO CA-RETURN-CODE
               MOVE "OPB0020" TO CA-REASON-CODE
               MOVE OB-DIFFERENCE TO WS-MONEY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "The opening batch is out of balance by "
                      FUNCTION TRIM(WS-MONEY-ED)
                      " GBP. The opening equity treatment must absorb "
                      "the difference explicitly"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000 - POST THE BATCH AS ONE BALANCED OPENING JOURNAL
      *>----------------------------------------------------------------
       3000-POST-BATCH.
           PERFORM 2000-VALIDATE THRU 2000-EXIT
           IF CA-RETURN-CODE = 12
               GO TO 3000-EXIT
           END-IF
           IF OB-ROW-CNT = 0
               MOVE "OPB0030" TO CA-REASON-CODE
               MOVE "No opening rows are loaded in that batch"
                   TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO WS-CORREL
           STRING "OPB-" FUNCTION TRIM(CA-COMPANY) "-"
                  FUNCTION TRIM(OB-BATCH-REF)
               DELIMITED BY SIZE INTO WS-CORREL
           PERFORM 3100-STAGE-FROM-BATCH THRU 3100-EXIT
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           IF OB-DIFFERENCE NOT = 0
               PERFORM 3200-EQUITY-LEG THRU 3200-EXIT
               IF CA-FAILED
                   GO TO 3000-EXIT
               END-IF
           END-IF
           INITIALIZE WS-JNLPOST-CA
           MOVE "POST" TO JH-FUNCTION
           MOVE CA-COMPANY TO JH-COMPANY
           MOVE CA-SITE TO JH-SITE
           MOVE CA-USER TO JH-USER
           MOVE CA-BUSINESS-DATE TO JH-BUSINESS-DATE
           MOVE WS-CORREL TO JH-CORRELATION-ID
           MOVE OB-AS-AT-DATE TO JP-POSTING-DATE
           MOVE OB-AS-AT-DATE TO JP-DOCUMENT-DATE
           MOVE "GBP" TO JP-CURRENCY
           MOVE "FIN" TO JP-SOURCE-MODULE
           MOVE OB-BATCH-REF TO JP-SOURCE-REF
           MOVE "FIN.OPENING" TO JP-EVENT-CODE
           MOVE "OPENING" TO JP-JOURNAL-TYPE
           MOVE "JNL" TO JP-SERIES-CODE
           MOVE SPACES TO JP-DESCRIPTION
           STRING "Opening balances at cut over, batch "
                  FUNCTION TRIM(OB-BATCH-REF)
               DELIMITED BY SIZE INTO JP-DESCRIPTION
           MOVE "fin.OpeningBalance" TO JP-EVIDENCE-REF
           CALL "JNLPOST" USING WS-JNLPOST-CA
           MOVE JP-NEW-JOURNAL-NO TO OB-JOURNAL-NUMBER
           IF JH-RETURN-CODE > 4
               MOVE JH-REASON-CODE TO CA-REASON-CODE
               MOVE JH-MESSAGE TO CA-MESSAGE
               MOVE JH-RETURN-CODE TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 3000-EXIT
           END-IF
           PERFORM 3300-MARK-POSTED THRU 3300-EXIT
           MOVE "POSTED" TO OB-STATUS
           MOVE SPACES TO CA-MESSAGE
           MOVE OB-CONTROL-GBP TO WS-MONEY-ED
           STRING "Opening journal " FUNCTION TRIM(OB-JOURNAL-NUMBER)
                  " posted. The subledger control carries "
                  FUNCTION TRIM(WS-MONEY-ED)
                  " GBP, reconciled once"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       3000-EXIT.
           EXIT.

      *> Subledger detail rows are summarised onto their control
      *> account - the detail is not posted twice (FIN-024, AC-018).
       3100-STAGE-FROM-BATCH.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, CurrencyCode, DimCostCentre, "
                  "DimProject, Narrative, SubledgerType, CreatedBy) "
                  "SELECT ?, ROW_NUMBER() OVER (ORDER BY "
                  "o.AccountCode, ISNULL(o.DimCostCentre,'')), "
                  "o.CompanyCode, o.AccountCode, "
                  "CASE WHEN SUM(o.DebitAmountGBP - "
                  "o.CreditAmountGBP) > 0 THEN SUM(o.DebitAmountGBP - "
                  "o.CreditAmountGBP) ELSE 0 END, CASE WHEN "
                  "SUM(o.DebitAmountGBP - o.CreditAmountGBP) < 0 THEN "
                  "-SUM(o.DebitAmountGBP - o.CreditAmountGBP) ELSE 0 "
                  "END, 'GBP', o.DimCostCentre, o.DimProject, "
                  "'Opening balance at cut over', MAX(o.SubledgerType)"
                  ", ? FROM fin.OpeningBalance o WHERE o.CompanyCode ="
                  " ? AND o.BatchRef = ? AND o.Status = 'LOADED' "
                  "GROUP BY o.CompanyCode, o.AccountCode, "
                  "o.DimCostCentre, o.DimProject HAVING "
                  "SUM(o.DebitAmountGBP - o.CreditAmountGBP) <> 0 "
                  "AND NOT EXISTS (SELECT 1 FROM fin.JournalStage s "
                  "WHERE s.CorrelationId = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE WS-CORREL TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE OB-BATCH-REF TO SQLIO-PARM-VAL(4)
           MOVE WS-CORREL TO SQLIO-PARM-VAL(5)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       3100-EXIT.
           EXIT.

       3200-EQUITY-LEG.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, CurrencyCode, Narrative, CreatedBy) "
                  "SELECT ?, 999, ?, ?, CASE WHEN ? < 0 THEN -CAST(? "
                  "AS decimal(18,2)) ELSE 0 END, CASE WHEN ? > 0 THEN "
                  "CAST(? AS decimal(18,2)) ELSE 0 END, 'GBP', "
                  "'Opening equity treatment at cut over', ? "
                  "WHERE NOT EXISTS (SELECT 1 FROM fin.JournalStage s "
                  "WHERE s.CorrelationId = ? AND s.LineNum = 999)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 9 TO SQLIO-PARM-CNT
           MOVE WS-CORREL TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE OB-EQUITY-ACCOUNT TO SQLIO-PARM-VAL(3)
           MOVE OB-DIFFERENCE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE CA-USER TO SQLIO-PARM-VAL(8)
           MOVE WS-CORREL TO SQLIO-PARM-VAL(9)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       3200-EXIT.
           EXIT.

       3300-MARK-POSTED.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.OpeningBalance SET Status = 'POSTED', "
                  "JournalNumber = ? WHERE CompanyCode = ? "
                  "AND BatchRef = ? AND Status = 'LOADED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE OB-JOURNAL-NUMBER TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE OB-BATCH-REF TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       3300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-NOT-NULL.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-IDX)
           END-PERFORM.
       8000-EXIT.
           EXIT.

       9000-REJECT.
           MOVE "Y" TO WS-REJECTED
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE "OPB0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "OPENBAL does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in OPENBAL" TO CA-MESSAGE
           END-IF
           MOVE 12 TO CA-RETURN-CODE
           MOVE "Y" TO WS-REJECTED
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
