      *>================================================================
      *> PROGRAM-ID : ARPOSTGL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : THE SALES LEDGER INTERFACE INTO THE NOMINAL.
      *>              RESOLVES EACH LEG OF A BUSINESS EVENT TO ITS
      *>              APPROVED ACCOUNT, STAGES THE LINES IN
      *>              fin.JournalStage AND CALLS fin.usp_PostJournal.
      *>              NOTHING IN THE SALES LEDGER WRITES TO THE
      *>              NOMINAL BY ANY OTHER ROUTE - COM-014 ALL OR
      *>              NOTHING, AND NO SILENT SUSPENSE ACCOUNT.
      *> CALLED BY  : ARINVPST, ARCRNOTE, RCPTALOC, ARDEPOS, DISHONOR,
      *>              ARADJUST, LATEINT, BADDEBT, DDCOLL
      *> INTERFACE  : CALL "ARPOSTGL" USING WS-POSTGL-CA
      *>              COPYBOOK ARPOSTGLCA
      *> CALLS      : SQLIO -> fin.usp_PostJournal
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-05-02 RJH  ORIGINAL MVS SALES LEDGER / NOMINAL BRIDGE
      *> 1996-02-11 DJM  CR0412 ANALYSIS CODES CARRIED ON EVERY LEG
      *> 2026-10-05 STERP STAGING TABLE AND EVENT ACCOUNT MAP
      *>================================================================
      *> REQ: AR-004, AR-006, AR-012, AR-020, AR-021
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARPOSTGL.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 02/05/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ARWORK.
       01  WS-MAP-ROW.
           05  WS-M-LEG                PIC X(12).
           05  WS-M-ACCOUNT            PIC X(12).
           05  WS-M-DRCR               PIC X(2).
           05  WS-M-CC                 PIC X(20).
           05  WS-M-PROJ               PIC X(20).
       01  WS-JNL-ROW.
           05  WS-J-ID                 PIC X(24).
           05  WS-J-NUMBER             PIC X(30).
           05  WS-J-DEBIT              PIC X(24).
           05  WS-J-CREDIT             PIC X(24).
           05  WS-J-DUP                PIC X(4).
       01  WS-LINE-NO                  PIC S9(4) VALUE 0.
       01  WS-STAGED                   PIC S9(4) VALUE 0.
       LINKAGE SECTION.
       COPY ARPOSTGLCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF AR-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE AR-FUNC
               WHEN "ACCOUNT"  PERFORM 1000-RESOLVE-ONE
               WHEN "POST"     PERFORM 2000-POST-JOURNAL
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
           INITIALIZE WS-MAP-ROW
           INITIALIZE WS-JNL-ROW
           MOVE 0 TO WS-LINE-NO WS-STAGED
           MOVE CA-FUNCTION OF DFHCOMMAREA TO AR-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO AR-CO
           MOVE CA-USER OF DFHCOMMAREA TO AR-USERID
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO AR-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO AR-CORREL
           MOVE "ARPOSTGL" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO AR-RC AR-MSG-CNT
           MOVE SPACES TO AR-REASON AR-MSG
           MOVE "AR-004" TO AR-REQ-REF
           MOVE SPACES TO PG-JOURNAL-NO
           MOVE 0 TO PG-JOURNAL-ID PG-TOTAL-DEBIT PG-TOTAL-CREDIT
           MOVE "N" TO PG-WAS-DUPLICATE
           IF PG-CURRENCY = SPACES OR PG-CURRENCY = LOW-VALUES
               MOVE "GBP" TO PG-CURRENCY
           END-IF
           IF PG-SOURCE-MODULE = SPACES
               MOVE "AR" TO PG-SOURCE-MODULE
           END-IF
           IF PG-JOURNAL-TYPE = SPACES
               MOVE "AR" TO PG-JOURNAL-TYPE
           END-IF
           IF PG-SERIES-CODE = SPACES
               MOVE "JNL" TO PG-SERIES-CODE
           END-IF
           IF PG-POSTING-DATE = 0
               MOVE AR-BUSDATE TO PG-POSTING-DATE
           END-IF
           IF PG-DOC-DATE = 0
               MOVE PG-POSTING-DATE TO PG-DOC-DATE
           END-IF
           IF PG-OWN-TRAN = SPACE OR PG-OWN-TRAN = LOW-VALUE
               MOVE "N" TO PG-OWN-TRAN
           END-IF.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF AR-CO = SPACES
               MOVE "GLP0001" TO AR-REASON
               MOVE "A company code is required to post" TO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF PG-EVENT-CODE = SPACES OR PG-EVENT-CODE = LOW-VALUES
               MOVE "GLP0002" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A business event code is required so that the "
                      "approved account mapping can be used"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "POST" AND AR-CORREL = SPACES
               MOVE "GLP0003" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A correlation id is required so that a "
                      "repeated posting has no second effect"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AR-FUNC = "POST"
              AND (PG-LEG-CNT < 2 OR PG-LEG-CNT > 30)
               MOVE "GLP0004" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A journal needs between two and thirty legs"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   ACCOUNT - answer the account for one leg of the event
       1000-RESOLVE-ONE SECTION.
       1000-START.
           MOVE PG-LOOKUP-LEG TO WS-M-LEG
           PERFORM 1100-READ-MAP
           IF AR-RC-BAD
               GO TO 1000-EXIT
           END-IF
           MOVE WS-M-ACCOUNT TO PG-LOOKUP-ACCOUNT.
       1000-EXIT.
           EXIT.

       1100-READ-MAP SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT m.LegCode, m.AccountCode, m.DrCr, "
                  "ISNULL(m.DimCostCentre,''), "
                  "ISNULL(m.DimProject,'') "
                  "FROM fin.EventAccountMap m "
                  "WHERE m.CompanyCode = ? AND m.EventCode = ? "
                  "AND m.LegCode = ? AND m.IsActive = 1"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE PG-EVENT-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE WS-M-LEG TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 2 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-MAP-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 8 TO AR-RC
               MOVE "GLP0005" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "No approved account mapping exists for event "
                      FUNCTION TRIM(PG-EVENT-CODE) " leg "
                      FUNCTION TRIM(WS-M-LEG)
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   POST - stage every leg then hand the journal to Finance
       2000-POST-JOURNAL SECTION.
       2000-START.
           PERFORM 2050-CLEAR-STAGE
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-STAGE-LEGS
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           IF WS-STAGED < 2
               MOVE 8 TO AR-RC
               MOVE "GLP0006" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A journal with fewer than two value bearing "
                      "legs cannot be posted"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 2000-EXIT
           END-IF
           IF PG-TOTAL-DEBIT NOT = PG-TOTAL-CREDIT
               MOVE 8 TO AR-RC
               MOVE "GLP0007" TO AR-REASON
               MOVE PG-TOTAL-DEBIT TO AR-MONEY-UK
               MOVE SPACES TO AR-MSG
               STRING "The sales ledger journal does not balance; "
                      "debits are GBP "
                      FUNCTION TRIM(AR-MONEY-UK)
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 2000-EXIT
           END-IF
           PERFORM 2300-CALL-POST.
       2000-EXIT.
           EXIT.

      *>   a retried attempt may have left staged lines behind
       2050-CLEAR-STAGE SECTION.
       2050-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE fin.JournalStage WHERE CorrelationId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE AR-CORREL TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-MAP-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2050-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-MAP-ROW.
       2050-EXIT.
           EXIT.

       2100-STAGE-LEGS SECTION.
       2100-START.
           MOVE 0 TO PG-TOTAL-DEBIT PG-TOTAL-CREDIT
           MOVE 0 TO WS-LINE-NO WS-STAGED
           PERFORM VARYING AR-I FROM 1 BY 1
               UNTIL AR-I > PG-LEG-CNT OR AR-RC-BAD
               IF PG-G-DEBIT(AR-I) = 0 AND PG-G-CREDIT(AR-I) = 0
                   CONTINUE
               ELSE
                   PERFORM 2150-ONE-LEG
               END-IF
           END-PERFORM.
       2100-EXIT.
           EXIT.

       2150-ONE-LEG SECTION.
       2150-START.
           MOVE SPACES TO WS-M-CC WS-M-PROJ
           IF PG-G-ACCOUNT(AR-I) = SPACES
              OR PG-G-ACCOUNT(AR-I) = LOW-VALUES
              OR PG-G-COST-CENTRE(AR-I) = SPACES
               MOVE PG-G-LEG-CODE(AR-I) TO WS-M-LEG
               PERFORM 1100-READ-MAP
               IF AR-RC-BAD
                   GO TO 2150-EXIT
               END-IF
               IF PG-G-ACCOUNT(AR-I) = SPACES
                  OR PG-G-ACCOUNT(AR-I) = LOW-VALUES
                   MOVE WS-M-ACCOUNT TO PG-G-ACCOUNT(AR-I)
               END-IF
      *>       the approved mapping also states the analysis the
      *>       account insists on, so a caller never has to know it
               IF PG-G-COST-CENTRE(AR-I) = SPACES
                   MOVE WS-M-CC TO PG-G-COST-CENTRE(AR-I)
               END-IF
               IF PG-G-PROJECT(AR-I) = SPACES
                   MOVE WS-M-PROJ TO PG-G-PROJECT(AR-I)
               END-IF
           END-IF
           ADD 1 TO WS-LINE-NO
           ADD 1 TO WS-STAGED
           ADD PG-G-DEBIT(AR-I) TO PG-TOTAL-DEBIT
           ADD PG-G-CREDIT(AR-I) TO PG-TOTAL-CREDIT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, CurrencyCode, DimCostCentre, "
                  "DimProject, Narrative, VatCode, SubledgerType, "
                  "SubledgerRef, SourceRef, BankAccountCode, "
                  "CreatedBy) VALUES (?, ?, ?, ?, ?, ?, ?, "
                  "NULLIF(?,''), NULLIF(?,''), ?, NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), ?, NULLIF(?,''), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 16 TO SQLIO-PARM-CNT
           MOVE AR-CORREL TO SQLIO-PARM-VAL(1)
           MOVE WS-LINE-NO TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE AR-CO TO SQLIO-PARM-VAL(3)
           MOVE PG-G-ACCOUNT(AR-I) TO SQLIO-PARM-VAL(4)
           MOVE PG-G-DEBIT(AR-I) TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(5)
           MOVE PG-G-CREDIT(AR-I) TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(6)
           MOVE PG-CURRENCY TO SQLIO-PARM-VAL(7)
           MOVE PG-G-COST-CENTRE(AR-I) TO SQLIO-PARM-VAL(8)
           MOVE PG-G-PROJECT(AR-I) TO SQLIO-PARM-VAL(9)
           MOVE PG-G-NARRATIVE(AR-I) TO SQLIO-PARM-VAL(10)
           MOVE PG-G-VAT-CODE(AR-I) TO SQLIO-PARM-VAL(11)
           MOVE PG-G-SUBLEDGER(AR-I) TO SQLIO-PARM-VAL(12)
           MOVE PG-G-SUBREF(AR-I) TO SQLIO-PARM-VAL(13)
           MOVE PG-SOURCE-REF TO SQLIO-PARM-VAL(14)
           MOVE PG-G-BANK-ACCT(AR-I) TO SQLIO-PARM-VAL(15)
           MOVE AR-USERID TO SQLIO-PARM-VAL(16)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 16
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-MAP-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2150-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-MAP-ROW.
       2150-EXIT.
           EXIT.

      *>   fin.usp_PostJournal owns the balance, period, account,
      *>   dimension and event mapping rules.  It raises its refusals
      *>   with THROW, which under SET XACT_ABORT ON also dooms this
      *>   connection's transaction - so a failure here is always a
      *>   rollback for the caller, never a partial document.
       2300-CALL-POST SECTION.
       2300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC fin.usp_PostJournal ?, ?, ?, ?, ?, ?, ?, "
                  "?, ?, ?, ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 14 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE AR-CORREL TO SQLIO-PARM-VAL(2)
           MOVE AR-USERID TO SQLIO-PARM-VAL(3)
           MOVE PG-DESCRIPTION TO SQLIO-PARM-VAL(4)
           MOVE PG-POSTING-DATE TO SQLIO-PARM-VAL(5)
           MOVE PG-JOURNAL-TYPE TO SQLIO-PARM-VAL(6)
           MOVE PG-DOC-DATE TO SQLIO-PARM-VAL(7)
           MOVE PG-CURRENCY TO SQLIO-PARM-VAL(8)
           MOVE PG-SOURCE-MODULE TO SQLIO-PARM-VAL(9)
           MOVE PG-SOURCE-REF TO SQLIO-PARM-VAL(10)
           MOVE PG-EVENT-CODE TO SQLIO-PARM-VAL(11)
           MOVE PG-SERIES-CODE TO SQLIO-PARM-VAL(12)
           IF PG-CURRENCY = "GBP" OR PG-FX-RATE = 0
               MOVE SPACES TO SQLIO-PARM-VAL(13)
               MOVE "Y" TO SQLIO-PARM-NULL(13)
           ELSE
               MOVE PG-FX-RATE TO AR-RATE-ED
               MOVE FUNCTION TRIM(AR-RATE-ED) TO SQLIO-PARM-VAL(13)
               MOVE "N" TO SQLIO-PARM-NULL(13)
           END-IF
           MOVE PG-EVIDENCE-REF TO SQLIO-PARM-VAL(14)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 12
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE "N" TO SQLIO-PARM-NULL(14)
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 4 TO SQLIO-COL-WIDTH(5)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-JNL-ROW
           IF SQLIO-ERROR
               PERFORM 9400-POST-FAILED
               GO TO 2300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 12 TO AR-RC
               MOVE "GLP0008" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The journal posting returned no result row"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 2300-EXIT
           END-IF
           COMPUTE PG-JOURNAL-ID = FUNCTION NUMVAL(WS-J-ID)
           MOVE WS-J-NUMBER TO PG-JOURNAL-NO
           IF WS-J-DUP(1:1) = "1"
               MOVE "Y" TO PG-WAS-DUPLICATE
               MOVE 4 TO AR-RC
               MOVE "GLP0009" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "That instruction was already posted as journal "
                      FUNCTION TRIM(PG-JOURNAL-NO)
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           ELSE
               MOVE SPACES TO AR-MSG
               STRING "Journal " FUNCTION TRIM(PG-JOURNAL-NO)
                      " posted"
                   DELIMITED BY SIZE INTO AR-MSG
           END-IF.
       2300-EXIT.
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
           MOVE "GLP0010" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "ARPOSTGL does not support function ["
                  FUNCTION TRIM(AR-FUNC) "]"
               DELIMITED BY SIZE INTO AR-MSG.
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

      *>   a refusal from fin.usp_PostJournal is a business message,
      *>   not a system failure - show it to the user as an error
       9400-POST-FAILED SECTION.
       9400-START.
           MOVE 8 TO AR-RC
           MOVE "GLP0011" TO AR-REASON
           MOVE SQLIO-ERRMSG TO AR-MSG
           ADD 1 TO AR-MSG-CNT.
       9400-EXIT.
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
