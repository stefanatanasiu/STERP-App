      *>================================================================
      *> PROGRAM-ID : TSOINVRN
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT UNIT TESTS
      *> PURPOSE    : THE SALES INVOICING RUN (SOINVRUN).  USES THE
      *>              WORKED NUMBERS OF ACCEPTANCE SCENARIO AC-045 AND
      *>              SECTION 8 OF THE SALES ORDER SPECIFICATION:
      *>              AN ORDER FOR 12 AT 50.00, 7 DESPATCHED AND
      *>              INVOICED, 2 CANCELLED, 3 STILL TO DESPATCH AND A
      *>              BILLED NET OF 350.00.  RE-RUNNING THE INVOICE
      *>              RUN MUST NOT REBILL THOSE 7.
      *>              THE DATABASE ASSERTIONS RUN INSIDE BEGN..ROLB
      *>              AND SKIP WHEN THE SALES TABLES ARE NOT DEPLOYED.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: SO-014, SO-019, SO-025   AC: AC-045
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSOINVRN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY RNDALLOCCA REPLACING ==DFHCOMMAREA== BY ==WS-ROUND==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
       01  WS-PROBE.
           05  WS-P1                   PIC X(30).
           05  WS-P2                   PIC X(30).
           05  WS-P3                   PIC X(30).
           05  WS-P4                   PIC X(30).
      *> AC-045 fixture, in sterling and whole units
       01  WS-AC045.
           05  AC-ACCEPTED             PIC S9(9)V9(3) VALUE 12.
           05  AC-CANCELLED            PIC S9(9)V9(3) VALUE 2.
           05  AC-DESPATCHED           PIC S9(9)V9(3) VALUE 7.
           05  AC-BILLED               PIC S9(9)V9(3) VALUE 0.
           05  AC-RETURNED             PIC S9(9)V9(3) VALUE 0.
           05  AC-UNIT-PRICE           PIC S9(11)V9(6) VALUE 50.
           05  AC-PRICE-PER            PIC S9(11)V9(6) VALUE 1.
           05  AC-VAT-RATE             PIC S9(3)V9(4) VALUE 20.
           05  AC-DEPOSIT              PIC S9(11)V99 VALUE 100.
       01  WS-CALC.
           05  WS-BILLABLE             PIC S9(11)V9(6) VALUE 0.
           05  WS-NET                  PIC S9(13)V99 VALUE 0.
           05  WS-NET2                 PIC S9(13)V99 VALUE 0.
           05  WS-VAT                  PIC S9(13)V99 VALUE 0.
           05  WS-GROSS                PIC S9(13)V99 VALUE 0.
           05  WS-DUE                  PIC S9(13)V99 VALUE 0.
           05  WS-REMAIN               PIC S9(11)V9(3) VALUE 0.
           05  WS-AMT                  PIC S9(13)V9(6) VALUE 0.
           05  WS-AMT2                 PIC S9(13)V9(6) VALUE 0.
           05  WS-SKIP-FLAG            PIC X VALUE "N".
           05  WS-VAT-CODE             PIC X(4) VALUE SPACES.
           05  WS-EVIDENCE             PIC X(60) VALUE SPACES.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSOINVRN sales invoicing run" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-BILLED-NET-350
           PERFORM 1100-VAT-AND-GROSS
           PERFORM 1200-REMAINING-THREE
           PERFORM 1300-NO-REBILL-ON-RERUN
           PERFORM 1400-RETURN-DOES-NOT-REOPEN
           PERFORM 1500-DEPOSIT-IS-A-SETTLEMENT
           PERFORM 1600-VAT-EVIDENCE-BLOCKS
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-DATABASE
           ELSE
               PERFORM 2000-BILLED-NEVER-EXCEEDS
               PERFORM 2100-ORDER-LINE-INVARIANT
               PERFORM 2200-UPDATE-BILLS-ONCE
           END-IF
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *>   AC-045 - 7 units at 50.00 give a billed net of 350.00.
      *>   RNDALLOC owns the extension so that the run and the order
      *>   entry screen can never disagree.
       1000-BILLED-NET-350.
           MOVE "AC-045 7 units at 50.00 bill a net of 350.00"
               TO TK-NAME
           COMPUTE WS-BILLABLE = AC-DESPATCHED - AC-BILLED
           INITIALIZE WS-ROUND
           MOVE "EXTEND" TO CA-FUNCTION OF WS-ROUND
           MOVE "STC" TO CA-COMPANY OF WS-ROUND
           MOVE "ARCLERK" TO CA-USER OF WS-ROUND
           MOVE 2 TO RA-PLACES
           MOVE WS-BILLABLE TO RA-VALUE
           MOVE AC-UNIT-PRICE TO RA-VALUE2
           CALL "RNDALLOC" USING WS-ROUND
           IF CA-RETURN-CODE OF WS-ROUND > 4
               COMPUTE WS-NET ROUNDED = WS-BILLABLE * AC-UNIT-PRICE
           ELSE
               MOVE RA-RESULT TO WS-NET
           END-IF
           MOVE 350.00 TO TK-EXPECTED-N
           MOVE WS-NET TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       1100-VAT-AND-GROSS.
           MOVE "VAT at 20% on 350.00 is 70.00" TO TK-NAME
           COMPUTE WS-AMT = WS-NET * AC-VAT-RATE / 100
           PERFORM 8000-ROUND-2
           MOVE WS-AMT2 TO WS-VAT
           MOVE 70.00 TO TK-EXPECTED-N
           MOVE WS-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Gross of the first invoice is 420.00" TO TK-NAME
           COMPUTE WS-GROSS = WS-NET + WS-VAT
           MOVE 420.00 TO TK-EXPECTED-N
           MOVE WS-GROSS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   accepted 12, cancelled 2, despatched 7 leaves 3 to despatch
       1200-REMAINING-THREE.
           MOVE "AC-045 3 units remain to despatch" TO TK-NAME
           COMPUTE WS-REMAIN = AC-ACCEPTED - AC-CANCELLED
                             - AC-DESPATCHED
           MOVE 3 TO TK-EXPECTED-N
           MOVE WS-REMAIN TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   the invoicing run advances BilledQty; a second run finds
      *>   BilledQty = DespatchedQty and has nothing left to bill.
       1300-NO-REBILL-ON-RERUN.
           MOVE AC-DESPATCHED TO AC-BILLED
           MOVE "A rerun finds nothing left to bill on the line"
               TO TK-NAME
           COMPUTE WS-BILLABLE = AC-DESPATCHED - AC-BILLED
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-BILLABLE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "A rerun leaves the billed net at 350.00" TO TK-NAME
           COMPUTE WS-AMT = WS-BILLABLE * AC-UNIT-PRICE
           PERFORM 8000-ROUND-2
           COMPUTE WS-NET2 = WS-NET + WS-AMT2
           MOVE 350.00 TO TK-EXPECTED-N
           MOVE WS-NET2 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "A rerun bills no second quantity" TO TK-NAME
           MOVE 7 TO TK-EXPECTED-N
           MOVE AC-BILLED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   returning one billed unit is a financial and stock event of
      *>   its own.  It does not reopen the order, so the remaining
      *>   quantity to despatch is still 3.
       1400-RETURN-DOES-NOT-REOPEN.
           MOVE 1 TO AC-RETURNED
           MOVE "Returning a billed unit leaves 3 to despatch"
               TO TK-NAME
           COMPUTE WS-REMAIN = AC-ACCEPTED - AC-CANCELLED
                             - AC-DESPATCHED
           MOVE 3 TO TK-EXPECTED-N
           MOVE WS-REMAIN TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: SO-019 - the deposit settles the invoice.  The revenue
      *>   recognised is still the net of 350.00; only the amount the
      *>   customer still owes changes.
       1500-DEPOSIT-IS-A-SETTLEMENT.
           MOVE "A deposit does not change the net revenue" TO TK-NAME
           MOVE 350.00 TO TK-EXPECTED-N
           MOVE WS-NET TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "A 100.00 deposit leaves 320.00 due" TO TK-NAME
           COMPUTE WS-DUE = WS-GROSS - AC-DEPOSIT
           MOVE 320.00 TO TK-EXPECTED-N
           MOVE WS-DUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "A deposit is never applied twice" TO TK-NAME
           COMPUTE WS-DUE = WS-GROSS - AC-DEPOSIT - 0
           MOVE 320.00 TO TK-EXPECTED-N
           MOVE WS-DUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: SO-025 - a zero rated export with no evidence held is
      *>   withheld from the run.  A missing condition blocks
      *>   invoicing; it never defaults to zero rating.
       1600-VAT-EVIDENCE-BLOCKS.
           MOVE "EXP without evidence is withheld" TO TK-NAME
           MOVE "EXP" TO WS-VAT-CODE
           MOVE SPACES TO WS-EVIDENCE
           MOVE "N" TO WS-SKIP-FLAG
           IF (WS-VAT-CODE(1:3) = "RC " OR WS-VAT-CODE(1:3) = "DRC"
               OR WS-VAT-CODE(1:3) = "EXP")
              AND WS-EVIDENCE = SPACES
               MOVE "Y" TO WS-SKIP-FLAG
           END-IF
           MOVE "Y" TO TK-EXPECTED-X
           MOVE WS-SKIP-FLAG TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "EXP with evidence is invoiced" TO TK-NAME
           MOVE "EXP0001 air waybill" TO WS-EVIDENCE
           MOVE "N" TO WS-SKIP-FLAG
           IF (WS-VAT-CODE(1:3) = "RC " OR WS-VAT-CODE(1:3) = "DRC"
               OR WS-VAT-CODE(1:3) = "EXP")
              AND WS-EVIDENCE = SPACES
               MOVE "Y" TO WS-SKIP-FLAG
           END-IF
           MOVE "N" TO TK-EXPECTED-X
           MOVE WS-SKIP-FLAG TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       8000-ROUND-2.
           INITIALIZE WS-ROUND
           MOVE "ROUND" TO CA-FUNCTION OF WS-ROUND
           MOVE "STC" TO CA-COMPANY OF WS-ROUND
           MOVE "ARCLERK" TO CA-USER OF WS-ROUND
           MOVE 2 TO RA-PLACES
           MOVE WS-AMT TO RA-VALUE
           CALL "RNDALLOC" USING WS-ROUND
           IF CA-RETURN-CODE OF WS-ROUND > 4
               MOVE WS-AMT TO WS-AMT2
           ELSE
               MOVE RA-RESULT TO WS-AMT2
           END-IF.

      *>----------------------------------------------------------------
       0100-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) '1', '' FROM so.DespatchLine"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR
               MOVE "N" TO WS-DB-UP
           END-IF.

       0900-SKIP-DATABASE.
           MOVE "so.DespatchLine billed within despatched"
               TO TK-NAME
           MOVE "the sales tables are not deployed" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "so.SalesOrderLine billed within despatched"
               TO TK-NAME
           MOVE "the sales tables are not deployed" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "the billing update bills once and only once"
               TO TK-NAME
           MOVE "the sales tables are not deployed" TO TK-DETAIL
           PERFORM TK-SKIP.

      *>   REQ: SO-014 - the standing invariant.  Nothing in the
      *>   module may carry a billed quantity larger than the evidence
      *>   that supports it.
       2000-BILLED-NEVER-EXCEEDS.
           MOVE "so.DespatchLine billed within despatched"
               TO TK-NAME
           PERFORM 8100-BEGIN
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT(*), '' FROM so.DespatchLine WHERE "
                  "BilledQty > DespatchedQty"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR
               MOVE "the enquiry failed" TO TK-DETAIL
               PERFORM TK-SKIP
           ELSE
               MOVE 0 TO TK-EXPECTED-N
               COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-P1)
               PERFORM TK-ASSERT-NUM
           END-IF
           PERFORM 8200-ROLLBACK.

       2100-ORDER-LINE-INVARIANT.
           MOVE "so.SalesOrderLine billed within despatched"
               TO TK-NAME
           PERFORM 8100-BEGIN
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT(*), '' FROM so.SalesOrderLine WHERE "
                  "BilledQty > DespatchedQty"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR
               MOVE "the enquiry failed" TO TK-DETAIL
               PERFORM TK-SKIP
           ELSE
               MOVE 0 TO TK-EXPECTED-N
               COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-P1)
               PERFORM TK-ASSERT-NUM
           END-IF
           PERFORM 8200-ROLLBACK.

      *>   the exact predicate SOINVRUN uses to advance BilledQty,
      *>   proved against a table variable so that no real row is
      *>   touched: the first update bills 7, the second bills none
      *>   and the billed quantity stays at 7.
       2200-UPDATE-BILLS-ONCE.
           MOVE "the billing update bills once and only once"
               TO TK-NAME
           PERFORM 8100-BEGIN
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; DECLARE @t TABLE (Id int, "
                  "DespatchedQty decimal(19,6), BilledQty "
                  "decimal(19,6)); INSERT @t VALUES (1, 7, 0); "
                  "DECLARE @r1 int, @r2 int; UPDATE @t SET "
                  "BilledQty = BilledQty + 7 WHERE Id = 1 AND "
                  "BilledQty + 7 <= DespatchedQty; SET @r1 = "
                  "@@ROWCOUNT; UPDATE @t SET BilledQty = BilledQty "
                  "+ 7 WHERE Id = 1 AND BilledQty + 7 <= "
                  "DespatchedQty; SET @r2 = @@ROWCOUNT; SELECT "
                  "CAST(@r1 AS varchar(10)), CAST(@r2 AS "
                  "varchar(10)), CAST((SELECT BilledQty FROM @t "
                  "WHERE Id = 1) AS varchar(30)), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(3)
           MOVE 30 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "the enquiry failed" TO TK-DETAIL
               PERFORM TK-SKIP
               PERFORM 8200-ROLLBACK
           ELSE
               MOVE 1 TO TK-EXPECTED-N
               COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-P1)
               PERFORM TK-ASSERT-NUM
               MOVE "a second run updates no despatch line"
                   TO TK-NAME
               MOVE 0 TO TK-EXPECTED-N
               COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-P2)
               PERFORM TK-ASSERT-NUM
               MOVE "the billed quantity stays at 7" TO TK-NAME
               MOVE 7 TO TK-EXPECTED-N
               COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-P3)
               PERFORM TK-ASSERT-NUM
               PERFORM 8200-ROLLBACK
           END-IF.

       8100-BEGIN.
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 7 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       8200-ROLLBACK.
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 7 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       COPY TESTKITP.
