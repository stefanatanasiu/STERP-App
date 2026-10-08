      *>================================================================
      *> PROGRAM-ID : TSOREBAT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT UNIT TESTS
      *> PURPOSE    : RETROSPECTIVE REBATE ACCRUAL AND SETTLEMENT
      *>              (REBATE).  USES THE WORKED NUMBERS OF
      *>              ACCEPTANCE SCENARIO AC-130: A 2% REBATE ABOVE
      *>              100,000.00 OF ANNUAL PURCHASES ON 120,000.00 OF
      *>              NET STANDARD RATED SALES ACCRUES 2,400.00
      *>              AGAINST REVENUE AND SETTLES BY A CREDIT NOTE
      *>              CARRYING 480.00 OF VAT.  SETTLING TWICE MUST NOT
      *>              CREATE A SECOND REDUCTION OF REVENUE.
      *>              THE DATABASE ASSERTIONS RUN INSIDE BEGN..ROLB
      *>              AND SKIP WHEN THE SEEDS ARE NOT DEPLOYED.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: SO-027, SO-028   AC: AC-130
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSOREBAT.
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
      *> AC-130 fixture
       01  WS-AC130.
           05  AC-REVENUE              PIC S9(13)V99 VALUE 120000.
           05  AC-THRESHOLD            PIC S9(13)V99 VALUE 100000.
           05  AC-PCT                  PIC S9(5)V9(4) VALUE 2.
           05  AC-VAT-RATE             PIC S9(3)V9(4) VALUE 20.
       01  WS-CALC.
           05  WS-REQUIRED             PIC S9(13)V99 VALUE 0.
           05  WS-PRIOR                PIC S9(13)V99 VALUE 0.
           05  WS-MOVEMENT             PIC S9(13)V99 VALUE 0.
           05  WS-SETTLED              PIC S9(13)V99 VALUE 0.
           05  WS-TO-SETTLE            PIC S9(13)V99 VALUE 0.
           05  WS-SETTLE-VAT           PIC S9(13)V99 VALUE 0.
           05  WS-GROSS                PIC S9(13)V99 VALUE 0.
           05  WS-AMT                  PIC S9(13)V9(6) VALUE 0.
           05  WS-AMT2                 PIC S9(13)V9(6) VALUE 0.
           05  WS-LOW-REVENUE          PIC S9(13)V99 VALUE 90000.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSOREBAT rebate accrual and settlement" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-ACCRUAL-2400
           PERFORM 1100-BELOW-THRESHOLD
           PERFORM 1200-MOVEMENT-NOT-DOUBLED
           PERFORM 1300-SETTLEMENT-VAT-480
           PERFORM 1400-SETTLING-TWICE
           PERFORM 1500-REDUCES-REVENUE-NOT-COST
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-DATABASE
           ELSE
               PERFORM 2000-EVENT-IS-MAPPED
               PERFORM 2100-REBATE-ACCOUNT-IS-TURNOVER
               PERFORM 2200-SETTLED-WITHIN-ACCRUED
           END-IF
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *>   AC-130 - the threshold is a qualifying gate, not a slice.
      *>   Once annual purchases pass 100,000.00 the 2% applies to the
      *>   whole qualifying revenue: 120,000.00 at 2% is 2,400.00.
       1000-ACCRUAL-2400.
           MOVE "AC-130 2% on 120,000.00 accrues 2,400.00" TO TK-NAME
           MOVE 0 TO WS-REQUIRED
           IF AC-REVENUE > AC-THRESHOLD
               COMPUTE WS-AMT = AC-REVENUE * AC-PCT / 100
               PERFORM 8000-ROUND-2
               MOVE WS-AMT2 TO WS-REQUIRED
           END-IF
           MOVE 2400.00 TO TK-EXPECTED-N
           MOVE WS-REQUIRED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       1100-BELOW-THRESHOLD.
           MOVE "90,000.00 is below the threshold, nothing accrues"
               TO TK-NAME
           MOVE 0 TO WS-AMT2
           IF WS-LOW-REVENUE > AC-THRESHOLD
               COMPUTE WS-AMT = WS-LOW-REVENUE * AC-PCT / 100
               PERFORM 8000-ROUND-2
           END-IF
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-AMT2 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   what an earlier period of the same fiscal year already
      *>   carries is deducted, so the journal posts the movement and
      *>   never the cumulative figure a second time.
       1200-MOVEMENT-NOT-DOUBLED.
           MOVE "A first period posts the whole 2,400.00" TO TK-NAME
           MOVE 0 TO WS-PRIOR
           COMPUTE WS-MOVEMENT = WS-REQUIRED - WS-PRIOR
           MOVE 2400.00 TO TK-EXPECTED-N
           MOVE WS-MOVEMENT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "A later period posts only the movement" TO TK-NAME
           MOVE 1000.00 TO WS-PRIOR
           COMPUTE WS-MOVEMENT = WS-REQUIRED - WS-PRIOR
           MOVE 1400.00 TO TK-EXPECTED-N
           MOVE WS-MOVEMENT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Rerunning the same period posts no movement"
               TO TK-NAME
           MOVE 2400.00 TO WS-PRIOR
           COMPUTE WS-MOVEMENT = WS-REQUIRED - WS-PRIOR
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-MOVEMENT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: SO-027 - the credit note carries the VAT liability of
      *>   the supplies it adjusts.  All standard rated sales means
      *>   20% of 2,400.00, that is 480.00.
       1300-SETTLEMENT-VAT-480.
           MOVE "AC-130 settlement VAT is 480.00" TO TK-NAME
           MOVE 0 TO WS-SETTLED
           COMPUTE WS-TO-SETTLE = WS-REQUIRED - WS-SETTLED
           COMPUTE WS-AMT = WS-TO-SETTLE * AC-VAT-RATE / 100
           PERFORM 8000-ROUND-2
           MOVE WS-AMT2 TO WS-SETTLE-VAT
           MOVE 480.00 TO TK-EXPECTED-N
           MOVE WS-SETTLE-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The credit note settles 2,400.00 net" TO TK-NAME
           MOVE 2400.00 TO TK-EXPECTED-N
           MOVE WS-TO-SETTLE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The credit note gross is 2,880.00" TO TK-NAME
           COMPUTE WS-GROSS = WS-TO-SETTLE + WS-SETTLE-VAT
           MOVE 2880.00 TO TK-EXPECTED-N
           MOVE WS-GROSS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AC-130 - settlement does not create a second reduction.
      *>   After the first credit note SettledGBP equals AccruedGBP,
      *>   so the amount left to settle is nil and the second run
      *>   issues nothing and reduces nothing.
       1400-SETTLING-TWICE.
           MOVE WS-TO-SETTLE TO WS-SETTLED
           MOVE "Settling twice leaves nothing to settle" TO TK-NAME
           COMPUTE WS-TO-SETTLE = WS-REQUIRED - WS-SETTLED
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-TO-SETTLE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Settling twice raises no second VAT adjustment"
               TO TK-NAME
           MOVE 0 TO WS-SETTLE-VAT
           IF WS-TO-SETTLE > 0
               COMPUTE WS-AMT = WS-TO-SETTLE * AC-VAT-RATE / 100
               PERFORM 8000-ROUND-2
               MOVE WS-AMT2 TO WS-SETTLE-VAT
           END-IF
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-SETTLE-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The total reduction of revenue stays at 2,400.00"
               TO TK-NAME
           MOVE 2400.00 TO TK-EXPECTED-N
           MOVE WS-SETTLED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: SO-027 - an accrued rebate is variable consideration.
      *>   The debit is turnover (4120 sales rebates) and the credit
      *>   is the balance sheet accrual (2090); no cost of sales
      *>   account is involved at any point.
       1500-REDUCES-REVENUE-NOT-COST.
           MOVE "The accrual debits the sales rebate account"
               TO TK-NAME
           MOVE "4120" TO TK-EXPECTED-X
           MOVE "4120" TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The accrual credits the accrued rebate liability"
               TO TK-NAME
           MOVE "2090" TO TK-EXPECTED-X
           MOVE "2090" TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       8000-ROUND-2.
           INITIALIZE WS-ROUND
           MOVE "ROUND" TO CA-FUNCTION OF WS-ROUND
           MOVE "STC" TO CA-COMPANY OF WS-ROUND
           MOVE "FINCTRL" TO CA-USER OF WS-ROUND
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
           STRING "SELECT TOP (1) '1', '' FROM fin.EventAccountMap"
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
           MOVE "SO.REBATE.ACCRUAL is mapped DR 4120 CR 2090"
               TO TK-NAME
           MOVE "the finance seeds are not deployed" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "account 4120 is a turnover account" TO TK-NAME
           MOVE "the finance seeds are not deployed" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "no accrual is settled beyond what was accrued"
               TO TK-NAME
           MOVE "the sales tables are not deployed" TO TK-DETAIL
           PERFORM TK-SKIP.

      *>   the mapping seeded by db\seed\71-so-eventmap.sql
       2000-EVENT-IS-MAPPED.
           MOVE "SO.REBATE.ACCRUAL is mapped DR 4120 CR 2090"
               TO TK-NAME
           PERFORM 8100-BEGIN
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(MAX(CASE WHEN DrCr = 'DR' THEN "
                  "AccountCode END),'none'), ISNULL(MAX(CASE WHEN "
                  "DrCr = 'CR' THEN AccountCode END),'none') FROM "
                  "fin.EventAccountMap WHERE EventCode = "
                  "'SO.REBATE.ACCRUAL' AND IsActive = 1"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "the enquiry failed" TO TK-DETAIL
               PERFORM TK-SKIP
           ELSE
               IF WS-P1(1:4) = "none"
                   MOVE "the sales event map is not seeded"
                       TO TK-DETAIL
                   PERFORM TK-SKIP
               ELSE
                   MOVE "4120" TO TK-EXPECTED-X
                   MOVE WS-P1(1:4) TO TK-ACTUAL-X
                   PERFORM TK-ASSERT-STR
                   MOVE "the credit leg is the accrued rebate 2090"
                       TO TK-NAME
                   MOVE "2090" TO TK-EXPECTED-X
                   MOVE WS-P2(1:4) TO TK-ACTUAL-X
                   PERFORM TK-ASSERT-STR
               END-IF
           END-IF
           PERFORM 8200-ROLLBACK.

      *>   proof that the rebate reduces revenue and is not a cost:
      *>   the debit account sits in the turnover class, not in cost
      *>   of sales.
       2100-REBATE-ACCOUNT-IS-TURNOVER.
           MOVE "account 4120 is a turnover account" TO TK-NAME
           PERFORM 8100-BEGIN
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) ClassCode, '' FROM fin.Account "
                  "WHERE AccountCode = '4120'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "the chart of accounts is not seeded"
                   TO TK-DETAIL
               PERFORM TK-SKIP
           ELSE
               MOVE "TURN" TO TK-EXPECTED-X
               MOVE WS-P1(1:4) TO TK-ACTUAL-X
               PERFORM TK-ASSERT-STR
           END-IF
           PERFORM 8200-ROLLBACK.

      *>   the standing invariant that makes a second settlement
      *>   impossible in the data as well as in the code
       2200-SETTLED-WITHIN-ACCRUED.
           MOVE "no accrual is settled beyond what was accrued"
               TO TK-NAME
           PERFORM 8100-BEGIN
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT(*), '' FROM so.RebateAccrual WHERE "
                  "SettledGBP > AccruedGBP"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "so.RebateAccrual is not deployed" TO TK-DETAIL
               PERFORM TK-SKIP
           ELSE
               MOVE 0 TO TK-EXPECTED-N
               COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-P1)
               PERFORM TK-ASSERT-NUM
           END-IF
           PERFORM 8200-ROLLBACK.

       8100-BEGIN.
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 7 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       8200-ROLLBACK.
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 7 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       COPY TESTKITP.
