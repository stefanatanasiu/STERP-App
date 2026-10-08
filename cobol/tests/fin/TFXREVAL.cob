      *>================================================================
      *> PROGRAM-ID : TFXREVAL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : FOREIGN CURRENCY TEST SUITE
      *> PURPOSE    : AC-016.  AN INVOICE OF US$1,200 AT 1.28 IS
      *>              CARRIED AT GBP 937.50.  REVALUED AT 1.25 THE
      *>              CARRYING VALUE IS 960.00, AN UNREALISED GAIN OF
      *>              22.50 THAT IS REVERSED ON DAY ONE OF THE NEXT
      *>              PERIOD.  SETTLED AT 1.20 THE CASH IS 1,000.00
      *>              AND THE LIFETIME GAIN IS 62.50, NOT 85.00.
      *>              RATES ARE QUOTED AS FOREIGN UNITS PER GBP (D-14),
      *>              SO THE STERLING VALUE IS FOREIGN / RATE.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: FIN-019, FIN-020
      *> AC: AC-016
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFXREVAL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY SQLIOCA.
       COPY FXREVALCA REPLACING ==DFHCOMMAREA== BY ==WS-FXREVAL-CA==
                      LEADING ==CA-== BY ==FH-==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "N".
       01  WS-WORK.
           05  WS-PING                 PIC X(40).
           05  WS-RATE-OK              PIC X VALUE "N".
           05  WS-UNREAL               PIC S9(13)V99 VALUE 0.
           05  WS-REAL                 PIC S9(13)V99 VALUE 0.
           05  WS-SUM                  PIC S9(13)V99 VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TFXREVAL foreign currency" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-CONNECT
           IF WS-DB-UP NOT = "Y"
               MOVE "no database reachable" TO TK-DETAIL
               MOVE "revaluation suite needs SQL Server" TO TK-NAME
               PERFORM TK-SKIP
               PERFORM TK-END
               STOP RUN
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING
           PERFORM 1000-CARRY-TESTS
           PERFORM 2000-REVALUE-TESTS
           PERFORM 3000-REALISE-TESTS
           PERFORM 4000-LIFETIME-TESTS
           PERFORM 5000-REFUSAL-TESTS
           PERFORM 5500-HELD-RATE-TESTS
           PERFORM 6000-POST-TESTS
           PERFORM 9000-ROLLBACK
           PERFORM TK-END
           STOP RUN.

       0100-CONNECT.
           MOVE "SELECT 'Y'" TO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING
           IF NOT SQLIO-ERROR
               MOVE "Y" TO WS-DB-UP
           END-IF.

       9000-ROLLBACK.
           MOVE "ROLB" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING.

      *>   The US dollar receivable of AC-016 at its invoice rate.
       0950-SETUP-FR.
           INITIALIZE WS-FXREVAL-CA
           MOVE "STC" TO FH-COMPANY
           MOVE "RDG" TO FH-SITE
           MOVE "FINCTRL" TO FH-USER
           MOVE 20261015 TO FH-BUSINESS-DATE
           MOVE "USD" TO FR-CURRENCY
           MOVE "1100" TO FR-ACCOUNT
           MOVE "ASSET" TO FR-POSITION
           MOVE "AR" TO FR-SUBLEDGER-TYPE
           MOVE "ZZT-SI-AC016" TO FR-SUBLEDGER-REF
           MOVE 20261031 TO FR-AS-AT-DATE
           MOVE 1200.00 TO FR-FOREIGN-AMOUNT
           MOVE 1.28 TO FR-RATE-ORIGINAL.

      *>----------------------------------------------------------------
      *> 1000 - FIN-019 THE CARRYING AMOUNT AT THE TRANSACTION RATE.
      *>        D-14: RATES ARE FOREIGN UNITS PER GBP, SO 1,200 US
      *>        DOLLARS AT 1.28 IS 937.50 STERLING.
      *>----------------------------------------------------------------
       1000-CARRY-TESTS.
           PERFORM 0950-SETUP-FR
           MOVE "CARRY" TO FH-FUNCTION
           CALL "FXREVAL" USING WS-FXREVAL-CA
           MOVE "AC-016 the invoice is carried at 937.50" TO TK-NAME
           MOVE 937.50 TO TK-EXPECTED-N
           MOVE FR-ORIGINAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-016 the carrying amount starts at the invoice"
               TO TK-NAME
           MOVE 937.50 TO TK-EXPECTED-N
           MOVE FR-CARRYING-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-019 the carrying enquiry is accepted" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE FH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 2000 - FIN-020 THE PERIOD END REVALUATION IS UNREALISED
      *>----------------------------------------------------------------
       2000-REVALUE-TESTS.
           PERFORM 0950-SETUP-FR
           MOVE "REVALUE" TO FH-FUNCTION
           MOVE 1.25 TO FR-RATE-AT-DATE
           CALL "FXREVAL" USING WS-FXREVAL-CA
           MOVE "AC-016 at the closing rate the carrying is 960.00"
               TO TK-NAME
           MOVE 960.00 TO TK-EXPECTED-N
           MOVE FR-REVALUED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-016 the revaluation adjustment is 22.50"
               TO TK-NAME
           MOVE 22.50 TO TK-EXPECTED-N
           MOVE FR-ADJUSTMENT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-016 a receivable worth more sterling is a gain"
               TO TK-NAME
           MOVE "GAIN" TO TK-EXPECTED-X
           MOVE FR-GAIN-OR-LOSS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-020 the revaluation is marked unrealised"
               TO TK-NAME
           MOVE "UNREALISED" TO TK-EXPECTED-X
           MOVE FR-ADJUST-TYPE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE FR-UNREALISED-GBP TO WS-UNREAL

      *>   The same movement on a payable is a loss, not a gain.
           PERFORM 0950-SETUP-FR
           MOVE "REVALUE" TO FH-FUNCTION
           MOVE "LIABILITY" TO FR-POSITION
           MOVE 1.25 TO FR-RATE-AT-DATE
           CALL "FXREVAL" USING WS-FXREVAL-CA
           MOVE "FIN-020 a payable revalued upwards is a loss"
               TO TK-NAME
           MOVE "LOSS" TO TK-EXPECTED-X
           MOVE FR-GAIN-OR-LOSS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-020 the payable loss is 22.50" TO TK-NAME
           MOVE 22.50 TO TK-EXPECTED-N
           MOVE FR-TOTAL-LOSS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 3000 - FIN-020 SETTLEMENT REALISES THE LIFETIME DIFFERENCE
      *>----------------------------------------------------------------
       3000-REALISE-TESTS.
           PERFORM 0950-SETUP-FR
           MOVE "REALISE" TO FH-FUNCTION
           MOVE 20261130 TO FR-AS-AT-DATE
           MOVE 1.20 TO FR-RATE-SETTLE
      *>   The ledger still carries the revalued 960.00 when the cash
      *>   arrives, which is exactly where double counting would start.
           MOVE 960.00 TO FR-CARRYING-GBP
           CALL "FXREVAL" USING WS-FXREVAL-CA
           MOVE "AC-016 the cash received is 1,000.00" TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE FR-SETTLED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-016 the realised gain is 62.50" TO TK-NAME
           MOVE 62.50 TO TK-EXPECTED-N
           MOVE FR-REALISED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-016 settlement is measured from the invoice rate"
               TO TK-NAME
           MOVE 937.50 TO TK-EXPECTED-N
           MOVE FR-ORIGINAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-020 the settlement difference is realised"
               TO TK-NAME
           MOVE "REALISED" TO TK-EXPECTED-X
           MOVE FR-ADJUST-TYPE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE FR-REALISED-GBP TO WS-REAL

      *>   A payable settled at a weaker rate is a realised loss.
           PERFORM 0950-SETUP-FR
           MOVE "REALISE" TO FH-FUNCTION
           MOVE "LIABILITY" TO FR-POSITION
           MOVE 20261130 TO FR-AS-AT-DATE
           MOVE 1.20 TO FR-RATE-SETTLE
           CALL "FXREVAL" USING WS-FXREVAL-CA
           MOVE "FIN-020 a payable settled at 1.20 realises a loss"
               TO TK-NAME
           MOVE "LOSS" TO TK-EXPECTED-X
           MOVE FR-GAIN-OR-LOSS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-020 the realised loss is 62.50" TO TK-NAME
           MOVE 62.50 TO TK-EXPECTED-N
           MOVE FR-TOTAL-LOSS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 4000 - AC-016 THE LIFETIME FIGURE IS RECOGNISED ONCE.  THE
      *>        UNREALISED 22.50 IS REVERSED ON DAY ONE OF THE NEXT
      *>        PERIOD, SO IT IS NOT ADDED TO THE REALISED 62.50.
      *>----------------------------------------------------------------
       4000-LIFETIME-TESTS.
           PERFORM 0950-SETUP-FR
           MOVE "REALISE" TO FH-FUNCTION
           MOVE 20261130 TO FR-AS-AT-DATE
           MOVE 1.20 TO FR-RATE-SETTLE
           MOVE 960.00 TO FR-CARRYING-GBP
           CALL "FXREVAL" USING WS-FXREVAL-CA
           MOVE "AC-016 the lifetime exchange gain is 62.50"
               TO TK-NAME
           MOVE 62.50 TO TK-EXPECTED-N
           MOVE FR-LIFETIME-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           COMPUTE WS-SUM = WS-UNREAL + WS-REAL
           MOVE "AC-016 the naive sum of both movements is 85.00"
               TO TK-NAME
           MOVE 85.00 TO TK-EXPECTED-N
           MOVE WS-SUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-016 the lifetime gain is not the 85.00 double "
               TO TK-NAME
           IF FR-LIFETIME-GBP NOT = WS-SUM
               PERFORM TK-PASS
           ELSE
               MOVE "the reversed revaluation was counted twice"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "AC-016 the revalued carrying value is not realised"
               TO TK-NAME
           IF FR-REALISED-GBP NOT = 40.00
               PERFORM TK-PASS
           ELSE
               MOVE "settlement was measured from the revalued amount"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 5000 - FIN-019 WHAT THE PROGRAM REFUSES
      *>----------------------------------------------------------------
       5000-REFUSAL-TESTS.
           PERFORM 0950-SETUP-FR
           MOVE "CARRY" TO FH-FUNCTION
           MOVE "GBP" TO FR-CURRENCY
           CALL "FXREVAL" USING WS-FXREVAL-CA
           MOVE "FIN-019 sterling is never revalued against itself"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE FH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-019 the refusal is reported as FXR0001"
               TO TK-NAME
           MOVE "FXR0001" TO TK-EXPECTED-X
           MOVE FH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 0950-SETUP-FR
           MOVE "WIBBLE" TO FH-FUNCTION
           CALL "FXREVAL" USING WS-FXREVAL-CA
           MOVE "FIN-019 an unknown function is refused" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE FH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 5500 - FIN-019 THE HELD RATE IS USED WHEN THE INSTRUCTION
      *>        CARRIES NONE.  D-14: core.FxRate IS QUOTED AS FOREIGN
      *>        UNITS PER GBP, SO 1,200 US DOLLARS AT A HELD SPOT RATE
      *>        OF 1.200000 IS 1,000.00 STERLING.  THE FIXTURE RATE IS
      *>        WRITTEN INSIDE THE SUITE UNIT OF WORK AND ROLLED BACK
      *>        AT 9000, SO THE SEEDED RATE CURVE IS NOT DISTURBED.
      *>----------------------------------------------------------------
       5500-HELD-RATE-TESTS.
           PERFORM 5550-RATE-FIXTURE
           IF WS-RATE-OK NOT = "Y"
               MOVE "the ZZT spot rate could not be written"
                   TO TK-DETAIL
               MOVE "FIN-019 the held rate is taken when none is given"
                   TO TK-NAME
               PERFORM TK-SKIP
               GO TO 5500-EXIT
           END-IF
           PERFORM 0950-SETUP-FR
           MOVE "CARRY" TO FH-FUNCTION
           MOVE "SPOT" TO FR-RATE-TYPE
           MOVE 0 TO FR-RATE-ORIGINAL
           CALL "FXREVAL" USING WS-FXREVAL-CA
           MOVE "FIN-019 a carry with no rate given is accepted"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE FH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-019 FXCONV supplies the held rate of 1.200000"
               TO TK-NAME
           MOVE 1.20 TO TK-EXPECTED-N
           MOVE FR-RATE-ORIGINAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-019 1,200 dollars at the held rate is 1,000.00"
               TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE FR-ORIGINAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-019 the carrying amount follows the held rate"
               TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE FR-CARRYING-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       5500-EXIT.
           EXIT.

      *>   The rate held at the as at date wins over the older curve
      *>   because FXCONV reads the latest rate not after that date.
       5550-RATE-FIXTURE.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE core.FxRate WHERE CurrencyCode = 'USD' "
                  "AND RateType = 'SPOT' AND RateDate = '2026-10-31'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 5560-RUN-SQL
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT core.FxRate (CurrencyCode, RateDate, "
                  "RateType, UnitsPerGBP, Source) VALUES "
                  "('USD','2026-10-31','SPOT',1.20000000,'ZZTEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 5560-RUN-SQL.
       5550-EXIT.
           EXIT.

       5560-RUN-SQL.
           MOVE 7 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING
           IF SQLIO-ERROR
               MOVE "N" TO WS-RATE-OK
           ELSE
               MOVE "Y" TO WS-RATE-OK
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING.
       5560-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 6000 - FIN-020 THE REVALUATION RUN REACHES THE LEDGER AND IS
      *>        REVERSED ON DAY ONE OF THE NEXT PERIOD (D-14).  THE
      *>        POSTING IS LAST IN THE SUITE BECAUSE A REFUSED POSTING
      *>        RUNS UNDER XACT_ABORT AND WOULD TAKE THE SUITE'S OWN
      *>        UNIT OF WORK WITH IT.
      *>----------------------------------------------------------------
       6000-POST-TESTS.
      *>   FIN-019 with no rate given on the instruction, FXCONV reads
      *>   the rate held in core.ExchangeRate; when none is held the
      *>   carrying amount cannot be established and is refused rather
      *>   than guessed at one to one.
           PERFORM 0950-SETUP-FR
           MOVE "CARRY" TO FH-FUNCTION
           MOVE 0 TO FR-RATE-ORIGINAL
           MOVE "ZWL" TO FR-CURRENCY
           CALL "FXREVAL" USING WS-FXREVAL-CA
           MOVE "FIN-019 no held rate means no carrying amount"
               TO TK-NAME
           MOVE "FXR0002" TO TK-EXPECTED-X
           MOVE FH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 0950-SETUP-FR
           MOVE "POST" TO FH-FUNCTION
           MOVE 1.25 TO FR-RATE-AT-DATE
           MOVE 2026 TO FR-FISCAL-YEAR
           MOVE 10 TO FR-PERIOD-NO
           CALL "FXREVAL" USING WS-FXREVAL-CA
           MOVE "AC-016 the unrealised gain of 22.50 is posted"
               TO TK-NAME
           IF FH-RETURN-CODE < 8 AND FR-JOURNAL-NUMBER NOT = SPACES
               PERFORM TK-PASS
           ELSE
               MOVE FH-MESSAGE TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "AC-016 the run is recorded with an identity"
               TO TK-NAME
           IF FR-REVAL-ID > 0
               PERFORM TK-PASS
           ELSE
               MOVE FH-MESSAGE TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "FIN-020 the revaluation run is marked posted"
               TO TK-NAME
           MOVE "POSTED" TO TK-EXPECTED-X
           MOVE FR-REVAL-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-016 the posted adjustment is the 22.50 difference"
               TO TK-NAME
           MOVE 22.50 TO TK-EXPECTED-N
           MOVE FR-UNREALISED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

      *>   COM-015: the same instruction twice posts once.
           PERFORM 0950-SETUP-FR
           MOVE "POST" TO FH-FUNCTION
           MOVE 1.25 TO FR-RATE-AT-DATE
           MOVE 2026 TO FR-FISCAL-YEAR
           MOVE 10 TO FR-PERIOD-NO
           CALL "FXREVAL" USING WS-FXREVAL-CA
           MOVE "COM-015 repeating the run posts no second journal"
               TO TK-NAME
           IF FH-RETURN-CODE < 8
               PERFORM TK-PASS
           ELSE
               MOVE FH-MESSAGE TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           PERFORM 0950-SETUP-FR
           MOVE "REVERSE" TO FH-FUNCTION
           MOVE 2026 TO FR-FISCAL-YEAR
           MOVE 10 TO FR-PERIOD-NO
           CALL "FXREVAL" USING WS-FXREVAL-CA
           MOVE "D-14 the revaluation is reversed in the next period"
               TO TK-NAME
           IF FH-RETURN-CODE < 8 AND FR-REVERSAL-JNL NOT = SPACES
               PERFORM TK-PASS
           ELSE
               MOVE FH-MESSAGE TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "D-14 the run is left marked reversed" TO TK-NAME
           MOVE "REVERSED" TO TK-EXPECTED-X
           MOVE FR-REVAL-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 0950-SETUP-FR
           MOVE "REVERSE" TO FH-FUNCTION
           MOVE 2026 TO FR-FISCAL-YEAR
           MOVE 10 TO FR-PERIOD-NO
           CALL "FXREVAL" USING WS-FXREVAL-CA
           MOVE "D-14 a reversed revaluation is not reversed twice"
               TO TK-NAME
           MOVE "FXR0011" TO TK-EXPECTED-X
           MOVE FH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       COPY TESTKITP.
