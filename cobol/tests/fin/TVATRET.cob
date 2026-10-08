      *>================================================================
      *> PROGRAM-ID : TVATRET
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : VALUE ADDED TAX TEST SUITE
      *> PURPOSE    : VAT DETERMINATION AND THE NINE BOX RETURN.
      *>              PER LINE ROUNDING (AC-015), THE SPECIAL
      *>              MECHANISMS, THE TAX POINT DRIVEN RETURN PERIOD
      *>              AND THE BOX IDENTITIES OF FIN-025:
      *>                  BOX 3 = BOX 1 + BOX 2
      *>                  BOX 5 = BOX 3 - BOX 4
      *>              EVERY WRITE RUNS INSIDE ONE BEGN ... ROLB SO THE
      *>              SUITE IS REPEATABLE AND LEAVES NOTHING BEHIND.
      *>              THE FIXTURE PERIODS LIVE IN 2033 SO THAT THEY
      *>              CANNOT COLLIDE WITH SEEDED DEMONSTRATION DATA.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: FIN-016, FIN-017, FIN-018, FIN-025, FIN-026, FIN-027
      *> REQ: FIN-028, FIN-029
      *> AC: AC-015, AC-018
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TVATRET.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY SQLIOCA.
       COPY VATDETCA REPLACING ==DFHCOMMAREA== BY ==WS-VATDET-CA==
                      LEADING ==CA-== BY ==VH-==.
       COPY VATRET9CA REPLACING ==DFHCOMMAREA== BY ==WS-VATRET9-CA==
                      LEADING ==CA-== BY ==RH-==.
       COPY VATERRCCA REPLACING ==DFHCOMMAREA== BY ==WS-VATERRC-CA==
                      LEADING ==CA-== BY ==EH-==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "N".
           05  WS-FIXTURE-OK           PIC X VALUE "Y".
       01  WS-WORK.
           05  WS-PING                 PIC X(40).
           05  WS-ANSWER               PIC X(40).
           05  WS-AMT                  PIC S9(13)V99 VALUE 0.
           05  WS-BOX5                 PIC S9(13)V99 VALUE 0.
           05  WS-DRILL-SUM            PIC S9(13)V99 VALUE 0.
           05  WS-SAVE-B1              PIC S9(13)V99 VALUE 0.
           05  WS-SAVE-B4              PIC S9(13)V99 VALUE 0.
           05  WS-ERROR-ID             PIC S9(9) COMP-5 VALUE 0.
           05  WS-IX                   PIC S9(4) COMP-5 VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TVATRET value added tax" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-CONNECT
           IF WS-DB-UP NOT = "Y"
               MOVE "no database reachable" TO TK-DETAIL
               MOVE "VAT suite needs SQL Server" TO TK-NAME
               PERFORM TK-SKIP
               PERFORM TK-END
               STOP RUN
           END-IF
           PERFORM 0200-BEGIN-WORK
           PERFORM 1000-RATE-TESTS
           PERFORM 2000-ROUNDING-TESTS
           PERFORM 3000-SELF-ACCOUNT-TESTS
           PERFORM 4000-PERIOD-TESTS
           PERFORM 5000-RETURN-TESTS
           PERFORM 6000-CORRECTION-TESTS
           PERFORM 7000-DRILL-TESTS
           PERFORM 7500-FINALISE-TESTS
           PERFORM 8000-ERROR-CORRECTION-DB-TESTS
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

      *>   One transaction owns every fixture row written below.  The
      *>   programs under test never commit a unit of work they did
      *>   not open, so the rollback at 9000 undoes all of it.
       0200-BEGIN-WORK.
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING
           PERFORM 0300-FIXTURES.

       9000-ROLLBACK.
           MOVE "ROLB" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING.

       0300-FIXTURES.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.VatPeriod (CompanyCode, VatPeriodKey, "
                  "VatRegNo, StartDate, EndDate, FilingDueDate, "
                  "PaymentDueDate, Frequency, Status) VALUES "
                  "('STC','ZZTVQ1','GB100100100','2033-07-01',"
                  "'2033-09-30','2033-11-07','2033-11-07',"
                  "'QUARTERLY','FINALISED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.VatPeriod (CompanyCode, VatPeriodKey, "
                  "VatRegNo, StartDate, EndDate, FilingDueDate, "
                  "PaymentDueDate, Frequency, Status) VALUES "
                  "('STC','ZZTVQ2','GB100100100','2033-10-01',"
                  "'2033-12-31','2034-02-07','2034-02-07',"
                  "'QUARTERLY','OPEN')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
      *>   ZZTVQ3 is the open quarter a next return correction is
      *>   carried into (FIN-027).
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.VatPeriod (CompanyCode, VatPeriodKey, "
                  "VatRegNo, StartDate, EndDate, FilingDueDate, "
                  "PaymentDueDate, Frequency, Status) VALUES "
                  "('STC','ZZTVQ3','GB100100100','2034-01-01',"
                  "'2034-03-31','2034-05-07','2034-05-07',"
                  "'QUARTERLY','OPEN')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
      *>   Fixture F1 of AC-018: revenue 80.00 carrying 16.00 output
      *>   VAT, purchases 120.00 carrying 24.00 recoverable input VAT.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.VatTransaction (CompanyCode, "
                  "SourceModule, DocumentType, DocumentRef, "
                  "TaxPointDate, PostingDate, VatPeriodKey, VatCode, "
                  "RatePct, Mechanism, Direction, NetAmount, "
                  "VatAmount, NetAmountGBP, VatAmountGBP, "
                  "RecoverableVatGBP, VatBox, NetBox) VALUES "
                  "('STC','AR','SALES INVOICE','ZZTV-SI-001',"
                  "'2033-11-15','2033-11-15','ZZTVQ2','S',20.0000,"
                  "'NORMAL','OUTPUT',80.00,16.00,80.00,16.00,0,1,6)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.VatTransaction (CompanyCode, "
                  "SourceModule, DocumentType, DocumentRef, "
                  "TaxPointDate, PostingDate, VatPeriodKey, VatCode, "
                  "RatePct, Mechanism, Direction, NetAmount, "
                  "VatAmount, NetAmountGBP, VatAmountGBP, "
                  "RecoverableVatGBP, VatBox, NetBox) VALUES "
                  "('STC','AP','PURCHASE INVOICE','ZZTV-PI-001',"
                  "'2033-11-20','2033-11-20','ZZTVQ2','S',20.0000,"
                  "'NORMAL','INPUT',120.00,24.00,120.00,24.00,"
                  "24.00,4,7)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL.

       0900-RUN-SQL.
           MOVE 8 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING
           IF SQLIO-ERROR
               MOVE "N" TO WS-FIXTURE-OK
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING.

       0920-READ-ONE.
           MOVE 7 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ANSWER
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ANSWER
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "?" TO WS-ANSWER
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ANSWER.

       0950-SETUP-VD.
           INITIALIZE WS-VATDET-CA
           MOVE "STC" TO VH-COMPANY
           MOVE "RDG" TO VH-SITE
           MOVE "VATOWNER" TO VH-USER
           MOVE 20261015 TO VH-BUSINESS-DATE
           MOVE 20331115 TO VD-TAX-POINT
           MOVE "GBP" TO VD-CURRENCY.

      *>----------------------------------------------------------------
      *> 1000 - FIN-016 THE RATE IN FORCE AT THE TAX POINT
      *>----------------------------------------------------------------
       1000-RATE-TESTS.
           PERFORM 0950-SETUP-VD
           MOVE "RATE" TO VH-FUNCTION
           MOVE "S" TO VD-VAT-CODE
           CALL "VATDET" USING WS-VATDET-CA
           MOVE "AC-015 the standard rate is 20 per cent" TO TK-NAME
           MOVE 20 TO TK-EXPECTED-N
           MOVE VD-RATE-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-VD
           MOVE "RATE" TO VH-FUNCTION
           MOVE "R" TO VD-VAT-CODE
           CALL "VATDET" USING WS-VATDET-CA
           MOVE "AC-015 the reduced rate is 5 per cent" TO TK-NAME
           MOVE 5 TO TK-EXPECTED-N
           MOVE VD-RATE-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

      *>   A rate reaches a document by its tax point and never
      *>   reaches back before the day it came into force.
           PERFORM 0950-SETUP-VD
           MOVE "RATE" TO VH-FUNCTION
           MOVE "DRC" TO VD-VAT-CODE
           CALL "VATDET" USING WS-VATDET-CA
           MOVE "AC-015 a new rate is dated from its own start"
               TO TK-NAME
           MOVE 20210301 TO TK-EXPECTED-N
           MOVE VD-RATE-FROM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-VD
           MOVE "RATE" TO VH-FUNCTION
           MOVE "DRC" TO VD-VAT-CODE
           MOVE 20200101 TO VD-TAX-POINT
           CALL "VATDET" USING WS-VATDET-CA
           MOVE "AC-015 a tax point before the rate has no rate"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE VH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-VD
           MOVE "RATE" TO VH-FUNCTION
           MOVE "ZZ9" TO VD-VAT-CODE
           CALL "VATDET" USING WS-VATDET-CA
           MOVE "FIN-016 an unknown VAT code is not found" TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE VH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 2000 - AC-015 EACH LINE IS ROUNDED ON ITS OWN
      *>        TWO NET LINES OF 0.03 AT 20 PER CENT CARRY 0.6 OF A
      *>        PENNY EACH.  ROUNDED PER LINE THAT IS 0.01 EACH AND
      *>        THE STANDARD RATED VAT OF THE DOCUMENT IS 0.02, NOT
      *>        THE 0.01 A SINGLE DOCUMENT LEVEL ROUNDING WOULD GIVE.
      *>----------------------------------------------------------------
       2000-ROUNDING-TESTS.
           PERFORM 0950-SETUP-VD
           MOVE "LINE" TO VH-FUNCTION
           MOVE "S" TO VD-VAT-CODE
           MOVE 0.03 TO VD-NET-AMOUNT
           CALL "VATDET" USING WS-VATDET-CA
           MOVE "AC-015 0.6 of a penny on one line rounds to 0.01"
               TO TK-NAME
           MOVE 0.01 TO TK-EXPECTED-N
           MOVE VD-VAT-AMOUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-VD
           MOVE "DOC" TO VH-FUNCTION
           MOVE 3 TO VD-LINE-CNT
           MOVE "S" TO VD-L-VAT-CODE(1)
           MOVE 0.03 TO VD-L-NET(1)
           MOVE "S" TO VD-L-VAT-CODE(2)
           MOVE 0.03 TO VD-L-NET(2)
           MOVE "R" TO VD-L-VAT-CODE(3)
           MOVE 10.00 TO VD-L-NET(3)
           CALL "VATDET" USING WS-VATDET-CA
           MOVE "AC-015 the document totals its lines" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE VH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-015 the first standard line carries 0.01"
               TO TK-NAME
           MOVE 0.01 TO TK-EXPECTED-N
           MOVE VD-L-VAT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-015 the second standard line carries 0.01"
               TO TK-NAME
           MOVE 0.01 TO TK-EXPECTED-N
           MOVE VD-L-VAT(2) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           COMPUTE WS-AMT = VD-L-VAT(1) + VD-L-VAT(2)
           MOVE "AC-015 standard rated VAT totals 0.02 and not 0.01"
               TO TK-NAME
           MOVE 0.02 TO TK-EXPECTED-N
           MOVE WS-AMT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-015 the reduced rated line of 10.00 carries 0.50"
               TO TK-NAME
           MOVE 0.50 TO TK-EXPECTED-N
           MOVE VD-L-VAT(3) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-015 the invoice net is 10.06" TO TK-NAME
           MOVE 10.06 TO TK-EXPECTED-N
           MOVE VD-TOTAL-NET TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-015 the invoice VAT is 0.52" TO TK-NAME
           MOVE 0.52 TO TK-EXPECTED-N
           MOVE VD-TOTAL-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-015 the invoice gross is 10.58" TO TK-NAME
           MOVE 10.58 TO TK-EXPECTED-N
           MOVE VD-TOTAL-GROSS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

      *>   FIN-017 a prompt payment discount reduces the consideration
      *>   actually received, so the VAT and the gross both follow the
      *>   discounted net and not the headline price.
           PERFORM 0950-SETUP-VD
           MOVE "LINE" TO VH-FUNCTION
           MOVE "S" TO VD-VAT-CODE
           MOVE "OUTPUT" TO VD-DIRECTION
           MOVE 100.00 TO VD-NET-AMOUNT
           MOVE 5 TO VD-DISCOUNT-PCT
           CALL "VATDET" USING WS-VATDET-CA
           MOVE "FIN-017 VAT is due on the discounted net of 95.00"
               TO TK-NAME
           MOVE 19.00 TO TK-EXPECTED-N
           MOVE VD-VAT-AMOUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-017 the gross is the discounted net plus its VAT"
               TO TK-NAME
           MOVE 114.00 TO TK-EXPECTED-N
           MOVE VD-GROSS-AMOUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-VD
           MOVE "DOC" TO VH-FUNCTION
           MOVE 0 TO VD-LINE-CNT
           CALL "VATDET" USING WS-VATDET-CA
           MOVE "FIN-017 a document with no lines is refused"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE VH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 3000 - FIN-028 REVERSE CHARGE AND POSTPONED IMPORT VAT
      *>        THE SAME VALUE IS DECLARED AS OUTPUT TAX AND CLAIMED
      *>        AS INPUT TAX, AND NO VAT IS PAID TO THE SUPPLIER.
      *>----------------------------------------------------------------
       3000-SELF-ACCOUNT-TESTS.
           PERFORM 0950-SETUP-VD
           MOVE "SELFACC" TO VH-FUNCTION
           MOVE "RC" TO VD-VAT-CODE
           MOVE 1000.00 TO VD-NET-AMOUNT
           CALL "VATDET" USING WS-VATDET-CA
           MOVE "FIN-028 services from abroad reverse charge"
               TO TK-NAME
           MOVE "RCSERVICE" TO TK-EXPECTED-X
           MOVE VD-MECHANISM TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-028 the reverse charge declares 200.00 output VAT"
               TO TK-NAME
           MOVE 200.00 TO TK-EXPECTED-N
           MOVE VD-SELF-OUTPUT-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-028 the same 200.00 is claimed as input VAT"
               TO TK-NAME
           MOVE 200.00 TO TK-EXPECTED-N
           MOVE VD-SELF-INPUT-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-025 the output leg belongs in box 1" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE VD-OUT-VAT-BOX TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-025 the input leg belongs in box 4" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE VD-IN-VAT-BOX TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-028 self accounting is reported to the caller"
               TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE VH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-VD
           MOVE "SELFACC" TO VH-FUNCTION
           MOVE "PIV" TO VD-VAT-CODE
           MOVE 500.00 TO VD-NET-AMOUNT
           CALL "VATDET" USING WS-VATDET-CA
           MOVE "FIN-029 imported goods use postponed VAT accounting"
               TO TK-NAME
           MOVE "PVA" TO TK-EXPECTED-X
           MOVE VD-MECHANISM TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-029 postponed import VAT declares 100.00"
               TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE VD-SELF-OUTPUT-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-029 postponed import VAT reclaims 100.00"
               TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE VD-SELF-INPUT-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-029 the imported net is declared in box 7"
               TO TK-NAME
           MOVE 7 TO TK-EXPECTED-N
           MOVE VD-IN-NET-BOX TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-029 an import adds nothing to the sales net box"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE VD-OUT-NET-BOX TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-VD
           MOVE "SELFACC" TO VH-FUNCTION
           MOVE "S" TO VD-VAT-CODE
           MOVE 1000.00 TO VD-NET-AMOUNT
           CALL "VATDET" USING WS-VATDET-CA
           MOVE "FIN-028 an ordinary standard rated line self acco"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE VD-SELF-OUTPUT-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 4000 - FIN-018 THE TAX POINT DECIDES THE RETURN PERIOD.  AN
      *>        ITEM DATED IN A FINALISED PERIOD IS REPORTED IN THE
      *>        NEXT OPEN RETURN AS A LATE ENTRY (AC-015).
      *>----------------------------------------------------------------
       4000-PERIOD-TESTS.
           IF WS-FIXTURE-OK NOT = "Y"
               MOVE "VAT period fixtures were not written" TO TK-DETAIL
               MOVE "FIN-018 VAT return period tests" TO TK-NAME
               PERFORM TK-SKIP
               GO TO 4000-EXIT
           END-IF
           PERFORM 0950-SETUP-VD
           MOVE "PERIOD" TO VH-FUNCTION
           MOVE "S" TO VD-VAT-CODE
           CALL "VATDET" USING WS-VATDET-CA
           MOVE "FIN-018 the tax point selects the open return"
               TO TK-NAME
           MOVE "ZZTVQ2" TO TK-EXPECTED-X
           MOVE VD-VAT-PERIOD-KEY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-018 an item in the open period is not late"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE VD-IS-LATE-ENTRY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 0950-SETUP-VD
           MOVE "PERIOD" TO VH-FUNCTION
           MOVE "S" TO VD-VAT-CODE
           MOVE 20330815 TO VD-TAX-POINT
           CALL "VATDET" USING WS-VATDET-CA
           MOVE "AC-015 an item dated in a finalised period is late"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE VD-IS-LATE-ENTRY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-015 the late item moves to the next open return"
               TO TK-NAME
           MOVE "ZZTVQ2" TO TK-EXPECTED-X
           MOVE VD-VAT-PERIOD-KEY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-015 the late entry is reported, not refused"
               TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE VH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000 - FIN-025 THE NINE BOX RETURN OVER FIXTURE F1 OF AC-018:
      *>        REVENUE 80.00 WITH 16.00 OUTPUT VAT AND PURCHASES OF
      *>        120.00 WITH 24.00 INPUT VAT, SO 8.00 IS RECLAIMABLE.
      *>----------------------------------------------------------------
       5000-RETURN-TESTS.
           IF WS-FIXTURE-OK NOT = "Y"
               MOVE "VAT fixtures were not written" TO TK-DETAIL
               MOVE "AC-018 nine box return tests" TO TK-NAME
               PERFORM TK-SKIP
               GO TO 5000-EXIT
           END-IF
           PERFORM 5100-SETUP-VR
           MOVE "PREPARE" TO RH-FUNCTION
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE "AC-018 the return prepares for the open period"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE RH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-018 box 1 output VAT is 16.00" TO TK-NAME
           MOVE 16.00 TO TK-EXPECTED-N
           MOVE VR-BOX1 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-018 box 2 acquisitions are nil" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE VR-BOX2 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-018 box 4 input VAT is 24.00" TO TK-NAME
           MOVE 24.00 TO TK-EXPECTED-N
           MOVE VR-BOX4 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-018 box 6 is 80 whole pounds of outputs"
               TO TK-NAME
           MOVE 80 TO TK-EXPECTED-N
           MOVE VR-BOX6 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-018 box 7 is 120 whole pounds of inputs"
               TO TK-NAME
           MOVE 120 TO TK-EXPECTED-N
           MOVE VR-BOX7 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

      *>   FIN-025: the two identities of the nine box return.
           MOVE "FIN-025 box 3 equals box 1 plus box 2" TO TK-NAME
           COMPUTE WS-AMT = VR-BOX1 + VR-BOX2
           MOVE WS-AMT TO TK-EXPECTED-N
           MOVE VR-BOX3 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-025 box 3 is 16.00" TO TK-NAME
           MOVE 16.00 TO TK-EXPECTED-N
           MOVE VR-BOX3 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-025 box 5 equals box 3 less box 4" TO TK-NAME
           COMPUTE WS-AMT = VR-BOX3 - VR-BOX4
           MOVE WS-AMT TO TK-EXPECTED-N
           MOVE VR-BOX5 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-018 8.00 of net VAT is reclaimable from HMRC"
               TO TK-NAME
           MOVE -8.00 TO TK-EXPECTED-N
           MOVE VR-BOX5 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-018 a negative box 5 is a repayment" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE VR-IS-REPAYMENT TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE VR-BOX5 TO WS-BOX5

      *>   COM-015: preparing the same return twice is one return.
           PERFORM 5100-SETUP-VR
           MOVE "PREPARE" TO RH-FUNCTION
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE "FIN-026 preparing twice does not change the figures"
               TO TK-NAME
           MOVE WS-BOX5 TO TK-EXPECTED-N
           MOVE VR-BOX5 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

      *>   COM-003: preparing a return needs the VAT authority.
           PERFORM 5100-SETUP-VR
           MOVE "GLCLERK" TO RH-USER
           MOVE "PREPARE" TO RH-FUNCTION
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE "FIN-026 a ledger clerk may not prepare the return"
               TO TK-NAME
           IF RH-RETURN-CODE > 4
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           PERFORM 5100-SETUP-VR
           MOVE "WIBBLE" TO RH-FUNCTION
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE "FIN-026 an unknown function is refused" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE RH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       5000-EXIT.
           EXIT.

       5100-SETUP-VR.
           INITIALIZE WS-VATRET9-CA
           MOVE "STC" TO RH-COMPANY
           MOVE "RDG" TO RH-SITE
           MOVE "VATOWNER" TO RH-USER
           MOVE 20261015 TO RH-BUSINESS-DATE
           MOVE "ZZTVQ2" TO VR-VAT-PERIOD-KEY
           MOVE "INVOICE" TO VR-BASIS.

      *>----------------------------------------------------------------
      *> 6000 - FIN-027 THE ERROR CORRECTION THRESHOLD.  A NET ERROR
      *>        UP TO 10,000.00, OR UP TO ONE PER CENT OF THE BOX 6
      *>        TURNOVER CAPPED AT 50,000.00, GOES ON THE NEXT RETURN.
      *>        ANYTHING LARGER, AND ANY DELIBERATE ERROR, IS ROUTED
      *>        FOR SEPARATE DISCLOSURE TO HMRC.
      *>----------------------------------------------------------------
       6000-CORRECTION-TESTS.
           PERFORM 6100-SETUP-VE
           MOVE 9000.00 TO VE-NET-ERROR
           MOVE 500000.00 TO VE-BOX6-TURNOVER
           CALL "VATERRC" USING WS-VATERRC-CA
           MOVE "FIN-027 a 9,000.00 error is under the flat limit"
               TO TK-NAME
           MOVE "NEXTRETURN" TO TK-EXPECTED-X
           MOVE VE-METHOD TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-027 the flat threshold of 10,000.00 applies"
               TO TK-NAME
           MOVE 10000.00 TO TK-EXPECTED-N
           MOVE VE-THRESHOLD-USED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-027 a correction within the threshold is allowed"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE EH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 6100-SETUP-VE
           MOVE 12000.00 TO VE-NET-ERROR
           MOVE 500000.00 TO VE-BOX6-TURNOVER
           CALL "VATERRC" USING WS-VATERRC-CA
           MOVE "FIN-027 12,000.00 on 500,000.00 turnover is disclosed"
               TO TK-NAME
           MOVE "DISCLOSURE" TO TK-EXPECTED-X
           MOVE VE-METHOD TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-027 a disclosure is held for approval" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE EH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

      *>   One per cent of a larger turnover beats the flat limit.
           PERFORM 6100-SETUP-VE
           MOVE 12000.00 TO VE-NET-ERROR
           MOVE 2000000.00 TO VE-BOX6-TURNOVER
           CALL "VATERRC" USING WS-VATERRC-CA
           MOVE "FIN-027 one per cent of turnover raises the limit"
               TO TK-NAME
           MOVE 20000.00 TO TK-EXPECTED-N
           MOVE VE-THRESHOLD-USED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-027 the same error now goes on the next return"
               TO TK-NAME
           MOVE "NEXTRETURN" TO TK-EXPECTED-X
           MOVE VE-METHOD TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   The one per cent limit is capped at 50,000.00.
           PERFORM 6100-SETUP-VE
           MOVE 60000.00 TO VE-NET-ERROR
           MOVE 10000000.00 TO VE-BOX6-TURNOVER
           CALL "VATERRC" USING WS-VATERRC-CA
           MOVE "FIN-027 the one per cent limit is capped at 50,000"
               TO TK-NAME
           MOVE 50000.00 TO TK-EXPECTED-N
           MOVE VE-THRESHOLD-USED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-027 an error above the cap is disclosed"
               TO TK-NAME
           MOVE "DISCLOSURE" TO TK-EXPECTED-X
           MOVE VE-METHOD TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 6100-SETUP-VE
           MOVE 100.00 TO VE-NET-ERROR
           MOVE 500000.00 TO VE-BOX6-TURNOVER
           MOVE "Y" TO VE-IS-DELIBERATE
           CALL "VATERRC" USING WS-VATERRC-CA
           MOVE "FIN-027 a deliberate error is always disclosed"
               TO TK-NAME
           MOVE "DISCLOSURE" TO TK-EXPECTED-X
           MOVE VE-METHOD TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-027 a deliberate error is never within threshold"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE VE-WITHIN-THRESHOLD TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       6100-SETUP-VE.
           INITIALIZE WS-VATERRC-CA
           MOVE "STC" TO EH-COMPANY
           MOVE "RDG" TO EH-SITE
           MOVE "VATOWNER" TO EH-USER
           MOVE 20261015 TO EH-BUSINESS-DATE
           MOVE "ASSESS" TO EH-FUNCTION
           MOVE "ZZTVQ1" TO VE-ORIG-PERIOD-KEY
           MOVE "ZZTVQ2" TO VE-CORR-PERIOD-KEY
           MOVE 20261015 TO VE-DISCOVERED-DATE
           MOVE "N" TO VE-IS-DELIBERATE
           MOVE "Supplier invoice coded to the wrong VAT code"
               TO VE-REASON.

      *>----------------------------------------------------------------
      *> 7000 - FIN-029 EVERY BOX DRILLS DOWN TO THE RECORDS BEHIND IT
      *>        AND THE DRILLED ROWS ADD BACK TO THE BOX TOTAL.
      *>----------------------------------------------------------------
       7000-DRILL-TESTS.
           IF WS-FIXTURE-OK NOT = "Y"
               MOVE "VAT fixtures were not written" TO TK-DETAIL
               MOVE "FIN-029 the box drill down tests" TO TK-NAME
               PERFORM TK-SKIP
               GO TO 7000-EXIT
           END-IF
           PERFORM 5100-SETUP-VR
           MOVE "PREPARE" TO RH-FUNCTION
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE VR-BOX1 TO WS-SAVE-B1
           MOVE VR-BOX4 TO WS-SAVE-B4

           PERFORM 5100-SETUP-VR
           MOVE "DRILL" TO RH-FUNCTION
           MOVE 1 TO VR-BOX-NO
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE "FIN-029 box 1 drills down" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE RH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-029 one sales invoice stands behind box 1"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE VR-DRILL-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-029 the drilled document is the sales invoice"
               TO TK-NAME
           MOVE "ZZTV-SI-001" TO TK-EXPECTED-X
           MOVE VR-D-DOC-REF(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-029 the drilled VAT is 16.00" TO TK-NAME
           MOVE 16.00 TO TK-EXPECTED-N
           MOVE VR-D-VAT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 7100-SUM-DRILL
           MOVE "FIN-029 the box 1 drill reconciles to the box total"
               TO TK-NAME
           MOVE WS-SAVE-B1 TO TK-EXPECTED-N
           MOVE WS-DRILL-SUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 5100-SETUP-VR
           MOVE "DRILL" TO RH-FUNCTION
           MOVE 4 TO VR-BOX-NO
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE "FIN-029 one purchase invoice stands behind box 4"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE VR-DRILL-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-029 the drilled input VAT is 24.00" TO TK-NAME
           MOVE 24.00 TO TK-EXPECTED-N
           MOVE VR-D-VAT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 7100-SUM-DRILL
           MOVE "FIN-029 the box 4 drill reconciles to the box total"
               TO TK-NAME
           MOVE WS-SAVE-B4 TO TK-EXPECTED-N
           MOVE WS-DRILL-SUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 5100-SETUP-VR
           MOVE "DRILL" TO RH-FUNCTION
           MOVE 0 TO VR-BOX-NO
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE "FIN-029 there is no box zero to drill into"
               TO TK-NAME
           MOVE "VTR0020" TO TK-EXPECTED-X
           MOVE RH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       7000-EXIT.
           EXIT.

       7100-SUM-DRILL.
           MOVE 0 TO WS-DRILL-SUM
           PERFORM VARYING WS-IX FROM 1 BY 1
               UNTIL WS-IX > VR-DRILL-CNT
               ADD VR-D-AMOUNT(WS-IX) TO WS-DRILL-SUM
           END-PERFORM.

      *>----------------------------------------------------------------
      *> 7500 - FIN-026 THE CONTROL CHECK, FINALISATION AND MTD
      *>        SUBMISSION.  COM-003 AND SOD-VAT (D-07): THE PREPARER
      *>        OF A RETURN MAY NEVER APPROVE IT, SO FIN.VAT.PREPARE
      *>        AND FIN.VAT.APPROVE ARE HELD BY DIFFERENT ROLES.
      *>        ONCE FINALISED THE FIGURES ARE FROZEN AND MAY ONLY BE
      *>        PUT RIGHT THROUGH AN ERROR CORRECTION (FIN-027).
      *>----------------------------------------------------------------
       7500-FINALISE-TESTS.
           IF WS-FIXTURE-OK NOT = "Y"
               MOVE "VAT fixtures were not written" TO TK-DETAIL
               MOVE "FIN-026 the finalisation tests" TO TK-NAME
               PERFORM TK-SKIP
               GO TO 7500-EXIT
           END-IF
      *>   FIN-025: box 5 against the VAT control account movement.
      *>   The 2033 fixture quarter carries no control account journal,
      *>   so the whole of box 5 is reported as unexplained.
           PERFORM 5100-SETUP-VR
           MOVE "CHECK" TO RH-FUNCTION
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE "FIN-025 the VAT control account carries nothing"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE VR-CONTROL-BALANCE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-025 the whole of box 5 is unexplained"
               TO TK-NAME
           MOVE -8.00 TO TK-EXPECTED-N
           MOVE VR-CONTROL-DIFF TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-025 the difference is reported as VTR0050"
               TO TK-NAME
           MOVE "VTR0050" TO TK-EXPECTED-X
           MOVE RH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   SOD-VAT: the VAT owner prepares, so may not approve.
           PERFORM 5100-SETUP-VR
           MOVE "FINALISE" TO RH-FUNCTION
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE "SOD-VAT the preparer may not finalise the return"
               TO TK-NAME
           IF RH-RETURN-CODE > 4
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status FROM fin.VatReturn WHERE "
                  "CompanyCode = 'STC' AND VatPeriodKey = 'ZZTVQ2'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-READ-ONE
           MOVE "SOD-VAT the refused return is still a draft"
               TO TK-NAME
           MOVE "DRAFT" TO TK-EXPECTED-X
           MOVE WS-ANSWER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   The financial controller holds FIN.VAT.APPROVE.
           PERFORM 5100-SETUP-VR
           MOVE "FINCTRL" TO RH-USER
           MOVE "FINALISE" TO RH-FUNCTION
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE "FIN-026 the controller finalises the return"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE RH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-026 the return is reported as finalised"
               TO TK-NAME
           MOVE "FINALISED" TO TK-EXPECTED-X
           MOVE VR-RETURN-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status FROM fin.VatReturn WHERE "
                  "CompanyCode = 'STC' AND VatPeriodKey = 'ZZTVQ2'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-READ-ONE
           MOVE "FIN-026 the ledger holds the finalised return"
               TO TK-NAME
           MOVE "FINALISED" TO TK-EXPECTED-X
           MOVE WS-ANSWER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(8)) FROM "
                  "fin.VatReturnBox b JOIN fin.VatReturn r ON "
                  "r.VatReturnId = b.VatReturnId WHERE "
                  "r.CompanyCode = 'STC' AND r.VatPeriodKey = 'ZZTVQ2'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-READ-ONE
           MOVE "FIN-026 all nine boxes are frozen" TO TK-NAME
           MOVE "9" TO TK-EXPECTED-X
           MOVE WS-ANSWER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(8)) FROM "
                  "fin.VatReturnDetail d JOIN fin.VatReturn r ON "
                  "r.VatReturnId = d.VatReturnId WHERE "
                  "r.CompanyCode = 'STC' AND r.VatPeriodKey = 'ZZTVQ2'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-READ-ONE
           MOVE "FIN-026 the four box contributions are frozen too"
               TO TK-NAME
           MOVE "4" TO TK-EXPECTED-X
           MOVE WS-ANSWER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status FROM fin.VatPeriod WHERE "
                  "CompanyCode = 'STC' AND VatPeriodKey = 'ZZTVQ2'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-READ-ONE
           MOVE "FIN-026 the VAT period closes with the return"
               TO TK-NAME
           MOVE "FINALISED" TO TK-EXPECTED-X
           MOVE WS-ANSWER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   A finalised return cannot change.
           PERFORM 5100-SETUP-VR
           MOVE "PREPARE" TO RH-FUNCTION
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE "FIN-026 a finalised return is not prepared again"
               TO TK-NAME
           MOVE "VTR0010" TO TK-EXPECTED-X
           MOVE RH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-026 the frozen box 5 is read back unchanged"
               TO TK-NAME
           MOVE -8.00 TO TK-EXPECTED-N
           MOVE VR-BOX5 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 5100-SETUP-VR
           MOVE "FINCTRL" TO RH-USER
           MOVE "FINALISE" TO RH-FUNCTION
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE "FIN-026 a return is never finalised twice" TO TK-NAME
           MOVE "VTR0030" TO TK-EXPECTED-X
           MOVE RH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-026 the second finalisation is refused"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE RH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

      *>   FIN-030: the MTD submission keeps the HMRC receipt.
           PERFORM 5100-SETUP-VR
           MOVE "FINCTRL" TO RH-USER
           MOVE "SUBMIT" TO RH-FUNCTION
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE "FIN-030 a submission without a receipt is refused"
               TO TK-NAME
           MOVE "VTR0041" TO TK-EXPECTED-X
           MOVE RH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 5100-SETUP-VR
           MOVE "GLCLERK" TO RH-USER
           MOVE "SUBMIT" TO RH-FUNCTION
           MOVE "ZZT-HMRC-0001" TO VR-SUBMISSION-REF
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE "COM-003 a ledger clerk may not submit to HMRC"
               TO TK-NAME
           IF RH-RETURN-CODE > 4
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           PERFORM 5100-SETUP-VR
           MOVE "FINCTRL" TO RH-USER
           MOVE "SUBMIT" TO RH-FUNCTION
           MOVE "ZZT-HMRC-0001" TO VR-SUBMISSION-REF
           MOVE "HMRC accepted the return" TO VR-HMRC-RESPONSE
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE "FIN-030 the finalised return is submitted" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE RH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-030 the return is reported as submitted"
               TO TK-NAME
           MOVE "SUBMITTED" TO TK-EXPECTED-X
           MOVE VR-RETURN-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status + '|' + ISNULL(SubmissionRef,'') "
                  "FROM fin.VatReturn WHERE CompanyCode = 'STC' "
                  "AND VatPeriodKey = 'ZZTVQ2'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-READ-ONE
           MOVE "FIN-030 the HMRC receipt is retained" TO TK-NAME
           MOVE "SUBMITTED|ZZT-HMRC-0001" TO TK-EXPECTED-X
           MOVE WS-ANSWER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 5100-SETUP-VR
           MOVE "FINCTRL" TO RH-USER
           MOVE "FINALISE" TO RH-FUNCTION
           CALL "VATRET9" USING WS-VATRET9-CA
           MOVE "FIN-026 a submitted return can no longer change"
               TO TK-NAME
           MOVE "VTR0030" TO TK-EXPECTED-X
           MOVE RH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       7500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 8000 - FIN-027 THE ONLY AMENDMENT TO A SUBMITTED RETURN IS AN
      *>        ERROR CORRECTION.  D-02: A NET ERROR WITHIN THE
      *>        THRESHOLD IS CARRIED ON THE NEXT RETURN, ANYTHING
      *>        LARGER IS DISCLOSED SEPARATELY, AND NEITHER REACHES
      *>        BOX 1 OR BOX 4 UNTIL IT IS APPROVED.
      *>----------------------------------------------------------------
       8000-ERROR-CORRECTION-DB-TESTS.
           IF WS-FIXTURE-OK NOT = "Y"
               MOVE "VAT fixtures were not written" TO TK-DETAIL
               MOVE "FIN-027 the recorded error correction tests"
                   TO TK-NAME
               PERFORM TK-SKIP
               GO TO 8000-EXIT
           END-IF
           PERFORM 6100-SETUP-VE
           MOVE "RECORD" TO EH-FUNCTION
           MOVE "ZZTVQ2" TO VE-ORIG-PERIOD-KEY
           MOVE "ZZTVQ3" TO VE-CORR-PERIOD-KEY
           MOVE SPACES TO VE-REASON
           MOVE 500.00 TO VE-NET-ERROR
           MOVE 500000.00 TO VE-BOX6-TURNOVER
           CALL "VATERRC" USING WS-VATERRC-CA
           MOVE "FIN-027 a correction must say how the error arose"
               TO TK-NAME
           MOVE "VEC0001" TO TK-EXPECTED-X
           MOVE EH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 6100-SETUP-VE
           MOVE "RECORD" TO EH-FUNCTION
           MOVE "ZZTVQ2" TO VE-ORIG-PERIOD-KEY
           MOVE "ZZTVQ3" TO VE-CORR-PERIOD-KEY
           MOVE 500.00 TO VE-NET-ERROR
           MOVE 500000.00 TO VE-BOX6-TURNOVER
           MOVE "ZZTV-SI-001" TO VE-SOURCE-REFS
           CALL "VATERRC" USING WS-VATERRC-CA
           MOVE "FIN-027 the submitted return is amended by correction"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE EH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-027 a 500.00 error goes on the next return"
               TO TK-NAME
           MOVE "NEXTRETURN" TO TK-EXPECTED-X
           MOVE VE-METHOD TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-027 the correction is recorded and open"
               TO TK-NAME
           MOVE "OPEN" TO TK-EXPECTED-X
           MOVE VE-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-027 the correction is given an identity"
               TO TK-NAME
           IF VE-ERROR-ID > 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE VE-ERROR-ID TO WS-ERROR-ID
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Method + '|' + Status FROM "
                  "fin.VatErrorCorrection WHERE CompanyCode = 'STC' "
                  "AND OriginalVatPeriodKey = 'ZZTVQ2' "
                  "AND NetErrorAmount = 500.00"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-READ-ONE
           MOVE "FIN-027 the ledger holds the open correction"
               TO TK-NAME
           MOVE "NEXTRETURN|OPEN" TO TK-EXPECTED-X
           MOVE WS-ANSWER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   COM-003: approving a correction needs FIN.VAT.CORRECT.
           PERFORM 6100-SETUP-VE
           MOVE "APPROVE" TO EH-FUNCTION
           MOVE "GLCLERK" TO EH-USER
           MOVE WS-ERROR-ID TO VE-ERROR-ID
           CALL "VATERRC" USING WS-VATERRC-CA
           MOVE "COM-003 a ledger clerk may not approve a correction"
               TO TK-NAME
           IF EH-RETURN-CODE > 4
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           PERFORM 6100-SETUP-VE
           MOVE "APPROVE" TO EH-FUNCTION
           MOVE "SYSADM" TO EH-USER
           CALL "VATERRC" USING WS-VATERRC-CA
           MOVE "FIN-027 the correction to approve must be named"
               TO TK-NAME
           MOVE "VEC0003" TO TK-EXPECTED-X
           MOVE EH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 6100-SETUP-VE
           MOVE "APPROVE" TO EH-FUNCTION
           MOVE "SYSADM" TO EH-USER
           MOVE WS-ERROR-ID TO VE-ERROR-ID
           MOVE "ZZTVQ3" TO VE-CORR-PERIOD-KEY
           CALL "VATERRC" USING WS-VATERRC-CA
           MOVE "FIN-027 the correction is approved" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE EH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-027 the approved correction is reported back"
               TO TK-NAME
           MOVE "APPROVED" TO TK-EXPECTED-X
           MOVE VE-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status + '|' + "
                  "ISNULL(CorrectionVatPeriodKey,'NONE') FROM "
                  "fin.VatErrorCorrection WHERE CompanyCode = 'STC' "
                  "AND OriginalVatPeriodKey = 'ZZTVQ2' "
                  "AND NetErrorAmount = 500.00"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-READ-ONE
           MOVE "FIN-027 the correction is carried into ZZTVQ3"
               TO TK-NAME
           MOVE "APPROVED|ZZTVQ3" TO TK-EXPECTED-X
           MOVE WS-ANSWER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 6100-SETUP-VE
           MOVE "APPROVE" TO EH-FUNCTION
           MOVE "SYSADM" TO EH-USER
           MOVE WS-ERROR-ID TO VE-ERROR-ID
           CALL "VATERRC" USING WS-VATERRC-CA
           MOVE "FIN-027 an approved correction is not approved twice"
               TO TK-NAME
           MOVE "VEC0004" TO TK-EXPECTED-X
           MOVE EH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   D-02: above the threshold the correction leaves the next
      *>   return alone and is disclosed to HMRC on its own.
           PERFORM 6100-SETUP-VE
           MOVE "RECORD" TO EH-FUNCTION
           MOVE "ZZTVQ2" TO VE-ORIG-PERIOD-KEY
           MOVE "ZZTVQ3" TO VE-CORR-PERIOD-KEY
           MOVE 60000.00 TO VE-NET-ERROR
           MOVE 500000.00 TO VE-BOX6-TURNOVER
           CALL "VATERRC" USING WS-VATERRC-CA
           MOVE "FIN-027 60,000.00 is above the threshold" TO TK-NAME
           MOVE "DISCLOSURE" TO TK-EXPECTED-X
           MOVE VE-METHOD TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-027 a disclosure is held for approval"
               TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE EH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Method + '|' + "
                  "ISNULL(CorrectionVatPeriodKey,'NONE') FROM "
                  "fin.VatErrorCorrection WHERE CompanyCode = 'STC' "
                  "AND OriginalVatPeriodKey = 'ZZTVQ2' "
                  "AND NetErrorAmount = 60000.00"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-READ-ONE
           MOVE "FIN-027 a disclosure is not carried to a return"
               TO TK-NAME
           MOVE "DISCLOSURE|NONE" TO TK-EXPECTED-X
           MOVE WS-ANSWER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       8000-EXIT.
           EXIT.

       COPY TESTKITP.
