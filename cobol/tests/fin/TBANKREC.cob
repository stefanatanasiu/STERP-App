      *>================================================================
      *> PROGRAM-ID : TBANKREC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : BANK RECONCILIATION TEST SUITE
      *> PURPOSE    : AC-014.  ONE STATEMENT AMOUNT OF 100.00 IS TIED
      *>              TO TWO BOOK ENTRIES OF 60.00 AND 40.00, SO THE
      *>              MATCH IS GROUPED AND BRIDGES EXACTLY.  AMOUNT,
      *>              CURRENCY AND DATE ARE THE EVIDENCE: SIMILARITY
      *>              ALONE NEVER RECONCILES UNEQUAL TOTALS, A BANK
      *>              FEE IS EXPENSED SEPARATELY AND A TRANSFER
      *>              BETWEEN OWN ACCOUNTS CREATES NO REVENUE.
      *>              EVERY WRITE RUNS INSIDE ONE BEGN ... ROLB SO THE
      *>              SUITE IS REPEATABLE AND LEAVES NOTHING BEHIND.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: FIN-013, FIN-014, FIN-015
      *> AC: AC-014
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TBANKREC.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY SQLIOCA.
       COPY BANKRECCA REPLACING ==DFHCOMMAREA== BY ==WS-BANKREC-CA==
                      LEADING ==CA-== BY ==BH-==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "N".
           05  WS-FIXTURE-OK           PIC X VALUE "Y".
       01  WS-WORK.
           05  WS-PING                 PIC X(40).
           05  WS-ID-ROW.
               10  WS-ID-TEXT          PIC X(18).
           05  WS-ADJ-TEXT             PIC X(10).
           05  WS-ACC-TEXT             PIC X(20).
           05  WS-FIX-ERR              PIC X(120).
           05  WS-RECON-ID             PIC S9(9) COMP-5.
           05  WS-MATCH-ID             PIC S9(9) COMP-5.
           05  WS-ST-ID1               PIC S9(9) COMP-5.
           05  WS-ST-ID2               PIC S9(9) COMP-5.
           05  WS-BK-ID1               PIC S9(9) COMP-5.
           05  WS-BK-ID2               PIC S9(9) COMP-5.
           05  WS-BK-ID3               PIC S9(9) COMP-5.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TBANKREC bank reconciliation" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-CONNECT
           IF WS-DB-UP NOT = "Y"
               MOVE "no database reachable" TO TK-DETAIL
               MOVE "bank suite needs SQL Server" TO TK-NAME
               PERFORM TK-SKIP
               PERFORM TK-END
               STOP RUN
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING
           PERFORM 1000-BRIDGE-TESTS
           PERFORM 2000-GROUP-TESTS
           PERFORM 3000-EVIDENCE-TESTS
           PERFORM 4000-CHARGE-TESTS
           PERFORM 5000-TRANSFER-TESTS
      *>   A refused posting runs under SET XACT_ABORT ON and takes
      *>   the whole unit of work with it, so the fixtures are
      *>   written after the refusal tests and under a live
      *>   transaction of their own.
           PERFORM 0200-ENSURE-TXN
           PERFORM 0300-FIXTURES
           IF WS-FIXTURE-OK = "Y"
               PERFORM 6000-DB-MATCH-TESTS
               PERFORM 6500-COMPLETE-TESTS
               PERFORM 7000-UNMATCH-TESTS
               PERFORM 8000-POSTING-TESTS
           ELSE
               MOVE "bank fixtures could not be written" TO TK-DETAIL
               IF WS-FIX-ERR NOT = SPACES
                   MOVE WS-FIX-ERR TO TK-DETAIL
               END-IF
               MOVE "FIN-014 the database backed bank tests"
                   TO TK-NAME
               PERFORM TK-SKIP
           END-IF
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

      *>   SQL Server kills the whole unit of work when a posting is
      *>   refused under XACT_ABORT, so open a fresh one when the
      *>   suite is about to write again.
       0200-ENSURE-TXN.
           MOVE SPACES TO SQLIO-STMT
           MOVE "SELECT CAST(@@TRANCOUNT AS varchar(18))"
               TO SQLIO-STMT
           PERFORM 0910-READ-ID
           IF FUNCTION NUMVAL(WS-ID-TEXT) = 0
               MOVE "BEGN" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-PING
           END-IF.

       0950-SETUP-BR.
           INITIALIZE WS-BANKREC-CA
           MOVE "STC" TO BH-COMPANY
           MOVE "RDG" TO BH-SITE
           MOVE "FINCTRL" TO BH-USER
           MOVE 20261015 TO BH-BUSINESS-DATE
           MOVE "GBP1" TO BR-BANK-ACCOUNT
           MOVE "GBP" TO BR-CURRENCY
           MOVE 20261031 TO BR-AS-AT-DATE.

      *>----------------------------------------------------------------
      *> 0300 - FIXTURES.  TWO THROW AWAY BANK ACCOUNTS, ONE OPEN
      *>        RECONCILIATION, TWO STATEMENT LINES AND A POSTED CASH
      *>        BOOK JOURNAL GIVE THE SUITE REAL StatementLineId AND
      *>        JournalLineId KEYS TO RECONCILE.  EVERYTHING IS
      *>        WRITTEN INSIDE THE SUITE'S BEGN ... ROLB.
      *>----------------------------------------------------------------
       0300-FIXTURES.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.BankAccount (CompanyCode, "
                  "BankAccountCode, Name, AccountCode, "
                  "CurrencyCode, StatementBalance, StatementDate, "
                  "Status) VALUES ('STC','ZZTB1',"
                  "'Test recon current account','1300','GBP',0,"
                  "'2026-10-31','ACTIVE'),('STC','ZZTB2',"
                  "'Test recon deposit account','1330','GBP',0,"
                  "'2026-10-31','ACTIVE')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.BankRecon (CompanyCode, "
                  "BankAccountCode, StatementRef, FromDate, "
                  "ToDate, CurrencyCode, OpeningStatementBalance, "
                  "ClosingStatementBalance, Status, CreatedBy) "
                  "VALUES ('STC','ZZTB1','ZZT-STMT-2610',"
                  "'2026-10-01','2026-10-31','GBP',0,100.00,"
                  "'OPEN','FINCTRL')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ReconId AS varchar(18)) FROM "
                  "fin.BankRecon WHERE CompanyCode = 'STC' AND "
                  "StatementRef = 'ZZT-STMT-2610'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0910-READ-ID
           COMPUTE WS-RECON-ID = FUNCTION NUMVAL(WS-ID-TEXT)
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.BankStatementLine (CompanyCode, "
                  "BankAccountCode, StatementRef, LineNum, "
                  "EntryDate, Description, Amount, CurrencyCode, "
                  "EntryType, ReconId) SELECT 'STC','ZZTB1',"
                  "'ZZT-STMT-2610',v.n,'2026-10-31',v.d,v.a,'GBP',"
                  "'RECEIPT',r.ReconId FROM fin.BankRecon r "
                  "CROSS JOIN (VALUES (1,'Customer receipt',"
                  "100.00),(2,'Second customer receipt',60.00)) "
                  "AS v(n,d,a) WHERE r.CompanyCode = 'STC' AND "
                  "r.StatementRef = 'ZZT-STMT-2610'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
           MOVE 1 TO SQLIO-INT-EDIT
           PERFORM 0310-READ-STMT-LINE
           COMPUTE WS-ST-ID1 = FUNCTION NUMVAL(WS-ID-TEXT)
           MOVE 2 TO SQLIO-INT-EDIT
           PERFORM 0310-READ-STMT-LINE
           COMPUTE WS-ST-ID2 = FUNCTION NUMVAL(WS-ID-TEXT)
           PERFORM 0315-JOURNAL-FIXTURE
           MOVE "25.00" TO WS-ADJ-TEXT
           MOVE "ZZTB1" TO WS-ACC-TEXT
           PERFORM 0320-SET-STMT-BAL
           MOVE "15.00" TO WS-ADJ-TEXT
           MOVE "ZZTB2" TO WS-ACC-TEXT
           PERFORM 0320-SET-STMT-BAL.

       0310-READ-STMT-LINE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(StatementLineId AS varchar(18)) "
                  "FROM fin.BankStatementLine WHERE CompanyCode = "
                  "'STC' AND BankAccountCode = 'ZZTB1' AND "
                  "StatementRef = 'ZZT-STMT-2610' AND LineNum = "
                  FUNCTION TRIM(SQLIO-INT-EDIT)
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0910-READ-ID.

       0315-JOURNAL-FIXTURE.
      *>   A posted cash book journal of 100.00 made of two receipts
      *>   of 60.00 and 40.00 against the test bank account.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.Journal (CompanyCode, JournalNumber, "
                  "JournalType, Description, SourceModule, "
                  "DocumentDate, PostingDate, FiscalYear, PeriodNo, "
                  "CurrencyCode, TotalDebit, TotalCredit, "
                  "TotalDebitGBP, TotalCreditGBP, Status, PostedBy) "
                  "VALUES ('STC','ZZT-BNK-0001','CASH',"
                  "'Test cash book receipts','FIN','2026-10-31',"
                  "'2026-10-31',2026,10,'GBP',100.00,100.00,"
                  "100.00,100.00,'POSTED','FINCTRL')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalLine (JournalId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, DebitAmountGBP, CreditAmountGBP, "
                  "CurrencyCode, Narrative, BankAccountCode) "
                  "SELECT j.JournalId, v.n, 'STC', v.acc, v.dr, "
                  "v.cr, v.dr, v.cr, 'GBP', v.nar, v.bk FROM "
                  "fin.Journal j CROSS JOIN (VALUES "
                  "(1,'1300',60.00,0.00,'Receipt part one',"
                  "'ZZTB1'),(2,'1300',40.00,0.00,"
                  "'Receipt part two','ZZTB1'),(3,'4000',0.00,"
                  "100.00,'Sales settled',NULL)) AS "
                  "v(n,acc,dr,cr,nar,bk) WHERE j.CompanyCode = "
                  "'STC' AND j.JournalNumber = 'ZZT-BNK-0001'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
           MOVE 1 TO SQLIO-INT-EDIT
           PERFORM 0318-READ-BOOK-LINE
           COMPUTE WS-BK-ID1 = FUNCTION NUMVAL(WS-ID-TEXT)
           MOVE 2 TO SQLIO-INT-EDIT
           PERFORM 0318-READ-BOOK-LINE
           COMPUTE WS-BK-ID2 = FUNCTION NUMVAL(WS-ID-TEXT)
           MOVE 3 TO SQLIO-INT-EDIT
           PERFORM 0318-READ-BOOK-LINE
           COMPUTE WS-BK-ID3 = FUNCTION NUMVAL(WS-ID-TEXT).

       0318-READ-BOOK-LINE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(l.JournalLineId AS varchar(18)) "
                  "FROM fin.JournalLine l JOIN fin.Journal j ON "
                  "j.JournalId = l.JournalId WHERE j.CompanyCode "
                  "= 'STC' AND j.JournalNumber = 'ZZT-BNK-0001' "
                  "AND l.LineNum = " FUNCTION TRIM(SQLIO-INT-EDIT)
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0910-READ-ID.

       0320-SET-STMT-BAL.
      *>   Park the statement balance a known distance from the
      *>   bridged book balance, so the unexplained difference the
      *>   bridge reports is exactly minus WS-ADJ-TEXT whatever else
      *>   the demo data holds for the account.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE b SET StatementBalance = "
                  "ISNULL(k.Book,0) - ISNULL(k.OutRec,0) + "
                  "ISNULL(k.OutPay,0) + " FUNCTION TRIM(WS-ADJ-TEXT)
                  " FROM fin.BankAccount b OUTER APPLY (SELECT "
                  "SUM(v.SignedGBP) AS Book, SUM(CASE WHEN "
                  "v.ReconMatchId IS NULL AND v.SignedGBP > 0 "
                  "THEN v.SignedGBP ELSE 0 END) AS OutRec, "
                  "SUM(CASE WHEN v.ReconMatchId IS NULL AND "
                  "v.SignedGBP < 0 THEN -v.SignedGBP ELSE 0 END) "
                  "AS OutPay FROM fin.vw_CashBook v WHERE "
                  "v.CompanyCode = b.CompanyCode AND "
                  "v.BankAccountCode = b.BankAccountCode AND "
                  "v.PostingDate <= '2026-10-31') k WHERE "
                  "b.CompanyCode = 'STC' AND b.BankAccountCode = '"
                  FUNCTION TRIM(WS-ACC-TEXT) "'"
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
               IF WS-FIX-ERR = SPACES
                   MOVE SQLIO-ERRMSG(1:120) TO WS-FIX-ERR
               END-IF
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING.

       0910-READ-ID.
           MOVE SPACES TO WS-ID-ROW
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 18 TO SQLIO-COL-WIDTH(1)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-FIXTURE-OK
               MOVE "0" TO WS-ID-TEXT
               IF WS-FIX-ERR = SPACES
                   MOVE SQLIO-ERRMSG(1:120) TO WS-FIX-ERR
               END-IF
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW.

      *>----------------------------------------------------------------
      *> 1000 - FIN-013 THE STATEMENT TO BOOK BRIDGE
      *>----------------------------------------------------------------
       1000-BRIDGE-TESTS.
           PERFORM 0950-SETUP-BR
           MOVE "BRIDGE" TO BH-FUNCTION
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "FIN-013 the cash book bridges to the statement"
               TO TK-NAME
           IF BH-RETURN-CODE < 8
               PERFORM TK-PASS
           ELSE
               MOVE BH-MESSAGE TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           PERFORM 0950-SETUP-BR
           MOVE "BRIDGE" TO BH-FUNCTION
           MOVE "NOSUCHACCT" TO BR-BANK-ACCOUNT
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "FIN-013 an unknown bank account is refused"
               TO TK-NAME
           IF BH-RETURN-CODE > 4
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 2000 - AC-014 ONE STATEMENT LINE OF 100.00 AGAINST TWO BOOK
      *>        ENTRIES OF 60.00 AND 40.00 IS A GROUPED MATCH THAT
      *>        BRIDGES EXACTLY.
      *>----------------------------------------------------------------
       2000-GROUP-TESTS.
           PERFORM 0950-SETUP-BR
           MOVE "MATCH" TO BH-FUNCTION
           MOVE "ZZT-BRM-AC014" TO BR-MATCH-REF
           MOVE 1 TO BR-STMT-CNT
           MOVE 100.00 TO BR-ST-AMOUNT(1)
           MOVE 2 TO BR-BOOK-CNT
           MOVE 60.00 TO BR-BK-AMOUNT(1)
           MOVE 40.00 TO BR-BK-AMOUNT(2)
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "AC-014 the statement side of the match is 100.00"
               TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE BR-STMT-TOTAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-014 the book side totals 60.00 and 40.00"
               TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE BR-BOOK-TOTAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-014 the grouped match leaves nothing outstanding"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE BR-DIFFERENCE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-014 a many to many tie is a grouped match"
               TO TK-NAME
           MOVE "GROUPED" TO TK-EXPECTED-X
           MOVE BR-MATCH-TYPE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 0950-SETUP-BR
           MOVE "MATCH" TO BH-FUNCTION
           MOVE 1 TO BR-STMT-CNT
           MOVE 100.00 TO BR-ST-AMOUNT(1)
           MOVE 1 TO BR-BOOK-CNT
           MOVE 100.00 TO BR-BK-AMOUNT(1)
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "FIN-014 one to one is an exact match" TO TK-NAME
           MOVE "EXACT" TO TK-EXPECTED-X
           MOVE BR-MATCH-TYPE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 3000 - FIN-014 AMOUNT IS EVIDENCE.  A LIKELY LOOKING DATE OR
      *>        REFERENCE DOES NOT RECONCILE UNEQUAL TOTALS; THE
      *>        RESIDUE IS EXPLAINED IN WRITING OR THE MATCH FAILS.
      *>----------------------------------------------------------------
       3000-EVIDENCE-TESTS.
           PERFORM 0950-SETUP-BR
           MOVE "MATCH" TO BH-FUNCTION
           MOVE 1 TO BR-STMT-CNT
           MOVE 100.00 TO BR-ST-AMOUNT(1)
           MOVE 1 TO BR-BOOK-CNT
           MOVE 60.00 TO BR-BK-AMOUNT(1)
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "AC-014 unequal totals are not quietly reconciled"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE BH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-014 the unexplained residue of 40.00 is reported"
               TO TK-NAME
           MOVE 40.00 TO TK-EXPECTED-N
           MOVE BR-DIFFERENCE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-014 the refusal is reported as BNK0021" TO TK-NAME
           MOVE "BNK0021" TO TK-EXPECTED-X
           MOVE BH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 0950-SETUP-BR
           MOVE "MATCH" TO BH-FUNCTION
           MOVE 1 TO BR-STMT-CNT
           MOVE 100.00 TO BR-ST-AMOUNT(1)
           MOVE 1 TO BR-BOOK-CNT
           MOVE 60.00 TO BR-BK-AMOUNT(1)
           MOVE "Supplier took a 40.00 settlement discount"
               TO BR-NARRATIVE
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "AC-014 a written explanation carries the residue"
               TO TK-NAME
           MOVE "EXPLAINED" TO TK-EXPECTED-X
           MOVE BR-MATCH-TYPE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 0950-SETUP-BR
           MOVE "MATCH" TO BH-FUNCTION
           MOVE 0 TO BR-STMT-CNT
           MOVE 0 TO BR-BOOK-CNT
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "FIN-014 a match needs entries on both sides"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE BH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-014 the empty match is reported as BNK0020"
               TO TK-NAME
           MOVE "BNK0020" TO TK-EXPECTED-X
           MOVE BH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   COM-003: reconciling a bank account needs the authority.
           PERFORM 0950-SETUP-BR
           MOVE "MATCH" TO BH-FUNCTION
           MOVE "VATOWNER" TO BH-USER
           MOVE 1 TO BR-STMT-CNT
           MOVE 100.00 TO BR-ST-AMOUNT(1)
           MOVE 1 TO BR-BOOK-CNT
           MOVE 100.00 TO BR-BK-AMOUNT(1)
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "FIN-014 the VAT owner may not reconcile the bank"
               TO TK-NAME
           IF BH-RETURN-CODE > 4
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 4000 - FIN-015 A BANK FEE IS AN EXPENSE IN ITS OWN RIGHT AND
      *>        IS NEVER NETTED INTO THE MATCHED AMOUNT.
      *>----------------------------------------------------------------
       4000-CHARGE-TESTS.
           PERFORM 0950-SETUP-BR
           MOVE "CHARGE" TO BH-FUNCTION
           MOVE 3.00 TO BR-AMOUNT
           MOVE "CHARGE" TO BR-ENTRY-TYPE
           MOVE "Quarterly account maintenance fee" TO BR-NARRATIVE
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "AC-014 a bank fee is its own instruction, not a match"
               TO TK-NAME
           IF BR-MATCH-TYPE = SPACES AND BR-BOOK-TOTAL = 0
               PERFORM TK-PASS
           ELSE
               MOVE BH-MESSAGE TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "AC-014 the fee does not change the matched amount"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE BR-STMT-TOTAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-BR
           MOVE "CHARGE" TO BH-FUNCTION
           MOVE 0 TO BR-AMOUNT
           MOVE "CHARGE" TO BR-ENTRY-TYPE
           MOVE "Nil fee" TO BR-NARRATIVE
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "FIN-015 a charge of nothing is refused" TO TK-NAME
           IF BH-RETURN-CODE > 4
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 5000 - FIN-015 A TRANSFER BETWEEN OWN ACCOUNTS MOVES VALUE.
      *>        IT IS NOT INCOME, AND IT IS NOT MADE ACROSS CURRENCIES
      *>        WITHOUT AN APPROVED RATE.
      *>----------------------------------------------------------------
       5000-TRANSFER-TESTS.
           PERFORM 0950-SETUP-BR
           MOVE "TRANSFER" TO BH-FUNCTION
           MOVE "DEP1" TO BR-TO-BANK-ACCOUNT
           MOVE 100.00 TO BR-AMOUNT
           MOVE "Sweep to the deposit account" TO BR-NARRATIVE
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "AC-014 a transfer moves value and creates no revenue"
               TO TK-NAME
           IF BR-STMT-TOTAL = 0 AND BR-BOOK-TOTAL = 0
             AND BR-MATCH-TYPE = SPACES
               PERFORM TK-PASS
           ELSE
               MOVE BH-MESSAGE TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           PERFORM 0950-SETUP-BR
           MOVE "TRANSFER" TO BH-FUNCTION
           MOVE "EUR1" TO BR-TO-BANK-ACCOUNT
           MOVE 100.00 TO BR-AMOUNT
           MOVE "Sweep to the euro account" TO BR-NARRATIVE
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "AC-014 a transfer across currencies is refused"
               TO TK-NAME
           IF BH-RETURN-CODE > 4
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           PERFORM 0950-SETUP-BR
           MOVE "TRANSFER" TO BH-FUNCTION
           MOVE "GBP1" TO BR-TO-BANK-ACCOUNT
           MOVE 100.00 TO BR-AMOUNT
           MOVE "Transfer to itself" TO BR-NARRATIVE
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "FIN-015 an account may not transfer to itself"
               TO TK-NAME
           IF BH-RETURN-CODE > 4
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 6000 - FIN-014 THE MATCH IS WRITTEN AGAINST REAL KEYS.  ONE
      *>        STATEMENT LINE OF 100.00 IS TIED TO TWO BOOK LINES OF
      *>        60.00 AND 40.00, AND NEITHER SIDE CAN BE RECONCILED
      *>        A SECOND TIME (AC-014).
      *>----------------------------------------------------------------
       6000-DB-MATCH-TESTS.
           PERFORM 0950-SETUP-BR
           MOVE "COMPLETE" TO BH-FUNCTION
           MOVE "ZZTB1" TO BR-BANK-ACCOUNT
           MOVE WS-RECON-ID TO BR-RECON-ID
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "FIN-013 an unexplained difference blocks completion"
               TO TK-NAME
           MOVE "BNK0060" TO TK-EXPECTED-X
           MOVE BH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-013 the bridge reports the 25.00 still adrift"
               TO TK-NAME
           MOVE -25.00 TO TK-EXPECTED-N
           MOVE BR-UNEXPLAINED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-BR
           MOVE "MATCH" TO BH-FUNCTION
           MOVE "ZZTB1" TO BR-BANK-ACCOUNT
           MOVE WS-RECON-ID TO BR-RECON-ID
           MOVE "ZZT-BRM-DB01" TO BR-MATCH-REF
           MOVE 1 TO BR-STMT-CNT
           MOVE WS-ST-ID1 TO BR-ST-LINE-ID(1)
           MOVE 100.00 TO BR-ST-AMOUNT(1)
           MOVE 2 TO BR-BOOK-CNT
           MOVE WS-BK-ID1 TO BR-BK-LINE-ID(1)
           MOVE 60.00 TO BR-BK-AMOUNT(1)
           MOVE WS-BK-ID2 TO BR-BK-LINE-ID(2)
           MOVE 40.00 TO BR-BK-AMOUNT(2)
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE BR-MATCH-ID TO WS-MATCH-ID
           MOVE "AC-014 the grouped match against real keys is taken"
               TO TK-NAME
           IF BH-RETURN-CODE = 0
               PERFORM TK-PASS
           ELSE
               MOVE BH-MESSAGE TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "AC-014 three entries are tied into the match group"
               TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           MOVE BR-MATCHED-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-014 one statement line to two book lines is group"
               TO TK-NAME
           MOVE "GROUPED" TO TK-EXPECTED-X
           MOVE BR-MATCH-TYPE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-014 the match group is given an identity"
               TO TK-NAME
           IF WS-MATCH-ID > 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE WS-ST-ID1 TO SQLIO-INT-EDIT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(18)) FROM "
                  "fin.BankStatementLine WHERE MatchStatus = "
                  "'MATCHED' AND MatchedAmount = 100.00 AND "
                  "StatementLineId = " FUNCTION TRIM(SQLIO-INT-EDIT)
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0910-READ-ID
           MOVE "AC-014 the statement line now carries the match"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-ID-TEXT)
           PERFORM TK-ASSERT-NUM
           MOVE WS-MATCH-ID TO SQLIO-INT-EDIT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(18)) FROM "
                  "fin.BankReconMatchLine WHERE MatchId = "
                  FUNCTION TRIM(SQLIO-INT-EDIT)
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0910-READ-ID
           MOVE "AC-014 both sides of the group are written down"
               TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-ID-TEXT)
           PERFORM TK-ASSERT-NUM
           MOVE WS-BK-ID1 TO SQLIO-INT-EDIT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(18)) FROM "
                  "fin.JournalLine WHERE ReconMatchId IS NOT NULL "
                  "AND JournalLineId = "
                  FUNCTION TRIM(SQLIO-INT-EDIT)
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0910-READ-ID
           MOVE "AC-014 the book line is no longer outstanding"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-ID-TEXT)
           PERFORM TK-ASSERT-NUM

      *>   A transaction is reconciled once and once only.
           PERFORM 0950-SETUP-BR
           MOVE "MATCH" TO BH-FUNCTION
           MOVE "ZZTB1" TO BR-BANK-ACCOUNT
           MOVE WS-RECON-ID TO BR-RECON-ID
           MOVE "ZZT-BRM-DB02" TO BR-MATCH-REF
           MOVE 1 TO BR-STMT-CNT
           MOVE WS-ST-ID1 TO BR-ST-LINE-ID(1)
           MOVE 100.00 TO BR-ST-AMOUNT(1)
           MOVE 1 TO BR-BOOK-CNT
           MOVE WS-BK-ID3 TO BR-BK-LINE-ID(1)
           MOVE 100.00 TO BR-BK-AMOUNT(1)
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "AC-014 a statement line is not reconciled twice"
               TO TK-NAME
           MOVE "BNK0022" TO TK-EXPECTED-X
           MOVE BH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 0950-SETUP-BR
           MOVE "MATCH" TO BH-FUNCTION
           MOVE "ZZTB1" TO BR-BANK-ACCOUNT
           MOVE WS-RECON-ID TO BR-RECON-ID
           MOVE "ZZT-BRM-DB03" TO BR-MATCH-REF
           MOVE 1 TO BR-STMT-CNT
           MOVE WS-ST-ID2 TO BR-ST-LINE-ID(1)
           MOVE 60.00 TO BR-ST-AMOUNT(1)
           MOVE 1 TO BR-BOOK-CNT
           MOVE WS-BK-ID1 TO BR-BK-LINE-ID(1)
           MOVE 60.00 TO BR-BK-AMOUNT(1)
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "AC-014 a book entry is not reconciled twice either"
               TO TK-NAME
           MOVE "BNK0023" TO TK-EXPECTED-X
           MOVE BH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 6500 - FIN-013 A RECONCILIATION IS COMPLETED ONLY WHEN THE
      *>        STATEMENT AND THE CASH BOOK BRIDGE EXACTLY.
      *>----------------------------------------------------------------
       6500-COMPLETE-TESTS.
           MOVE "0" TO WS-ADJ-TEXT
           MOVE "ZZTB1" TO WS-ACC-TEXT
           PERFORM 0320-SET-STMT-BAL
           PERFORM 0950-SETUP-BR
           MOVE "COMPLETE" TO BH-FUNCTION
           MOVE "ZZTB1" TO BR-BANK-ACCOUNT
           MOVE WS-RECON-ID TO BR-RECON-ID
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "FIN-013 a bridged reconciliation can be completed"
               TO TK-NAME
           IF BH-RETURN-CODE < 8
               PERFORM TK-PASS
           ELSE
               MOVE BH-MESSAGE TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "FIN-013 nothing is left unexplained at completion"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE BR-UNEXPLAINED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-013 the reconciliation is reported complete"
               TO TK-NAME
           MOVE "COMPLETE" TO TK-EXPECTED-X
           MOVE BR-RECON-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE WS-RECON-ID TO SQLIO-INT-EDIT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(18)) FROM "
                  "fin.BankRecon WHERE Status = 'COMPLETE' AND "
                  "CompletedBy = 'FINCTRL' AND ReconId = "
                  FUNCTION TRIM(SQLIO-INT-EDIT)
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0910-READ-ID
           MOVE "FIN-013 the completion is recorded with its author"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-ID-TEXT)
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 7000 - FIN-014 A MATCH IS REOPENED ONLY WITH A WRITTEN
      *>        REASON, AND THE STATEMENT EVIDENCE SURVIVES IT.
      *>----------------------------------------------------------------
       7000-UNMATCH-TESTS.
           PERFORM 0950-SETUP-BR
           MOVE "UNMATCH" TO BH-FUNCTION
           MOVE "ZZTB1" TO BR-BANK-ACCOUNT
           MOVE WS-MATCH-ID TO BR-MATCH-ID
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "FIN-014 reopening a match needs a written reason"
               TO TK-NAME
           MOVE "BNK0030" TO TK-EXPECTED-X
           MOVE BH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 0950-SETUP-BR
           MOVE "UNMATCH" TO BH-FUNCTION
           MOVE "ZZTB1" TO BR-BANK-ACCOUNT
           MOVE WS-MATCH-ID TO BR-MATCH-ID
           MOVE "The receipt belonged to the deposit account"
               TO BR-REASON
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "FIN-014 a reasoned reversal is accepted" TO TK-NAME
           IF BH-RETURN-CODE < 8
               PERFORM TK-PASS
           ELSE
               MOVE BH-MESSAGE TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE WS-MATCH-ID TO SQLIO-INT-EDIT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(18)) FROM "
                  "fin.BankReconMatch WHERE Status = 'REVERSED' "
                  "AND ReverseReason IS NOT NULL AND MatchId = "
                  FUNCTION TRIM(SQLIO-INT-EDIT)
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0910-READ-ID
           MOVE "FIN-014 the reversal keeps the reason on the group"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-ID-TEXT)
           PERFORM TK-ASSERT-NUM
           MOVE WS-ST-ID1 TO SQLIO-INT-EDIT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(18)) FROM "
                  "fin.BankStatementLine WHERE MatchStatus = "
                  "'UNMATCHED' AND MatchedAmount = 0 AND "
                  "StatementLineId = " FUNCTION TRIM(SQLIO-INT-EDIT)
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0910-READ-ID
           MOVE "FIN-014 the statement line is free again, not gone"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-ID-TEXT)
           PERFORM TK-ASSERT-NUM
           MOVE WS-BK-ID1 TO SQLIO-INT-EDIT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(18)) FROM "
                  "fin.JournalLine WHERE ReconMatchId IS NULL AND "
                  "JournalLineId = " FUNCTION TRIM(SQLIO-INT-EDIT)
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0910-READ-ID
           MOVE "FIN-014 the book entry is outstanding once more"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-ID-TEXT)
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 8000 - FIN-015 CHARGES AND TRANSFERS REACH THE LEDGER.  THE
      *>        FEE IS AN EXPENSE OF ITS OWN AND THE TRANSFER MOVES
      *>        VALUE WITHOUT CREATING ANY.
      *>----------------------------------------------------------------
       8000-POSTING-TESTS.
           PERFORM 0200-ENSURE-TXN
           PERFORM 0950-SETUP-BR
           MOVE "CHARGE" TO BH-FUNCTION
           MOVE "ZZTB2" TO BR-BANK-ACCOUNT
           MOVE 3.00 TO BR-AMOUNT
           MOVE "CHARGE" TO BR-ENTRY-TYPE
           MOVE "Quarterly account maintenance fee" TO BR-NARRATIVE
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "FIN-015 a bank charge posts to the ledger" TO TK-NAME
           IF BH-RETURN-CODE < 8 AND BR-JOURNAL-NUMBER NOT = SPACES
               PERFORM TK-PASS
           ELSE
               MOVE BH-MESSAGE TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(18)) FROM "
                  "fin.Journal j JOIN fin.JournalLine l ON "
                  "l.JournalId = j.JournalId WHERE j.EventCode = "
                  "'FIN.BANKCHARGE' AND l.AccountCode = '6420' AND "
                  "l.DebitAmount = 3.00 AND j.JournalNumber = '"
                  FUNCTION TRIM(BR-JOURNAL-NUMBER) "'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0910-READ-ID
           MOVE "FIN-015 the fee is charged to bank charges, 6420"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-ID-TEXT)
           PERFORM TK-ASSERT-NUM

           PERFORM 0200-ENSURE-TXN
           PERFORM 0950-SETUP-BR
           MOVE "TRANSFER" TO BH-FUNCTION
           MOVE "ZZTB2" TO BR-BANK-ACCOUNT
           MOVE "ZZTB1" TO BR-TO-BANK-ACCOUNT
           MOVE 100.00 TO BR-AMOUNT
           MOVE "Sweep back to the current account" TO BR-NARRATIVE
           CALL "BANKREC" USING WS-BANKREC-CA
           MOVE "FIN-015 a transfer between own accounts posts"
               TO TK-NAME
           IF BH-RETURN-CODE < 8 AND BR-JOURNAL-NUMBER NOT = SPACES
               PERFORM TK-PASS
           ELSE
               MOVE BH-MESSAGE TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(SUM(l.DebitAmount) - "
                  "SUM(l.CreditAmount) AS varchar(18)) FROM "
                  "fin.Journal j JOIN fin.JournalLine l ON "
                  "l.JournalId = j.JournalId WHERE j.EventCode = "
                  "'FIN.BANKTRANSFER' AND j.JournalNumber = '"
                  FUNCTION TRIM(BR-JOURNAL-NUMBER) "'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0910-READ-ID
           MOVE "FIN-015 the transfer conserves value, it creates no"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-ID-TEXT)
           PERFORM TK-ASSERT-NUM.

       COPY TESTKITP.
