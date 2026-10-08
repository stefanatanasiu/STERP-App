      *>================================================================
      *> PROGRAM-ID : BANKREC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER (CASH BOOK)
      *> PURPOSE    : CASH BOOK ENQUIRY, BANK CHARGES AND INTEREST,
      *>              ACCOUNT TRANSFERS AND MANY TO MANY BANK
      *>              RECONCILIATION (FIN-013, FIN-014, FIN-015).
      *>              A MATCH GROUP TIES N STATEMENT ENTRIES TO M BOOK
      *>              ENTRIES.  AC-014: ONE 100.00 STATEMENT AMOUNT
      *>              MATCHES BOOK ENTRIES OF 60.00 AND 40.00; THE
      *>              60.00 CANNOT THEN BE MATCHED AGAIN.
      *>              A SUBLEDGER RECEIPT APPEARS ONCE, AS THE BANK LEG
      *>              OF ITS OWN JOURNAL - NEVER AS A SECOND, SEPARATE
      *>              BANK MOVEMENT.
      *>              A TRANSFER CONSERVES VALUE: THE FEE IS STATED
      *>              SEPARATELY AND NO TRADING REVENUE IS CREATED.
      *> CALLED BY  : frmBankRecon, CASHBOOK, .NET CLIENT
      *> INTERFACE  : CALL "BANKREC" USING WS-BANKREC-CA
      *>              COPYBOOK BANKRECCA
      *> CALLS      : PERMCHK, JNLPOST, AUDITLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-03-19 JMC CR0455 ORIGINAL - ONE TO ONE MATCHING ONLY
      *> 2000-06-12 RJH CR1203 GROUPED MATCHING FOR PAYING-IN BOOKS
      *> 2026-10-05 STERP MATCH GROUPS, REOPENING IS AUDITED
      *>================================================================
      *> REQ: FIN-013, FIN-014, FIN-015
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKREC.
       AUTHOR. J M CLEARY.
       DATE-WRITTEN. 19/03/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
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
           05  WS-AMT                  PIC S9(13)V99 VALUE 0.
           05  WS-CORREL               PIC X(36).
           05  WS-BANK-ACCOUNT         PIC X(12).
           05  WS-TO-ACCOUNT           PIC X(12).
           05  WS-MONEY-ED             PIC -(11)9.99.
       01  WS-BRIDGE-ROW.
           05  WS-G-COMPANY            PIC X(10).
           05  WS-G-CODE               PIC X(20).
           05  WS-G-NAME               PIC X(80).
           05  WS-G-CURRENCY           PIC X(4).
           05  WS-G-ACCOUNT            PIC X(12).
           05  WS-G-STMTBAL            PIC X(22).
           05  WS-G-STMTDATE           PIC X(8).
           05  WS-G-LASTREC            PIC X(8).
           05  WS-G-BOOKBAL            PIC X(22).
           05  WS-G-OUTREC             PIC X(22).
           05  WS-G-OUTPAY             PIC X(22).
           05  WS-G-UNMVAL             PIC X(22).
           05  WS-G-UNMCNT             PIC X(10).
           05  WS-G-BRIDGE             PIC X(22).
       01  WS-CHK-ROW.
           05  WS-K-COUNT              PIC X(18).
           05  WS-K-ID                 PIC X(18).
       01  WS-TO-ROW.
           05  WS-N-ACCOUNT            PIC X(12).
           05  WS-N-CURRENCY           PIC X(4).
       01  WS-EVENT                    PIC X(30).
      *>   A LINKed program must never commit a unit of work it did
      *>   not open, so the depth is read before anything is started.
       01  WS-TRAN-ROW.
           05  WS-TRAN-DEPTH           PIC X(8).
       01  WS-OWN-TXN                  PIC X VALUE "N".
       LINKAGE SECTION.
       COPY BANKRECCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           PERFORM 0200-EDIT-REQUEST THRU 0200-EXIT
           IF WS-IS-REJECTED
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "BRIDGE"   PERFORM 1000-BRIDGE THRU 1000-EXIT
               WHEN "MATCH"    PERFORM 2000-MATCH THRU 2000-EXIT
               WHEN "UNMATCH"  PERFORM 3000-UNMATCH THRU 3000-EXIT
               WHEN "CHARGE"   PERFORM 4000-CHARGE THRU 4000-EXIT
               WHEN "TRANSFER" PERFORM 5000-TRANSFER THRU 5000-EXIT
               WHEN "COMPLETE" PERFORM 6000-COMPLETE THRU 6000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.

       0100-INITIALISE.
           MOVE "N" TO WS-REJECTED
           MOVE "BANKREC" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "FIN-014" TO CA-REQ-REF
           MOVE 0 TO BR-STMT-TOTAL
           MOVE 0 TO BR-BOOK-TOTAL
           MOVE 0 TO BR-DIFFERENCE
           MOVE 0 TO BR-BOOK-BALANCE
           MOVE 0 TO BR-STMT-BALANCE
           MOVE 0 TO BR-OUT-RECEIPTS
           MOVE 0 TO BR-OUT-PAYMENTS
           MOVE 0 TO BR-BRIDGED-BALANCE
           MOVE 0 TO BR-UNEXPLAINED
           MOVE 0 TO BR-UNMATCHED-CNT
           MOVE 0 TO BR-MATCHED-CNT
           MOVE SPACES TO BR-MATCH-TYPE
           MOVE SPACES TO BR-JOURNAL-NUMBER
           MOVE SPACES TO BR-RECON-STATUS
           IF BR-AS-AT-DATE = 0
               MOVE CA-BUSINESS-DATE TO BR-AS-AT-DATE
           END-IF
           IF BR-CURRENCY = SPACES
               MOVE "GBP" TO BR-CURRENCY
           END-IF
           IF BR-STMT-CNT > 20
               MOVE 20 TO BR-STMT-CNT
           END-IF
           IF BR-BOOK-CNT > 20
               MOVE 20 TO BR-BOOK-CNT
           END-IF.
       0100-EXIT.
           EXIT.

       0200-EDIT-REQUEST.
           IF CA-COMPANY = SPACES
               MOVE "BNK0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF BR-BANK-ACCOUNT = SPACES
               MOVE "BNK0002" TO CA-REASON-CODE
               MOVE "A bank account code is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000 - THE STATEMENT TO BOOK BRIDGE
      *>----------------------------------------------------------------
       1000-BRIDGE.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC fin.usp_BankOutstanding ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE BR-BANK-ACCOUNT TO SQLIO-PARM-VAL(2)
           MOVE BR-AS-AT-DATE TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 14 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 80 TO SQLIO-COL-WIDTH(3)
           MOVE 4 TO SQLIO-COL-WIDTH(4)
           MOVE 12 TO SQLIO-COL-WIDTH(5)
           MOVE 22 TO SQLIO-COL-WIDTH(6)
           MOVE 8 TO SQLIO-COL-WIDTH(7)
           MOVE 8 TO SQLIO-COL-WIDTH(8)
           MOVE 22 TO SQLIO-COL-WIDTH(9)
           MOVE 22 TO SQLIO-COL-WIDTH(10)
           MOVE 22 TO SQLIO-COL-WIDTH(11)
           MOVE 22 TO SQLIO-COL-WIDTH(12)
           MOVE 10 TO SQLIO-COL-WIDTH(13)
           MOVE 22 TO SQLIO-COL-WIDTH(14)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BRIDGE-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "BNK0003" TO CA-REASON-CODE
               MOVE "That bank account is not held for the company"
                   TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 1000-EXIT
           END-IF
           MOVE WS-G-CURRENCY(1:3) TO BR-CURRENCY
           MOVE WS-G-ACCOUNT TO WS-BANK-ACCOUNT
           COMPUTE BR-STMT-BALANCE = FUNCTION NUMVAL(WS-G-STMTBAL)
           COMPUTE BR-BOOK-BALANCE = FUNCTION NUMVAL(WS-G-BOOKBAL)
           COMPUTE BR-OUT-RECEIPTS = FUNCTION NUMVAL(WS-G-OUTREC)
           COMPUTE BR-OUT-PAYMENTS = FUNCTION NUMVAL(WS-G-OUTPAY)
           COMPUTE BR-UNMATCHED-CNT = FUNCTION NUMVAL(WS-G-UNMCNT)
           COMPUTE BR-BRIDGED-BALANCE = FUNCTION NUMVAL(WS-G-BRIDGE)
           COMPUTE BR-UNEXPLAINED =
               BR-BOOK-BALANCE - BR-BRIDGED-BALANCE
           MOVE SPACES TO CA-MESSAGE
           MOVE BR-UNEXPLAINED TO WS-MONEY-ED
           IF BR-UNEXPLAINED = 0
               STRING "Statement and book balances bridge exactly"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE 4 TO CA-RETURN-CODE
               MOVE "BNK0010" TO CA-REASON-CODE
               STRING FUNCTION TRIM(WS-MONEY-ED)
                      " GBP of the difference is still unexplained"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000 - MATCH N STATEMENT ENTRIES TO M BOOK ENTRIES
      *>----------------------------------------------------------------
       2000-MATCH.
           MOVE "FIN.BANK.RECONCILE" TO SQLIO-PARM-VAL(40)
           PERFORM 8100-AUTHORITY THRU 8100-EXIT
           IF WS-IS-REJECTED
               GO TO 2000-EXIT
           END-IF
           IF BR-STMT-CNT = 0 OR BR-BOOK-CNT = 0
               MOVE "BNK0020" TO CA-REASON-CODE
               MOVE "A match needs at least one statement entry and on"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           MOVE 0 TO BR-STMT-TOTAL
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > BR-STMT-CNT
               ADD BR-ST-AMOUNT(WS-IDX) TO BR-STMT-TOTAL
           END-PERFORM
           MOVE 0 TO BR-BOOK-TOTAL
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > BR-BOOK-CNT
               ADD BR-BK-AMOUNT(WS-IDX) TO BR-BOOK-TOTAL
           END-PERFORM
           COMPUTE BR-DIFFERENCE = BR-STMT-TOTAL - BR-BOOK-TOTAL
      *>   Date or reference similarity alone does not reconcile
      *>   unequal totals (FIN-014): either they agree or the residue
      *>   is explained in writing.
           IF BR-DIFFERENCE NOT = 0 AND BR-NARRATIVE = SPACES
               MOVE "BNK0021" TO CA-REASON-CODE
               MOVE BR-DIFFERENCE TO WS-MONEY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "The grouped match is out by "
                      FUNCTION TRIM(WS-MONEY-ED)
                      " GBP. Unequal totals need a written explanation"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF BR-DIFFERENCE = 0
               IF BR-STMT-CNT = 1 AND BR-BOOK-CNT = 1
                   MOVE "EXACT" TO BR-MATCH-TYPE
               ELSE
                   MOVE "GROUPED" TO BR-MATCH-TYPE
               END-IF
           ELSE
               MOVE "EXPLAINED" TO BR-MATCH-TYPE
           END-IF
           PERFORM 2100-CHECK-FREE THRU 2100-EXIT
           IF WS-IS-REJECTED
               GO TO 2000-EXIT
           END-IF
           IF BR-MATCH-REF = SPACES
               MOVE SPACES TO BR-MATCH-REF
               STRING "BRM" FUNCTION TRIM(CA-COMPANY) "-"
                      BR-AS-AT-DATE "-" BR-STMT-CNT BR-BOOK-CNT
                   DELIMITED BY SIZE INTO BR-MATCH-REF
           END-IF
           PERFORM 9500-BEGIN-WORK THRU 9500-EXIT
           PERFORM 2200-WRITE-GROUP THRU 2200-EXIT
           IF CA-FAILED
               PERFORM 9520-ROLLB-WORK THRU 9520-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 2300-ADD-STATEMENT THRU 2300-EXIT
               VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > BR-STMT-CNT OR CA-FAILED
           PERFORM 2400-ADD-BOOK THRU 2400-EXIT
               VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > BR-BOOK-CNT OR CA-FAILED
           IF CA-FAILED
               PERFORM 9520-ROLLB-WORK THRU 9520-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 9510-COMMIT-WORK THRU 9510-EXIT
           COMPUTE BR-MATCHED-CNT = BR-STMT-CNT + BR-BOOK-CNT
           PERFORM 8200-AUDIT THRU 8200-EXIT
           MOVE SPACES TO CA-MESSAGE
           MOVE BR-STMT-TOTAL TO WS-MONEY-ED
           STRING "Matched " FUNCTION TRIM(WS-MONEY-ED)
                  " across " BR-STMT-CNT " statement and "
                  BR-BOOK-CNT " book entries"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

      *> A transaction cannot be reconciled twice (FIN-014, AC-014).
       2100-CHECK-FREE.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > BR-STMT-CNT OR WS-IS-REJECTED
               MOVE SPACES TO SQLIO-STMT
               STRING "SELECT CAST(COUNT(*) AS varchar(10)), '0' "
                      "FROM fin.BankReconMatchLine ml JOIN "
                      "fin.BankReconMatch m ON m.MatchId = ml.MatchId "
                      "AND m.Status = 'MATCHED' WHERE "
                      "ml.StatementLineId = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 1 TO SQLIO-PARM-CNT
               MOVE BR-ST-LINE-ID(WS-IDX) TO SQLIO-INT-EDIT
               MOVE FUNCTION TRIM(SQLIO-INT-EDIT)
                   TO SQLIO-PARM-VAL(1)
               MOVE "N" TO SQLIO-PARM-NULL(1)
               PERFORM 2150-RUN-COUNT THRU 2150-EXIT
               IF WS-K-COUNT NOT = SPACES
                   IF FUNCTION NUMVAL(WS-K-COUNT) > 0
                       MOVE "BNK0022" TO CA-REASON-CODE
                       MOVE "That statement entry is already reconcile"
                           TO CA-MESSAGE
                       PERFORM 9000-REJECT THRU 9000-EXIT
                   END-IF
               END-IF
           END-PERFORM
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > BR-BOOK-CNT OR WS-IS-REJECTED
               MOVE SPACES TO SQLIO-STMT
               STRING "SELECT CAST(COUNT(*) AS varchar(10)), '0' "
                      "FROM fin.BankReconMatchLine ml JOIN "
                      "fin.BankReconMatch m ON m.MatchId = ml.MatchId "
                      "AND m.Status = 'MATCHED' WHERE "
                      "ml.JournalLineId = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 1 TO SQLIO-PARM-CNT
               MOVE BR-BK-LINE-ID(WS-IDX) TO SQLIO-INT-EDIT
               MOVE FUNCTION TRIM(SQLIO-INT-EDIT)
                   TO SQLIO-PARM-VAL(1)
               MOVE "N" TO SQLIO-PARM-NULL(1)
               PERFORM 2150-RUN-COUNT THRU 2150-EXIT
               IF WS-K-COUNT NOT = SPACES
                   IF FUNCTION NUMVAL(WS-K-COUNT) > 0
                       MOVE "BNK0023" TO CA-REASON-CODE
                       MOVE "That book entry is already reconciled in "
                           TO CA-MESSAGE
                       PERFORM 9000-REJECT THRU 9000-EXIT
                   END-IF
               END-IF
           END-PERFORM.
       2100-EXIT.
           EXIT.

       2150-RUN-COUNT.
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 18 TO SQLIO-COL-WIDTH(2)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CHK-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       2150-EXIT.
           EXIT.

       2200-WRITE-GROUP.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.BankReconMatch (CompanyCode, "
                  "BankAccountCode, ReconId, MatchRef, MatchType, "
                  "StatementTotal, BookTotal, DifferenceAmount, "
                  "CurrencyCode, MatchDate, Explanation, CreatedBy) "
                  "OUTPUT inserted.MatchId, '0' VALUES (?, ?, "
                  "NULLIF(CAST(? AS int),0), ?, ?, ?, ?, ?, ?, ?, "
                  "NULLIF(?,''), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 12 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE BR-BANK-ACCOUNT TO SQLIO-PARM-VAL(2)
           MOVE BR-RECON-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE BR-MATCH-REF TO SQLIO-PARM-VAL(4)
           MOVE BR-MATCH-TYPE TO SQLIO-PARM-VAL(5)
           MOVE BR-STMT-TOTAL TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE BR-BOOK-TOTAL TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE BR-DIFFERENCE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE BR-CURRENCY TO SQLIO-PARM-VAL(9)
           MOVE BR-AS-AT-DATE TO SQLIO-PARM-VAL(10)
           MOVE BR-NARRATIVE TO SQLIO-PARM-VAL(11)
           MOVE CA-USER TO SQLIO-PARM-VAL(12)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 18 TO SQLIO-COL-WIDTH(1)
           MOVE 18 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CHK-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2200-EXIT
           END-IF
           COMPUTE BR-MATCH-ID = FUNCTION NUMVAL(WS-K-COUNT).
       2200-EXIT.
           EXIT.

       2300-ADD-STATEMENT.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.BankReconMatchLine (MatchId, "
                  "CompanyCode, SideType, StatementLineId, Amount, "
                  "CurrencyCode) VALUES (?, ?, 'STATEMENT', ?, ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE BR-MATCH-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE BR-ST-LINE-ID(WS-IDX) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE BR-ST-AMOUNT(WS-IDX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE BR-CURRENCY TO SQLIO-PARM-VAL(5)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CHK-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.BankStatementLine SET MatchStatus = "
                  "'MATCHED', MatchedAmount = Amount, ReconId = "
                  "COALESCE(NULLIF(CAST(? AS int),0), ReconId), "
                  "MatchedAt = "
                  "SYSDATETIME(), MatchedBy = ? "
                  "WHERE StatementLineId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE BR-RECON-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE BR-ST-LINE-ID(WS-IDX) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CHK-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       2300-EXIT.
           EXIT.

       2400-ADD-BOOK.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.BankReconMatchLine (MatchId, "
                  "CompanyCode, SideType, JournalLineId, Amount, "
                  "CurrencyCode) VALUES (?, ?, 'BOOK', ?, ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE BR-MATCH-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE BR-BK-LINE-ID(WS-IDX) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE BR-BK-AMOUNT(WS-IDX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE BR-CURRENCY TO SQLIO-PARM-VAL(5)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CHK-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2400-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.JournalLine SET ReconMatchId = ? "
                  "WHERE JournalLineId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE BR-MATCH-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE BR-BK-LINE-ID(WS-IDX) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CHK-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       2400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000 - REVERSE A MATCH GROUP.  THE STATEMENT EVIDENCE STAYS.
      *>----------------------------------------------------------------
       3000-UNMATCH.
           MOVE "FIN.BANK.RECONCILE" TO SQLIO-PARM-VAL(40)
           PERFORM 8100-AUTHORITY THRU 8100-EXIT
           IF WS-IS-REJECTED
               GO TO 3000-EXIT
           END-IF
           IF BR-REASON = SPACES
               MOVE "BNK0030" TO CA-REASON-CODE
               MOVE "Reopening a match needs a recorded reason"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
      *>   The caller may already hold a unit of work, so open and
      *>   close only what this run opened itself (9500 / 9510 / 9520).
           PERFORM 9500-BEGIN-WORK THRU 9500-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.BankStatementLine SET MatchStatus = "
                  "'UNMATCHED', MatchedAmount = 0, MatchedAt = NULL, "
                  "MatchedBy = NULL WHERE StatementLineId IN (SELECT "
                  "ml.StatementLineId FROM fin.BankReconMatchLine ml "
                  "WHERE ml.MatchId = ? AND ml.SideType = 'STATEMENT')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 3100-RUN-ONE THRU 3100-EXIT
           IF CA-FAILED
               PERFORM 9520-ROLLB-WORK THRU 9520-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.JournalLine SET ReconMatchId = NULL "
                  "WHERE JournalLineId IN (SELECT ml.JournalLineId "
                  "FROM fin.BankReconMatchLine ml WHERE ml.MatchId = ?"
                  " AND ml.SideType = 'BOOK')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 3100-RUN-ONE THRU 3100-EXIT
           IF CA-FAILED
               PERFORM 9520-ROLLB-WORK THRU 9520-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.BankReconMatch SET Status = 'REVERSED', "
                  "ReversedBy = ?, ReversedAt = SYSDATETIME(), "
                  "ReverseReason = ? WHERE MatchId = ? "
                  "AND Status = 'MATCHED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE BR-REASON TO SQLIO-PARM-VAL(2)
           MOVE BR-MATCH-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CHK-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               PERFORM 9520-ROLLB-WORK THRU 9520-EXIT
               GO TO 3000-EXIT
           END-IF
           PERFORM 9510-COMMIT-WORK THRU 9510-EXIT
           PERFORM 8200-AUDIT THRU 8200-EXIT
           MOVE "The match is reopened and audited. The statement evid"
               TO CA-MESSAGE.
       3000-EXIT.
           EXIT.

       3100-RUN-ONE.
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE BR-MATCH-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CHK-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       3100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000 - BANK CHARGE OR INTEREST.  MOST UK BANK CHARGES ARE
      *>        EXEMPT FROM VAT, SO NO INPUT TAX IS TAKEN UNLESS A
      *>        VALID VAT INVOICE SHOWS IT (FIN-015).
      *>----------------------------------------------------------------
       4000-CHARGE.
           PERFORM 1000-BRIDGE THRU 1000-EXIT
           IF CA-RETURN-CODE > 4
               GO TO 4000-EXIT
           END-IF
           IF BR-AMOUNT NOT > 0
               MOVE "BNK0040" TO CA-REASON-CODE
               MOVE "A bank charge must have a positive value"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF BR-EXPENSE-ACCOUNT = SPACES
               MOVE "6420" TO BR-EXPENSE-ACCOUNT
           END-IF
           IF BR-DIM-CC = SPACES
               MOVE "ADMIN" TO BR-DIM-CC
           END-IF
           MOVE SPACES TO WS-CORREL
           STRING "BCH-" FUNCTION TRIM(CA-COMPANY) "-"
                  FUNCTION TRIM(BR-BANK-ACCOUNT) "-" BR-AS-AT-DATE
               DELIMITED BY SIZE INTO WS-CORREL
           MOVE 1 TO WS-LINE-NO
           MOVE BR-EXPENSE-ACCOUNT TO WS-TO-ACCOUNT
           MOVE BR-AMOUNT TO WS-AMT
           PERFORM 7000-STAGE-DEBIT THRU 7000-EXIT
           MOVE 2 TO WS-LINE-NO
           MOVE WS-BANK-ACCOUNT TO WS-TO-ACCOUNT
           PERFORM 7100-STAGE-CREDIT THRU 7100-EXIT
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           MOVE "FIN.BANKCHARGE" TO WS-EVENT
           PERFORM 7200-POST-IT THRU 7200-EXIT.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000 - TRANSFER BETWEEN TWO BANK ACCOUNTS.  VALUE IS
      *>        CONSERVED; ANY FEE IS A SEPARATE EXPENSE LINE.
      *>----------------------------------------------------------------
       5000-TRANSFER.
           PERFORM 1000-BRIDGE THRU 1000-EXIT
           IF CA-RETURN-CODE > 4
               GO TO 5000-EXIT
           END-IF
           IF BR-TO-BANK-ACCOUNT = SPACES
               MOVE "BNK0050" TO CA-REASON-CODE
               MOVE "The receiving bank account is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           IF BR-TO-BANK-ACCOUNT = BR-BANK-ACCOUNT
               MOVE "BNK0051" TO CA-REASON-CODE
               MOVE "A transfer needs two different bank accounts"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           IF BR-AMOUNT NOT > 0
               MOVE "BNK0052" TO CA-REASON-CODE
               MOVE "A transfer must have a positive value"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           PERFORM 5100-READ-TO-ACCOUNT THRU 5100-EXIT
           IF CA-FAILED
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO WS-CORREL
           STRING "BTR-" FUNCTION TRIM(CA-COMPANY) "-"
                  FUNCTION TRIM(BR-BANK-ACCOUNT) "-"
                  FUNCTION TRIM(BR-TO-BANK-ACCOUNT) "-" BR-AS-AT-DATE
               DELIMITED BY SIZE INTO WS-CORREL
           MOVE 1 TO WS-LINE-NO
           MOVE BR-AMOUNT TO WS-AMT
           PERFORM 7000-STAGE-DEBIT THRU 7000-EXIT
           MOVE 2 TO WS-LINE-NO
           MOVE WS-BANK-ACCOUNT TO WS-TO-ACCOUNT
           PERFORM 7100-STAGE-CREDIT THRU 7100-EXIT
           IF CA-FAILED
               GO TO 5000-EXIT
           END-IF
           MOVE "FIN.BANKTRANSFER" TO WS-EVENT
           PERFORM 7200-POST-IT THRU 7200-EXIT
           IF CA-RETURN-CODE < 8
               MOVE SPACES TO CA-MESSAGE
               MOVE BR-AMOUNT TO WS-MONEY-ED
               STRING "Transferred " FUNCTION TRIM(WS-MONEY-ED) " "
                      FUNCTION TRIM(BR-CURRENCY) " as journal "
                      FUNCTION TRIM(BR-JOURNAL-NUMBER)
                      ". No trading revenue arises on a transfer"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       5000-EXIT.
           EXIT.

       5100-READ-TO-ACCOUNT.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT b.AccountCode, b.CurrencyCode "
                  "FROM fin.BankAccount b WHERE b.CompanyCode = ? "
                  "AND b.BankAccountCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE BR-TO-BANK-ACCOUNT TO SQLIO-PARM-VAL(2)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 4 TO SQLIO-COL-WIDTH(2)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TO-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 5100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "BNK0053" TO CA-REASON-CODE
               MOVE "The receiving bank account is not held"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5100-EXIT
           END-IF
           IF WS-N-CURRENCY(1:3) NOT = BR-CURRENCY
               MOVE "BNK0054" TO CA-REASON-CODE
               MOVE "A cross currency transfer needs an approved rate "
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5100-EXIT
           END-IF
           MOVE WS-N-ACCOUNT TO WS-TO-ACCOUNT.
       5100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 6000 - COMPLETE A RECONCILIATION ONLY WHEN IT BRIDGES
      *>----------------------------------------------------------------
       6000-COMPLETE.
           MOVE "FIN.BANK.RECONCILE" TO SQLIO-PARM-VAL(40)
           PERFORM 8100-AUTHORITY THRU 8100-EXIT
           IF WS-IS-REJECTED
               GO TO 6000-EXIT
           END-IF
           PERFORM 1000-BRIDGE THRU 1000-EXIT
           IF CA-RETURN-CODE > 4
               GO TO 6000-EXIT
           END-IF
           IF BR-UNEXPLAINED NOT = 0
               MOVE "BNK0060" TO CA-REASON-CODE
               MOVE BR-UNEXPLAINED TO WS-MONEY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "The reconciliation cannot be completed while "
                      FUNCTION TRIM(WS-MONEY-ED)
                      " GBP of difference is unexplained"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 6000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.BankRecon SET Status = 'COMPLETE', "
                  "BookBalance = ?, OutstandingReceipts = ?, "
                  "OutstandingPayments = ?, UnexplainedDifference = 0,"
                  " CompletedBy = ?, CompletedAt = SYSDATETIME() "
                  "WHERE ReconId = ? AND CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE BR-BOOK-BALANCE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE BR-OUT-RECEIPTS TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE BR-OUT-PAYMENTS TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE BR-RECON-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(6)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CHK-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 6000-EXIT
           END-IF
           MOVE "COMPLETE" TO BR-RECON-STATUS
           PERFORM 8200-AUDIT THRU 8200-EXIT
           MOVE "Reconciliation complete. The matched entries and the "
               TO CA-MESSAGE.
       6000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 7000 - SHARED STAGING AND POSTING
      *>----------------------------------------------------------------
       7000-STAGE-DEBIT.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, CurrencyCode, DimCostCentre, "
                  "Narrative, BankAccountCode, CreatedBy) "
                  "SELECT ?, ?, ?, ?, ?, 0, ?, NULLIF(?,''), ?, "
                  "NULLIF(?,''), ? WHERE NOT EXISTS (SELECT 1 FROM "
                  "fin.JournalStage s WHERE s.CorrelationId = ? "
                  "AND s.LineNum = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 7050-BIND-LINE THRU 7050-EXIT.
       7000-EXIT.
           EXIT.

       7100-STAGE-CREDIT.
      *>   The credit side binds exactly the same twelve parameters as
      *>   the debit side (7050), so the cost centre marker must stay
      *>   in the list even though line two never carries one.  With a
      *>   literal NULL here every parameter after it slid one place
      *>   and the correlation id reached s.LineNum, which failed the
      *>   whole posting under XACT_ABORT.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, CurrencyCode, DimCostCentre, "
                  "Narrative, BankAccountCode, CreatedBy) "
                  "SELECT ?, ?, ?, ?, 0, ?, ?, NULLIF(?,''), ?, "
                  "NULLIF(?,''), ? WHERE NOT EXISTS (SELECT 1 FROM "
                  "fin.JournalStage s WHERE s.CorrelationId = ? "
                  "AND s.LineNum = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 7050-BIND-LINE THRU 7050-EXIT.
       7100-EXIT.
           EXIT.

       7050-BIND-LINE.
           MOVE 11 TO SQLIO-PARM-CNT
           MOVE WS-CORREL TO SQLIO-PARM-VAL(1)
           MOVE WS-LINE-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE WS-TO-ACCOUNT TO SQLIO-PARM-VAL(4)
           MOVE WS-AMT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE BR-CURRENCY TO SQLIO-PARM-VAL(6)
           IF WS-LINE-NO = 1
               MOVE BR-DIM-CC TO SQLIO-PARM-VAL(7)
           ELSE
               MOVE SPACES TO SQLIO-PARM-VAL(7)
           END-IF
           MOVE BR-NARRATIVE TO SQLIO-PARM-VAL(8)
           IF WS-TO-ACCOUNT = WS-BANK-ACCOUNT
               MOVE BR-BANK-ACCOUNT TO SQLIO-PARM-VAL(9)
           ELSE
               MOVE SPACES TO SQLIO-PARM-VAL(9)
           END-IF
           MOVE CA-USER TO SQLIO-PARM-VAL(10)
           MOVE WS-CORREL TO SQLIO-PARM-VAL(11)
           MOVE 12 TO SQLIO-PARM-CNT
           MOVE WS-LINE-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(12)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CHK-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       7050-EXIT.
           EXIT.

       7200-POST-IT.
           INITIALIZE WS-JNLPOST-CA
           MOVE "POST" TO JH-FUNCTION
           MOVE CA-COMPANY TO JH-COMPANY
           MOVE CA-SITE TO JH-SITE
           MOVE CA-USER TO JH-USER
           MOVE CA-BUSINESS-DATE TO JH-BUSINESS-DATE
           MOVE WS-CORREL TO JH-CORRELATION-ID
           MOVE BR-AS-AT-DATE TO JP-POSTING-DATE
           MOVE BR-AS-AT-DATE TO JP-DOCUMENT-DATE
           MOVE BR-CURRENCY TO JP-CURRENCY
           MOVE "FIN" TO JP-SOURCE-MODULE
           MOVE BR-BANK-ACCOUNT TO JP-SOURCE-REF
           MOVE WS-EVENT TO JP-EVENT-CODE
           MOVE "CASH" TO JP-JOURNAL-TYPE
           MOVE "JNL" TO JP-SERIES-CODE
           MOVE BR-NARRATIVE TO JP-DESCRIPTION
           IF JP-DESCRIPTION = SPACES
               MOVE "Bank movement recorded from the statement"
                   TO JP-DESCRIPTION
           END-IF
           MOVE SPACES TO JP-EVIDENCE-REF
           STRING "fin.BankStatementLine/"
                  FUNCTION TRIM(BR-STATEMENT-REF)
               DELIMITED BY SIZE INTO JP-EVIDENCE-REF
           CALL "JNLPOST" USING WS-JNLPOST-CA
           MOVE JP-NEW-JOURNAL-NO TO BR-JOURNAL-NUMBER
           IF JH-RETURN-CODE > 4
               MOVE JH-REASON-CODE TO CA-REASON-CODE
               MOVE JH-MESSAGE TO CA-MESSAGE
               MOVE JH-RETURN-CODE TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
           ELSE
               MOVE JH-MESSAGE TO CA-MESSAGE
           END-IF.
       7200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-NOT-NULL.
           PERFORM VARYING WS-LINE-NO FROM 1 BY 1
               UNTIL WS-LINE-NO > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-LINE-NO)
           END-PERFORM.
       8000-EXIT.
           EXIT.

       8100-AUTHORITY.
           INITIALIZE WS-PERMCHK-CA
           MOVE "CHECK" TO KH-FUNCTION
           MOVE CA-COMPANY TO KH-COMPANY
           MOVE CA-SITE TO KH-SITE
           MOVE CA-USER TO KH-USER
           MOVE CA-BUSINESS-DATE TO KH-BUSINESS-DATE
           MOVE "FIN.BANK.RECONCILE" TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERMCHK-CA
           IF KH-RETURN-CODE > 4
               MOVE KH-REASON-CODE TO CA-REASON-CODE
               MOVE KH-MESSAGE TO CA-MESSAGE
               MOVE KH-RETURN-CODE TO CA-RETURN-CODE
               MOVE "Y" TO WS-REJECTED
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       8100-EXIT.
           EXIT.

       8200-AUDIT.
           INITIALIZE WS-AUDITLOG-CA
           MOVE "WRITE" TO AH-FUNCTION
           MOVE CA-COMPANY TO AH-COMPANY
           MOVE CA-USER TO AH-USER
           MOVE CA-BUSINESS-DATE TO AH-BUSINESS-DATE
           MOVE "fin.BankReconMatch" TO AU-ENTITY-TYPE
           MOVE BR-MATCH-REF TO AU-ENTITY-KEY
           MOVE CA-FUNCTION TO AU-ACTION
           MOVE BR-STMT-TOTAL TO WS-MONEY-ED
           MOVE WS-MONEY-ED TO AU-NEW-VALUE
           MOVE BR-REASON TO AU-REASON
           MOVE "COBOL" TO AU-SOURCE
           CALL "AUDITLOG" USING WS-AUDITLOG-CA.
       8200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 9500 - TRANSACTION CONTROL.  BANKREC IS LINKED TO BY CLIENTS
      *>        AND BY TEST HARNESSES THAT ALREADY HOLD A UNIT OF
      *>        WORK, SO IT ONLY COMMITS WHAT IT OPENED ITSELF.
      *>----------------------------------------------------------------
       9500-BEGIN-WORK.
           MOVE "N" TO WS-OWN-TXN
           MOVE SPACES TO SQLIO-STMT
           MOVE "SELECT CAST(@@TRANCOUNT AS varchar(8))"
               TO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-COL-WIDTH(1)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TRAN-ROW
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "0" TO WS-TRAN-DEPTH
           END-IF
           IF FUNCTION NUMVAL(WS-TRAN-DEPTH) = 0
               MOVE "Y" TO WS-OWN-TXN
               MOVE "BEGN" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-TRAN-ROW
           END-IF.
       9500-EXIT.
           EXIT.

       9510-COMMIT-WORK.
           IF WS-OWN-TXN = "Y"
               MOVE "COMT" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-TRAN-ROW
               MOVE "N" TO WS-OWN-TXN
           END-IF.
       9510-EXIT.
           EXIT.

       9520-ROLLB-WORK.
           IF WS-OWN-TXN = "Y"
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-TRAN-ROW
               MOVE "N" TO WS-OWN-TXN
           END-IF.
       9520-EXIT.
           EXIT.

       9000-REJECT.
           MOVE "Y" TO WS-REJECTED
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE "BNK0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "BANKREC does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in BANKREC" TO CA-MESSAGE
           END-IF
           MOVE 12 TO CA-RETURN-CODE
           MOVE "Y" TO WS-REJECTED
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
