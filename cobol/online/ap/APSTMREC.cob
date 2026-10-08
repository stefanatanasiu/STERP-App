      *>================================================================
      *> PROGRAM-ID : APSTMREC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : SUPPLIER STATEMENT RECONCILIATION.  EACH
      *>              STATEMENT LINE IS COMPARED WITH THE LEDGER ON
      *>              THE NORMALISED DOCUMENT REFERENCE AND
      *>              CLASSIFIED: MATCHED, MISSING FROM OUR LEDGER,
      *>              MISSING FROM THEIRS, A TIMING DIFFERENCE OR
      *>              DISPUTED.
      *>              AN UNMATCHED LINE IS A REVIEW ITEM WITH AN
      *>              OWNER - IT IS NEVER AUTHORITY TO POST OR TO PAY
      *>              (AP-019).
      *> CALLED BY  : .NET STATEMENT RECONCILIATION FORM, APCLOSE
      *> INTERFACE  : CALL "APSTMREC" USING WS-APSTMREC-CA
      *>              COPYBOOK APSTMRECCA
      *> CALLS      : SQLIO, APSVC
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-10-04 DJM  ORIGINAL STATEMENT RECONCILIATION
      *> 2009-07-13 AJS  CR2451 NORMALISED REFERENCE MATCHING
      *> 2026-10-05 STERP OWNED EXCEPTIONS FOR EVERY DIFFERENCE
      *> 2026-10-07 STERP RECONCILE WAS NINE CHARACTERS AND COULD
      *>                 NEVER MATCH THE EIGHT BYTE CA-FUNCTION
      *>================================================================
      *> REQ: AP-019, AP-022
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APSTMREC.
       AUTHOR. D J MILLER.
       DATE-WRITTEN. 04/10/1994.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APSVCARE.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-MATCH-ROW.
           05  WS-M-ID                 PIC X(24).
           05  WS-M-RESIDUAL           PIC X(24).
           05  WS-M-ORIGINAL           PIC X(24).
           05  WS-M-STATUS             PIC X(12).
           05  WS-M-BLOCKED            PIC X(1).
       01  WS-BAL-ROW.
           05  WS-B-LEDGER             PIC X(24).
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(40).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-MONEY-ED             PIC -(11)9.99.
           05  WS-COUNT-ED             PIC Z(5)9.
       LINKAGE SECTION.
       COPY APSTMRECCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "LOAD"      PERFORM 2000-LOAD THRU 2000-EXIT
               WHEN "RECONCIL" PERFORM 3000-RECONCILE THRU 3000-EXIT
               WHEN "RESOLVE"   PERFORM 5000-RESOLVE THRU 5000-EXIT
               WHEN OTHER       PERFORM 9100-BAD-FUNC THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "APSTMREC" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "AP-019" TO CA-REQ-REF
           MOVE 0 TO SR-EXCEPTION-CNT SR-MATCHED-CNT
           MOVE 0 TO SR-TIMING-DIFF SR-MISSING-OURS
           MOVE 0 TO SR-MISSING-THEIRS SR-DISPUTED-AMT
           MOVE 0 TO SR-EXPLAINED SR-UNEXPLAINED
           MOVE FUNCTION UPPER-CASE(CA-COMPANY) TO CA-COMPANY
           IF SR-CURRENCY = SPACES
               MOVE "GBP" TO SR-CURRENCY
           END-IF
           IF SR-STATEMENT-DATE = 0
               MOVE CA-BUSINESS-DATE TO SR-STATEMENT-DATE
           END-IF
           IF SR-LINE-CNT > 40
               MOVE 40 TO SR-LINE-CNT
           END-IF
           IF CA-COMPANY = SPACES OR SR-SUPPLIER = SPACES
               MOVE "APQ0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A company and a supplier are required"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2000-LOAD SECTION.
       2000-START.
           INITIALIZE WS-APSVC
           MOVE "PERM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "AP.STATEMENT.RECONCILE" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-FLAG-1 NOT = "Y"
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APQ0002" TO CA-REASON-CODE
               MOVE SVC-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; "
                  "INSERT ap.SupplierStatement (CompanyCode, "
                  "SupplierCode, StatementRef, StatementDate, "
                  "CurrencyCode, StatementBalance, Status, Note, "
                  "CreatedBy) SELECT ?, ?, ?, CAST(? AS date), ?, ?, "
                  "'OPEN', NULLIF(?,''), ? WHERE NOT EXISTS (SELECT 1"
                  " FROM ap.SupplierStatement s WHERE s.CompanyCode ="
                  " ? AND s.SupplierCode = ? AND s.StatementRef = ?);"
                  " SELECT CAST(ISNULL(SCOPE_IDENTITY(), (SELECT "
                  "StatementId FROM ap.SupplierStatement WHERE "
                  "CompanyCode = ? AND SupplierCode = ? AND "
                  "StatementRef = ?)) AS varchar(24))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SR-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE SR-STATEMENT-REF TO SQLIO-PARM-VAL(3)
           MOVE SR-STATEMENT-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE SR-CURRENCY TO SQLIO-PARM-VAL(5)
           MOVE SR-STATEMENT-BAL TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE SR-NOTE(1:200) TO SQLIO-PARM-VAL(7)
           MOVE CA-USER TO SQLIO-PARM-VAL(8)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(9)
           MOVE SR-SUPPLIER TO SQLIO-PARM-VAL(10)
           MOVE SR-STATEMENT-REF TO SQLIO-PARM-VAL(11)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(12)
           MOVE SR-SUPPLIER TO SQLIO-PARM-VAL(13)
           MOVE SR-STATEMENT-REF TO SQLIO-PARM-VAL(14)
           MOVE 14 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 14
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2000-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-C-VALUE)
           MOVE WS-NUM TO SR-STATEMENT-ID
           MOVE "OPEN" TO SR-STATUS
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > SR-LINE-CNT OR WS-FATAL
               PERFORM 2100-WRITE-LINE THRU 2100-EXIT
           END-PERFORM
           MOVE SPACES TO CA-MESSAGE
           MOVE SR-LINE-CNT TO WS-COUNT-ED
           STRING "Statement " FUNCTION TRIM(SR-STATEMENT-REF)
                  " loaded with " FUNCTION TRIM(WS-COUNT-ED)
                  " line(s)" DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

       2100-WRITE-LINE SECTION.
       2100-START.
           INITIALIZE WS-APSVC
           MOVE "NORMREF" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE SR-L-REF(WS-I) TO SVC-TEXT-3
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-TEXT-OUT(1:40) TO SR-L-NORMALISED(WS-I)
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ap.SupplierStatementLine (StatementId, "
                  "LineNum, SupplierDocRef, DocumentDate, DueDate, "
                  "Amount, MatchStatus) SELECT ?, ?, ?, "
                  "CASE WHEN ? = 0 THEN NULL ELSE CAST(? AS date) "
                  "END, CASE WHEN ? = 0 THEN NULL ELSE "
                  "CAST(? AS date) END, ?, 'UNMATCHED' "
                  "WHERE NOT EXISTS (SELECT 1 FROM "
                  "ap.SupplierStatementLine x WHERE x.StatementId = ?"
                  " AND x.LineNum = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SR-STATEMENT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE WS-I TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE SR-L-REF(WS-I) TO SQLIO-PARM-VAL(3)
           MOVE SR-L-DATE(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE SR-L-DUE(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE SR-L-AMOUNT(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE SR-STATEMENT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE WS-I TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE 10 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 10
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> EACH LINE IS LOOKED UP ON THE NORMALISED REFERENCE.  THE
      *> CLASSIFICATION IS KEPT CONSERVATIVE: A LINE WE CANNOT FIND IS
      *> MISSINGOURS AND IS OWNED FOR REVIEW, NEVER POSTED.
      *>----------------------------------------------------------------
       3000-RECONCILE SECTION.
       3000-START.
           PERFORM 3100-LEDGER-BALANCE THRU 3100-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > SR-LINE-CNT OR WS-FATAL
               PERFORM 3200-ONE-LINE THRU 3200-EXIT
           END-PERFORM
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           COMPUTE SR-DIFFERENCE =
               SR-STATEMENT-BAL - SR-LEDGER-BAL
           COMPUTE SR-EXPLAINED =
               SR-TIMING-DIFF + SR-MISSING-OURS
                   + SR-MISSING-THEIRS + SR-DISPUTED-AMT
           COMPUTE SR-UNEXPLAINED = SR-DIFFERENCE - SR-EXPLAINED
           PERFORM 3300-SAVE-TOTALS THRU 3300-EXIT
           IF SR-EXCEPTION-CNT > 0
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APQ0010" TO CA-REASON-CODE
               MOVE SR-EXCEPTION-CNT TO WS-COUNT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-COUNT-ED)
                      " statement line(s) need review. An unmatched "
                      "line is a review item, not authority to post "
                      "or pay" DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           ELSE
               MOVE 0 TO CA-RETURN-CODE
               MOVE SR-DIFFERENCE TO WS-MONEY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Statement reconciled; difference GBP "
                      FUNCTION TRIM(WS-MONEY-ED)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       3000-EXIT.
           EXIT.

       3100-LEDGER-BALANCE SECTION.
       3100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(ResidualAmount),0) AS "
                  "varchar(24)) FROM ap.OpenItem WHERE CompanyCode = "
                  "? AND SupplierCode = ? AND SettlementStatus IN "
                  "('OPEN','PARTIAL') AND PostingDate <= "
                  "CAST(? AS date)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SR-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE SR-STATEMENT-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BAL-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3100-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               COMPUTE SR-LEDGER-BAL = FUNCTION NUMVAL(WS-B-LEDGER)
           END-IF.
       3100-EXIT.
           EXIT.

       3200-ONE-LINE SECTION.
       3200-START.
           INITIALIZE WS-APSVC
           MOVE "NORMREF" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE SR-L-REF(WS-I) TO SVC-TEXT-3
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-TEXT-OUT(1:40) TO SR-L-NORMALISED(WS-I)
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(o.OpenItemId AS varchar(24)), "
                  "CAST(o.ResidualAmount AS varchar(24)), "
                  "CAST(o.OriginalAmount AS varchar(24)), "
                  "o.SettlementStatus, CAST(o.IsBlocked AS char(1)) "
                  "FROM ap.OpenItem o LEFT JOIN ap.PurchaseInvoice p "
                  "ON p.PurchaseInvoiceId = o.PurchaseInvoiceId "
                  "WHERE o.CompanyCode = ? AND o.SupplierCode = ? AND"
                  " (ISNULL(p.SupplierDocRefNormalised,'') = ? OR "
                  "ISNULL(o.SupplierDocRef,'') = ? OR "
                  "o.DocumentNumber = ?) ORDER BY o.OpenItemId DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SR-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE SR-L-NORMALISED(WS-I) TO SQLIO-PARM-VAL(3)
           MOVE SR-L-REF(WS-I) TO SQLIO-PARM-VAL(4)
           MOVE SR-L-REF(WS-I) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 1 TO SQLIO-COL-WIDTH(5)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-MATCH-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3200-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "MISSINGOURS" TO SR-L-STATUS(WS-I)
               MOVE 0 TO SR-L-OURS(WS-I)
               MOVE SR-L-AMOUNT(WS-I) TO SR-L-DIFF(WS-I)
               ADD SR-L-AMOUNT(WS-I) TO SR-MISSING-OURS
               ADD 1 TO SR-EXCEPTION-CNT
               MOVE SPACES TO SR-L-NOTE(WS-I)
               STRING "Not on our ledger; review before any posting"
                   DELIMITED BY SIZE INTO SR-L-NOTE(WS-I)
               PERFORM 3400-RAISE THRU 3400-EXIT
               GO TO 3200-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-M-ID)
           MOVE WS-NUM TO SR-L-ITEM-ID(WS-I)
           COMPUTE SR-L-OURS(WS-I) =
               FUNCTION NUMVAL(WS-M-RESIDUAL)
           COMPUTE SR-L-DIFF(WS-I) =
               SR-L-AMOUNT(WS-I) - SR-L-OURS(WS-I)
           EVALUATE TRUE
               WHEN SR-L-DIFF(WS-I) = 0
                   MOVE "MATCHED" TO SR-L-STATUS(WS-I)
                   ADD 1 TO SR-MATCHED-CNT
               WHEN WS-M-STATUS = "SETTLED"
                   MOVE "TIMING" TO SR-L-STATUS(WS-I)
                   ADD SR-L-DIFF(WS-I) TO SR-TIMING-DIFF
                   ADD 1 TO SR-EXCEPTION-CNT
                   MOVE SPACES TO SR-L-NOTE(WS-I)
                   STRING "Paid by us, not yet shown on the "
                          "statement" DELIMITED BY SIZE
                       INTO SR-L-NOTE(WS-I)
               WHEN WS-M-BLOCKED = "1"
                   MOVE "DISPUTED" TO SR-L-STATUS(WS-I)
                   ADD SR-L-DIFF(WS-I) TO SR-DISPUTED-AMT
                   ADD 1 TO SR-EXCEPTION-CNT
                   MOVE SPACES TO SR-L-NOTE(WS-I)
                   STRING "Blocked or disputed on our ledger"
                       DELIMITED BY SIZE INTO SR-L-NOTE(WS-I)
               WHEN OTHER
                   MOVE "UNMATCHED" TO SR-L-STATUS(WS-I)
                   ADD SR-L-DIFF(WS-I) TO SR-MISSING-THEIRS
                   ADD 1 TO SR-EXCEPTION-CNT
                   MOVE SPACES TO SR-L-NOTE(WS-I)
                   STRING "Amount differs from our open item"
                       DELIMITED BY SIZE INTO SR-L-NOTE(WS-I)
                   PERFORM 3400-RAISE THRU 3400-EXIT
           END-EVALUATE
           PERFORM 3500-UPDATE-LINE THRU 3500-EXIT.
       3200-EXIT.
           EXIT.

       3300-SAVE-TOTALS SECTION.
       3300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.SupplierStatement SET LedgerBalance = ?,"
                  " DifferenceAmount = ?, Status = ?, ReconciledBy = "
                  "?, ReconciledAt = SYSDATETIME(), Note = "
                  "COALESCE(NULLIF(?,''), Note) WHERE StatementId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SR-LEDGER-BAL TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE SR-DIFFERENCE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           IF SR-EXCEPTION-CNT > 0
               MOVE "DISPUTED" TO SQLIO-PARM-VAL(3)
               MOVE "DISPUTED" TO SR-STATUS
           ELSE
               MOVE "RECONCILED" TO SQLIO-PARM-VAL(3)
               MOVE "RECONCILED" TO SR-STATUS
           END-IF
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE SR-NOTE(1:200) TO SQLIO-PARM-VAL(5)
           MOVE SR-STATEMENT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE 6 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       3300-EXIT.
           EXIT.

       3400-RAISE SECTION.
       3400-START.
           INITIALIZE WS-APSVC
           MOVE "EXCEPT" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "WARNING" TO SVC-EXC-SEVERITY
           MOVE "STATEMENT DIFFERENCE" TO SVC-EXC-CATEGORY
           MOVE "SUPPSTATEMENT" TO SVC-EXC-DOCTYPE
           MOVE SR-STATEMENT-REF TO SVC-EXC-DOCREF
           MOVE SR-OWNER-USER TO SVC-EXC-OWNER
           MOVE "APMGR" TO SVC-EXC-ROLE
           MOVE SR-L-AMOUNT(WS-I) TO SVC-EXC-AMOUNT
           MOVE SPACES TO SVC-EXC-MESSAGE
           STRING "Statement line "
                  FUNCTION TRIM(SR-L-REF(WS-I))
                  " from " FUNCTION TRIM(SR-SUPPLIER)
                  " needs review: "
                  FUNCTION TRIM(SR-L-NOTE(WS-I))
               DELIMITED BY SIZE INTO SVC-EXC-MESSAGE
           CALL "APSVC" USING WS-APSVC.
       3400-EXIT.
           EXIT.

       3500-UPDATE-LINE SECTION.
       3500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.SupplierStatementLine SET MatchStatus = "
                  "?, MatchedOpenItemId = NULLIF(?,0), Note = "
                  "NULLIF(?,'') WHERE StatementId = ? AND LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE SR-L-STATUS(WS-I) TO SQLIO-PARM-VAL(1)
           MOVE SR-L-ITEM-ID(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE SR-L-NOTE(WS-I) TO SQLIO-PARM-VAL(3)
           MOVE SR-STATEMENT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE WS-I TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       3500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       5000-RESOLVE SECTION.
       5000-START.
           IF SR-NOTE = SPACES
               MOVE "APQ0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Resolving a statement reconciliation needs a "
                      "recorded outcome" DELIMITED BY SIZE
                   INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.SupplierStatement SET Status = "
                  "'RECONCILED', ReconciledBy = ?, ReconciledAt = "
                  "SYSDATETIME(), Note = ? WHERE CompanyCode = ? AND "
                  "SupplierCode = ? AND StatementRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE SR-NOTE(1:200) TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE SR-SUPPLIER TO SQLIO-PARM-VAL(4)
           MOVE SR-STATEMENT-REF TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           MOVE "RECONCILED" TO SR-STATUS
           MOVE SPACES TO CA-MESSAGE
           STRING "Statement reconciliation resolved and recorded"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8500-EXEC-UPDATE SECTION.
       8500-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 8500-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       8500-EXIT.
           EXIT.

       8900-SVC-HEADER SECTION.
       8900-START.
           MOVE CA-COMPANY TO SVC-COMPANY
           MOVE CA-SITE TO SVC-SITE
           MOVE CA-USER TO SVC-USER
           MOVE CA-BUSINESS-DATE TO SVC-DATE
           MOVE CA-CORRELATION-ID TO SVC-CORRELATION.
       8900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-FATAL-SW
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNC SECTION.
       9100-START.
           MOVE "APQ0099" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "APSTMREC does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE "Y" TO WS-FATAL-SW
           MOVE 12 TO CA-RETURN-CODE
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in APSTMREC" TO CA-MESSAGE
           END-IF
           ADD 1 TO CA-MSG-COUNT
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       9300-EXIT.
           EXIT.
