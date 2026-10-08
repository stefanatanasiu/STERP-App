      *>================================================================
      *> PROGRAM-ID : POAPPRV
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : APPROVAL OF A PURCHASE ORDER FOR ITS EXACT
      *>              VALUE, SUPPLIER, TERMS AND DEMAND, WITH
      *>              AUTOMATIC REAPPROVAL WHEN THE APPROVED CONTENT
      *>              MATERIALLY CHANGES (PO-006, PO-007, D-07).
      *> CALLED BY  : POORDER (ISSUE), the .NET purchasing facade
      *> INTERFACE  : CALL "POAPPRV" USING DFHCOMMAREA (POAPPRVCA)
      *> CALLS      : SQLIO -> core.usp_RequestApproval,
      *>              core.usp_DecideApproval,
      *>              core.usp_InvalidateApprovals, po.PurchaseOrder
      *>----------------------------------------------------------------
      *> THE CONTENT HASH IS THE WHOLE POINT.  AN APPROVAL IS TIED TO
      *> ONE EXACT ORDER CONTENT; CHANGE THE SUPPLIER, THE VALUE, THE
      *> TERMS, A LINE QUANTITY OR A DESTINATION AND THE HASH MOVES,
      *> SO THE OLD AUTHORITY NO LONGER FITS AND ISSUE IS REFUSED
      *> UNTIL SOMEONE APPROVES THE NEW CONTENT.  A PRINTED DRAFT IS
      *> NEVER AN AUTHORISED COMMITMENT.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-07-02 RJH  CR0820 ORDER AUTHORISATION
      *> 2026-10-06 STERP CONTENT HASH AND REAPPROVAL
      *>================================================================
      *> REQ: PO-006, PO-007, D-07
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POAPPRV.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 02/07/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-ORD-ROW.
           05  WS-O-ID                 PIC X(20).
           05  WS-O-SUPPLIER           PIC X(20).
           05  WS-O-CURRENCY           PIC X(3).
           05  WS-O-TERMS              PIC X(12).
           05  WS-O-INCOTERMS          PIC X(3).
           05  WS-O-NET                PIC X(24).
           05  WS-O-VAT                PIC X(24).
           05  WS-O-GROSS              PIC X(24).
           05  WS-O-STATUS             PIC X(14).
           05  WS-O-HASH               PIC X(64).
           05  WS-O-LINKED-SO          PIC X(30).
       01  WS-LINE-ROW.
           05  WS-L-CNT                PIC X(12).
           05  WS-L-NET                PIC X(24).
           05  WS-L-CHECK              PIC X(16).
       01  WS-APR-ROW.
           05  WS-A-ID                 PIC X(20).
           05  WS-A-STATUS             PIC X(14).
           05  WS-A-PERM               PIC X(40).
           05  WS-A-ROLE               PIC X(20).
           05  WS-A-REQBY              PIC X(20).
           05  WS-A-HASH               PIC X(64).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-K                    PIC S9(4) VALUE 0.
           05  WS-D                    PIC S9(4) VALUE 0.
           05  WS-LEN                  PIC S9(4) VALUE 0.
           05  WS-TYPE-LEN             PIC S9(4) VALUE 0.
           05  WS-TYPE-IN              PIC X(30) VALUE SPACES.
           05  WS-TYPE-FOLD            PIC X(30) VALUE SPACES.
           05  WS-CHAR-VAL             PIC S9(9) COMP-5 VALUE 0.
           05  WS-AMT-ED               PIC -(11)9.99.
       01  WS-HASH-TEXT                PIC X(300) VALUE SPACES.
       01  WS-HEX-DIGITS               PIC X(16)
                                       VALUE "0123456789abcdef".
       01  WS-SEED-TABLE.
           05  FILLER                  PIC 9(9) VALUE 000000017.
           05  FILLER                  PIC 9(9) VALUE 000000331.
           05  FILLER                  PIC 9(9) VALUE 000005381.
           05  FILLER                  PIC 9(9) VALUE 000052711.
           05  FILLER                  PIC 9(9) VALUE 000648391.
           05  FILLER                  PIC 9(9) VALUE 009737333.
           05  FILLER                  PIC 9(9) VALUE 174440041.
           05  FILLER                  PIC 9(9) VALUE 000000093.
       01  WS-SEEDS REDEFINES WS-SEED-TABLE.
           05  WS-SEED                 PIC 9(9) OCCURS 8 TIMES.
       01  WS-MULT-TABLE.
           05  FILLER                  PIC 9(5) VALUE 00031.
           05  FILLER                  PIC 9(5) VALUE 00033.
           05  FILLER                  PIC 9(5) VALUE 00037.
           05  FILLER                  PIC 9(5) VALUE 00039.
           05  FILLER                  PIC 9(5) VALUE 00131.
           05  FILLER                  PIC 9(5) VALUE 00137.
           05  FILLER                  PIC 9(5) VALUE 00139.
           05  FILLER                  PIC 9(5) VALUE 16777.
       01  WS-MULTS REDEFINES WS-MULT-TABLE.
           05  WS-MULT                 PIC 9(5) OCCURS 8 TIMES.
       01  WS-ACC-TABLE.
           05  WS-ACC                  PIC S9(18) COMP-3
                                       OCCURS 8 TIMES.
       01  WS-MODULUS                  PIC S9(18) COMP-3
                                       VALUE 4294967296.
       01  WS-SCRATCH                  PIC S9(18) COMP-3 VALUE 0.
       LINKAGE SECTION.
       COPY POAPPRVCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "HASH"
                   PERFORM 2000-BUILD-HASH THRU 2000-EXIT
                   IF NOT WS-FATAL
                       PERFORM 2500-STORE-HASH THRU 2500-EXIT
                   END-IF
               WHEN "REQUEST"
                   PERFORM 3000-REQUEST THRU 3000-EXIT
               WHEN "DECIDE"
                   PERFORM 4000-DECIDE THRU 4000-EXIT
               WHEN "CHECK"
                   PERFORM 5000-CHECK THRU 5000-EXIT
               WHEN "INVALID"
                   PERFORM 6000-INVALIDATE THRU 6000-EXIT
               WHEN OTHER
                   MOVE "APV0008" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                          " is not supported by POAPPRV"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 8 TO CA-RETURN-CODE
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "POAPPRV" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE 0 TO AV-ORDER-ID AV-APPROVAL-ID AV-LINE-CNT
           MOVE 0 TO AV-NET-AMOUNT AV-GROSS-AMOUNT AV-AMOUNT-GBP
           MOVE SPACES TO AV-CONTENT-HASH AV-STORED-HASH
           MOVE SPACES TO AV-SUPPLIER AV-CURRENCY AV-TERMS-CODE
           MOVE SPACES TO AV-ORDER-STATUS AV-APPROVAL-STATUS
           MOVE SPACES TO AV-REQUIRED-PERM AV-REQUIRED-ROLE
           MOVE SPACES TO AV-REQUESTED-BY
           MOVE "N" TO AV-WAS-INVALIDATED
           MOVE "NOTAPPROVED" TO AV-OUTCOME
           IF AV-APPROVAL-TYPE = SPACES
               MOVE "PURCHASEORDER" TO AV-APPROVAL-TYPE
           END-IF
      *>     THE APPROVAL MATRIX IS KEYED ON core.ApprovalRule
      *>     .ApprovalType, WHICH HAS NO PUNCTUATION IN IT.  CALLERS
      *>     HAVE ALWAYS SPELT THE TYPE THEIR OWN WAY - PURCHASE-ORDER
      *>     AND PURCHASE_ORDER WERE BOTH IN USE - AND A TYPE THAT DOES
      *>     NOT MATCH A RULE MAKES core.usp_RequestApproval THROW, SO
      *>     THE ORDER COULD NEVER BE APPROVED AND NEVER BE ISSUED.
      *>     FOLD THE PUNCTUATION OUT ONCE, HERE.
           PERFORM 1100-FOLD-TYPE THRU 1100-EXIT
           IF CA-COMPANY = SPACES OR AV-ORDER-NUMBER = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APV0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A company code and an order number are "
                      "needed before an order can be approved"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1100-FOLD-TYPE SECTION.
       1100-START.
           MOVE SPACES TO WS-TYPE-FOLD
           MOVE 0 TO WS-TYPE-LEN
           MOVE FUNCTION UPPER-CASE(AV-APPROVAL-TYPE) TO WS-TYPE-IN
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 30
               IF WS-TYPE-IN(WS-I:1) NOT = SPACE AND
                  WS-TYPE-IN(WS-I:1) NOT = "-" AND
                  WS-TYPE-IN(WS-I:1) NOT = "_" AND
                  WS-TYPE-IN(WS-I:1) NOT = "."
                   ADD 1 TO WS-TYPE-LEN
                   MOVE WS-TYPE-IN(WS-I:1)
                     TO WS-TYPE-FOLD(WS-TYPE-LEN:1)
               END-IF
           END-PERFORM
           IF WS-TYPE-LEN > 0
               MOVE WS-TYPE-FOLD TO AV-APPROVAL-TYPE
           ELSE
               MOVE "PURCHASEORDER" TO AV-APPROVAL-TYPE
           END-IF.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> READ THE ORDER AND ITS LINES AND REDUCE THEM TO ONE HASH.
      *> CANCELLED LINES ARE EXCLUDED: CANCELLING A LINE IS A REAL
      *> CHANGE TO WHAT WE ARE BUYING AND MUST MOVE THE HASH.
      *>----------------------------------------------------------------
       2000-BUILD-HASH SECTION.
       2000-START.
           PERFORM 2100-READ-ORDER THRU 2100-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-READ-LINES THRU 2200-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           IF AV-LINE-CNT = 0
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APV0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Order " FUNCTION TRIM(AV-ORDER-NUMBER)
                      " has no live lines, so there is nothing to "
                      "approve"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO WS-HASH-TEXT
           STRING FUNCTION TRIM(WS-O-SUPPLIER) "|"
                  FUNCTION TRIM(WS-O-CURRENCY) "|"
                  FUNCTION TRIM(WS-O-TERMS) "|"
                  FUNCTION TRIM(WS-O-INCOTERMS) "|"
                  FUNCTION TRIM(WS-O-NET) "|"
                  FUNCTION TRIM(WS-O-VAT) "|"
                  FUNCTION TRIM(WS-O-GROSS) "|"
                  FUNCTION TRIM(WS-O-LINKED-SO) "|"
                  FUNCTION TRIM(WS-L-CNT) "|"
                  FUNCTION TRIM(WS-L-NET) "|"
                  FUNCTION TRIM(WS-L-CHECK)
               DELIMITED BY SIZE INTO WS-HASH-TEXT
           PERFORM 8000-HASH THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2100-READ-ORDER SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(o.PurchaseOrderId AS varchar(20)), "
                  "o.SupplierCode, o.CurrencyCode, "
                  "ISNULL(o.TermsCode, ''), "
                  "ISNULL(o.IncotermsRule, ''), "
                  "CAST(o.NetAmount AS varchar(24)), "
                  "CAST(o.VatAmount AS varchar(24)), "
                  "CAST(o.GrossAmount AS varchar(24)), o.Status, "
                  "ISNULL(o.ContentHash, ''), "
                  "ISNULL(o.LinkedSalesOrderNumber, '') FROM "
                  "po.PurchaseOrder o WHERE o.CompanyCode = ? AND "
                  "o.OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AV-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 11 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 3 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 24 TO SQLIO-COL-WIDTH(8)
           MOVE 14 TO SQLIO-COL-WIDTH(9)
           MOVE 64 TO SQLIO-COL-WIDTH(10)
           MOVE 30 TO SQLIO-COL-WIDTH(11)
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ORD-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 2100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "APV0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Purchase order "
                      FUNCTION TRIM(AV-ORDER-NUMBER)
                      " is not on file for company "
                      FUNCTION TRIM(CA-COMPANY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 2100-EXIT
           END-IF
           COMPUTE AV-ORDER-ID = FUNCTION NUMVAL(WS-O-ID)
           MOVE WS-O-SUPPLIER TO AV-SUPPLIER
           MOVE WS-O-CURRENCY TO AV-CURRENCY
           MOVE WS-O-TERMS TO AV-TERMS-CODE
           MOVE WS-O-STATUS TO AV-ORDER-STATUS
           MOVE WS-O-HASH TO AV-STORED-HASH
           COMPUTE AV-NET-AMOUNT = FUNCTION NUMVAL(WS-O-NET)
           COMPUTE AV-GROSS-AMOUNT = FUNCTION NUMVAL(WS-O-GROSS)
           MOVE AV-GROSS-AMOUNT TO AV-AMOUNT-GBP.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2200-READ-LINES SECTION.
       2200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(12)), "
                  "CAST(ISNULL(SUM(l.NetAmount), 0) AS varchar(24)),"
                  " CAST(ISNULL(CHECKSUM_AGG(CHECKSUM(l.LineNum, "
                  "l.ItemCode, l.Description, l.OrderedQty, "
                  "l.UnitPrice, l.DiscountPct, "
                  "l.DestinationSiteCode, l.AccountCode)), 0) AS "
                  "varchar(16)) FROM po.PurchaseOrderLine l WHERE "
                  "l.CompanyCode = ? AND l.OrderNumber = ? AND "
                  "l.Status <> 'CANCELLED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AV-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 16 TO SQLIO-COL-WIDTH(3)
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 2200-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 0 TO AV-LINE-CNT
               GO TO 2200-EXIT
           END-IF
           COMPUTE AV-LINE-CNT = FUNCTION NUMVAL(WS-L-CNT).
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2500-STORE-HASH SECTION.
       2500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.PurchaseOrder SET ContentHash = ?, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() WHERE "
                  "CompanyCode = ? AND OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE AV-CONTENT-HASH TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE AV-ORDER-NUMBER TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ORD-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
           END-IF.
       2500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> REQUEST APPROVAL FOR THE CURRENT CONTENT.  THE SHARED
      *> PROCEDURE INVALIDATES ANY EARLIER APPROVAL WITH A DIFFERENT
      *> HASH, SO A MATERIAL CHANGE CANNOT INHERIT OLD AUTHORITY.
      *>----------------------------------------------------------------
       3000-REQUEST SECTION.
       3000-START.
           PERFORM 2000-BUILD-HASH THRU 2000-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           IF WS-O-STATUS = "CLOSED" OR WS-O-STATUS = "CANCELLED"
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APV0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Order " FUNCTION TRIM(AV-ORDER-NUMBER)
                      " is " FUNCTION TRIM(WS-O-STATUS)
                      " and cannot be submitted for approval"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 3000-EXIT
           END-IF
           IF AV-STORED-HASH NOT = SPACES AND
              AV-STORED-HASH NOT = AV-CONTENT-HASH
               MOVE "Y" TO AV-WAS-INVALIDATED
           END-IF
           PERFORM 2500-STORE-HASH THRU 2500-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_RequestApproval ?, 'PURCHASEORDER',"
                  " ?, ?, ?, ?, ?, ?, ?, NULL, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AV-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE AV-APPROVAL-TYPE TO SQLIO-PARM-VAL(3)
           MOVE AV-CONTENT-HASH TO SQLIO-PARM-VAL(4)
           IF AV-REQUESTED-BY = SPACES
               MOVE CA-USER TO AV-REQUESTED-BY
           END-IF
           MOVE AV-REQUESTED-BY TO SQLIO-PARM-VAL(5)
           MOVE AV-GROSS-AMOUNT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE "GBP" TO SQLIO-PARM-VAL(7)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(9)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           IF CA-CORRELATION-ID = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(9)
           END-IF
           MOVE 9 TO SQLIO-PARM-CNT
           PERFORM 7000-APPROVAL-COLS THRU 7000-EXIT
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-APR-ROW
           IF SQLIO-ERROR
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APV0009" TO CA-REASON-CODE
               MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
               GO TO 3000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APV0009" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No approval rule covers a purchase order of "
                      "this value"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 3000-EXIT
           END-IF
           PERFORM 7050-REREAD THRU 7050-EXIT
           PERFORM 7100-TAKE-APPROVAL THRU 7100-EXIT
           PERFORM 7200-SET-ORDER-STATUS THRU 7200-EXIT
           IF AV-APPROVAL-STATUS = "APPROVED"
               MOVE "APPROVED" TO AV-OUTCOME
               MOVE 0 TO CA-RETURN-CODE
           ELSE
               MOVE "PENDING" TO AV-OUTCOME
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APV0005" TO CA-REASON-CODE
               MOVE AV-GROSS-AMOUNT TO WS-AMT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Order " FUNCTION TRIM(AV-ORDER-NUMBER)
                      " for " FUNCTION TRIM(WS-AMT-ED)
                      " awaits approval by "
                      FUNCTION TRIM(AV-REQUIRED-ROLE)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4000-DECIDE SECTION.
       4000-START.
           IF AV-APPROVER = SPACES OR AV-DECISION = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APV0010" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "An approval decision needs both an approver "
                      "and a decision of APPROVE or REJECT"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           IF AV-APPROVAL-ID = 0
               PERFORM 5000-CHECK THRU 5000-EXIT
               IF AV-APPROVAL-ID = 0
                   MOVE 16 TO CA-RETURN-CODE
                   MOVE "APV0011" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "There is no approval request outstanding"
                          " for order "
                          FUNCTION TRIM(AV-ORDER-NUMBER)
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   GO TO 4000-EXIT
               END-IF
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_DecideApproval ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE AV-APPROVAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE AV-APPROVER TO SQLIO-PARM-VAL(2)
      *>     core.usp_DecideApproval wants the PAST TENSE.  The
      *>     COMMAREA offers APPROVE / REJECT, so translate here
      *>     rather than making every caller know the difference -
      *>     passing APPROVE straight through used to come back as
      *>     "the approver may not approve their own request", which
      *>     sent people looking for a segregation problem that was
      *>     never there.
           EVALUATE TRUE
               WHEN AV-DEC-APPROVE OR AV-DECISION = "APPROVED"
                   MOVE "APPROVED" TO SQLIO-PARM-VAL(3)
               WHEN AV-DEC-REJECT OR AV-DECISION = "REJECTED"
                   MOVE "REJECTED" TO SQLIO-PARM-VAL(3)
               WHEN OTHER
                   MOVE AV-DECISION TO SQLIO-PARM-VAL(3)
           END-EVALUATE
           MOVE AV-COMMENT TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-APR-ROW
           IF SQLIO-ERROR
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APV0007" TO CA-REASON-CODE
               MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           IF AV-DEC-APPROVE OR AV-DECISION = "APPROVED"
               MOVE "APPROVED" TO AV-APPROVAL-STATUS
               MOVE "APPROVED" TO AV-OUTCOME
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Order " FUNCTION TRIM(AV-ORDER-NUMBER)
                      " approved by " FUNCTION TRIM(AV-APPROVER)
                      " for its exact value, supplier and terms"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE "REJECTED" TO AV-APPROVAL-STATUS
               MOVE "REJECTED" TO AV-OUTCOME
               MOVE 8 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Order " FUNCTION TRIM(AV-ORDER-NUMBER)
                      " rejected by " FUNCTION TRIM(AV-APPROVER)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           PERFORM 7200-SET-ORDER-STATUS THRU 7200-EXIT.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> CHECK - WHAT POORDER ISSUE ASKS.  THE ANSWER IS ONLY YES
      *> WHEN AN APPROVED ROW EXISTS FOR THE HASH OF THE ORDER AS IT
      *> STANDS RIGHT NOW.
      *>----------------------------------------------------------------
       5000-CHECK SECTION.
       5000-START.
           PERFORM 2000-BUILD-HASH THRU 2000-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 CAST(a.ApprovalId AS varchar(20)), "
                  "a.Status, ISNULL(a.RequiredPermission, ''), "
                  "ISNULL(a.RequiredRole, ''), "
                  "ISNULL(a.RequestedBy, ''), "
                  "ISNULL(a.ContentHash, '') FROM core.Approval a "
                  "WHERE a.CompanyCode = ? AND a.DocumentType = "
                  "'PURCHASEORDER' AND a.DocumentRef = ? AND "
                  "a.Status IN ('APPROVED', 'PENDING') ORDER BY "
                  "CASE WHEN a.ContentHash = ? THEN 0 ELSE 1 END, "
                  "CASE a.Status WHEN 'APPROVED' THEN 0 ELSE 1 END, "
                  "a.ApprovalId DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AV-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE AV-CONTENT-HASH TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM 7000-APPROVAL-COLS THRU 7000-EXIT
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-APR-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "NOTAPPROVED" TO AV-OUTCOME
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APV0011" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Order " FUNCTION TRIM(AV-ORDER-NUMBER)
                      " has not been approved, so it is a draft "
                      "and not a supplier commitment"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 5000-EXIT
           END-IF
           PERFORM 7100-TAKE-APPROVAL THRU 7100-EXIT
           EVALUATE TRUE
               WHEN WS-A-HASH NOT = AV-CONTENT-HASH
                   MOVE "REAPPROVE" TO AV-OUTCOME
                   MOVE "Y" TO AV-WAS-INVALIDATED
                   MOVE 6 TO CA-RETURN-CODE
                   MOVE "APV0006" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Order " FUNCTION TRIM(AV-ORDER-NUMBER)
                          " has changed materially since it was "
                          "approved and must be approved again"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               WHEN AV-APPROVAL-STATUS = "APPROVED"
                   MOVE "APPROVED" TO AV-OUTCOME
                   MOVE 0 TO CA-RETURN-CODE
               WHEN OTHER
                   MOVE "PENDING" TO AV-OUTCOME
                   MOVE 6 TO CA-RETURN-CODE
                   MOVE "APV0005" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Order " FUNCTION TRIM(AV-ORDER-NUMBER)
                          " is still awaiting an approval decision"
                       DELIMITED BY SIZE INTO CA-MESSAGE
           END-EVALUATE.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       6000-INVALIDATE SECTION.
       6000-START.
           PERFORM 2000-BUILD-HASH THRU 2000-EXIT
           IF WS-FATAL
               GO TO 6000-EXIT
           END-IF
           IF AV-CHANGE-REASON = SPACES
               MOVE "Material change to the approved order content"
                 TO AV-CHANGE-REASON
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_InvalidateApprovals ?, "
                  "'PURCHASEORDER', ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AV-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE AV-CONTENT-HASH TO SQLIO-PARM-VAL(3)
           MOVE AV-CHANGE-REASON TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-APR-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 6000-EXIT
           END-IF
           PERFORM 2500-STORE-HASH THRU 2500-EXIT
           MOVE "Y" TO AV-WAS-INVALIDATED
           MOVE "REAPPROVE" TO AV-OUTCOME
           MOVE 4 TO CA-RETURN-CODE
           MOVE "APV0006" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Approvals for order "
                  FUNCTION TRIM(AV-ORDER-NUMBER)
                  " invalidated: "
                  FUNCTION TRIM(AV-CHANGE-REASON)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       6000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7000-APPROVAL-COLS SECTION.
       7000-START.
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 14 TO SQLIO-COL-WIDTH(2)
           MOVE 40 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 64 TO SQLIO-COL-WIDTH(6)
           MOVE 6 TO SQLIO-CURSOR.
       7000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> core.usp_RequestApproval RETURNS THE WHOLE APPROVAL ROW IN ITS
      *> OWN COLUMN ORDER, WHICH IS NOT THE ORDER THE CHECK QUERY USES.
      *> READING IT WITH THE CHECK LAYOUT PUT THE DOCUMENT TYPE INTO
      *> THE REQUIRED PERMISSION AND THE COMPANY CODE INTO THE STATUS,
      *> SO THE REQUEST IS RE-READ WITH THE SAME SIX COLUMNS IN THE
      *> SAME ORDER AS EVERYWHERE ELSE.  ONE EXTRA READ IS CHEAPER THAN
      *> AN AUTHORITY FIELD NOBODY CAN TRUST.
      *>----------------------------------------------------------------
       7050-REREAD SECTION.
       7050-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(a.ApprovalId AS varchar(20)), "
                  "a.Status, ISNULL(a.RequiredPermission, ''), "
                  "ISNULL(a.RequiredRole, ''), "
                  "ISNULL(a.RequestedBy, ''), "
                  "ISNULL(a.ContentHash, '') FROM core.Approval a "
                  "WHERE a.ApprovalId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-A-ID TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           PERFORM 7000-APPROVAL-COLS THRU 7000-EXIT
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-APR-ROW.
       7050-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7100-TAKE-APPROVAL SECTION.
       7100-START.
           COMPUTE AV-APPROVAL-ID = FUNCTION NUMVAL(WS-A-ID)
           MOVE WS-A-STATUS TO AV-APPROVAL-STATUS
           MOVE WS-A-PERM TO AV-REQUIRED-PERM
           MOVE WS-A-ROLE TO AV-REQUIRED-ROLE
           MOVE WS-A-REQBY TO AV-REQUESTED-BY.
       7100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7200-SET-ORDER-STATUS SECTION.
       7200-START.
           MOVE SPACES TO SQLIO-STMT
           EVALUATE AV-APPROVAL-STATUS
               WHEN "APPROVED"
                   STRING "UPDATE po.PurchaseOrder SET Status = "
                          "'APPROVED', ApprovalId = ?, ApprovedBy = "
                          "?, ApprovedAt = SYSDATETIME() WHERE "
                          "CompanyCode = ? AND OrderNumber = ? AND "
                          "Status IN ('DRAFT', 'AWAITAPPROVAL', "
                          "'AMENDED')"
                       DELIMITED BY SIZE INTO SQLIO-STMT
               WHEN "REJECTED"
                   STRING "UPDATE po.PurchaseOrder SET Status = "
                          "'DRAFT', ApprovalId = ?, ApprovedBy = ?, "
                          "ApprovedAt = NULL WHERE CompanyCode = ? "
                          "AND OrderNumber = ? AND Status IN "
                          "('AWAITAPPROVAL', 'AMENDED')"
                       DELIMITED BY SIZE INTO SQLIO-STMT
               WHEN OTHER
                   STRING "UPDATE po.PurchaseOrder SET Status = "
                          "'AWAITAPPROVAL', ApprovalId = ?, "
                          "ModifiedBy = ?, ModifiedAt = "
                          "SYSDATETIME() WHERE CompanyCode = ? AND "
                          "OrderNumber = ? AND Status IN ('DRAFT', "
                          "'AMENDED', 'AWAITAPPROVAL')"
                       DELIMITED BY SIZE INTO SQLIO-STMT
           END-EVALUATE
           MOVE AV-APPROVAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           IF AV-APPROVER = SPACES
               MOVE CA-USER TO SQLIO-PARM-VAL(2)
           ELSE
               MOVE AV-APPROVER TO SQLIO-PARM-VAL(2)
           END-IF
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE AV-ORDER-NUMBER TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ORD-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
           END-IF.
       7200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> EIGHT INDEPENDENT 32 BIT ACCUMULATORS WITH DIFFERENT SEEDS
      *> AND MULTIPLIERS, RENDERED AS 64 HEXADECIMAL CHARACTERS.  NOT
      *> A CRYPTOGRAPHIC HASH - A CHANGE DETECTOR.
      *>----------------------------------------------------------------
       8000-HASH SECTION.
       8000-START.
           PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > 8
               MOVE WS-SEED(WS-K) TO WS-ACC(WS-K)
           END-PERFORM
           MOVE 0 TO WS-LEN
           PERFORM VARYING WS-I FROM 300 BY -1
                   UNTIL WS-I < 1 OR WS-LEN > 0
               IF WS-HASH-TEXT(WS-I:1) NOT = SPACE
                   MOVE WS-I TO WS-LEN
               END-IF
           END-PERFORM
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN
               COMPUTE WS-CHAR-VAL =
                       FUNCTION ORD(WS-HASH-TEXT(WS-I:1))
               PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > 8
                   COMPUTE WS-SCRATCH = WS-ACC(WS-K) * WS-MULT(WS-K)
                           + WS-CHAR-VAL + WS-I
                   COMPUTE WS-ACC(WS-K) =
                           FUNCTION MOD(WS-SCRATCH, WS-MODULUS)
               END-PERFORM
           END-PERFORM
           MOVE SPACES TO AV-CONTENT-HASH
           PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > 8
               MOVE WS-ACC(WS-K) TO WS-SCRATCH
               PERFORM VARYING WS-D FROM 8 BY -1 UNTIL WS-D < 1
                   COMPUTE WS-I = FUNCTION MOD(WS-SCRATCH, 16) + 1
                   MOVE WS-HEX-DIGITS(WS-I:1) TO
                        AV-CONTENT-HASH(((WS-K - 1) * 8) + WS-D:1)
                   COMPUTE WS-SCRATCH =
                           FUNCTION INTEGER(WS-SCRATCH / 16)
               END-PERFORM
           END-PERFORM.
       8000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-SQL-FAIL SECTION.
       9000-START.
           MOVE 12 TO CA-RETURN-CODE
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           MOVE "Y" TO WS-FATAL-SW.
       9000-EXIT.
           EXIT.
       END PROGRAM POAPPRV.
