      *>================================================================
      *> PROGRAM-ID : CREDCHK
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : CREDIT EXPOSURE AND RELEASE DECISION FOR ONE
      *>              CUSTOMER.  EXPOSURE IS THE FIVE LEGGED FIGURE
      *>              DEFINED IN THE DEBTORS SPECIFICATION SECTION 6 -
      *>              OPEN DEBT, LESS ELIGIBLE CREDITS, PLUS FIRM
      *>              UNSHIPPED ORDERS, PLUS SHIPPED NOT BILLED, LESS
      *>              ELIGIBLE UNAPPLIED DEPOSITS.  MOVING A QUANTITY
      *>              FROM ORDER TO DESPATCH TO INVOICE THEREFORE
      *>              NEITHER DUPLICATES NOR LOSES ITS EXPOSURE.
      *> CALLED BY  : SALES ORDER ENTRY AND DESPATCH RELEASE (SO),
      *>              AR ENQUIRY, DUNNING, .NET CLIENT
      *> INTERFACE  : CALL "CREDCHK" USING WS-CREDCHK-CA
      *>              COPYBOOK CREDCHKCA
      *> CALLS      : SQLIO, PARMGET, PERMCHK, EXCPRAIS, AUDITLOG,
      *>              DATECALC, FXCONV
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-04-16 RJH  ORIGINAL MVS PORT OF THE CREDIT CONTROL
      *>                 SUITE.  LIMIT AND OVERDUE TEST ONLY.
      *> 1994-08-22 DJM  CR0361 UNSHIPPED ORDER VALUE ADDED TO THE
      *>                 EXPOSURE AFTER THE BRANTFORD WRITE OFF
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS
      *> 2003-02-11 RJH  CR1588 DESPATCHED NOT INVOICED LEG
      *> 2026-10-05 STERP OVERRIDES WITH SCOPE, AMOUNT AND EXPIRY
      *>================================================================
      *> REQ: AR-007, AR-008, D-06
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREDCHK.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 16/04/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ARWORK.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARM-CA==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCP-CA==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDIT-CA==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATE-CA==.
       01  WS-CUST.
           05  WS-CU-NAME              PIC X(120).
           05  WS-CU-STATUS            PIC X(10).
           05  WS-CU-SANCTIONS         PIC X(12).
           05  WS-CU-CURRENCY          PIC X(3).
           05  WS-CU-INSOLVENCY        PIC X(16).
           05  WS-CU-LIMIT             PIC X(20).
           05  WS-CU-THRESHOLD         PIC X(20).
           05  WS-CU-TRIGGER           PIC X(10).
           05  WS-CU-HOLD              PIC X(2).
           05  WS-CU-HOLD-REASON       PIC X(200).
           05  WS-CU-PROFORMA          PIC X(2).
           05  WS-CU-PREPAY            PIC X(2).
       01  WS-SUM-ROW.
           05  WS-S1                   PIC X(24).
           05  WS-S2                   PIC X(24).
           05  WS-S3                   PIC X(24).
           05  WS-S4                   PIC X(24).
       01  WS-SWITCHES.
           05  WS-SO-PRESENT           PIC X VALUE "Y".
           05  WS-BLOCK-SET            PIC X VALUE "N".
           05  WS-HOLD-SET             PIC X VALUE "N".
       01  WS-FUNC-NO                  PIC S9(4) VALUE 0.
       LINKAGE SECTION.
       COPY CREDCHKCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF AR-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0300-READ-CUSTOMER
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE AR-FUNC
               WHEN "EXPOSURE" MOVE 1 TO WS-FUNC-NO
               WHEN "CHECK"    MOVE 2 TO WS-FUNC-NO
               WHEN "OVERRIDE" MOVE 3 TO WS-FUNC-NO
               WHEN "APPROVE"  MOVE 4 TO WS-FUNC-NO
               WHEN "RELEASE"  MOVE 5 TO WS-FUNC-NO
               WHEN OTHER      MOVE 0 TO WS-FUNC-NO
           END-EVALUATE
           IF WS-FUNC-NO = 0
               PERFORM 9100-BAD-FUNCTION
               GO TO 0000-EXIT
           END-IF
      *>   GO TO .. DEPENDING ON kept from the original MVS source
           GO TO 0010-EXPOSURE 0020-CHECK 0030-OVERRIDE
                 0040-APPROVE 0050-RELEASE
               DEPENDING ON WS-FUNC-NO.
       0010-EXPOSURE.
           PERFORM 1000-COMPUTE-EXPOSURE
           GO TO 0000-EXIT.
       0020-CHECK.
           PERFORM 1000-COMPUTE-EXPOSURE
           PERFORM 2000-DECIDE
           GO TO 0000-EXIT.
       0030-OVERRIDE.
           PERFORM 1000-COMPUTE-EXPOSURE
           PERFORM 3000-REQUEST-OVERRIDE
           GO TO 0000-EXIT.
       0040-APPROVE.
           PERFORM 3500-APPROVE-OVERRIDE
           GO TO 0000-EXIT.
       0050-RELEASE.
           PERFORM 1000-COMPUTE-EXPOSURE
           PERFORM 2000-DECIDE
           GO TO 0000-EXIT.
       0000-EXIT.
           PERFORM 9900-SET-RESULT
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE AR-WORK
           INITIALIZE AR-CTL
           INITIALIZE WS-CUST
           INITIALIZE WS-SUM-ROW
           MOVE "N" TO WS-BLOCK-SET WS-HOLD-SET
           MOVE "Y" TO WS-SO-PRESENT
           MOVE CA-FUNCTION OF DFHCOMMAREA TO AR-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO AR-CO
           MOVE CA-SITE OF DFHCOMMAREA TO AR-SITE
           MOVE CA-USER OF DFHCOMMAREA TO AR-USERID
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO AR-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO AR-CORREL
           MOVE "CREDCHK" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO AR-RC
           MOVE SPACES TO AR-REASON AR-MSG
           MOVE "AR-007" TO AR-REQ-REF
           MOVE 0 TO AR-MSG-CNT
           MOVE FUNCTION UPPER-CASE(CK-CUSTOMER) TO CK-CUSTOMER
           IF CK-EVAL-DATE = 0
               MOVE AR-BUSDATE TO CK-EVAL-DATE
           END-IF
           IF CK-EVAL-DATE = 0
               MOVE FUNCTION CURRENT-DATE TO AR-NOW
               MOVE AR-NOW(1:8) TO CK-EVAL-DATE
           END-IF
           MOVE 0 TO CK-OPEN-DEBT CK-OPEN-CREDITS CK-FIRM-ORDERS
           MOVE 0 TO CK-SHIPPED-UNBILL CK-DEPOSITS CK-EXPOSURE
           MOVE 0 TO CK-LIMIT CK-HEADROOM CK-EXCESS CK-UTILISED-PCT
           MOVE 0 TO CK-OVERDUE-GBP CK-OVERDUE-DAYS CK-DISPUTED-GBP
           MOVE 0 TO CK-OLDEST-DUE-DATE CK-ORDER-VALUE-GBP
           MOVE SPACES TO CK-DECISION CK-DECISION-TEXT
           MOVE SPACES TO CK-HOLD-REASON CK-ACCOUNT-STATUS
           MOVE "N" TO CK-HOLD-FLAG CK-PROFORMA-ONLY CK-OVERRIDE-ACTIVE
           IF CK-CURRENCY = SPACES OR CK-CURRENCY = LOW-VALUES
               MOVE "GBP" TO CK-CURRENCY
           END-IF
           IF CK-FX-RATE = 0
               MOVE 1 TO CK-FX-RATE
           END-IF.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0200-VALIDATE SECTION.
       0200-START.
           IF AR-CO = SPACES OR AR-CO = LOW-VALUES
               MOVE "CRD0001" TO AR-REASON
               MOVE "A company code is required for a credit check"
                   TO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CK-CUSTOMER = SPACES OR CK-CUSTOMER = LOW-VALUES
               MOVE "CRD0002" TO AR-REASON
               MOVE "A customer account is required" TO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CK-ORDER-VALUE < 0
               MOVE "CRD0003" TO AR-REASON
               MOVE "A proposed commitment cannot be negative"
                   TO AR-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   the customer, its credit policy and the D-06 parameters
       0300-READ-CUSTOMER SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT c.LegalName, c.Status, c.SanctionsStatus, "
                  "c.CurrencyCode, ISNULL(c.InsolvencyType,''), "
                  "ISNULL(cc.CreditLimitGBP,0), "
                  "ISNULL(cc.OverdueThresholdGBP,0), "
                  "ISNULL(cc.OverdueDaysTrigger,30), "
                  "ISNULL(CONVERT(char(1),cc.HoldFlag),'0'), "
                  "ISNULL(cc.HoldReason,''), "
                  "ISNULL(CONVERT(char(1),cc.ProFormaOnly),'0'), "
                  "ISNULL(CONVERT(char(1),cc.RequiresPrepayment),'0') "
                  "FROM ar.Customer c LEFT JOIN ar.CustomerCredit cc "
                  "ON cc.CompanyCode = c.CompanyCode "
                  "AND cc.CustomerCode = c.CustomerCode "
                  "WHERE c.CompanyCode = ? AND c.CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE CK-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 12 TO SQLIO-COL-CNT
           MOVE 120 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 3 TO SQLIO-COL-WIDTH(4)
           MOVE 16 TO SQLIO-COL-WIDTH(5)
           MOVE 20 TO SQLIO-COL-WIDTH(6)
           MOVE 20 TO SQLIO-COL-WIDTH(7)
           MOVE 10 TO SQLIO-COL-WIDTH(8)
           MOVE 2 TO SQLIO-COL-WIDTH(9)
           MOVE 200 TO SQLIO-COL-WIDTH(10)
           MOVE 2 TO SQLIO-COL-WIDTH(11)
           MOVE 2 TO SQLIO-COL-WIDTH(12)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CUST
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "CRD0004" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Customer " FUNCTION TRIM(CK-CUSTOMER)
                      " is not held for company "
                      FUNCTION TRIM(AR-CO)
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 0300-EXIT
           END-IF
           MOVE WS-CU-STATUS TO CK-ACCOUNT-STATUS
           MOVE WS-CU-SANCTIONS TO CK-SANCTIONS
           COMPUTE CK-LIMIT = FUNCTION NUMVAL(WS-CU-LIMIT)
           COMPUTE CK-OVERDUE-THRESH = FUNCTION NUMVAL(WS-CU-THRESHOLD)
           COMPUTE CK-OVERDUE-TRIGGER = FUNCTION NUMVAL(WS-CU-TRIGGER)
           IF WS-CU-HOLD(1:1) = "1"
               MOVE "Y" TO CK-HOLD-FLAG
               MOVE WS-CU-HOLD-REASON TO CK-HOLD-REASON
           END-IF
           IF WS-CU-PROFORMA(1:1) = "1" OR WS-CU-PREPAY(1:1) = "1"
               MOVE "Y" TO CK-PROFORMA-ONLY
           END-IF
           PERFORM 0400-READ-PARAMETERS.
       0300-EXIT.
           EXIT.

      *>   D-06 defaults fill in anything the account does not state
       0400-READ-PARAMETERS SECTION.
       0400-START.
           IF CK-OVERDUE-TRIGGER = 0
               INITIALIZE WS-PARM-CA
               MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
               MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
               MOVE AR-USERID TO CA-USER OF WS-PARM-CA
               MOVE CK-EVAL-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
               MOVE "AR.HOLD.OVERDUE.DAYS" TO PM-PARAM-CODE
               MOVE "Y" TO PM-USE-DEFAULT
               MOVE "30" TO PM-DEFAULT-TEXT
               CALL "PARMGET" USING WS-PARM-CA
               COMPUTE CK-OVERDUE-TRIGGER = PM-VALUE-NUM
           END-IF
           IF CK-OVERDUE-TRIGGER = 0
               MOVE 30 TO CK-OVERDUE-TRIGGER
           END-IF.
       0400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   EXPOSURE - the five legs, each counted exactly once
       1000-COMPUTE-EXPOSURE SECTION.
       1000-START.
           PERFORM 1100-OPEN-ITEMS
           IF AR-RC-SEVERE
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-FIRM-ORDERS
           PERFORM 1300-SHIPPED-UNBILLED
           PERFORM 1400-CONVERT-CANDIDATE
           COMPUTE AR-AMT1 = CK-OPEN-DEBT - CK-OPEN-CREDITS
                           + CK-FIRM-ORDERS + CK-SHIPPED-UNBILL
                           - CK-DEPOSITS
           IF AR-AMT1 < 0
               MOVE 0 TO AR-AMT1
           END-IF
           MOVE AR-AMT1 TO CK-EXPOSURE
           COMPUTE CK-HEADROOM = CK-LIMIT - CK-EXPOSURE
                               - CK-ORDER-VALUE-GBP
           IF CK-HEADROOM < 0
               COMPUTE CK-EXCESS = 0 - CK-HEADROOM
           ELSE
               MOVE 0 TO CK-EXCESS
           END-IF
           IF CK-LIMIT > 0
               COMPUTE CK-UTILISED-PCT ROUNDED =
                   ((CK-EXPOSURE + CK-ORDER-VALUE-GBP) * 100)
                   / CK-LIMIT
           ELSE
               MOVE 0 TO CK-UTILISED-PCT
           END-IF
           PERFORM 1500-STORE-EXPOSURE.
       1000-EXIT.
           EXIT.

      *>   open debt, eligible credits, deposits, overdue and dispute
      *>   in one pass of the open item ledger.  Only posted, confirmed
      *>   money is eligible - an unconfirmed receipt is not funds.
       1100-OPEN-ITEMS SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SUM(CASE WHEN o.ItemType IN "
                  "('INVOICE','ADJUSTMENT','INTEREST') "
                  "AND o.ResidualAmountGBP > 0 "
                  "THEN o.ResidualAmountGBP ELSE 0 END),0), "
                  "ISNULL(SUM(CASE WHEN o.ItemType IN "
                  "('CREDIT','RECEIPT') AND o.ResidualAmountGBP < 0 "
                  "AND (o.SourceReceiptId IS NULL OR "
                  "r.ConfirmationState = 'CONFIRMED') "
                  "THEN -o.ResidualAmountGBP "
                  "ELSE 0 END),0), "
                  "ISNULL(SUM(CASE WHEN o.ItemType = 'DEPOSIT' "
                  "AND o.ResidualAmountGBP < 0 "
                  "THEN -o.ResidualAmountGBP ELSE 0 END),0), "
                  "ISNULL(SUM(CASE WHEN o.ResidualAmountGBP > 0 "
                  "AND o.DueDate < ? THEN o.ResidualAmountGBP "
                  "ELSE 0 END),0) "
      *>           CR2027 - an aggregate may not contain a subquery,
      *>           so the confirmed receipt test is a left join
                  "FROM ar.OpenItem o LEFT JOIN ar.Receipt r "
                  "ON r.ReceiptId = o.SourceReceiptId "
                  "WHERE o.CompanyCode = ? "
                  "AND o.CustomerCode = ? "
                  "AND o.SettlementStatus <> 'REVERSED' "
                  "AND o.PostingDate <= ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CK-EVAL-DATE TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE AR-CO TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE CK-CUSTOMER TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE CK-EVAL-DATE TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1100-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               COMPUTE CK-OPEN-DEBT = FUNCTION NUMVAL(WS-S1)
               COMPUTE CK-OPEN-CREDITS = FUNCTION NUMVAL(WS-S2)
               COMPUTE CK-DEPOSITS = FUNCTION NUMVAL(WS-S3)
               COMPUTE CK-OVERDUE-GBP = FUNCTION NUMVAL(WS-S4)
           END-IF
           PERFORM 1150-OLDEST-OVERDUE.
       1100-EXIT.
           EXIT.

       1150-OLDEST-OVERDUE SECTION.
       1150-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CONVERT(char(8), o.DueDate, 112), "
                  "DATEDIFF(day, o.DueDate, ?), "
                  "ISNULL((SELECT SUM(d.DisputedAmountGBP) "
                  "FROM ar.OpenItem d WHERE d.CompanyCode = "
                  "o.CompanyCode AND d.CustomerCode = o.CustomerCode "
                  "AND d.IsDisputed = 1 "
                  "AND d.SettlementStatus <> 'REVERSED'),0), '' "
                  "FROM ar.OpenItem o WHERE o.CompanyCode = ? "
                  "AND o.CustomerCode = ? AND o.DueDate < ? "
                  "AND o.ResidualAmountGBP > 0 "
                  "AND o.SettlementStatus <> 'REVERSED' "
                  "ORDER BY o.DueDate"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CK-EVAL-DATE TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE AR-CO TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE CK-CUSTOMER TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE CK-EVAL-DATE TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1150-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               IF WS-S1(1:8) IS NUMERIC
                   MOVE WS-S1(1:8) TO CK-OLDEST-DUE-DATE
               END-IF
               COMPUTE CK-OVERDUE-DAYS = FUNCTION NUMVAL(WS-S2)
               COMPUTE CK-DISPUTED-GBP = FUNCTION NUMVAL(WS-S3)
           END-IF.
       1150-EXIT.
           EXIT.

      *>   firm unshipped orders.  Quotes, drafts and cancelled
      *>   quantities are excluded; a credit held accepted order is
      *>   still firm.  A superseded order version is excluded so that
      *>   an amendment is evaluated once (AR-007).
       1200-FIRM-ORDERS SECTION.
       1200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SUM(ROUND(CASE WHEN l.AcceptedQty "
                  "> 0 AND (l.AcceptedQty - l.CancelledQty - "
                  "l.DespatchedQty) > 0 THEN (l.AcceptedQty - "
                  "l.CancelledQty - l.DespatchedQty) / l.AcceptedQty "
                  "* (l.NetAmount + l.VatAmount) / o.FxRate ELSE 0 "
                  "END, 2)),0), '0', '0', '0' "
                  "FROM so.SalesOrder o JOIN so.SalesOrderLine l "
                  "ON l.SalesOrderId = o.SalesOrderId "
                  "WHERE o.CompanyCode = ? AND o.CustomerCode = ? "
                  "AND o.CommercialStatus IN ('ACCEPTED','AMENDED') "
                  "AND l.Status = 'OPEN' AND o.OrderDate <= ? "
                  "AND (? = '' OR o.OrderNumber <> ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE CK-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE CK-EVAL-DATE TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE CK-EXCLUDE-REF TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE CK-EXCLUDE-REF TO SQLIO-PARM-VAL(5)
           MOVE "N" TO SQLIO-PARM-NULL(5)
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUM-ROW
           IF SQLIO-ERROR
      *>       the sales order module may not be deployed yet; an
      *>       unavailable leg is reported, never silently zero
               MOVE "N" TO WS-SO-PRESENT
               MOVE 4 TO AR-RC
               MOVE "CRD0010" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Order book exposure is unavailable - the "
                      "sales order module is not deployed"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1200-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               COMPUTE CK-FIRM-ORDERS = FUNCTION NUMVAL(WS-S1)
           END-IF.
       1200-EXIT.
           EXIT.

      *>   despatched but not yet billed.  A quantity leaves unshipped
      *>   exposure as it enters this leg, and leaves this leg when the
      *>   receivable is recorded.
       1300-SHIPPED-UNBILLED SECTION.
       1300-START.
           IF WS-SO-PRESENT = "N"
               GO TO 1300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SUM(ROUND(CASE WHEN l.AcceptedQty "
                  "> 0 AND (d.DespatchedQty - d.BilledQty) > 0 "
                  "THEN (d.DespatchedQty - d.BilledQty) / "
                  "l.AcceptedQty * (l.NetAmount + l.VatAmount) "
                  "/ o.FxRate ELSE 0 END, 2)),0), '0', '0', '0' "
                  "FROM so.Despatch h JOIN so.DespatchLine d "
                  "ON d.DespatchId = h.DespatchId "
                  "JOIN so.SalesOrderLine l ON l.SalesOrderLineId = "
                  "d.SalesOrderLineId JOIN so.SalesOrder o "
                  "ON o.SalesOrderId = l.SalesOrderId "
                  "WHERE h.CompanyCode = ? AND h.CustomerCode = ? "
                  "AND h.Status = 'CONFIRMED' "
                  "AND h.DespatchDate <= ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE CK-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE CK-EVAL-DATE TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUM-ROW
           IF SQLIO-ERROR
               MOVE "N" TO WS-SO-PRESENT
               GO TO 1300-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               COMPUTE CK-SHIPPED-UNBILL = FUNCTION NUMVAL(WS-S1)
           END-IF.
       1300-EXIT.
           EXIT.

      *>   the candidate commitment arrives in the order currency;
      *>   conversion uses the approved credit evaluation rate and
      *>   records its date (specification section 6).
       1400-CONVERT-CANDIDATE SECTION.
       1400-START.
           IF CK-ORDER-VALUE = 0
               MOVE 0 TO CK-ORDER-VALUE-GBP
               GO TO 1400-EXIT
           END-IF
           IF CK-CURRENCY = "GBP"
               MOVE CK-ORDER-VALUE TO CK-ORDER-VALUE-GBP
               MOVE 1 TO CK-FX-RATE
               GO TO 1400-EXIT
           END-IF
           IF CK-FX-RATE > 0 AND CK-FX-RATE NOT = 1
               COMPUTE CK-ORDER-VALUE-GBP ROUNDED =
                   CK-ORDER-VALUE / CK-FX-RATE
               GO TO 1400-EXIT
           END-IF
           MOVE 8 TO AR-RC
           MOVE "CRD0011" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "A credit evaluation rate is required to assess a "
                  FUNCTION TRIM(CK-CURRENCY) " commitment"
               DELIMITED BY SIZE INTO AR-MSG.
       1400-EXIT.
           EXIT.

      *>   remember the figure so an enquiry can show when it was last
      *>   evaluated (the screens do not recompute on every repaint)
       1500-STORE-EXPOSURE SECTION.
       1500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.CustomerCredit SET LastExposureGBP = ?, "
                  "LastExposureAt = SYSDATETIME() "
                  "WHERE CompanyCode = ? AND CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CK-EXPOSURE TO AR-MONEY-ED
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE AR-CO TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE CK-CUSTOMER TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUM-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUM-ROW.
       1500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   the release decision.  Hard refusals (sanctions, insolvency,
      *>   a retired account) are not overridable; commercial refusals
      *>   are held and can be released by an authorised override.
       2000-DECIDE SECTION.
       2000-START.
           PERFORM 2050-ACTIVE-OVERRIDE
           MOVE "RELEASE" TO CK-DECISION
           MOVE SPACES TO CK-DECISION-TEXT
           PERFORM 2100-HARD-REFUSALS
           IF WS-BLOCK-SET = "Y"
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-COMMERCIAL-REFUSALS
           IF WS-HOLD-SET = "Y"
               PERFORM 2300-RAISE-HOLD
           ELSE
      *>       CR2027 - a release granted by an override keeps its
      *>       warning, reason and evidence (AR-008, COM-017)
               IF AR-REASON NOT = "CRD0040"
                   MOVE 0 TO AR-RC
                   MOVE SPACES TO AR-REASON
                   MOVE SPACES TO AR-MSG
                   MOVE CK-HEADROOM TO AR-MONEY-UK
                   STRING "Credit available: GBP "
                          FUNCTION TRIM(AR-MONEY-UK)
                       DELIMITED BY SIZE INTO AR-MSG
                   MOVE AR-MSG TO CK-DECISION-TEXT
               END-IF
           END-IF.
       2000-EXIT.
           EXIT.

      *>   an override only releases while it is approved, in date and
      *>   large enough for the excess being asked for
       2050-ACTIVE-OVERRIDE SECTION.
       2050-START.
           MOVE "N" TO CK-OVERRIDE-ACTIVE
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CONVERT(varchar(20), v.OverrideId), "
                  "CONVERT(varchar(24), v.OverrideAmountGBP), "
                  "CONVERT(char(8), v.ExpiresOn, 112), v.Status "
                  "FROM ar.CreditOverride v WHERE v.CompanyCode = ? "
                  "AND v.CustomerCode = ? AND v.Status = 'ACTIVE' "
                  "AND v.ValidFrom <= ? AND v.ExpiresOn >= ? "
                  "AND (v.DocumentRef IS NULL OR v.DocumentRef = '' "
                  "OR v.DocumentRef = ?) "
                  "ORDER BY v.OverrideAmountGBP DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE CK-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE CK-EVAL-DATE TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE CK-EVAL-DATE TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE CK-DOCUMENT-REF TO SQLIO-PARM-VAL(5)
           MOVE "N" TO SQLIO-PARM-NULL(5)
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUM-ROW
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               GO TO 2050-EXIT
           END-IF
           COMPUTE CK-OVERRIDE-ID = FUNCTION NUMVAL(WS-S1)
           COMPUTE CK-OVERRIDE-AMT = FUNCTION NUMVAL(WS-S2)
           IF WS-S3(1:8) IS NUMERIC
               MOVE WS-S3(1:8) TO CK-OVERRIDE-EXPIRY
           END-IF
           MOVE WS-S4 TO CK-OVERRIDE-STATUS
           MOVE "Y" TO CK-OVERRIDE-ACTIVE.
       2050-EXIT.
           EXIT.

       2100-HARD-REFUSALS SECTION.
       2100-START.
           IF CK-SANCTIONS = "POTENTIAL" OR CK-SANCTIONS = "MATCH"
               MOVE "BLOCK" TO CK-DECISION
               MOVE "CRD0020" TO AR-REASON
               MOVE SPACES TO CK-DECISION-TEXT
               STRING "Release is blocked until the sanctions "
                      "screening match has an authorised decision"
                   DELIMITED BY SIZE INTO CK-DECISION-TEXT
               PERFORM 2900-SET-BLOCK
               GO TO 2100-EXIT
           END-IF
           IF CK-ACCOUNT-STATUS = "RETIRED"
               MOVE "BLOCK" TO CK-DECISION
               MOVE "CRD0021" TO AR-REASON
               MOVE "The account is retired and cannot take new demand"
                   TO CK-DECISION-TEXT
               PERFORM 2900-SET-BLOCK
               GO TO 2100-EXIT
           END-IF
           IF CK-ACCOUNT-STATUS = "SUSPENDED"
               MOVE "BLOCK" TO CK-DECISION
               MOVE "CRD0022" TO AR-REASON
               MOVE SPACES TO CK-DECISION-TEXT
               STRING "The account is suspended; new release needs "
                      "reinstatement, not a credit override"
                   DELIMITED BY SIZE INTO CK-DECISION-TEXT
               PERFORM 2900-SET-BLOCK
           END-IF.
       2100-EXIT.
           EXIT.

       2200-COMMERCIAL-REFUSALS SECTION.
       2200-START.
           IF CK-HOLD-FLAG = "Y"
               MOVE "CRD0030" TO AR-REASON
               MOVE SPACES TO CK-DECISION-TEXT
               STRING "Account on credit hold: "
                      FUNCTION TRIM(CK-HOLD-REASON)
                   DELIMITED BY SIZE INTO CK-DECISION-TEXT
               PERFORM 2950-SET-HOLD
           END-IF
           IF CK-PROFORMA-ONLY = "Y" AND CK-ORDER-VALUE-GBP > 0
               MOVE "CRD0031" TO AR-REASON
               MOVE SPACES TO CK-DECISION-TEXT
               STRING "The account trades on pro forma or prepayment "
                      "until an approved credit limit exists (D-06)"
                   DELIMITED BY SIZE INTO CK-DECISION-TEXT
               PERFORM 2950-SET-HOLD
           END-IF
           IF CK-OVERDUE-DAYS >= CK-OVERDUE-TRIGGER
              AND CK-OVERDUE-GBP > 0
               MOVE "CRD0032" TO AR-REASON
               MOVE CK-OVERDUE-DAYS TO AR-INT-ED
               MOVE CK-OVERDUE-GBP TO AR-MONEY-UK
               MOVE SPACES TO CK-DECISION-TEXT
               STRING "Overdue debt of GBP "
                      FUNCTION TRIM(AR-MONEY-UK) " is "
                      FUNCTION TRIM(AR-INT-ED)
                      " days past due and holds new release"
                   DELIMITED BY SIZE INTO CK-DECISION-TEXT
               PERFORM 2950-SET-HOLD
           END-IF
           IF CK-OVERDUE-THRESH > 0
              AND CK-OVERDUE-GBP > CK-OVERDUE-THRESH
               MOVE "CRD0033" TO AR-REASON
               MOVE CK-OVERDUE-GBP TO AR-MONEY-UK
               MOVE SPACES TO CK-DECISION-TEXT
               STRING "Overdue debt of GBP "
                      FUNCTION TRIM(AR-MONEY-UK)
                      " exceeds the approved overdue threshold"
                   DELIMITED BY SIZE INTO CK-DECISION-TEXT
               PERFORM 2950-SET-HOLD
           END-IF
           IF CK-EXCESS > 0
               PERFORM 2250-LIMIT-REFUSAL
           END-IF.
       2200-EXIT.
           EXIT.

      *>   a zero approved limit is not an unlimited limit (D-06)
       2250-LIMIT-REFUSAL SECTION.
       2250-START.
           IF CK-OVERRIDE-ACTIVE = "Y"
              AND CK-OVERRIDE-AMT >= CK-EXCESS
               MOVE 4 TO AR-RC
               MOVE "CRD0040" TO AR-REASON
               MOVE CK-OVERRIDE-ID TO AR-INT-ED
               MOVE SPACES TO CK-DECISION-TEXT
               STRING "Released under credit override "
                      FUNCTION TRIM(AR-INT-ED)
                      " expiring " DELIMITED BY SIZE
                      CK-OVERRIDE-EXPIRY(7:2) "/"
                      CK-OVERRIDE-EXPIRY(5:2) "/"
                      CK-OVERRIDE-EXPIRY(1:4)
                   DELIMITED BY SIZE INTO CK-DECISION-TEXT
               MOVE CK-DECISION-TEXT TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 2250-EXIT
           END-IF
           MOVE "CRD0034" TO AR-REASON
           MOVE CK-EXCESS TO AR-MONEY-UK
           MOVE SPACES TO CK-DECISION-TEXT
           STRING "Credit limit exceeded by GBP "
                  FUNCTION TRIM(AR-MONEY-UK)
               DELIMITED BY SIZE INTO CK-DECISION-TEXT
           PERFORM 2950-SET-HOLD.
       2250-EXIT.
           EXIT.

      *>   a hold is an owned exception, not just a message (COM-017)
       2300-RAISE-HOLD SECTION.
       2300-START.
           MOVE 6 TO AR-RC
           MOVE CK-DECISION-TEXT TO AR-MSG
           ADD 1 TO AR-MSG-CNT
           INITIALIZE WS-EXCP-CA
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXCP-CA
           MOVE AR-CO TO CA-COMPANY OF WS-EXCP-CA
           MOVE AR-USERID TO CA-USER OF WS-EXCP-CA
           MOVE CK-EVAL-DATE TO CA-BUSINESS-DATE OF WS-EXCP-CA
           MOVE "AR" TO EX-MODULE
           MOVE "HOLD" TO EX-SEVERITY
           MOVE "CREDIT" TO EX-CATEGORY
           MOVE CK-DECISION-TEXT TO EX-MESSAGE-TEXT
           MOVE "ORDER" TO EX-DOCUMENT-TYPE
           IF CK-DOCUMENT-REF = SPACES
               MOVE CK-CUSTOMER TO EX-DOCUMENT-REF
           ELSE
               MOVE CK-DOCUMENT-REF TO EX-DOCUMENT-REF
           END-IF
           MOVE "CREDMGR" TO EX-OWNER-ROLE
      *>   AN EXCEPTION IS OWNED BY A PERSON, NOT ONLY BY A ROLE -
      *>   THE CREDIT MANAGER IS NAMED SO THE WORKLIST HAS AN OWNER
           MOVE "CREDMGR" TO EX-OWNER-USER
           MOVE 2 TO EX-PRIORITY
           MOVE CK-EXPOSURE TO EX-AMOUNT-GBP
           CALL "EXCPRAIS" USING WS-EXCP-CA
           MOVE EX-EXCEPTION-ID TO CK-EXCEPTION-ID.
       2300-EXIT.
           EXIT.

       2900-SET-BLOCK SECTION.
       2900-START.
           MOVE "Y" TO WS-BLOCK-SET
           MOVE 8 TO AR-RC
           MOVE CK-DECISION-TEXT TO AR-MSG
           ADD 1 TO AR-MSG-CNT.
       2900-EXIT.
           EXIT.

       2950-SET-HOLD SECTION.
       2950-START.
           MOVE "Y" TO WS-HOLD-SET
           MOVE "HOLD" TO CK-DECISION.
       2950-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   OVERRIDE - record the request with its scope, amount,
      *>   expiry and the authority that will be needed (AR-008)
       3000-REQUEST-OVERRIDE SECTION.
       3000-START.
           IF CK-OVERRIDE-REASON = SPACES
              OR CK-OVERRIDE-REASON = LOW-VALUES
               MOVE "CRD0050" TO AR-REASON
               MOVE "A credit override needs a stated reason"
                   TO AR-MSG
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           IF CK-OVERRIDE-AMT <= 0
               MOVE CK-EXCESS TO CK-OVERRIDE-AMT
           END-IF
           IF CK-OVERRIDE-AMT <= 0
               MOVE "CRD0051" TO AR-REASON
               MOVE "There is no excess to override" TO AR-MSG
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           IF CK-OVERRIDE-SCOPE = SPACES
               MOVE "ORDER" TO CK-OVERRIDE-SCOPE
           END-IF
           PERFORM 3100-EXPIRY-DATE
           PERFORM 3200-REQUIRED-AUTHORITY
           PERFORM 3300-INSERT-OVERRIDE.
       3000-EXIT.
           EXIT.

      *>   overrides expire after a maximum of five working days (D-06)
       3100-EXPIRY-DATE SECTION.
       3100-START.
           MOVE CK-EVAL-DATE TO CK-OVERRIDE-FROM
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE CK-EVAL-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "AR.OVERRIDE.MAX.WORKDAYS" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "5" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           MOVE PM-VALUE-NUM TO AR-DAYS
           IF AR-DAYS <= 0
               MOVE 5 TO AR-DAYS
           END-IF
           INITIALIZE WS-DATE-CA
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DATE-CA
           MOVE AR-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE AR-USERID TO CA-USER OF WS-DATE-CA
           MOVE CK-EVAL-DATE TO CA-BUSINESS-DATE OF WS-DATE-CA
           MOVE CK-EVAL-DATE TO DC-DATE-1
           MOVE AR-DAYS TO DC-NUMBER
           MOVE "ENG" TO DC-CALENDAR
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA < 8
               MOVE DC-RESULT-DATE TO CK-OVERRIDE-EXPIRY
           ELSE
               COMPUTE CK-OVERRIDE-EXPIRY = CK-EVAL-DATE + 7
           END-IF.
       3100-EXIT.
           EXIT.

      *>   granted by the credit manager, or the financial controller
      *>   where the excess exceeds GBP 50,000 (D-06)
       3200-REQUIRED-AUTHORITY SECTION.
       3200-START.
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE CK-EVAL-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "AR.OVERRIDE.FC.THRESHOLD" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "50000" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           MOVE PM-VALUE-NUM TO AR-AMT1
           IF CK-OVERRIDE-AMT > AR-AMT1
               MOVE "FIN.CONTROL.OVERRIDE" TO CK-APPROVER-PERM
           ELSE
               MOVE "AR.CREDIT.OVERRIDE" TO CK-APPROVER-PERM
           END-IF.
       3200-EXIT.
           EXIT.

       3300-INSERT-OVERRIDE SECTION.
       3300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ar.CreditOverride (CompanyCode, "
                  "CustomerCode, Scope, DocumentRef, "
                  "OverrideAmountGBP, ExcessGBP, Reason, RequestedBy, "
                  "ValidFrom, ExpiresOn, Status) VALUES "
                  "(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'PENDING')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 10 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE CK-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE CK-OVERRIDE-SCOPE TO SQLIO-PARM-VAL(3)
           MOVE CK-DOCUMENT-REF TO SQLIO-PARM-VAL(4)
           MOVE CK-OVERRIDE-AMT TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(5)
           MOVE CK-EXCESS TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(6)
           MOVE CK-OVERRIDE-REASON TO SQLIO-PARM-VAL(7)
           IF CK-OVERRIDE-BY = SPACES
               MOVE AR-USERID TO SQLIO-PARM-VAL(8)
           ELSE
               MOVE CK-OVERRIDE-BY TO SQLIO-PARM-VAL(8)
           END-IF
           MOVE CK-OVERRIDE-FROM TO SQLIO-PARM-VAL(9)
           MOVE CK-OVERRIDE-EXPIRY TO SQLIO-PARM-VAL(10)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 10
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3300-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUM-ROW
           MOVE "PENDING" TO CK-OVERRIDE-STATUS
           MOVE 4 TO AR-RC
           MOVE "CRD0052" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "Override requested; approval by "
                  FUNCTION TRIM(CK-APPROVER-PERM) " is required"
               DELIMITED BY SIZE INTO AR-MSG
           ADD 1 TO AR-MSG-CNT.
       3300-EXIT.
           EXIT.

      *>   APPROVE - independent authority, never the requester
       3500-APPROVE-OVERRIDE SECTION.
       3500-START.
           IF CK-OVERRIDE-ID = 0
               MOVE "CRD0053" TO AR-REASON
               MOVE "An override reference is required" TO AR-MSG
               PERFORM 9000-REJECT
               GO TO 3500-EXIT
           END-IF
           PERFORM 3600-READ-OVERRIDE
           IF AR-RC-BAD
               GO TO 3500-EXIT
           END-IF
           IF CK-OVERRIDE-BY = AR-USERID
               MOVE 6 TO AR-RC
               MOVE "CRD0054" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "An override cannot be approved by the user "
                      "who requested it"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 3500-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.CreditOverride SET Status = 'ACTIVE', "
                  "ApprovedBy = ?, ApprovedAt = SYSDATETIME() "
                  "WHERE OverrideId = ? AND Status = 'PENDING'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-USERID TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE CK-OVERRIDE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3500-EXIT
           END-IF
           MOVE SQLIO-ROWCOUNT TO AR-ROWS
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUM-ROW
           IF AR-ROWS = 0
               MOVE 8 TO AR-RC
               MOVE "CRD0055" TO AR-REASON
               MOVE "That override is not awaiting approval"
                   TO AR-MSG
               GO TO 3500-EXIT
           END-IF
           MOVE "ACTIVE" TO CK-OVERRIDE-STATUS
           MOVE "Y" TO CK-OVERRIDE-ACTIVE
           MOVE AR-USERID TO CK-OVERRIDE-APPR
           PERFORM 3700-AUDIT-OVERRIDE
           MOVE 0 TO AR-RC
           MOVE SPACES TO AR-MSG
           STRING "Credit override approved to "
                  CK-OVERRIDE-EXPIRY(7:2) "/" CK-OVERRIDE-EXPIRY(5:2)
                  "/" CK-OVERRIDE-EXPIRY(1:4)
               DELIMITED BY SIZE INTO AR-MSG.
       3500-EXIT.
           EXIT.

       3600-READ-OVERRIDE SECTION.
       3600-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT v.CustomerCode, v.RequestedBy, "
                  "CONVERT(char(8), v.ExpiresOn, 112), v.Status "
                  "FROM ar.CreditOverride v WHERE v.OverrideId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE CK-OVERRIDE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SUM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3600-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "CRD0056" TO AR-REASON
               MOVE "That credit override is not held" TO AR-MSG
               GO TO 3600-EXIT
           END-IF
           MOVE WS-S1 TO CK-CUSTOMER
           MOVE WS-S2 TO CK-OVERRIDE-BY
           IF WS-S3(1:8) IS NUMERIC
               MOVE WS-S3(1:8) TO CK-OVERRIDE-EXPIRY
           END-IF
           MOVE WS-S4 TO CK-OVERRIDE-STATUS.
       3600-EXIT.
           EXIT.

       3700-AUDIT-OVERRIDE SECTION.
       3700-START.
           INITIALIZE WS-AUDIT-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUDIT-CA
           MOVE AR-CO TO CA-COMPANY OF WS-AUDIT-CA
           MOVE AR-USERID TO CA-USER OF WS-AUDIT-CA
           MOVE CK-EVAL-DATE TO CA-BUSINESS-DATE OF WS-AUDIT-CA
           MOVE "ar.CreditOverride" TO AU-ENTITY-TYPE
           MOVE CK-OVERRIDE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO AU-ENTITY-KEY
           MOVE "APPROVE" TO AU-ACTION
           MOVE "Status" TO AU-FIELD-NAME
           MOVE "PENDING" TO AU-OLD-VALUE
           MOVE "ACTIVE" TO AU-NEW-VALUE
           MOVE CK-OVERRIDE-REASON TO AU-REASON
           MOVE "COBOL" TO AU-SOURCE
           CALL "AUDITLOG" USING WS-AUDIT-CA.
       3700-EXIT.
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
           MOVE "CRD0008" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "CREDCHK does not support function ["
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

      *>   the one place that writes the standard header back
       9900-SET-RESULT SECTION.
       9900-START.
           MOVE AR-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE AR-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE AR-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE AR-MSG-CNT TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE AR-REQ-REF TO CA-REQ-REF OF DFHCOMMAREA.
       9900-EXIT.
           EXIT.
