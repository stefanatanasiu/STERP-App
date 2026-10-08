      *>================================================================
      *> PROGRAM-ID : ARENQ
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : THE SALES LEDGER ENQUIRY.  BALANCE, AGED
      *>              PROFILE, UNAPPLIED CREDITS AND DEPOSITS, CREDIT
      *>              UTILISATION, TURNOVER, RECEIPT EFFECTIVENESS,
      *>              AVERAGE DAYS TO PAY AND DSO.
      *>
      *> CALCULATION BASIS (AR-017, AR-022) - STATED HERE BECAUSE THE
      *> BUSINESS ARGUES ABOUT IT EVERY QUARTER:
      *>   AS AT      every figure is the position on EQ-AS-AT.  The
      *>              residual of an item is its original sterling
      *>              amount less the allocations EFFECTIVE ON OR
      *>              BEFORE that date, so a receipt entered later
      *>              never changes an earlier enquiry or report.
      *>   AGEING     by due date.  On the due date an invoice is NOT
      *>              yet overdue.  Buckets are not yet due, 1-30,
      *>              31-60, 61-90 and more than 90 days overdue.
      *>   CREDITS    credit balances are reported separately as
      *>              unapplied credits and deposits; they are never
      *>              aged as overdue positive debt.
      *>   EXPOSURE   gross debt including disputed amounts (D-06),
      *>              less cleared deposits.
      *>   TURNOVER   invoiced revenue, net of VAT, POSTED documents
      *>              only; credit notes deducted.  Order value is
      *>              shown separately and is NOT revenue (AR-022).
      *>   DSO        closing VAT inclusive receivables divided by
      *>              VAT inclusive credit sales of the period, times
      *>              the number of days in the period.  With no
      *>              credit sales the answer is UNDEFINED, not zero.
      *>   DAYS PAID  mean of (allocation effective date minus
      *>              invoice document date) over invoices settled in
      *>              the period.  This is not DSO and not the days
      *>              overdue of a single invoice.
      *> CALLED BY  : ARCTL (.NET), ARSTMT, ARAGE REPORTS
      *> INTERFACE  : CALL "ARENQ" USING WS-ENQ-CA
      *>              COPYBOOK ARENQCA
      *> CALLS      : SQLIO, DATECALC, PERMCHK
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-04-16 RJH  ORIGINAL SALES LEDGER PORT (MVS)
      *> 1993-07-19 KMP  CR0301 SALES LEDGER ENQUIRY SCREEN
      *> 1996-05-02 DJM  CR0611 FOUR BUCKET AGEING REPLACES THREE
      *> 2001-08-13 KMP  CR1288 DSO AND AVERAGE DAYS TO PAY ADDED
      *> 2026-10-05 STERP AS AT REPLAY, DSO BASIS MADE EXPLICIT
      *>================================================================
      *> REQ: AR-017, AR-021, AR-022
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARENQ.
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
       COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERM-CA==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATE-CA==.
       01  WS-HDR.
           05  WS-H-NAME               PIC X(120).
           05  WS-H-STATUS             PIC X(12).
           05  WS-H-CCY                PIC X(4).
           05  WS-H-LIMIT              PIC X(24).
           05  WS-H-HOLD               PIC X(4).
      *>   the aged profile row - fourteen columns, as it has been
      *>   since CR0611 widened it from three buckets to four
       01  WS-AGE.
           05  WS-A-BAL                PIC X(24).
           05  WS-A-DR                 PIC X(24).
           05  WS-A-CR                 PIC X(24).
           05  WS-A-DEP                PIC X(24).
           05  WS-A-NOTDUE             PIC X(24).
           05  WS-A-B1                 PIC X(24).
           05  WS-A-B2                 PIC X(24).
           05  WS-A-B3                 PIC X(24).
           05  WS-A-B4                 PIC X(24).
           05  WS-A-OVER               PIC X(24).
           05  WS-A-DISP               PIC X(24).
           05  WS-A-CNT                PIC X(24).
           05  WS-A-OLDEST             PIC X(10).
           05  WS-A-WORST              PIC X(24).
       01  WS-MET.
           05  WS-M-TURN               PIC X(24).
           05  WS-M-GROSS              PIC X(24).
           05  WS-M-ORDERS             PIC X(24).
           05  WS-M-RCPTVAL            PIC X(24).
           05  WS-M-RCPTCNT            PIC X(24).
           05  WS-M-DISHON             PIC X(24).
           05  WS-M-DISPCNT            PIC X(24).
           05  WS-M-AVGDAYS            PIC X(24).
       01  WS-SW.
           05  WS-DAYS                 PIC S9(7) VALUE 0.
           05  WS-LIMIT                PIC S9(11)V99 VALUE 0.
           05  WS-EXPOS                PIC S9(11)V99 VALUE 0.
       LINKAGE SECTION.
       COPY ARENQCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           PERFORM 0200-VALIDATE THRU 0200-EXIT
           IF AR-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0300-PERMISSION THRU 0300-EXIT
           IF AR-RC-BAD OR AR-RC-HOLD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0400-READ-HEADER THRU 0400-EXIT
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE AR-FUNC
               WHEN "BALANCE"  PERFORM 1000-BALANCE THRU 1000-EXIT
               WHEN "METRICS"  PERFORM 1000-BALANCE THRU 1000-EXIT
                               PERFORM 2000-METRICS THRU 2000-EXIT
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
           INITIALIZE WS-HDR
           INITIALIZE WS-AGE
           INITIALIZE WS-MET
           MOVE CA-FUNCTION OF DFHCOMMAREA TO AR-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO AR-CO
           MOVE CA-SITE OF DFHCOMMAREA TO AR-SITE
           MOVE CA-USER OF DFHCOMMAREA TO AR-USERID
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO AR-BUSDATE
           MOVE "ARENQ" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO AR-RC AR-MSG-CNT
           MOVE SPACES TO AR-REASON AR-MSG
           MOVE "AR-022" TO AR-REQ-REF
           IF EQ-AS-AT = 0
               MOVE AR-BUSDATE TO EQ-AS-AT
           END-IF
           IF EQ-TO-DATE = 0
               MOVE EQ-AS-AT TO EQ-TO-DATE
           END-IF
           IF EQ-FROM-DATE = 0
               PERFORM 0150-YEAR-BACK THRU 0150-EXIT
           END-IF
           MOVE 0 TO EQ-BALANCE EQ-DEBITS EQ-CREDITS EQ-DEPOSITS
           MOVE 0 TO EQ-NOT-DUE EQ-BUCKET-1 EQ-BUCKET-2 EQ-BUCKET-3
           MOVE 0 TO EQ-BUCKET-4 EQ-OVERDUE-TOTAL EQ-DISPUTED
           MOVE 0 TO EQ-OPEN-ITEMS EQ-OLDEST-DUE EQ-WORST-DAYS
           MOVE 0 TO EQ-CREDIT-LIMIT EQ-EXPOSURE EQ-UTILISED-PCT
           MOVE 0 TO EQ-TURNOVER-INV EQ-ORDER-VALUE EQ-CREDIT-SALES
           MOVE 0 TO EQ-RECEIPTS-VALUE EQ-RECEIPT-COUNT
           MOVE 0 TO EQ-DISHONOUR-COUNT EQ-DISPUTE-COUNT
           MOVE 0 TO EQ-AVG-DAYS-PAID EQ-PERIOD-DAYS EQ-DSO
           MOVE "N" TO EQ-HOLD-FLAG
           MOVE "N" TO EQ-DSO-DEFINED
           MOVE SPACES TO EQ-DSO-BASIS
           STRING "Closing VAT inclusive receivables / VAT "
                  "inclusive credit sales x period days"
               DELIMITED BY SIZE INTO EQ-DSO-BASIS.
       0100-EXIT.
           EXIT.

      *>   the default trading window is the twelve months to the
      *>   enquiry date - the old screen used 365 calendar days
       0150-YEAR-BACK SECTION.
       0150-START.
           INITIALIZE WS-DATE-CA
           MOVE "ADDMONTH" TO CA-FUNCTION OF WS-DATE-CA
           MOVE AR-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE AR-USERID TO CA-USER OF WS-DATE-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-DATE-CA
           MOVE EQ-TO-DATE TO DC-DATE-1
           MOVE -12 TO DC-NUMBER
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA > 4
               MOVE EQ-TO-DATE TO EQ-FROM-DATE
           ELSE
               MOVE DC-RESULT-DATE TO EQ-FROM-DATE
           END-IF.
       0150-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF AR-CO = SPACES
               MOVE "ENQ0001" TO AR-REASON
               MOVE "A company code is required" TO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF EQ-CUSTOMER = SPACES OR EQ-CUSTOMER = LOW-VALUES
               MOVE "ENQ0002" TO AR-REASON
               MOVE "A customer account is required" TO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF EQ-FROM-DATE > EQ-TO-DATE
               MOVE "ENQ0003" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The period start is later than the period "
                      "end"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           PERFORM 0250-PERIOD-DAYS THRU 0250-EXIT.
       0200-EXIT.
           EXIT.

       0250-PERIOD-DAYS SECTION.
       0250-START.
           INITIALIZE WS-DATE-CA
           MOVE "DAYSBTWN" TO CA-FUNCTION OF WS-DATE-CA
           MOVE AR-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE AR-USERID TO CA-USER OF WS-DATE-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-DATE-CA
           MOVE EQ-FROM-DATE TO DC-DATE-1
           MOVE EQ-TO-DATE TO DC-DATE-2
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA > 4
               MOVE 365 TO WS-DAYS
           ELSE
               COMPUTE WS-DAYS = DC-RESULT-NUM + 1
           END-IF
           IF WS-DAYS < 1
               MOVE 1 TO WS-DAYS
           END-IF
           MOVE WS-DAYS TO EQ-PERIOD-DAYS.
       0250-EXIT.
           EXIT.

      *>   COM-019 - a customer financial enquiry is a sensitive read
       0300-PERMISSION SECTION.
       0300-START.
           INITIALIZE WS-PERM-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PERM-CA
           MOVE AR-SITE TO CA-SITE OF WS-PERM-CA
           MOVE AR-USERID TO CA-USER OF WS-PERM-CA
           MOVE AR-BUSDATE TO CA-BUSINESS-DATE OF WS-PERM-CA
           MOVE "AR.CUSTOMER.ENQUIRE" TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERM-CA
           IF CA-RETURN-CODE OF WS-PERM-CA > 4
               MOVE 6 TO AR-RC
               MOVE "ENQ0004" TO AR-REASON
               MOVE CA-MESSAGE OF WS-PERM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       0300-EXIT.
           EXIT.

      *>   AR-021 - a retired customer with open items is still shown
       0400-READ-HEADER SECTION.
       0400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT c.LegalName, c.Status, c.CurrencyCode, "
                  "CONVERT(varchar(24), ISNULL(k.CreditLimitGBP,0)), "
                  "CONVERT(varchar(4), ISNULL(k.HoldFlag,0)) "
                  "FROM ar.Customer c LEFT JOIN ar.CustomerCredit k "
                  "ON k.CompanyCode = c.CompanyCode "
                  "AND k.CustomerCode = c.CustomerCode "
                  "WHERE c.CompanyCode = ? AND c.CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE EQ-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 120 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 4 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 4 TO SQLIO-COL-WIDTH(5)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-HDR
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0400-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "ENQ0010" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Customer " FUNCTION TRIM(EQ-CUSTOMER)
                      " is not on the sales ledger"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 0400-EXIT
           END-IF
           MOVE WS-H-NAME TO EQ-CUSTOMER-NAME
           MOVE WS-H-STATUS TO EQ-STATUS
           MOVE WS-H-CCY(1:3) TO EQ-CURRENCY
           COMPUTE WS-LIMIT = FUNCTION NUMVAL(WS-H-LIMIT)
           MOVE WS-LIMIT TO EQ-CREDIT-LIMIT
           IF WS-H-HOLD(1:1) = "1"
               MOVE "Y" TO EQ-HOLD-FLAG
           ELSE
               MOVE "N" TO EQ-HOLD-FLAG
           END-IF.
       0400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   BALANCE - the aged profile as at EQ-AS-AT.  The residual
      *>   is replayed from the allocations effective on or before
      *>   that date so that a later receipt cannot rewrite history.
      *>   REQ: AR-017, AR-021
       1000-BALANCE SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), ISNULL(SUM(x.Res),0)), "
                  "CONVERT(varchar(24), ISNULL(SUM(CASE WHEN "
                  "x.Res > 0 THEN x.Res ELSE 0 END),0)), "
                  "CONVERT(varchar(24), ISNULL(SUM(CASE WHEN "
                  "x.Res < 0 AND x.T <> 'DEPOSIT' THEN x.Res "
                  "ELSE 0 END),0)), "
                  "CONVERT(varchar(24), ISNULL(SUM(CASE WHEN "
                  "x.T = 'DEPOSIT' THEN x.Res ELSE 0 END),0)), "
                  "CONVERT(varchar(24), ISNULL(SUM(CASE WHEN "
                  "x.Res > 0 AND x.D >= x.AsAt THEN x.Res "
                  "ELSE 0 END),0)), "
                  "CONVERT(varchar(24), ISNULL(SUM(CASE WHEN "
                  "x.Res > 0 AND DATEDIFF(day, x.D, x.AsAt) "
                  "BETWEEN 1 AND 30 THEN x.Res ELSE 0 END),0)), "
                  "CONVERT(varchar(24), ISNULL(SUM(CASE WHEN "
                  "x.Res > 0 AND DATEDIFF(day, x.D, x.AsAt) "
                  "BETWEEN 31 AND 60 THEN x.Res ELSE 0 END),0)), "
                  "CONVERT(varchar(24), ISNULL(SUM(CASE WHEN "
                  "x.Res > 0 AND DATEDIFF(day, x.D, x.AsAt) "
                  "BETWEEN 61 AND 90 THEN x.Res ELSE 0 END),0)), "
                  "CONVERT(varchar(24), ISNULL(SUM(CASE WHEN "
                  "x.Res > 0 AND DATEDIFF(day, x.D, x.AsAt) > 90 "
                  "THEN x.Res ELSE 0 END),0)), "
                  "CONVERT(varchar(24), ISNULL(SUM(CASE WHEN "
                  "x.Res > 0 AND x.D < x.AsAt THEN x.Res "
                  "ELSE 0 END),0)), "
                  "CONVERT(varchar(24), ISNULL(SUM(CASE WHEN "
                  "x.P = 1 AND x.Res > 0 THEN x.Res ELSE 0 END),0)), "
                  "CONVERT(varchar(24), SUM(CASE WHEN "
                  "ABS(x.Res) >= 0.005 THEN 1 ELSE 0 END)), "
                  "ISNULL(CONVERT(varchar(10), MIN(CASE WHEN "
                  "x.Res > 0 AND x.D < x.AsAt THEN x.D END), "
                  "112),''), "
                  "CONVERT(varchar(24), ISNULL(MAX(CASE WHEN "
                  "x.Res > 0 THEN DATEDIFF(day, x.D, x.AsAt) "
                  "END),0)) FROM ("
                  "SELECT CONVERT(date, ?) AS AsAt, "
                  "o.ItemType AS T, o.DueDate AS D, "
                  "o.IsDisputed AS P, o.OriginalAmountGBP"
                  " - ISNULL((SELECT SUM(a.AppliedAmountGBP "
                  "+ a.DiscountAmountGBP + a.WriteOffAmountGBP) "
                  "FROM ar.Allocation a WHERE a.Status = 'ACTIVE' "
                  "AND a.EffectiveDate <= ? "
                  "AND a.TargetOpenItemId = o.OpenItemId),0)"
                  " + ISNULL((SELECT SUM(b.AppliedAmountGBP) "
                  "FROM ar.Allocation b WHERE b.Status = 'ACTIVE' "
                  "AND b.EffectiveDate <= ? "
                  "AND b.SourceOpenItemId = o.OpenItemId),0) AS Res "
                  "FROM ar.OpenItem o WHERE o.CompanyCode = ? "
                  "AND o.CustomerCode = ? "
                  "AND o.PostingDate <= ?) x"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE EQ-AS-AT TO SQLIO-PARM-VAL(1)
           MOVE EQ-AS-AT TO SQLIO-PARM-VAL(2)
           MOVE EQ-AS-AT TO SQLIO-PARM-VAL(3)
           MOVE AR-CO TO SQLIO-PARM-VAL(4)
           MOVE EQ-CUSTOMER TO SQLIO-PARM-VAL(5)
           MOVE EQ-AS-AT TO SQLIO-PARM-VAL(6)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 6
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 14 TO SQLIO-COL-CNT
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 14
               MOVE 24 TO SQLIO-COL-WIDTH(AR-I)
           END-PERFORM
           MOVE 10 TO SQLIO-COL-WIDTH(13)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-AGE
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-UNPACK-AGE THRU 1200-EXIT
           PERFORM 1300-EXPOSURE THRU 1300-EXIT
           MOVE SPACES TO AR-MSG
           MOVE EQ-BALANCE TO AR-MONEY-UK
           MOVE EQ-AS-AT TO AR-DATE-ED
           STRING "Balance GBP " FUNCTION TRIM(AR-MONEY-UK)
                  " for " FUNCTION TRIM(EQ-CUSTOMER)
                  " as at " AR-DATE-ED(7:2) "/" AR-DATE-ED(5:2)
                  "/" AR-DATE-ED(1:4)
               DELIMITED BY SIZE INTO AR-MSG.
       1000-EXIT.
           EXIT.

      *>   AR-017 - the residual used above is the original sterling
      *>   amount less the allocations applied to the item, plus what
      *>   the item was itself used to settle, counting only the
      *>   allocations effective on or before the as at date.  That
      *>   is what makes an aged report repeatable.
       1200-UNPACK-AGE SECTION.
       1200-START.
           COMPUTE EQ-BALANCE = FUNCTION NUMVAL(WS-A-BAL)
           COMPUTE EQ-DEBITS = FUNCTION NUMVAL(WS-A-DR)
           COMPUTE EQ-CREDITS = FUNCTION NUMVAL(WS-A-CR)
           COMPUTE EQ-DEPOSITS = FUNCTION NUMVAL(WS-A-DEP)
           COMPUTE EQ-NOT-DUE = FUNCTION NUMVAL(WS-A-NOTDUE)
           COMPUTE EQ-BUCKET-1 = FUNCTION NUMVAL(WS-A-B1)
           COMPUTE EQ-BUCKET-2 = FUNCTION NUMVAL(WS-A-B2)
           COMPUTE EQ-BUCKET-3 = FUNCTION NUMVAL(WS-A-B3)
           COMPUTE EQ-BUCKET-4 = FUNCTION NUMVAL(WS-A-B4)
           COMPUTE EQ-OVERDUE-TOTAL = FUNCTION NUMVAL(WS-A-OVER)
           COMPUTE EQ-DISPUTED = FUNCTION NUMVAL(WS-A-DISP)
           COMPUTE EQ-OPEN-ITEMS = FUNCTION NUMVAL(WS-A-CNT)
           COMPUTE EQ-WORST-DAYS = FUNCTION NUMVAL(WS-A-WORST)
           IF WS-A-OLDEST NOT = SPACES
               MOVE WS-A-OLDEST(1:8) TO AR-COMPACT-DATE
               MOVE AR-COMPACT-DATE TO AR-NUM-DATE
               MOVE AR-NUM-DATE TO EQ-OLDEST-DUE
           ELSE
               MOVE 0 TO EQ-OLDEST-DUE
           END-IF
      *>   the bucket totals must agree with the same date debit
      *>   balance, otherwise the report is not reconcilable
           COMPUTE AR-AMT1 = EQ-NOT-DUE + EQ-OVERDUE-TOTAL
           IF FUNCTION ABS(AR-AMT1 - EQ-DEBITS) > 0.005
               MOVE 4 TO AR-RC
               MOVE "ENQ0020" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The ageing buckets do not reconcile to the "
                      "same date debit balance"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       1200-EXIT.
           EXIT.

      *>   D-06 - disputes stay in exposure, cleared deposits reduce
      *>   it.  Utilisation on a nil limit is reported as zero and
      *>   the hold flag carries the real answer.
      *>   REQ: AR-022
       1300-EXPOSURE SECTION.
       1300-START.
           COMPUTE WS-EXPOS = EQ-DEBITS + EQ-CREDITS + EQ-DEPOSITS
           MOVE WS-EXPOS TO EQ-EXPOSURE
           IF WS-LIMIT > 0
               COMPUTE EQ-UTILISED-PCT ROUNDED =
                   (WS-EXPOS / WS-LIMIT) * 100
           ELSE
               MOVE 0 TO EQ-UTILISED-PCT
           END-IF
           IF WS-LIMIT > 0 AND WS-EXPOS > WS-LIMIT
               MOVE 4 TO AR-RC
               MOVE "ENQ0021" TO AR-REASON
               MOVE SPACES TO AR-MSG
               MOVE EQ-UTILISED-PCT TO AR-MONEY-UK
               STRING "Exposure is "
                      FUNCTION TRIM(AR-MONEY-UK)
                      " per cent of the approved credit limit"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       1300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   METRICS - trading, collections and DSO for the stated
      *>   period.  Invoiced revenue and order value are reported
      *>   separately so that nobody confuses the two (AR-022).
      *>   REQ: AR-022
       2000-METRICS SECTION.
       2000-START.
           PERFORM 2100-TRADING THRU 2100-EXIT
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-ORDER-VALUE THRU 2200-EXIT
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2300-RECEIPTS THRU 2300-EXIT
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2400-DAYS-TO-PAY THRU 2400-EXIT
           IF AR-RC-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2500-DSO THRU 2500-EXIT.
       2000-EXIT.
           EXIT.

       2100-TRADING SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), ISNULL(SUM(CASE "
                  "WHEN i.DocumentType = 'CREDITNOTE' "
                  "THEN -i.NetAmountGBP ELSE i.NetAmountGBP "
                  "END),0)), CONVERT(varchar(24), ISNULL(SUM(CASE "
                  "WHEN i.DocumentType = 'CREDITNOTE' "
                  "THEN -i.GrossAmountGBP ELSE i.GrossAmountGBP "
                  "END),0)) FROM ar.SalesInvoice i "
                  "WHERE i.CompanyCode = ? AND i.CustomerCode = ? "
                  "AND i.Status = 'POSTED' "
                  "AND i.DocumentType <> 'PROFORMA' "
                  "AND i.PostingDate BETWEEN ? AND ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE EQ-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE EQ-FROM-DATE TO SQLIO-PARM-VAL(3)
           MOVE EQ-TO-DATE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-MET
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2100-EXIT
           END-IF
           COMPUTE EQ-TURNOVER-INV = FUNCTION NUMVAL(WS-M-TURN)
           COMPUTE EQ-CREDIT-SALES = FUNCTION NUMVAL(WS-M-GROSS).
       2100-EXIT.
           EXIT.

      *>   order value is what the customer has asked for, not what
      *>   has been invoiced.  It never feeds revenue or DSO.
       2200-ORDER-VALUE SECTION.
       2200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), "
                  "ISNULL(SUM(s.GrossAmountGBP),0)) "
                  "FROM so.SalesOrder s WHERE s.CompanyCode = ? "
                  "AND s.CustomerCode = ? "
                  "AND s.CommercialStatus <> 'CANCELLED' "
                  "AND s.OrderDate BETWEEN ? AND ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE EQ-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE EQ-FROM-DATE TO SQLIO-PARM-VAL(3)
           MOVE EQ-TO-DATE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-M-ORDERS
           IF SQLIO-ERROR
      *>       distribution may not be installed on this company;
      *>       the enquiry still answers the ledger figures
               MOVE 0 TO EQ-ORDER-VALUE
               MOVE 0 TO SQLIO-SQLCODE
               GO TO 2200-EXIT
           END-IF
           COMPUTE EQ-ORDER-VALUE = FUNCTION NUMVAL(WS-M-ORDERS).
       2200-EXIT.
           EXIT.

      *>   receipt effectiveness: what was received, how many times,
      *>   how many came back and how many queries were raised
       2300-RECEIPTS SECTION.
       2300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), ISNULL(SUM(CASE WHEN "
                  "r.ConfirmationState = 'CONFIRMED' "
                  "THEN r.GrossReceivedGBP ELSE 0 END),0)), "
                  "CONVERT(varchar(24), SUM(CASE WHEN "
                  "r.ConfirmationState = 'CONFIRMED' THEN 1 "
                  "ELSE 0 END)), CONVERT(varchar(24), SUM(CASE WHEN "
                  "r.ConfirmationState IN ('DISHONOURED','REVERSED') "
                  "THEN 1 ELSE 0 END)), (SELECT CONVERT(varchar(24), "
                  "COUNT(*)) FROM ar.CollectionCase k "
                  "WHERE k.CompanyCode = r.CompanyCode "
                  "AND k.CustomerCode = ? "
                  "AND k.CaseType = 'DISPUTE' "
                  "AND CONVERT(date, k.OpenedAt) BETWEEN ? AND ?) "
                  "FROM ar.Receipt r WHERE r.CompanyCode = ? "
                  "AND r.CustomerCode = ? "
                  "AND r.ReceiptDate BETWEEN ? AND ? "
                  "GROUP BY r.CompanyCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE EQ-CUSTOMER TO SQLIO-PARM-VAL(1)
           MOVE EQ-FROM-DATE TO SQLIO-PARM-VAL(2)
           MOVE EQ-TO-DATE TO SQLIO-PARM-VAL(3)
           MOVE AR-CO TO SQLIO-PARM-VAL(4)
           MOVE EQ-CUSTOMER TO SQLIO-PARM-VAL(5)
           MOVE EQ-FROM-DATE TO SQLIO-PARM-VAL(6)
           MOVE EQ-TO-DATE TO SQLIO-PARM-VAL(7)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 7
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE 24 TO SQLIO-COL-WIDTH(AR-I)
           END-PERFORM
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-M-RCPTVAL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 0 TO EQ-RECEIPTS-VALUE EQ-RECEIPT-COUNT
               MOVE 0 TO EQ-DISHONOUR-COUNT EQ-DISPUTE-COUNT
               GO TO 2300-EXIT
           END-IF
           COMPUTE EQ-RECEIPTS-VALUE = FUNCTION NUMVAL(WS-M-RCPTVAL)
           COMPUTE EQ-RECEIPT-COUNT = FUNCTION NUMVAL(WS-M-RCPTCNT)
           COMPUTE EQ-DISHONOUR-COUNT = FUNCTION NUMVAL(WS-M-DISHON)
           COMPUTE EQ-DISPUTE-COUNT = FUNCTION NUMVAL(WS-M-DISPCNT).
       2300-EXIT.
           EXIT.

      *>   average days to pay - settlement date less invoice date,
      *>   over the invoices settled inside the period.  This is NOT
      *>   the days overdue of any single invoice (AR-022).
       2400-DAYS-TO-PAY SECTION.
       2400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), ISNULL(AVG(CONVERT("
                  "decimal(18,4), DATEDIFF(day, o.DocumentDate, "
                  "a.EffectiveDate))),0)) FROM ar.Allocation a "
                  "INNER JOIN ar.OpenItem o "
                  "ON o.OpenItemId = a.TargetOpenItemId "
                  "WHERE a.CompanyCode = ? AND a.CustomerCode = ? "
                  "AND a.Status = 'ACTIVE' "
                  "AND o.ItemType = 'INVOICE' "
                  "AND a.EffectiveDate BETWEEN ? AND ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE EQ-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE EQ-FROM-DATE TO SQLIO-PARM-VAL(3)
           MOVE EQ-TO-DATE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(AR-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-M-AVGDAYS
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2400-EXIT
           END-IF
           COMPUTE EQ-AVG-DAYS-PAID ROUNDED =
               FUNCTION NUMVAL(WS-M-AVGDAYS).
       2400-EXIT.
           EXIT.

      *>   DSO.  With no credit sales in the period the ratio has no
      *>   meaning, so it is reported as undefined rather than as
      *>   zero or as an enormous number (AR-022).
       2500-DSO SECTION.
       2500-START.
           IF EQ-CREDIT-SALES <= 0
               MOVE 0 TO EQ-DSO
               MOVE "N" TO EQ-DSO-DEFINED
               MOVE 4 TO AR-RC
               MOVE "ENQ0030" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Days sales outstanding is undefined: there "
                      "were no credit sales in the stated period"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 2500-EXIT
           END-IF
           COMPUTE EQ-DSO ROUNDED =
               (EQ-BALANCE / EQ-CREDIT-SALES) * EQ-PERIOD-DAYS
           MOVE "Y" TO EQ-DSO-DEFINED
           IF AR-RC-OK
               MOVE SPACES TO AR-MSG
               MOVE EQ-DSO TO AR-MONEY-UK
               MOVE EQ-PERIOD-DAYS TO AR-INT-ED
               STRING "DSO " FUNCTION TRIM(AR-MONEY-UK)
                      " days over a period of "
                      FUNCTION TRIM(AR-INT-ED) " days, "
                      FUNCTION TRIM(EQ-DSO-BASIS)
                   DELIMITED BY SIZE INTO AR-MSG
           END-IF.
       2500-EXIT.
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
           MOVE "ENQ0009" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "ARENQ does not support function ["
                  FUNCTION TRIM(AR-FUNC) "]"
               DELIMITED BY SIZE INTO AR-MSG
           ADD 1 TO AR-MSG-CNT.
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

       9900-SET-RESULT SECTION.
       9900-START.
           MOVE AR-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE AR-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE AR-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE AR-MSG-CNT TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE AR-REQ-REF TO CA-REQ-REF OF DFHCOMMAREA.
       9900-EXIT.
           EXIT.
