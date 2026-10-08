      *>================================================================
      *> PROGRAM-ID : BACKORD
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT
      *> PURPOSE    : PARTIAL FULFILMENT AND THE BACK ORDER BOOK.  TWO
      *>              QUANTITIES ARE DERIVED FOR EVERY LINE AND EVERY
      *>              SCHEDULE:
      *>                REMAINING TO DESPATCH = ACCEPTED - CANCELLED
      *>                                        - DESPATCHED
      *>                REMAINING TO INVOICE  = CHARGEABLE CONFIRMED
      *>                                        FULFILMENT - WITHDRAWN
      *>                                        BEFORE BILLING
      *>                                        - ALREADY BILLED
      *>              A CONFIRMED DESPATCH IS NEVER ERASED BY A
      *>              CANCELLATION, AND A RETURN OF BILLED GOODS IS A
      *>              CREDIT AND RETURN MATTER: IT CHANGES NEITHER
      *>              REMAINING QUANTITY.  COMPLETING ONE SCHEDULE
      *>              DOES NOT CLOSE AN UNFULFILLED BALANCE ON
      *>              ANOTHER, AND HEADER COMPLETION IS DERIVED FROM
      *>              THE LINE OUTCOMES PLUS ANY OUTSTANDING FINANCIAL
      *>              OR RETURN EXCEPTION - IT IS NOT A WAY TO FORCE
      *>              RESIDUAL QUANTITIES TO ZERO.
      *> CALLED BY  : CUSTOMER SERVICE, DESPATCH, BILLING, .NET CLIENT
      *> INTERFACE  : CALL "BACKORD" USING WS-BACKORD-CA
      *>              COPYBOOK BACKORDCA
      *> CALLS      : SQLIO, AUDITLOG
      *> NOTE       : CREATEBO and CLOSE do not release reservations;
      *>              SOAMEND owns that through STKRESV so that only
      *>              the unused part of a reservation is ever freed.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1992-06-30 RJH  CR0188 BACK ORDER BOOK
      *> 1999-11-30 KMP  Y2K - PROMISE DATES HOLD FOUR DIGIT YEARS
      *> 2007-09-11 KMP  CR1944 PER SCHEDULE COMPLETION
      *> 2026-10-06 STERP SO-014, SO-015 REMAINING QUANTITY RULES
      *>================================================================
      *> REQ: SO-015, SO-014
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BACKORD.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 30/06/1992.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUD-CA==.
       01  WS-CTL.
           05  WS-FUNC                 PIC X(8).
           05  WS-CO                   PIC X(10).
           05  WS-SITE                 PIC X(10).
           05  WS-USER                 PIC X(20).
           05  WS-BUSDATE              PIC 9(8).
           05  WS-CORREL               PIC X(36).
           05  WS-RC                   PIC 9(2).
           05  WS-REASON               PIC X(8).
           05  WS-MSG                  PIC X(200).
           05  WS-MSGCNT               PIC 9(2).
           05  WS-TRAN-OPEN            PIC X.
           05  WS-I                    PIC S9(4).
           05  WS-P                    PIC S9(4).
           05  WS-FOUND                PIC X.
           05  WS-NEW-SCHED            PIC S9(9).
           05  WS-LINE-ID              PIC X(30).
       01  WS-EDITS.
           05  WS-MONEY-ED             PIC -(13)9.99.
           05  WS-QTY-ED               PIC -(13)9.999999.
           05  WS-INT-ED               PIC -(9)9.
           05  WS-UK-MONEY             PIC Z,ZZZ,ZZZ,ZZ9.99-.
           05  WS-UK-QTY               PIC Z,ZZZ,ZZ9.999999-.
       01  WS-AMTS.
           05  WS-AMT1                 PIC S9(13)V99.
           05  WS-AMT2                 PIC S9(13)V99.
           05  WS-AMT3                 PIC S9(13)V99.
           05  WS-PPQ                  PIC S9(9)V9(6).
      *>   order header
       01  WS-OHDR.
           05  WS-H-ID                 PIC X(24).
           05  WS-H-CUST               PIC X(20).
           05  WS-H-CURR               PIC X(3).
           05  WS-H-COMM               PIC X(12).
           05  WS-H-FULFIL             PIC X(14).
           05  WS-H-BILLING            PIC X(14).
           05  WS-H-OPENRET            PIC X(10).
      *>   one line or schedule row of the back order book
       01  WS-BROW.
           05  WS-B-LINE               PIC X(10).
           05  WS-B-SCHED              PIC X(10).
           05  WS-B-ITEM               PIC X(40).
           05  WS-B-DESC               PIC X(100).
           05  WS-B-UOM                PIC X(10).
           05  WS-B-ACCEPTED           PIC X(24).
           05  WS-B-CANCELLED          PIC X(24).
           05  WS-B-DESPATCHED         PIC X(24).
           05  WS-B-BILLED             PIC X(24).
           05  WS-B-RETURNED           PIC X(24).
           05  WS-B-WITHDRAWN          PIC X(24).
           05  WS-B-PRICE              PIC X(24).
           05  WS-B-PPQ                PIC X(24).
           05  WS-B-D1                 PIC X(12).
           05  WS-B-D2                 PIC X(12).
           05  WS-B-PROMISE            PIC X(8).
           05  WS-B-REQUESTED          PIC X(8).
           05  WS-B-STATUS             PIC X(10).
       01  WS-TWO.
           05  WS-T1                   PIC X(30).
           05  WS-T2                   PIC X(30).
       LINKAGE SECTION.
       COPY BACKORDCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE-INPUT
           IF WS-RC > 4
               GO TO 0000-EXIT
           END-IF
           PERFORM 0300-READ-ORDER
           IF WS-RC > 4
               GO TO 0000-EXIT
           END-IF
           EVALUATE WS-FUNC
               WHEN "EVALUATE" PERFORM 1000-EVALUATE-BALANCE
               WHEN "LIST"     PERFORM 1000-EVALUATE-BALANCE
               WHEN "CREATEBO" PERFORM 2000-CREATE-BACKORDER
               WHEN "CLOSE"    PERFORM 3000-CLOSE-WHAT-IS-DONE
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE
           IF WS-RC > 4
               PERFORM 9500-ROLLBACK
           END-IF.
       0000-EXIT.
           PERFORM 9900-SET-RESULT
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE WS-CTL
           INITIALIZE WS-EDITS
           INITIALIZE WS-AMTS
           INITIALIZE WS-OHDR
           INITIALIZE WS-BROW
           INITIALIZE WS-TWO
           MOVE "N" TO WS-TRAN-OPEN
           MOVE 0 TO WS-RC WS-MSGCNT
           MOVE SPACES TO WS-REASON WS-MSG
           MOVE CA-FUNCTION OF DFHCOMMAREA TO WS-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO WS-CO
           MOVE CA-SITE OF DFHCOMMAREA TO WS-SITE
           MOVE CA-USER OF DFHCOMMAREA TO WS-USER
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO WS-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO WS-CORREL
           MOVE "BACKORD" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE "SO-015" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE 0 TO BO-ACCEPTED-QTY BO-CANCELLED-QTY
           MOVE 0 TO BO-DESPATCHED-QTY BO-BILLED-QTY
           MOVE 0 TO BO-RETURNED-QTY BO-WITHDRAWN-QTY
           MOVE 0 TO BO-REMAIN-DESPATCH BO-REMAIN-INVOICE
           MOVE 0 TO BO-BILLED-NET BO-OPEN-NET
           MOVE 0 TO BO-OPEN-LINE-CNT BO-OPEN-SCHED-CNT
           MOVE 0 TO BO-EXCEPTION-CNT BO-CLOSED-LINES
           MOVE 0 TO BO-CLOSED-SCHEDS BO-CARRIED-QTY
           MOVE 0 TO BO-LINE-CNT
           MOVE SPACES TO BO-EXCEPTION-TEXT
           MOVE "N" TO BO-HEADER-COMPLETE
           IF BO-INSTRUCTION = SPACES
               MOVE "KEEP" TO BO-INSTRUCTION
           END-IF
           IF BO-BY-SCHEDULE = SPACE OR BO-BY-SCHEDULE = LOW-VALUE
               MOVE "N" TO BO-BY-SCHEDULE
           END-IF.
       0100-EXIT.
           EXIT.

       0200-VALIDATE-INPUT SECTION.
       0200-START.
           IF WS-CO = SPACES OR BO-ORDER-NUMBER = SPACES
               MOVE "BKO0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A company code and an order number are "
                      "required" DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF WS-FUNC = "CREATEBO"
               IF NOT BO-KEEP AND NOT BO-CANCEL AND
                  NOT BO-SUBSTITUTE
                   MOVE "BKO0004" TO WS-REASON
                   MOVE SPACES TO WS-MSG
                   STRING "The customer's instruction must be KEEP, "
                          "CANCEL or SUBSTITUTE"
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM 9000-REJECT
                   GO TO 0200-EXIT
               END-IF
               IF BO-SUBSTITUTE AND (BO-SUBST-ITEM = SPACES OR
                  BO-SUBST-APPROVAL = 0)
                   MOVE "BKO0005" TO WS-REASON
                   MOVE SPACES TO WS-MSG
                   STRING "A substitution needs a substitute item "
                          "and an approval reference"
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM 9000-REJECT
               END-IF
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0300-READ-ORDER SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), o.SalesOrderId), "
                  "o.CustomerCode, o.CurrencyCode, "
                  "o.CommercialStatus, o.FulfilmentStatus, "
                  "o.BillingStatus, CONVERT(varchar(10), (SELECT "
                  "COUNT(*) FROM so.ReturnAuthorisation ra WHERE "
                  "ra.CompanyCode = o.CompanyCode AND "
                  "ra.OriginalOrderNumber = o.OrderNumber AND "
                  "ra.Status NOT IN ('CLOSED','REJECTED'))) FROM "
                  "so.SalesOrder o WHERE o.CompanyCode = ? AND "
                  "o.OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE BO-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 14 TO SQLIO-COL-WIDTH(5)
           MOVE 14 TO SQLIO-COL-WIDTH(6)
           MOVE 10 TO SQLIO-COL-WIDTH(7)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-OHDR
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO WS-RC
               MOVE "BKO0002" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Order " FUNCTION TRIM(BO-ORDER-NUMBER)
                      " is not held for company "
                      FUNCTION TRIM(WS-CO)
                   DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
               GO TO 0300-EXIT
           END-IF
           COMPUTE BO-SALES-ORDER-ID = FUNCTION NUMVAL(WS-H-ID)
           MOVE WS-H-CUST TO BO-CUSTOMER
           MOVE WS-H-CURR TO BO-CURRENCY
           MOVE WS-H-COMM TO BO-COMM-STATUS
           MOVE WS-H-FULFIL TO BO-FULFIL-STATUS
           MOVE WS-H-BILLING TO BO-BILLING-STATUS
           COMPUTE BO-EXCEPTION-CNT = FUNCTION NUMVAL(WS-H-OPENRET).
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   EVALUATE / LIST - the two remaining quantities, per line and
      *>   optionally per schedule.  Nothing is written.
       1000-EVALUATE-BALANCE SECTION.
       1000-START.
           PERFORM 1100-READ-LINE-VIEW
           IF WS-RC > 4
               GO TO 1000-EXIT
           END-IF
           IF BO-SCHEDULE-VIEW
               PERFORM 1300-READ-SCHEDULE-VIEW
           END-IF
           PERFORM 1800-DERIVE-COMPLETION
           PERFORM 1900-REPORT-BALANCE.
       1000-EXIT.
           EXIT.

      *>   the withdrawn leg is a return of goods that had not been
      *>   billed; a return against an invoice is a credit matter and
      *>   is deliberately excluded here
       1100-READ-LINE-VIEW SECTION.
       1100-START.
           MOVE 0 TO BO-LINE-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(10), l.LineNum), '0', "
                  "ISNULL(l.ItemCode,''), l.Description, "
                  "ISNULL(l.UomCode,''), CONVERT(varchar(24), "
                  "l.AcceptedQty), CONVERT(varchar(24), "
                  "l.CancelledQty), CONVERT(varchar(24), "
                  "l.DespatchedQty), CONVERT(varchar(24), "
                  "l.BilledQty), CONVERT(varchar(24), l.ReturnedQty),"
                  " CONVERT(varchar(24), ISNULL((SELECT "
                  "SUM(rl.AcceptedQty) FROM so.ReturnLine rl JOIN "
                  "so.ReturnAuthorisation ra ON ra.ReturnId = "
                  "rl.ReturnId WHERE ra.CompanyCode = l.CompanyCode "
                  "AND ra.OriginalOrderNumber = l.OrderNumber AND "
                  "rl.ItemCode = l.ItemCode AND "
                  "ra.OriginalInvoiceNumber IS NULL),0)), "
                  "CONVERT(varchar(24), l.UnitPrice), "
                  "CONVERT(varchar(24), l.PricePerQty), "
                  "CONVERT(varchar(12), l.DiscountPct1), "
                  "CONVERT(varchar(12), l.DiscountPct2), '00000000', "
                  "'00000000', l.Status FROM so.SalesOrderLine l "
                  "WHERE l.CompanyCode = ? AND l.OrderNumber = ? "
                  "ORDER BY l.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE BO-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           PERFORM 1200-SET-BOOK-COLUMNS
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1100-EXIT
           END-IF
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BROW
           PERFORM UNTIL SQLIO-SQLCODE NOT = 0 OR BO-LINE-CNT >= 40
               ADD 1 TO BO-LINE-CNT
               MOVE BO-LINE-CNT TO WS-I
               PERFORM 1250-DERIVE-ONE-ROW
               PERFORM 1280-ADD-TO-ORDER-TOTALS
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-BROW
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BROW
           COMPUTE BO-REMAIN-DESPATCH = BO-ACCEPTED-QTY
                   - BO-CANCELLED-QTY - BO-DESPATCHED-QTY
           COMPUTE BO-REMAIN-INVOICE = BO-DESPATCHED-QTY
                   - BO-WITHDRAWN-QTY - BO-BILLED-QTY
           IF BO-REMAIN-INVOICE < 0
               MOVE 0 TO BO-REMAIN-INVOICE
           END-IF.
       1100-EXIT.
           EXIT.

       1200-SET-BOOK-COLUMNS SECTION.
       1200-START.
           MOVE 18 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 40 TO SQLIO-COL-WIDTH(3)
           MOVE 100 TO SQLIO-COL-WIDTH(4)
           MOVE 10 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 24 TO SQLIO-COL-WIDTH(8)
           MOVE 24 TO SQLIO-COL-WIDTH(9)
           MOVE 24 TO SQLIO-COL-WIDTH(10)
           MOVE 24 TO SQLIO-COL-WIDTH(11)
           MOVE 24 TO SQLIO-COL-WIDTH(12)
           MOVE 24 TO SQLIO-COL-WIDTH(13)
           MOVE 12 TO SQLIO-COL-WIDTH(14)
           MOVE 12 TO SQLIO-COL-WIDTH(15)
           MOVE 8 TO SQLIO-COL-WIDTH(16)
           MOVE 8 TO SQLIO-COL-WIDTH(17)
           MOVE 10 TO SQLIO-COL-WIDTH(18).
       1200-EXIT.
           EXIT.

      *>   the two rules of specification 05 section 4, applied to one
      *>   row.  A return of billed goods is in BO-L-RETURNED but is
      *>   not in BO-L-WITHDRAWN, so it changes neither remainder.
       1250-DERIVE-ONE-ROW SECTION.
       1250-START.
           COMPUTE BO-L-LINE-NUM(WS-I) = FUNCTION NUMVAL(WS-B-LINE)
           COMPUTE BO-L-SCHED-NO(WS-I) = FUNCTION NUMVAL(WS-B-SCHED)
           MOVE WS-B-ITEM TO BO-L-ITEM(WS-I)
           MOVE WS-B-DESC TO BO-L-DESC(WS-I)
           MOVE WS-B-UOM TO BO-L-UOM(WS-I)
           COMPUTE BO-L-ACCEPTED(WS-I) =
                   FUNCTION NUMVAL(WS-B-ACCEPTED)
           COMPUTE BO-L-CANCELLED(WS-I) =
                   FUNCTION NUMVAL(WS-B-CANCELLED)
           COMPUTE BO-L-DESPATCHED(WS-I) =
                   FUNCTION NUMVAL(WS-B-DESPATCHED)
           COMPUTE BO-L-BILLED(WS-I) = FUNCTION NUMVAL(WS-B-BILLED)
           COMPUTE BO-L-RETURNED(WS-I) =
                   FUNCTION NUMVAL(WS-B-RETURNED)
           COMPUTE BO-L-WITHDRAWN(WS-I) =
                   FUNCTION NUMVAL(WS-B-WITHDRAWN)
           COMPUTE BO-L-UNIT-PRICE(WS-I) =
                   FUNCTION NUMVAL(WS-B-PRICE)
           COMPUTE BO-L-PROMISE(WS-I) = FUNCTION NUMVAL(WS-B-PROMISE)
           COMPUTE BO-L-REQUESTED(WS-I) =
                   FUNCTION NUMVAL(WS-B-REQUESTED)
           MOVE WS-B-STATUS TO BO-L-STATUS(WS-I)
           MOVE BO-INSTRUCTION TO BO-L-INSTRUCT(WS-I)
           COMPUTE BO-L-REM-DESP(WS-I) = BO-L-ACCEPTED(WS-I)
                   - BO-L-CANCELLED(WS-I) - BO-L-DESPATCHED(WS-I)
           IF BO-L-REM-DESP(WS-I) < 0
               MOVE 0 TO BO-L-REM-DESP(WS-I)
           END-IF
           COMPUTE BO-L-REM-INV(WS-I) = BO-L-DESPATCHED(WS-I)
                   - BO-L-WITHDRAWN(WS-I) - BO-L-BILLED(WS-I)
           IF BO-L-REM-INV(WS-I) < 0
               MOVE 0 TO BO-L-REM-INV(WS-I)
           END-IF
           PERFORM 1270-BILLED-NET.
       1250-EXIT.
           EXIT.

      *>   the billed value uses the captured price, not a fresh one
       1270-BILLED-NET SECTION.
       1270-START.
           COMPUTE WS-PPQ = FUNCTION NUMVAL(WS-B-PPQ)
           IF WS-PPQ = 0
               MOVE 1 TO WS-PPQ
           END-IF
           COMPUTE WS-AMT1 ROUNDED = BO-L-BILLED(WS-I) / WS-PPQ
                   * BO-L-UNIT-PRICE(WS-I)
           COMPUTE WS-AMT2 ROUNDED = WS-AMT1 *
                   (1 - FUNCTION NUMVAL(WS-B-D1) / 100)
           COMPUTE WS-AMT3 ROUNDED = WS-AMT2 *
                   (1 - FUNCTION NUMVAL(WS-B-D2) / 100)
           MOVE WS-AMT3 TO BO-L-BILLED-NET(WS-I).
       1270-EXIT.
           EXIT.

       1280-ADD-TO-ORDER-TOTALS SECTION.
       1280-START.
           ADD BO-L-ACCEPTED(WS-I) TO BO-ACCEPTED-QTY
           ADD BO-L-CANCELLED(WS-I) TO BO-CANCELLED-QTY
           ADD BO-L-DESPATCHED(WS-I) TO BO-DESPATCHED-QTY
           ADD BO-L-BILLED(WS-I) TO BO-BILLED-QTY
           ADD BO-L-RETURNED(WS-I) TO BO-RETURNED-QTY
           ADD BO-L-WITHDRAWN(WS-I) TO BO-WITHDRAWN-QTY
           ADD BO-L-BILLED-NET(WS-I) TO BO-BILLED-NET
           IF BO-L-REM-DESP(WS-I) > 0
               ADD 1 TO BO-OPEN-LINE-CNT
               COMPUTE WS-AMT1 ROUNDED = BO-L-REM-DESP(WS-I)
                       * BO-L-UNIT-PRICE(WS-I)
               ADD WS-AMT1 TO BO-OPEN-NET
           END-IF.
       1280-EXIT.
           EXIT.

      *>   CR1944 - a schedule has its own balance.  Completing one
      *>   schedule cannot close the balance on another, so each row
      *>   is derived from its own quantity and its own despatches.
       1300-READ-SCHEDULE-VIEW SECTION.
       1300-START.
           MOVE 0 TO BO-LINE-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(10), s.LineNum), "
                  "CONVERT(varchar(10), s.ScheduleNo), "
                  "ISNULL(l.ItemCode,''), l.Description, "
                  "ISNULL(l.UomCode,''), CONVERT(varchar(24), "
                  "s.Quantity), '0', CONVERT(varchar(24), "
                  "s.DespatchedQty), '0', '0', '0', "
                  "CONVERT(varchar(24), l.UnitPrice), "
                  "CONVERT(varchar(24), l.PricePerQty), "
                  "CONVERT(varchar(12), l.DiscountPct1), "
                  "CONVERT(varchar(12), l.DiscountPct2), "
                  "CONVERT(char(8), s.CurrentPromiseDate, 112), "
                  "CONVERT(char(8), s.RequestedDate, 112), s.Status "
                  "FROM so.DeliverySchedule s JOIN "
                  "so.SalesOrderLine l ON l.SalesOrderLineId = "
                  "s.SalesOrderLineId WHERE s.CompanyCode = ? AND "
                  "s.OrderNumber = ? ORDER BY s.LineNum, s.ScheduleNo"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE BO-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           PERFORM 1200-SET-BOOK-COLUMNS
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1300-EXIT
           END-IF
           MOVE 0 TO BO-OPEN-SCHED-CNT
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BROW
           PERFORM UNTIL SQLIO-SQLCODE NOT = 0 OR BO-LINE-CNT >= 40
               ADD 1 TO BO-LINE-CNT
               MOVE BO-LINE-CNT TO WS-I
               PERFORM 1250-DERIVE-ONE-ROW
               IF BO-L-REM-DESP(WS-I) > 0 AND
                  BO-L-STATUS(WS-I) NOT = "CANCELLED"
                   ADD 1 TO BO-OPEN-SCHED-CNT
               END-IF
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-BROW
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BROW.
       1300-EXIT.
           EXIT.

      *>   header completion is DERIVED - never asserted.  Quantities
      *>   still to despatch, still to invoice, or an open return all
      *>   keep the order incomplete.
       1800-DERIVE-COMPLETION SECTION.
       1800-START.
           MOVE "N" TO BO-HEADER-COMPLETE
           MOVE SPACES TO BO-EXCEPTION-TEXT
           IF BO-REMAIN-DESPATCH > 0
               MOVE BO-REMAIN-DESPATCH TO WS-UK-QTY
               STRING FUNCTION TRIM(WS-UK-QTY)
                      " still to despatch"
                   DELIMITED BY SIZE INTO BO-EXCEPTION-TEXT
               GO TO 1800-EXIT
           END-IF
           IF BO-REMAIN-INVOICE > 0
               MOVE BO-REMAIN-INVOICE TO WS-UK-QTY
               STRING FUNCTION TRIM(WS-UK-QTY)
                      " despatched but not yet invoiced"
                   DELIMITED BY SIZE INTO BO-EXCEPTION-TEXT
               GO TO 1800-EXIT
           END-IF
           IF BO-EXCEPTION-CNT > 0
               MOVE BO-EXCEPTION-CNT TO WS-INT-ED
               STRING FUNCTION TRIM(WS-INT-ED)
                      " return authorisation(s) await disposition"
                   DELIMITED BY SIZE INTO BO-EXCEPTION-TEXT
               GO TO 1800-EXIT
           END-IF
           MOVE "Y" TO BO-HEADER-COMPLETE.
       1800-EXIT.
           EXIT.

       1900-REPORT-BALANCE SECTION.
       1900-START.
           IF WS-RC > 4
               GO TO 1900-EXIT
           END-IF
           MOVE BO-REMAIN-DESPATCH TO WS-UK-QTY
           MOVE BO-BILLED-NET TO WS-UK-MONEY
           MOVE SPACES TO WS-MSG
           STRING "Remaining to despatch " FUNCTION TRIM(WS-UK-QTY)
                  "; billed net " FUNCTION TRIM(WS-UK-MONEY)
               DELIMITED BY SIZE INTO WS-MSG
           IF BO-OPEN-LINE-CNT > 0 AND WS-RC < 4
               MOVE 4 TO WS-RC
               MOVE "BKO0006" TO WS-REASON
               ADD 1 TO WS-MSGCNT
           END-IF.
       1900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   CREATEBO - SO-015.  The remaining quantity is carried
      *>   forward as a new schedule on the same line so the customer
      *>   keeps one commercial line, with the REQUESTED date and the
      *>   ORIGINAL promise copied from the schedule it came from.
       2000-CREATE-BACKORDER SECTION.
       2000-START.
           IF BO-LINE-NUM = 0
               MOVE "BKO0008" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A line number is required to carry a balance "
                      "forward" DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           PERFORM 1100-READ-LINE-VIEW
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-FIND-THE-LINE
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           IF BO-CARRIED-QTY <= 0
               MOVE 4 TO WS-RC
               MOVE "BKO0006" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Line " FUNCTION TRIM(WS-INT-ED)
                      " has no remaining quantity to carry forward"
                   DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-READ-LINE-KEY
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           PERFORM 8000-BEGIN-TRAN
           IF BO-CANCEL
               PERFORM 2400-CANCEL-THE-BALANCE
           ELSE
               PERFORM 2300-CARRY-FORWARD
           END-IF
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           PERFORM 7500-AUDIT-THE-CHANGE
           MOVE BO-CARRIED-QTY TO WS-UK-QTY
           MOVE SPACES TO WS-MSG
           EVALUATE TRUE
               WHEN BO-CANCEL
                   STRING FUNCTION TRIM(WS-UK-QTY)
                          " unfulfilled units withdrawn at the "
                          "customer's instruction; despatched and "
                          "billed quantities are untouched"
                       DELIMITED BY SIZE INTO WS-MSG
               WHEN BO-SUBSTITUTE
                   STRING FUNCTION TRIM(WS-UK-QTY)
                          " carried forward on back order "
                          FUNCTION TRIM(BO-BACKORDER-NUMBER)
                          " for substitution by "
                          FUNCTION TRIM(BO-SUBST-ITEM)
                       DELIMITED BY SIZE INTO WS-MSG
               WHEN OTHER
                   STRING FUNCTION TRIM(WS-UK-QTY)
                          " carried forward on back order "
                          FUNCTION TRIM(BO-BACKORDER-NUMBER)
                          " with the promise dates preserved"
                       DELIMITED BY SIZE INTO WS-MSG
           END-EVALUATE.
       2000-EXIT.
           EXIT.

       2100-FIND-THE-LINE SECTION.
       2100-START.
           MOVE "N" TO WS-FOUND
           MOVE 0 TO BO-CARRIED-QTY
           MOVE BO-LINE-NUM TO WS-INT-ED
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > BO-LINE-CNT
               IF BO-L-LINE-NUM(WS-I) = BO-LINE-NUM
                   MOVE "Y" TO WS-FOUND
                   MOVE BO-L-REM-DESP(WS-I) TO BO-CARRIED-QTY
                   MOVE BO-L-UNIT-PRICE(WS-I) TO BO-UNIT-PRICE
                   MOVE BO-INSTRUCTION TO BO-L-INSTRUCT(WS-I)
               END-IF
           END-PERFORM
           IF WS-FOUND NOT = "Y"
               MOVE 16 TO WS-RC
               MOVE "BKO0008" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Line " FUNCTION TRIM(WS-INT-ED) " of order "
                      FUNCTION TRIM(BO-ORDER-NUMBER)
                      " is not held" DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
           END-IF.
       2100-EXIT.
           EXIT.

      *>   the line key and the highest schedule number so far
       2200-READ-LINE-KEY SECTION.
       2200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), l.SalesOrderLineId), "
                  "CONVERT(varchar(10), ISNULL((SELECT "
                  "MAX(s.ScheduleNo) FROM so.DeliverySchedule s "
                  "WHERE s.SalesOrderLineId = l.SalesOrderLineId),0))"
                  " FROM so.SalesOrderLine l WHERE l.CompanyCode = ? "
                  "AND l.OrderNumber = ? AND l.LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE BO-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE BO-LINE-NUM TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2200-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO WS-RC
               MOVE "BKO0008" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Order line not found when carrying a balance "
                      "forward" DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
               GO TO 2200-EXIT
           END-IF
           MOVE WS-T1 TO WS-LINE-ID
           COMPUTE WS-NEW-SCHED = FUNCTION NUMVAL(WS-T2) + 1
           MOVE SPACES TO BO-BACKORDER-NUMBER
           MOVE WS-NEW-SCHED TO WS-INT-ED
           STRING FUNCTION TRIM(BO-ORDER-NUMBER) "/BO"
                  FUNCTION TRIM(WS-INT-ED)
               DELIMITED BY SIZE INTO BO-BACKORDER-NUMBER.
       2200-EXIT.
           EXIT.

      *>   the new schedule inherits the requested date and the
      *>   ORIGINAL promise of the earliest open schedule, so the back
      *>   order still reports against the service originally offered
       2300-CARRY-FORWARD SECTION.
       2300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO so.DeliverySchedule ("
                  "SalesOrderLineId, CompanyCode, OrderNumber, "
                  "LineNum, ScheduleNo, Quantity, DespatchedQty, "
                  "SiteCode, RequestedDate, OriginalPromiseDate, "
                  "CurrentPromiseDate, ChangeReason, ChangeCount, "
                  "FulfilmentMethod, Priority, Status) SELECT TOP (1)"
                  " l.SalesOrderLineId, l.CompanyCode, l.OrderNumber,"
                  " l.LineNum, CONVERT(int, ?), "
                  "CONVERT(decimal(19,6), ?), 0, ISNULL((SELECT TOP "
                  "(1) s2.SiteCode FROM so.DeliverySchedule s2 WHERE "
                  "s2.SalesOrderLineId = l.SalesOrderLineId ORDER BY "
                  "s2.ScheduleNo), ?), ISNULL((SELECT MIN("
                  "s3.RequestedDate) FROM so.DeliverySchedule s3 "
                  "WHERE s3.SalesOrderLineId = l.SalesOrderLineId), "
                  "CONVERT(date, ?, 112)), ISNULL((SELECT MIN("
                  "s4.OriginalPromiseDate) FROM so.DeliverySchedule "
                  "s4 WHERE s4.SalesOrderLineId = "
                  "l.SalesOrderLineId), CONVERT(date, ?, 112)), "
                  "CONVERT(date, ?, 112), ?, 0, 'BACKORDER', 5, "
                  "'OPEN' FROM so.SalesOrderLine l WHERE "
                  "l.SalesOrderLineId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 8 TO SQLIO-PARM-CNT
           MOVE WS-NEW-SCHED TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE BO-CARRIED-QTY TO WS-QTY-ED
           MOVE FUNCTION TRIM(WS-QTY-ED) TO SQLIO-PARM-VAL(2)
           MOVE WS-SITE TO SQLIO-PARM-VAL(3)
           MOVE WS-BUSDATE TO SQLIO-PARM-VAL(4)
           MOVE WS-BUSDATE TO SQLIO-PARM-VAL(5)
           MOVE WS-BUSDATE TO SQLIO-PARM-VAL(6)
           MOVE SPACES TO WS-MSG
           IF BO-SUBSTITUTE
               MOVE BO-SUBST-APPROVAL TO WS-INT-ED
               STRING "Back order " FUNCTION TRIM(BO-BACKORDER-NUMBER)
                      " - substitute " FUNCTION TRIM(BO-SUBST-ITEM)
                      " under approval " FUNCTION TRIM(WS-INT-ED)
                   DELIMITED BY SIZE INTO WS-MSG
           ELSE
               STRING "Back order " FUNCTION TRIM(BO-BACKORDER-NUMBER)
                      " - customer instruction KEEP"
                   DELIMITED BY SIZE INTO WS-MSG
           END-IF
           MOVE WS-MSG TO SQLIO-PARM-VAL(7)
           MOVE WS-LINE-ID TO SQLIO-PARM-VAL(8)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 8
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS.
       2300-EXIT.
           EXIT.

      *>   CANCEL - the customer declines the balance.  Only the
      *>   unfulfilled part becomes CancelledQty; the despatched and
      *>   billed quantities are not in the SET list.
       2400-CANCEL-THE-BALANCE SECTION.
       2400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrderLine SET CancelledQty = "
                  "CancelledQty + CONVERT(decimal(19,6), ?), Status "
                  "= CASE WHEN DespatchedQty > 0 THEN 'CLOSED' ELSE "
                  "'CANCELLED' END WHERE SalesOrderLineId = ? AND "
                  "AcceptedQty - CancelledQty - DespatchedQty >= "
                  "CONVERT(decimal(19,6), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE BO-CARRIED-QTY TO WS-QTY-ED
           MOVE FUNCTION TRIM(WS-QTY-ED) TO SQLIO-PARM-VAL(1)
           MOVE WS-LINE-ID TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(WS-QTY-ED) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC > 4
               GO TO 2400-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.DeliverySchedule SET Quantity = "
                  "DespatchedQty, Status = CASE WHEN DespatchedQty > "
                  "0 THEN 'COMPLETE' ELSE 'CANCELLED' END, "
                  "ChangeReason = ? WHERE SalesOrderLineId = ? AND "
                  "Status NOT IN ('CANCELLED','COMPLETE') AND "
                  "Quantity > DespatchedQty"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE BO-REASON-TEXT TO SQLIO-PARM-VAL(1)
           MOVE WS-LINE-ID TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           PERFORM 8200-EXEC-NO-ROWS.
       2400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   CLOSE - each schedule is judged on its own balance, each
      *>   line on its own, and the header only on the derived
      *>   outcome of all of them.
       3000-CLOSE-WHAT-IS-DONE SECTION.
       3000-START.
           PERFORM 8000-BEGIN-TRAN
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.DeliverySchedule SET Status = "
                  "'COMPLETE' WHERE CompanyCode = ? AND OrderNumber "
                  "= ? AND Status IN ('OPEN','PARTIAL') AND "
                  "DespatchedQty >= Quantity"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE BO-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC > 4
               GO TO 3000-EXIT
           END-IF
           MOVE SQLIO-ROWCOUNT TO BO-CLOSED-SCHEDS
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrderLine SET Status = 'CLOSED' "
                  "WHERE CompanyCode = ? AND OrderNumber = ? AND "
                  "Status = 'OPEN' AND AcceptedQty - CancelledQty - "
                  "DespatchedQty <= 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE BO-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC > 4
               GO TO 3000-EXIT
           END-IF
           MOVE SQLIO-ROWCOUNT TO BO-CLOSED-LINES
           PERFORM 8100-COMMIT-TRAN
           PERFORM 1100-READ-LINE-VIEW
           IF WS-RC > 4
               GO TO 3000-EXIT
           END-IF
           PERFORM 1800-DERIVE-COMPLETION
           IF BO-IS-COMPLETE
               PERFORM 3100-COMPLETE-THE-HEADER
           ELSE
               MOVE 4 TO WS-RC
               MOVE "BKO0007" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Schedules and lines closed, but the order is "
                      "not complete: "
                      FUNCTION TRIM(BO-EXCEPTION-TEXT)
                   DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
           END-IF.
       3000-EXIT.
           EXIT.

       3100-COMPLETE-THE-HEADER SECTION.
       3100-START.
           PERFORM 8000-BEGIN-TRAN
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrder SET CommercialStatus = "
                  "'COMPLETED', FulfilmentStatus = 'DESPATCHED', "
                  "BillingStatus = 'BILLED', ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE CompanyCode = ? "
                  "AND OrderNumber = ? AND CommercialStatus NOT IN "
                  "('CANCELLED','COMPLETED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-USER TO SQLIO-PARM-VAL(1)
           MOVE WS-CO TO SQLIO-PARM-VAL(2)
           MOVE BO-ORDER-NUMBER TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC > 4
               GO TO 3100-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           MOVE "COMPLETED" TO BO-COMM-STATUS
           PERFORM 7500-AUDIT-THE-CHANGE
           MOVE SPACES TO WS-MSG
           STRING "Order " FUNCTION TRIM(BO-ORDER-NUMBER)
                  " completed - every quantity is accounted for"
               DELIMITED BY SIZE INTO WS-MSG.
       3100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       7500-AUDIT-THE-CHANGE SECTION.
       7500-START.
           IF WS-RC > 4
               GO TO 7500-EXIT
           END-IF
           INITIALIZE WS-AUD-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUD-CA
           MOVE WS-CO TO CA-COMPANY OF WS-AUD-CA
           MOVE WS-SITE TO CA-SITE OF WS-AUD-CA
           MOVE WS-USER TO CA-USER OF WS-AUD-CA
           MOVE WS-BUSDATE TO CA-BUSINESS-DATE OF WS-AUD-CA
           MOVE WS-CORREL TO CA-CORRELATION-ID OF WS-AUD-CA
           MOVE "so.SalesOrder" TO AU-ENTITY-TYPE
           MOVE SPACES TO AU-ENTITY-KEY
           STRING FUNCTION TRIM(WS-CO) "/"
                  FUNCTION TRIM(BO-ORDER-NUMBER)
               DELIMITED BY SIZE INTO AU-ENTITY-KEY
           MOVE WS-FUNC TO AU-ACTION
           MOVE "RemainingToDespatch" TO AU-FIELD-NAME
           MOVE BO-CARRIED-QTY TO WS-QTY-ED
           MOVE WS-QTY-ED TO AU-OLD-VALUE
           MOVE BO-REMAIN-DESPATCH TO WS-QTY-ED
           MOVE WS-QTY-ED TO AU-NEW-VALUE
           MOVE SPACES TO AU-REASON
           STRING "BACKORD " FUNCTION TRIM(WS-FUNC) " "
                  FUNCTION TRIM(BO-INSTRUCTION) " "
                  FUNCTION TRIM(BO-REASON-TEXT)
               DELIMITED BY SIZE INTO AU-REASON
           MOVE SPACES TO AU-AUTHORITY-REF
           IF BO-SUBST-APPROVAL NOT = 0
               MOVE BO-SUBST-APPROVAL TO WS-INT-ED
               STRING "SUBSTITUTION APPROVAL "
                      FUNCTION TRIM(WS-INT-ED)
                   DELIMITED BY SIZE INTO AU-AUTHORITY-REF
           END-IF
           MOVE "BACKORD" TO AU-SOURCE
           MOVE "N" TO AU-SENSITIVE
           CALL "AUDITLOG" USING WS-AUD-CA.
       7500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-BEGIN-TRAN SECTION.
       8000-START.
           IF WS-RC > 4 OR WS-TRAN-OPEN = "Y"
               GO TO 8000-EXIT
           END-IF
           MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
           ELSE
               MOVE "Y" TO WS-TRAN-OPEN
           END-IF.
       8000-EXIT.
           EXIT.

       8100-COMMIT-TRAN SECTION.
       8100-START.
           IF WS-TRAN-OPEN NOT = "Y"
               GO TO 8100-EXIT
           END-IF
           MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
           MOVE "COMT" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
           END-IF
           MOVE "N" TO WS-TRAN-OPEN.
       8100-EXIT.
           EXIT.

       8200-EXEC-NO-ROWS SECTION.
       8200-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 8200-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO.
       8200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO WS-RC
           ADD 1 TO WS-MSGCNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE 8 TO WS-RC
           MOVE "BKO0003" TO WS-REASON
           MOVE SPACES TO WS-MSG
           STRING "BACKORD does not support function ["
                  FUNCTION TRIM(WS-FUNC) "]"
               DELIMITED BY SIZE INTO WS-MSG
           ADD 1 TO WS-MSGCNT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE 12 TO WS-RC
           MOVE "SQL0001" TO WS-REASON
           MOVE SQLIO-ERRMSG TO WS-MSG
           ADD 1 TO WS-MSGCNT.
       9300-EXIT.
           EXIT.

       9500-ROLLBACK SECTION.
       9500-START.
           IF WS-TRAN-OPEN = "Y"
               MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-TWO
               MOVE "N" TO WS-TRAN-OPEN
           END-IF.
       9500-EXIT.
           EXIT.

       9900-SET-RESULT SECTION.
       9900-START.
           MOVE WS-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE WS-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE WS-MSGCNT TO CA-MSG-COUNT OF DFHCOMMAREA.
       9900-EXIT.
           EXIT.
