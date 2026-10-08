      *>================================================================
      *> PROGRAM-ID : MARGIN
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : SALES MARGIN AND MARKUP ON THE DEFINED EX VAT NET
      *>              REVENUE.  EXPECTED (ESTIMATE BASIS) AND REALISED
      *>              (ATTRIBUTABLE ACTUAL COST) ARE NEVER MIXED, AND A
      *>              ZERO DENOMINATOR PRODUCES AN EXPLICIT UNDEFINED
      *>              RESULT RATHER THAN A FLATTERING ZERO.
      *> CALLED BY  : SALES ORDER ENTRY AND INVOICING (SO), THE JOB
      *>              PROFITABILITY REPORT, THE .NET CLIENT
      *> INTERFACE  : CALL "MARGIN" USING WS-MARGIN-CA
      *>              COPYBOOK MARGINCA
      *> CALLS      : COSTNORM (COST BASIS), SQLIO
      *>              (cst.JobCostTransaction, cst.usp_OrderMargin)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-11-05 RJH  CR0601 ORIGINAL ORDER MARGIN ENQUIRY
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT DOCUMENT DATES
      *> 2006-10-16 DJM  CR1455 EXPECTED AND REALISED SEPARATED
      *> 2026-10-06 STERP CST-013 EX VAT REVENUE, UNDEFINED STATES
      *>================================================================
      *> REQ: CST-013
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MARGIN.
       AUTHOR. R J HARTLEY.
       DATE-WRITTEN. 05/11/1997.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY CSTWORK.
       COPY COSTNORMCA REPLACING ==DFHCOMMAREA== BY ==WS-CN-CA==.
       01  WS-FLAGS.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-GROSS                PIC S9(13)V9(2) VALUE 0.
           05  WS-NET                  PIC S9(13)V9(2) VALUE 0.
           05  WS-COST                 PIC S9(13)V9(2) VALUE 0.
       LINKAGE SECTION.
       COPY MARGINCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION OF DFHCOMMAREA
               WHEN "EXPECTED" PERFORM 1000-EXPECTED
               WHEN "LINE"     PERFORM 1000-EXPECTED
               WHEN "REALISED" PERFORM 2000-REALISED
               WHEN "ORDER"    PERFORM 3000-ORDER
               WHEN "JOB"      PERFORM 4000-JOB
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED
           MOVE "MARGIN" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "CST-013" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE 0 TO WS-GROSS WS-NET WS-COST
           MOVE 0 TO MG-NET-REVENUE MG-VAT-AMOUNT MG-MARGIN-VALUE
           MOVE 0 TO MG-MARGIN-PCT MG-MARKUP-PCT
           MOVE 0 TO MG-LINES-COUNTED MG-LINES-NO-COST
           MOVE SPACES TO MG-WARNING MG-COST-BASIS-USED
           MOVE "OK" TO MG-MARGIN-STATE MG-MARKUP-STATE
           MOVE "COMPLETE" TO MG-COMPLETENESS
           MOVE FUNCTION UPPER-CASE(MG-COST-BASIS) TO MG-COST-BASIS
           MOVE FUNCTION UPPER-CASE(MG-ITEM-CODE) TO MG-ITEM-CODE
           IF MG-CURRENCY = SPACES
               MOVE "GBP" TO MG-CURRENCY
           END-IF
           IF MG-EFF-DATE = 0
               MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO MG-EFF-DATE
           END-IF
           EVALUATE CA-FUNCTION OF DFHCOMMAREA
               WHEN "REALISED" MOVE "REALISED" TO MG-RESULT-NATURE
               WHEN "JOB"      MOVE "REALISED" TO MG-RESULT-NATURE
               WHEN OTHER      MOVE "EXPECTED" TO MG-RESULT-NATURE
           END-EVALUATE
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY OF DFHCOMMAREA = SPACES
               MOVE "CST0701" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A company is required" TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF MG-IS-VAT-INCL AND MG-VAT-RATE = 0
               MOVE "CST0707" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A VAT inclusive revenue needs a VAT rate"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION OF DFHCOMMAREA = "LINE" AND MG-QUANTITY <= 0
               MOVE "CST0705" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "Quantity must be greater than zero"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  EXPECTED MARGIN - ACCEPTED NET SELLING VALUE LESS THE
      *>       CHOSEN ESTIMATE.  ANALYTICAL ONLY, NEVER FINANCIAL.
      *>----------------------------------------------------------------
       1000-EXPECTED SECTION.
       1000-START.
           PERFORM 5000-NET-REVENUE
           PERFORM 5100-ESTIMATE-COST
           PERFORM 6000-COMPUTE-MARGIN.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  REALISED MARGIN - RECOGNISED NET REVENUE LESS THE
      *>       ATTRIBUTABLE ACTUAL OR VALUED COST OF SALES PLUS THE
      *>       DEFINED ADJUSTMENTS (CREDITS, LATE LANDED COST).
      *>----------------------------------------------------------------
       2000-REALISED SECTION.
       2000-START.
           PERFORM 5000-NET-REVENUE
           IF MG-COST-AMOUNT = 0 AND MG-UNIT-COST NOT = 0
               COMPUTE MG-COST-AMOUNT ROUNDED =
                   MG-UNIT-COST * MG-QUANTITY
           END-IF
           IF MG-COST-AMOUNT = 0
               MOVE "INCOMPLETE" TO MG-COMPLETENESS
               MOVE "CST0704" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "No attributable cost of sales was supplied"
                   TO CA-MESSAGE OF DFHCOMMAREA
               MOVE "Realised margin is incomplete without a cost"
                   TO MG-WARNING
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               ADD 1 TO MG-LINES-NO-COST
           END-IF
           MOVE "ACTUAL" TO MG-COST-BASIS-USED
           PERFORM 6000-COMPUTE-MARGIN.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  WHOLE ORDER MARGIN.  THE SET AGGREGATION LIVES IN A
      *>       STORED PROCEDURE; THE RULE LIVES HERE.
      *>----------------------------------------------------------------
       3000-ORDER SECTION.
       3000-START.
           IF MG-DOC-REF = SPACES
               MOVE "CST0706" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "An order number is required"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC cst.usp_OrderMargin ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE MG-DOC-REF TO SQLIO-PARM-VAL(2)
           IF MG-COST-BASIS = SPACES
               MOVE "STANDARD" TO SQLIO-PARM-VAL(3)
           ELSE
               MOVE MG-COST-BASIS TO SQLIO-PARM-VAL(3)
           END-IF
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 5 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF NOT WS-GOT-ROW
               IF NOT WS-SQL-BAD
                   MOVE "CST0706" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
                   STRING "Sales order " FUNCTION TRIM(MG-DOC-REF)
                          " was not found"
                       DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
                   MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA
               END-IF
               GO TO 3000-EXIT
           END-IF
           MOVE FUNCTION NUMVAL(WS-COL(1)) TO MG-NET-REVENUE
           MOVE FUNCTION NUMVAL(WS-COL(2)) TO MG-COST-AMOUNT
           MOVE FUNCTION NUMVAL(WS-COL(3)) TO MG-LINES-COUNTED
           MOVE FUNCTION NUMVAL(WS-COL(4)) TO MG-LINES-NO-COST
           MOVE WS-COL(5) TO MG-COST-BASIS-USED
           IF MG-LINES-NO-COST > 0
               MOVE "INCOMPLETE" TO MG-COMPLETENESS
               MOVE "Some order lines have no cost basis"
                   TO MG-WARNING
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
           END-IF
           PERFORM 6000-COMPUTE-MARGIN.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  JOB MARGIN - RECOGNISED JOB REVENUE LESS THE RECOGNISED
      *>       ACTUAL AND ACCRUED COST OF THE JOB AND ITS SUB-JOBS.
      *>----------------------------------------------------------------
       4000-JOB SECTION.
       4000-START.
           IF MG-JOB-REF = SPACES
               MOVE "CST0706" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A job reference is required"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT SUM(CASE WHEN t.ElementCode = 'REVENUE' "
                  "THEN -t.AmountGBP ELSE 0 END), SUM(CASE WHEN "
                  "t.ElementCode <> 'REVENUE' AND t.CostNature IN "
                  "('ACTUAL','ACCRUED') THEN t.AmountGBP ELSE 0 END), "
                  "COUNT(*) FROM cst.JobCostTransaction t WHERE "
                  "t.CompanyCode = ? AND (t.JobRef = ? OR t.JobRef IN "
                  "(SELECT JobRef FROM cst.Job WHERE CompanyCode = ? "
                  "AND ParentJobRef = ?))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE MG-JOB-REF TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(3)
           MOVE MG-JOB-REF TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3) SQLIO-PARM-NULL(4)
           MOVE 3 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-SQL-BAD
               GO TO 4000-EXIT
           END-IF
           IF WS-GOT-ROW
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO MG-NET-REVENUE
               MOVE FUNCTION NUMVAL(WS-COL(2)) TO MG-COST-AMOUNT
               MOVE FUNCTION NUMVAL(WS-COL(3)) TO MG-LINES-COUNTED
           END-IF
           IF MG-GROSS-REVENUE NOT = 0
               MOVE MG-GROSS-REVENUE TO MG-NET-REVENUE
           END-IF
           MOVE "ACTUAL" TO MG-COST-BASIS-USED
           PERFORM 6000-COMPUTE-MARGIN.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000  NET REVENUE.  VAT IS STRIPPED FIRST, THEN THE DISCOUNT
      *>       IS TAKEN OFF.  VAT NEVER ENTERS A MARGIN (CST-013).
      *>----------------------------------------------------------------
       5000-NET-REVENUE SECTION.
       5000-START.
           MOVE MG-GROSS-REVENUE TO WS-GROSS
           IF WS-GROSS = 0 AND MG-QUANTITY NOT = 0
               COMPUTE WS-GROSS ROUNDED = MG-QUANTITY * MG-UNIT-PRICE
           END-IF
           IF MG-IS-VAT-INCL
               COMPUTE WS-NET ROUNDED =
                   WS-GROSS / (1 + (MG-VAT-RATE / 100))
               COMPUTE MG-VAT-AMOUNT = WS-GROSS - WS-NET
           ELSE
               MOVE WS-GROSS TO WS-NET
               COMPUTE MG-VAT-AMOUNT ROUNDED =
                   WS-NET * MG-VAT-RATE / 100
           END-IF
           COMPUTE WS-NET = WS-NET - MG-DISCOUNT
           MOVE WS-NET TO MG-NET-REVENUE
           IF MG-CURRENCY NOT = "GBP" AND MG-FX-RATE > 0
               COMPUTE MG-NET-REVENUE ROUNDED =
                   MG-NET-REVENUE / MG-FX-RATE
           END-IF.
       5000-EXIT.
           EXIT.

      *>  THE ESTIMATE SIDE.  IF NO COST WAS PASSED WE ASK COSTNORM
      *>  FOR THE REQUESTED BASIS - AND ACCEPT "NOT AVAILABLE".
       5100-ESTIMATE-COST SECTION.
       5100-START.
           IF MG-COST-AMOUNT NOT = 0
               MOVE MG-COST-BASIS TO MG-COST-BASIS-USED
               GO TO 5100-EXIT
           END-IF
           IF MG-UNIT-COST NOT = 0
               COMPUTE MG-COST-AMOUNT ROUNDED =
                   MG-UNIT-COST * MG-QUANTITY
               MOVE MG-COST-BASIS TO MG-COST-BASIS-USED
               GO TO 5100-EXIT
           END-IF
           IF MG-ITEM-CODE = SPACES OR MG-QUANTITY <= 0
               MOVE "INCOMPLETE" TO MG-COMPLETENESS
               GO TO 5100-EXIT
           END-IF
           INITIALIZE WS-CN-CA
           MOVE "COSTNORM" TO CA-PROGRAM OF WS-CN-CA
           MOVE "CONTEXT" TO CA-FUNCTION OF WS-CN-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-CN-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-CN-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-CN-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-CN-CA
           MOVE MG-ITEM-CODE TO CN-ITEM-CODE
           MOVE MG-QUANTITY TO CN-QUANTITY
           MOVE MG-EFF-DATE TO CN-EFF-DATE
           MOVE "ENQUIRY" TO CN-PURPOSE
           IF MG-COST-BASIS = SPACES
               MOVE "STANDARD" TO CN-BASIS
           ELSE
               MOVE MG-COST-BASIS TO CN-BASIS
           END-IF
           CALL "COSTNORM" USING WS-CN-CA
           IF CA-RETURN-CODE OF WS-CN-CA > 4
               MOVE "INCOMPLETE" TO MG-COMPLETENESS
               MOVE "CST0704" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-CN-CA TO MG-WARNING
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               ADD 1 TO MG-LINES-NO-COST
           ELSE
               MOVE CN-EXT-COST-GBP TO MG-COST-AMOUNT
               MOVE CN-UNIT-COST-GBP TO MG-UNIT-COST
               MOVE CN-BASIS-USED TO MG-COST-BASIS-USED
           END-IF
           ADD 1 TO MG-LINES-COUNTED.
       5100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 6000  THE ARITHMETIC - AND THE HONEST ANSWER WHEN IT CANNOT
      *>       BE DONE.  A ZERO REVENUE DOES NOT MEAN A ZERO MARGIN
      *>       PERCENTAGE, IT MEANS THERE IS NO PERCENTAGE.
      *>----------------------------------------------------------------
       6000-COMPUTE-MARGIN SECTION.
       6000-START.
           COMPUTE WS-COST = MG-COST-AMOUNT + MG-ADJUSTMENTS
           MOVE WS-COST TO MG-COST-AMOUNT
           COMPUTE MG-MARGIN-VALUE = MG-NET-REVENUE - MG-COST-AMOUNT
           IF MG-NET-REVENUE = 0
               MOVE "UNDEFINED" TO MG-MARGIN-STATE
               MOVE 0 TO MG-MARGIN-PCT
               IF CA-RETURN-CODE OF DFHCOMMAREA = 0
                   MOVE "CST0702" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE "Net revenue is zero - margin percentage is "
                        TO CA-MESSAGE OF DFHCOMMAREA
                   STRING FUNCTION TRIM(CA-MESSAGE OF DFHCOMMAREA)
                          "undefined"
                       DELIMITED BY SIZE INTO WS-WORK-TEXT
                   MOVE WS-WORK-TEXT TO CA-MESSAGE OF DFHCOMMAREA
                   MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               END-IF
           ELSE
               COMPUTE MG-MARGIN-PCT ROUNDED =
                   MG-MARGIN-VALUE * 100 / MG-NET-REVENUE
           END-IF
           IF MG-COST-AMOUNT = 0
               MOVE "UNDEFINED" TO MG-MARKUP-STATE
               MOVE 0 TO MG-MARKUP-PCT
           ELSE
               COMPUTE MG-MARKUP-PCT ROUNDED =
                   MG-MARGIN-VALUE * 100 / MG-COST-AMOUNT
           END-IF.
       6000-EXIT.
           EXIT.

       8000-SINGLE-ROW SECTION.
       8000-START.
           MOVE "N" TO WS-ROW-FOUND
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           EVALUATE TRUE
               WHEN SQLIO-SQLCODE = 0
                   MOVE "Y" TO WS-ROW-FOUND
               WHEN SQLIO-SQLCODE = 100
                   CONTINUE
               WHEN OTHER
                   MOVE "Y" TO WS-SQL-FAILED
                   MOVE "CST9001" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SQLIO-ERRMSG TO CA-MESSAGE OF DFHCOMMAREA
                   MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA
           END-EVALUATE.
       8000-EXIT.
           EXIT.

       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "CST0708" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           STRING "Function " FUNCTION TRIM(CA-FUNCTION OF DFHCOMMAREA)
                  " is not supported by MARGIN"
               DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
