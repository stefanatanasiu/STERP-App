      *>================================================================
      *> PROGRAM-ID : SOMARGN
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT
      *> PURPOSE    : MARGIN AND MARKUP ON THE EX VAT NET SELLING
      *>              AMOUNT AGAINST AN EXPLICITLY SELECTED
      *>              COMPARABLE COST BASIS.  MARGIN AND MARKUP ARE
      *>              DIFFERENT MEASURES AND BOTH ARE RETURNED.  A
      *>              ZERO DENOMINATOR IS "NOT DEFINED" PLUS A REVIEW
      *>              CONDITION AND A MISSING COST IS NOT ZERO - IT
      *>              HOLDS THE LINE UNTIL A COST BASIS EXISTS.
      *> CALLED BY  : QUOTATION ENTRY, SALES ORDER ENTRY, SALES
      *>              ANALYSIS, THE .NET CLIENT
      *> INTERFACE  : CALL "SOMARGN" USING WS-SOMARGN-CA
      *>              COPYBOOK SOMARGNCA
      *> CALLS      : PARMGET, EXCPRAIS, SQLIO (cst.ItemCost)
      *>
      *> THE COSTING TEAM'S MARGIN PROGRAM IS THE INTENDED OWNER OF
      *> THE COMPARABLE COST BASIS.  cobol\online\cst\MARGIN.cob HAS
      *> NOT BEEN DELIVERED, SO THE COST IS READ LOCALLY FROM
      *> cst.ItemCost IN THE ORDER STANDARD, AVERAGE, LAST BUY.
      *> TODO SO-TD: switch to CST MARGIN when delivered
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-11-05 RJH  CR0601 ORDER MARGIN ENQUIRY (SALES COPY)
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT COST EFFECTIVE DATES
      *> 2006-10-16 DJM  CR1455 MARGIN AND MARKUP SHOWN SEPARATELY
      *> 2012-03-19 DLS  CR2461 BELOW POLICY MARGIN NEEDS APPROVAL
      *> 2026-10-06 STERP SO-006, SO-023 MISSING COST IS NOT ZERO
      *>================================================================
      *> REQ: SO-006, SO-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOMARGN.
       AUTHOR. R J HOLLOWAY.
       DATE-WRITTEN. 05/11/1997.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARM-CA==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCP-CA==.
       01  WS-CONTROL.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-SQL-FAILED           PIC X VALUE "N".
               88  WS-SQL-BAD                    VALUE "Y".
           05  WS-IX                   PIC S9(9) COMP-5 VALUE 0.
           05  WS-JX                   PIC S9(9) COMP-5 VALUE 0.
           05  WS-DATE-N               PIC 9(8) VALUE 0.
       01  WS-COSTS.
           05  WS-STD-COST             PIC S9(11)V9(6) VALUE 0.
           05  WS-AVG-COST             PIC S9(11)V9(6) VALUE 0.
           05  WS-LAST-COST            PIC S9(11)V9(6) VALUE 0.
           05  WS-UNIT-COST            PIC S9(11)V9(6) VALUE 0.
           05  WS-COST-TOTAL           PIC S9(13)V9(2) VALUE 0.
           05  WS-NET                  PIC S9(13)V9(2) VALUE 0.
           05  WS-TARGET-FRACTION      PIC S9(5)V9(8) VALUE 0.
           05  WS-PRICE-QTY            PIC S9(13)V9(6) VALUE 0.
       01  WS-EDIT.
           05  WS-MSG-NUM              PIC -(11)9.99.
           05  WS-MSG-PCT              PIC -(6)9.9(4).
       01  WS-ROW.
           05  WS-R-COL1               PIC X(100).
           05  WS-R-COL2               PIC X(100).
           05  WS-R-COL3               PIC X(100).
           05  WS-R-COL4               PIC X(100).
       LINKAGE SECTION.
       COPY SOMARGNCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           PERFORM 0200-VALIDATE THRU 0200-EXIT
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION OF DFHCOMMAREA
               WHEN "LINE"    PERFORM 1000-LINE THRU 1000-EXIT
               WHEN "EXPLAIN" PERFORM 1000-LINE THRU 1000-EXIT
               WHEN "CHECK"   PERFORM 2000-CHECK THRU 2000-EXIT
               WHEN "PROPOSE" PERFORM 5000-PROPOSE THRU 5000-EXIT
               WHEN OTHER   PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE
           PERFORM 6000-EXPLAIN THRU 6000-EXIT.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE.
           MOVE "SOMARGN" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "SO-006" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE "N" TO WS-ERROR WS-SQL-FAILED
           MOVE 0 TO WS-IX WS-JX WS-DATE-N
           MOVE 0 TO WS-STD-COST WS-AVG-COST WS-LAST-COST
           MOVE 0 TO WS-UNIT-COST WS-COST-TOTAL WS-NET
           MOVE 0 TO WS-TARGET-FRACTION WS-PRICE-QTY
           MOVE FUNCTION UPPER-CASE(CA-FUNCTION OF DFHCOMMAREA)
               TO CA-FUNCTION OF DFHCOMMAREA
           MOVE FUNCTION UPPER-CASE(SM-COST-BASIS) TO SM-COST-BASIS
           MOVE FUNCTION UPPER-CASE(SM-ITEM-CODE) TO SM-ITEM-CODE
           IF SM-EFF-DATE = 0
               MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO SM-EFF-DATE
           END-IF
           IF SM-CURRENCY = SPACES
               MOVE "GBP" TO SM-CURRENCY
           END-IF
           IF SM-SITE-CODE = SPACES
               MOVE CA-SITE OF DFHCOMMAREA TO SM-SITE-CODE
           END-IF
           IF SM-PRICE-PER-QTY <= 0
               MOVE 1 TO SM-PRICE-PER-QTY
           END-IF
           IF SM-NET-AMOUNT-GBP = 0
               MOVE SM-NET-AMOUNT TO SM-NET-AMOUNT-GBP
           END-IF
           MOVE 0 TO SM-MARGIN-VALUE SM-MARGIN-PCT SM-MARKUP-PCT
           MOVE 0 TO SM-EXCEPTION-ID
           MOVE 0 TO SM-PROPOSED-PRICE SM-PROPOSED-NET
           MOVE 0 TO SM-PROPOSED-MARGIN
           MOVE SPACES TO SM-MARGIN-STATE SM-MARKUP-STATE
           MOVE SPACES TO SM-REVIEW-REASON SM-EXPLANATION
           MOVE SPACES TO SM-COST-BASIS-USED SM-COST-SOURCE
           MOVE SPACES TO SM-MIN-SOURCE
           MOVE "N" TO SM-BELOW-MINIMUM SM-APPROVAL-REQD
           MOVE "N" TO SM-COST-FOUND SM-PRICE-CHANGED
           MOVE "N" TO SM-AMENDMENT-REQD SM-USED-CST-MARGIN
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 10
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE.
           IF CA-COMPANY OF DFHCOMMAREA = SPACES
               MOVE "MRG0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A company is required" TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION OF DFHCOMMAREA = "PROPOSE"
               IF SM-QUANTITY <= 0
                   MOVE "MRG0009" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE "A positive quantity is needed to propose a"
                       TO CA-MESSAGE OF DFHCOMMAREA
                   PERFORM 9000-REJECT THRU 9000-EXIT
                   GO TO 0200-EXIT
               END-IF
               IF SM-TARGET-MARGIN >= 100
                   MOVE "MRG0006" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
                   STRING "A target margin must be below 100 per "
                          "cent - there is no such selling price"
                       DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
                   PERFORM 9000-REJECT THRU 9000-EXIT
               END-IF
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  THE MEASUREMENT ITSELF.
      *>----------------------------------------------------------------
       1000-LINE.
           PERFORM 3000-COST-BASIS THRU 3000-EXIT
           IF WS-INPUT-BAD
               GO TO 1000-EXIT
           END-IF
           IF SM-COST-FOUND NOT = "Y"
               PERFORM 3900-NO-COST THRU 3900-EXIT
               GO TO 1000-EXIT
           END-IF
           PERFORM 3500-CALCULATE THRU 3500-EXIT.
       1000-EXIT.
           EXIT.

       2000-CHECK.
           PERFORM 1000-LINE THRU 1000-EXIT
           IF WS-INPUT-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 4000-POLICY-MINIMUM THRU 4000-EXIT.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  THE COMPARABLE COST BASIS.  A COST SUPPLIED BY THE
      *>       CALLER IS USED AS GIVEN AND ITS ORIGIN IS RECORDED;
      *>       OTHERWISE THE ITEM COST RECORD IS READ IN THE ORDER
      *>       STANDARD, AVERAGE, LAST BUY.  NOTHING HERE SUBSTITUTES
      *>       ZERO FOR A COST THAT IS NOT HELD (SO-006).
      *>----------------------------------------------------------------
       3000-COST-BASIS.
           IF SM-COST-AMOUNT NOT = 0 OR SM-UNIT-COST NOT = 0
               PERFORM 3100-SUPPLIED-COST THRU 3100-EXIT
               GO TO 3000-EXIT
           END-IF
           IF SM-ITEM-CODE = SPACES
               GO TO 3000-EXIT
           END-IF
           PERFORM 3200-READ-ITEM-COST THRU 3200-EXIT
           IF WS-SQL-BAD
               GO TO 3000-EXIT
           END-IF
           PERFORM 3300-PICK-BASIS THRU 3300-EXIT.
       3000-EXIT.
           EXIT.

       3100-SUPPLIED-COST.
           IF SM-COST-AMOUNT = 0 AND SM-QUANTITY > 0
               COMPUTE SM-COST-AMOUNT ROUNDED =
                   SM-UNIT-COST * SM-QUANTITY
           END-IF
           IF SM-UNIT-COST = 0 AND SM-QUANTITY > 0
               COMPUTE SM-UNIT-COST ROUNDED =
                   SM-COST-AMOUNT / SM-QUANTITY
           END-IF
           IF SM-COST-BASIS = SPACES OR SM-ON-SUPPLIED
               MOVE "SUPPLIED" TO SM-COST-BASIS-USED
           ELSE
               MOVE SM-COST-BASIS TO SM-COST-BASIS-USED
           END-IF
           MOVE "COMMAREA" TO SM-COST-SOURCE
           MOVE "Y" TO SM-COST-FOUND.
       3100-EXIT.
           EXIT.

      *>   TODO SO-TD: switch to CALL "MARGIN" USING MARGINCA when the
      *>   costing team delivers cobol\online\cst\MARGIN.cob
       3200-READ-ITEM-COST.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 ISNULL(c.StandardUnitCost,0), "
                  "ISNULL(c.AverageUnitCost,0), "
                  "ISNULL(c.LastPurchaseCost,0), c.CostMethod FROM "
                  "cst.ItemCost c WHERE c.CompanyCode = ? AND "
                  "c.ItemCode = ? AND (c.SiteCode = ? OR "
                  "c.SiteCode = '*') ORDER BY CASE WHEN c.SiteCode "
                  "= ? THEN 0 ELSE 1 END"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE SM-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE SM-SITE-CODE TO SQLIO-PARM-VAL(3)
           MOVE SM-SITE-CODE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3200-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 3200-EXIT
           END-IF
           COMPUTE WS-STD-COST = FUNCTION NUMVAL(WS-R-COL1)
           COMPUTE WS-AVG-COST = FUNCTION NUMVAL(WS-R-COL2)
           COMPUTE WS-LAST-COST = FUNCTION NUMVAL(WS-R-COL3).
       3200-EXIT.
           EXIT.

       3300-PICK-BASIS.
           MOVE 0 TO WS-UNIT-COST
           EVALUATE TRUE
               WHEN SM-ON-STANDARD AND WS-STD-COST NOT = 0
                   MOVE WS-STD-COST TO WS-UNIT-COST
                   MOVE "STANDARD" TO SM-COST-BASIS-USED
               WHEN SM-ON-AVERAGE AND WS-AVG-COST NOT = 0
                   MOVE WS-AVG-COST TO WS-UNIT-COST
                   MOVE "AVERAGE" TO SM-COST-BASIS-USED
               WHEN SM-ON-LASTBUY AND WS-LAST-COST NOT = 0
                   MOVE WS-LAST-COST TO WS-UNIT-COST
                   MOVE "LASTBUY" TO SM-COST-BASIS-USED
               WHEN WS-STD-COST NOT = 0
                   MOVE WS-STD-COST TO WS-UNIT-COST
                   MOVE "STANDARD" TO SM-COST-BASIS-USED
               WHEN WS-AVG-COST NOT = 0
                   MOVE WS-AVG-COST TO WS-UNIT-COST
                   MOVE "AVERAGE" TO SM-COST-BASIS-USED
               WHEN WS-LAST-COST NOT = 0
                   MOVE WS-LAST-COST TO WS-UNIT-COST
                   MOVE "LASTBUY" TO SM-COST-BASIS-USED
               WHEN OTHER
                   MOVE 0 TO WS-UNIT-COST
           END-EVALUATE
           IF WS-UNIT-COST = 0
               GO TO 3300-EXIT
           END-IF
           MOVE WS-UNIT-COST TO SM-UNIT-COST
           IF SM-QUANTITY > 0
               COMPUTE SM-COST-AMOUNT ROUNDED =
                   WS-UNIT-COST * SM-QUANTITY
           ELSE
               MOVE WS-UNIT-COST TO SM-COST-AMOUNT
           END-IF
           MOVE "CST.ITEMCOST" TO SM-COST-SOURCE
           MOVE "Y" TO SM-COST-FOUND.
       3300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3500  THE TWO MEASURES.  THEY SHARE A NUMERATOR AND HAVE
      *>       DIFFERENT DENOMINATORS, WHICH IS EXACTLY WHY BOTH ARE
      *>       SHOWN (SO-006).
      *>----------------------------------------------------------------
       3500-CALCULATE.
           COMPUTE WS-COST-TOTAL =
               SM-COST-AMOUNT + SM-COST-ADJUSTMENT
           MOVE WS-COST-TOTAL TO SM-COST-AMOUNT
           MOVE SM-NET-AMOUNT TO WS-NET
           COMPUTE SM-MARGIN-VALUE = WS-NET - WS-COST-TOTAL
           IF WS-NET = 0
               MOVE 0 TO SM-MARGIN-PCT
               MOVE "UNDEFINED" TO SM-MARGIN-STATE
               MOVE SPACES TO SM-REVIEW-REASON
               STRING "Net selling amount is zero so the margin "
                      "percentage is not defined - review the line"
                   DELIMITED BY SIZE INTO SM-REVIEW-REASON
               IF CA-RETURN-CODE OF DFHCOMMAREA < 4
                   MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
                   MOVE "MRG0003" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SM-REVIEW-REASON TO CA-MESSAGE OF DFHCOMMAREA
                   ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               END-IF
           ELSE
               COMPUTE SM-MARGIN-PCT ROUNDED =
                   (WS-NET - WS-COST-TOTAL) / WS-NET * 100
               MOVE "OK" TO SM-MARGIN-STATE
           END-IF
           IF WS-COST-TOTAL = 0
               MOVE 0 TO SM-MARKUP-PCT
               MOVE "UNDEFINED" TO SM-MARKUP-STATE
               IF SM-REVIEW-REASON = SPACES
                   MOVE SPACES TO SM-REVIEW-REASON
                   STRING "Comparable cost is zero so the markup "
                          "percentage is not defined - review"
                       DELIMITED BY SIZE INTO SM-REVIEW-REASON
               END-IF
               IF CA-RETURN-CODE OF DFHCOMMAREA < 4
                   MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
                   MOVE "MRG0004" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SM-REVIEW-REASON TO CA-MESSAGE OF DFHCOMMAREA
                   ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               END-IF
           ELSE
               COMPUTE SM-MARKUP-PCT ROUNDED =
                   (WS-NET - WS-COST-TOTAL) / WS-COST-TOTAL * 100
               MOVE "OK" TO SM-MARKUP-STATE
           END-IF.
       3500-EXIT.
           EXIT.

      *>   A MISSING COST IS NOT ZERO.  THE LINE IS HELD UNTIL A
      *>   COMPARABLE COST EXISTS, BECAUSE A HUNDRED PER CENT MARGIN
      *>   IS THE MOST DANGEROUS NUMBER ON THE SCREEN.
       3900-NO-COST.
           MOVE "INCOMPLETE" TO SM-MARGIN-STATE
           MOVE "INCOMPLETE" TO SM-MARKUP-STATE
           MOVE 0 TO SM-MARGIN-PCT SM-MARKUP-PCT SM-MARGIN-VALUE
           MOVE "Y" TO SM-APPROVAL-REQD
           MOVE SPACES TO SM-REVIEW-REASON
           STRING "No comparable cost is held for item "
                  FUNCTION TRIM(SM-ITEM-CODE)
                  " - the margin cannot be measured"
               DELIMITED BY SIZE INTO SM-REVIEW-REASON
           MOVE "MRG0002" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SM-REVIEW-REASON TO CA-MESSAGE OF DFHCOMMAREA
           MOVE 6 TO CA-RETURN-CODE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA.
       3900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  THE POLICY MINIMUM.  THE PRICE LIST MINIMUM WINS WHEN
      *>       ONE IS SUPPLIED, OTHERWISE SO.MARGIN.MIN.PCT, WHOSE
      *>       BASELINE IS THIRTY PER CENT.
      *>----------------------------------------------------------------
       4000-POLICY-MINIMUM.
           IF SM-MIN-MARGIN-PCT NOT = 0
               MOVE "PRICELIST" TO SM-MIN-SOURCE
           ELSE
               PERFORM 4100-READ-MIN-PARM THRU 4100-EXIT
           END-IF
           IF SM-MARGIN-STATE NOT = "OK"
               GO TO 4000-EXIT
           END-IF
           IF SM-MARGIN-PCT >= SM-MIN-MARGIN-PCT
               GO TO 4000-EXIT
           END-IF
           MOVE "Y" TO SM-BELOW-MINIMUM
           MOVE "Y" TO SM-APPROVAL-REQD
           MOVE SPACES TO SM-REVIEW-REASON
           MOVE SM-MARGIN-PCT TO WS-MSG-PCT
           STRING "Margin of " FUNCTION TRIM(WS-MSG-PCT)
                  "% is below the policy minimum of "
               DELIMITED BY SIZE INTO SM-REVIEW-REASON
           MOVE SM-MIN-MARGIN-PCT TO WS-MSG-PCT
           STRING FUNCTION TRIM(SM-REVIEW-REASON)
                  FUNCTION TRIM(WS-MSG-PCT)
                  "% and needs approval"
               DELIMITED BY SIZE INTO SM-REVIEW-REASON
           MOVE "MRG0005" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SM-REVIEW-REASON TO CA-MESSAGE OF DFHCOMMAREA
           MOVE 6 TO CA-RETURN-CODE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           IF SM-DO-RAISE
               PERFORM 4200-RAISE-EXCEPTION THRU 4200-EXIT
           END-IF.
       4000-EXIT.
           EXIT.

       4100-READ-MIN-PARM.
           MOVE 30.0000 TO SM-MIN-MARGIN-PCT
           MOVE "BASELINE" TO SM-MIN-SOURCE
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-PARM-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-PARM-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "SO.MARGIN.MIN.PCT" TO PM-PARAM-CODE
           MOVE SM-EFF-DATE TO PM-AS-OF
           MOVE "30" TO PM-DEFAULT-TEXT
           MOVE "Y" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARM-CA
           IF CA-RETURN-CODE OF WS-PARM-CA < 8
               AND PM-VALUE-NUM > 0
               MOVE PM-VALUE-NUM TO SM-MIN-MARGIN-PCT
               MOVE "SO.MARGIN.MIN.PCT" TO SM-MIN-SOURCE
           END-IF.
       4100-EXIT.
           EXIT.

       4200-RAISE-EXCEPTION.
           INITIALIZE WS-EXCP-CA
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXCP-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-EXCP-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-EXCP-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-EXCP-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-EXCP-CA
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA
               TO CA-CORRELATION-ID OF WS-EXCP-CA
           MOVE "so" TO EX-MODULE
           MOVE "HOLD" TO EX-SEVERITY
           MOVE "SO.MARGIN.BELOWMINIMUM" TO EX-CATEGORY
           MOVE SM-REVIEW-REASON TO EX-MESSAGE-TEXT
           IF SM-DOC-TYPE = SPACES
               MOVE "SALESORDER" TO EX-DOCUMENT-TYPE
           ELSE
               MOVE SM-DOC-TYPE TO EX-DOCUMENT-TYPE
           END-IF
           MOVE SM-DOC-REF TO EX-DOCUMENT-REF
           MOVE "SALESMGR" TO EX-OWNER-ROLE
           MOVE 2 TO EX-PRIORITY
           MOVE SM-NET-AMOUNT-GBP TO EX-AMOUNT-GBP
           MOVE SM-QUANTITY TO EX-QUANTITY
           CALL "EXCPRAIS" USING WS-EXCP-CA
           IF CA-RETURN-CODE OF WS-EXCP-CA < 8
               MOVE EX-EXCEPTION-ID TO SM-EXCEPTION-ID
           END-IF.
       4200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000  PROPOSE.  A TARGET MARGIN BECOMES A PROPOSED PRICE AND
      *>       NOTHING ELSE.  THE LIVE PRICE IS NOT TOUCHED, AND AN
      *>       ACCEPTED PRICE NEEDS A CONTROLLED AMENDMENT (SO-006).
      *>----------------------------------------------------------------
       5000-PROPOSE.
           PERFORM 3000-COST-BASIS THRU 3000-EXIT
           IF WS-INPUT-BAD
               GO TO 5000-EXIT
           END-IF
           IF SM-COST-FOUND NOT = "Y"
               PERFORM 3900-NO-COST THRU 3900-EXIT
               GO TO 5000-EXIT
           END-IF
           COMPUTE WS-COST-TOTAL =
               SM-COST-AMOUNT + SM-COST-ADJUSTMENT
           COMPUTE WS-TARGET-FRACTION = 1 - (SM-TARGET-MARGIN / 100)
           IF WS-TARGET-FRACTION <= 0
               MOVE "MRG0006" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "A target margin of 100 per cent or more has "
                      "no selling price"
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           COMPUTE SM-PROPOSED-NET ROUNDED =
               WS-COST-TOTAL / WS-TARGET-FRACTION
           COMPUTE WS-PRICE-QTY ROUNDED =
               SM-QUANTITY / SM-PRICE-PER-QTY
           IF WS-PRICE-QTY = 0
               MOVE "MRG0009" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "The quantity and price-per give no priceable"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           COMPUTE SM-PROPOSED-PRICE ROUNDED =
               SM-PROPOSED-NET / WS-PRICE-QTY
           IF SM-PROPOSED-NET NOT = 0
               COMPUTE SM-PROPOSED-MARGIN ROUNDED =
                   (SM-PROPOSED-NET - WS-COST-TOTAL)
                   / SM-PROPOSED-NET * 100
           END-IF
      *>   the current position is still measured so that the user
      *>   can see what the proposal would change
           IF SM-NET-AMOUNT NOT = 0
               PERFORM 3500-CALCULATE THRU 3500-EXIT
           END-IF
           MOVE "N" TO SM-PRICE-CHANGED
           IF SM-IS-ACCEPTED
               MOVE "Y" TO SM-AMENDMENT-REQD
               MOVE "MRG0007" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "The accepted price is unchanged - a "
                      "controlled amendment is required before a "
                      "new price can take effect"
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF DFHCOMMAREA TO SM-REVIEW-REASON
               MOVE 6 TO CA-RETURN-CODE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           ELSE
               MOVE "N" TO SM-AMENDMENT-REQD
           END-IF.
       5000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 6000  THE SENTENCE THE SALES OFFICE READS.
      *>----------------------------------------------------------------
       6000-EXPLAIN.
           IF CA-REASON-CODE OF DFHCOMMAREA = "MRG0008"
               GO TO 6000-EXIT
           END-IF
           MOVE SPACES TO SM-EXPLANATION
           MOVE SM-NET-AMOUNT TO WS-MSG-NUM
           STRING "Net " FUNCTION TRIM(SM-CURRENCY) " "
                  FUNCTION TRIM(WS-MSG-NUM) " less cost "
               DELIMITED BY SIZE INTO SM-EXPLANATION
           MOVE SM-COST-AMOUNT TO WS-MSG-NUM
           STRING FUNCTION TRIM(SM-EXPLANATION)
                  FUNCTION TRIM(WS-MSG-NUM) " on the "
                  FUNCTION TRIM(SM-COST-BASIS-USED) " basis"
               DELIMITED BY SIZE INTO SM-EXPLANATION
           IF SM-MARGIN-STATE = "OK"
               MOVE SM-MARGIN-PCT TO WS-MSG-PCT
               STRING FUNCTION TRIM(SM-EXPLANATION) " gives margin "
                      FUNCTION TRIM(WS-MSG-PCT) "%"
                   DELIMITED BY SIZE INTO SM-EXPLANATION
           ELSE
               STRING FUNCTION TRIM(SM-EXPLANATION) " gives margin "
                      FUNCTION TRIM(SM-MARGIN-STATE)
                   DELIMITED BY SIZE INTO SM-EXPLANATION
           END-IF
           IF SM-MARKUP-STATE = "OK"
               MOVE SM-MARKUP-PCT TO WS-MSG-PCT
               STRING FUNCTION TRIM(SM-EXPLANATION) " and markup "
                      FUNCTION TRIM(WS-MSG-PCT) "%"
                   DELIMITED BY SIZE INTO SM-EXPLANATION
           ELSE
               STRING FUNCTION TRIM(SM-EXPLANATION) " and markup "
                      FUNCTION TRIM(SM-MARKUP-STATE)
                   DELIMITED BY SIZE INTO SM-EXPLANATION
           END-IF
           IF SM-NEEDS-APPROVAL
               STRING FUNCTION TRIM(SM-EXPLANATION)
                      " - approval is required"
                   DELIMITED BY SIZE INTO SM-EXPLANATION
           END-IF
           IF CA-MESSAGE OF DFHCOMMAREA = SPACES
               MOVE SM-EXPLANATION TO CA-MESSAGE OF DFHCOMMAREA
           END-IF.
       6000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 9000  ERROR EXITS
      *>----------------------------------------------------------------
       9000-REJECT.
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE "Y" TO WS-ERROR
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE "MRG0008" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           STRING "Function " FUNCTION TRIM(CA-FUNCTION OF DFHCOMMAREA)
                  " is not one of LINE, CHECK, PROPOSE or EXPLAIN"
               DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE "Y" TO WS-ERROR
           MOVE "Y" TO WS-SQL-FAILED
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE OF DFHCOMMAREA
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in SOMARGN"
                   TO CA-MESSAGE OF DFHCOMMAREA
           END-IF
           MOVE "SQL0001" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA.
       9300-EXIT.
           EXIT.
