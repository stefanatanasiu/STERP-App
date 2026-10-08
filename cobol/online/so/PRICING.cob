      *>================================================================
      *> PROGRAM-ID : PRICING
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT
      *> PURPOSE    : DETERMINISTIC EFFECTIVE DATED SELLING PRICE
      *>              SELECTION.  THE PRECEDENCE IS FIXED - AN
      *>              APPROVED MANUAL PRICE, THEN A CONTRACT OR
      *>              CUSTOMER ITEM AGREEMENT, THEN A CUSTOMER
      *>              SPECIFIC PRICE, THEN THE CUSTOMER'S PRICE GROUP
      *>              LIST, THEN THE ITEM STANDARD LIST.  WITHIN A
      *>              LEVEL THE CURRENCY, THE UNIT AND THE QUANTITY
      *>              BAND MUST ALL MATCH, AND TWO EQUALLY RANKED
      *>              PRICES THAT DISAGREE BLOCK CONFIRMATION.
      *> CALLED BY  : SALES ORDER ENTRY, QUOTATION ENTRY, SODISC,
      *>              THE .NET CLIENT PRICE ENQUIRY
      *> INTERFACE  : CALL "PRICING" USING WS-PRICING-CA
      *>              COPYBOOK PRICINGCA
      *> CALLS      : SQLIO (so.PriceList, so.PriceListLine), PARMGET
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-03-16 RJH  CR0912 SALES ORDER PRICING REWRITE (ACUCOBOL)
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT PRICE EFFECTIVE DATES
      *> 2003-05-09 DJM  CR1077 QUANTITY BREAKS AND PRICE-PER QTY
      *> 2008-09-22 KMP  CR1702 CONTRACT PRICES OUTRANK CUSTOMER LISTS
      *> 2026-10-06 STERP SO-004 EXPLAINABLE AND UNAMBIGUOUS PRICING
      *>================================================================
      *> REQ: SO-004
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRICING.
       AUTHOR. R J HOLLOWAY.
       DATE-WRITTEN. 16/03/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARM-CA==.
       01  WS-CONTROL.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-SQL-FAILED           PIC X VALUE "N".
               88  WS-SQL-BAD                    VALUE "Y".
           05  WS-DONE                 PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-IX                   PIC S9(9) COMP-5 VALUE 0.
           05  WS-JX                   PIC S9(9) COMP-5 VALUE 0.
           05  WS-CNT                  PIC S9(9) COMP-5 VALUE 0.
           05  WS-ELIGIBLE             PIC X VALUE "N".
           05  WS-DATE-N               PIC 9(8) VALUE 0.
           05  WS-REJECT-CUR           PIC S9(9) COMP-5 VALUE 0.
           05  WS-REJECT-UOM           PIC S9(9) COMP-5 VALUE 0.
           05  WS-REJECT-QTY           PIC S9(9) COMP-5 VALUE 0.
           05  WS-ROWS-SEEN            PIC S9(9) COMP-5 VALUE 0.
       01  WS-BEST.
           05  WS-BEST-IX              PIC S9(9) COMP-5 VALUE 0.
           05  WS-BEST-LEVEL           PIC 9(2) VALUE 99.
           05  WS-BEST-BREAK           PIC S9(13)V9(6) VALUE -1.
           05  WS-BEST-PRICE           PIC S9(11)V9(6) VALUE 0.
           05  WS-BEST-PER             PIC S9(9)V9(6) VALUE 0.
           05  WS-BEST-DISC            PIC S9(5)V9(4) VALUE 0.
       01  WS-CAND-WORK.
           05  WS-W-LEVEL              PIC 9(1) VALUE 0.
           05  WS-W-SCOPE              PIC X(12) VALUE SPACES.
           05  WS-W-LIST               PIC X(20) VALUE SPACES.
           05  WS-W-CURRENCY           PIC X(3) VALUE SPACES.
           05  WS-W-UOM                PIC X(10) VALUE SPACES.
           05  WS-W-QTYBRK             PIC S9(13)V9(6) VALUE 0.
           05  WS-W-PRICE              PIC S9(11)V9(6) VALUE 0.
           05  WS-W-PER                PIC S9(9)V9(6) VALUE 0.
           05  WS-W-DISC               PIC S9(5)V9(4) VALUE 0.
           05  WS-W-MINMGN             PIC S9(5)V9(4) VALUE 0.
           05  WS-W-FROM               PIC 9(8) VALUE 0.
           05  WS-W-TO                 PIC 9(8) VALUE 0.
           05  WS-W-WHY                PIC X(70) VALUE SPACES.
       01  WS-EDIT.
           05  WS-MSG-NUM              PIC -(11)9.9(4).
           05  WS-MSG-QTY              PIC -(11)9.9(2).
           05  WS-MSG-INT              PIC -(8)9.
      *>   every column arrives from SQLIO as left justified text
       01  WS-ROW.
           05  WS-R-LIST               PIC X(100).
           05  WS-R-LEVEL              PIC X(100).
           05  WS-R-SCOPE              PIC X(100).
           05  WS-R-CURRENCY           PIC X(100).
           05  WS-R-UOM                PIC X(100).
           05  WS-R-QTYBRK             PIC X(100).
           05  WS-R-PRICE              PIC X(100).
           05  WS-R-PER                PIC X(100).
           05  WS-R-DISC               PIC X(100).
           05  WS-R-MINMGN             PIC X(100).
           05  WS-R-FROM               PIC X(100).
           05  WS-R-TO                 PIC X(100).
           05  WS-R-CUST               PIC X(100).
           05  WS-R-GROUP              PIC X(100).
           05  WS-R-CONTRACT           PIC X(100).
       LINKAGE SECTION.
       COPY PRICINGCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           PERFORM 0200-VALIDATE THRU 0200-EXIT
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION OF DFHCOMMAREA
               WHEN "SELECT"  PERFORM 1000-SELECT THRU 1000-EXIT
               WHEN "EXPLAIN" PERFORM 1000-SELECT THRU 1000-EXIT
               WHEN "BREAKS"  PERFORM 2000-BREAKS THRU 2000-EXIT
               WHEN OTHER     PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
      *> 0100  WORKING STORAGE PERSISTS BETWEEN CALLS, SO EVERYTHING
      *>       THAT ACCUMULATES IS CLEARED HERE.
      *>----------------------------------------------------------------
       0100-INITIALISE.
           MOVE "PRICING" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "SO-004" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE "N" TO WS-ERROR WS-SQL-FAILED WS-DONE
           MOVE 0 TO WS-IX WS-JX WS-CNT WS-ROWS-SEEN
           MOVE 0 TO WS-REJECT-CUR WS-REJECT-UOM WS-REJECT-QTY
           MOVE 0 TO WS-BEST-IX
           MOVE 99 TO WS-BEST-LEVEL
           MOVE -1 TO WS-BEST-BREAK
           MOVE 0 TO WS-BEST-PRICE WS-BEST-PER WS-BEST-DISC
           MOVE FUNCTION UPPER-CASE(CA-FUNCTION OF DFHCOMMAREA)
               TO CA-FUNCTION OF DFHCOMMAREA
           MOVE FUNCTION UPPER-CASE(PR-ITEM-CODE) TO PR-ITEM-CODE
           MOVE FUNCTION UPPER-CASE(PR-CUSTOMER) TO PR-CUSTOMER
           MOVE FUNCTION UPPER-CASE(PR-CURRENCY) TO PR-CURRENCY
           MOVE FUNCTION UPPER-CASE(PR-UOM-CODE) TO PR-UOM-CODE
           IF PR-CURRENCY = SPACES
               MOVE "GBP" TO PR-CURRENCY
           END-IF
      *>   the order date is the default price effective date
           IF PR-PRICE-DATE = 0
               MOVE PR-ORDER-DATE TO PR-PRICE-DATE
           END-IF
           IF PR-PRICE-DATE = 0
               MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO PR-PRICE-DATE
           END-IF
           IF PR-MAX-LEVEL < 1 OR PR-MAX-LEVEL > 5
               MOVE 5 TO PR-MAX-LEVEL
           END-IF
           MOVE 0 TO PR-UNIT-PRICE PR-PRICE-PER-QTY PR-DISCOUNT-PCT
           MOVE 0 TO PR-MIN-MARGIN-PCT PR-QTY-BREAK
           MOVE 0 TO PR-PRICE-LEVEL PR-VALID-FROM PR-VALID-TO
           MOVE 0 TO PR-CAND-CNT PR-BREAK-CNT
           MOVE SPACES TO PR-PRICE-SCOPE PR-PRICE-LIST-CODE
           MOVE SPACES TO PR-PRICE-CURRENCY PR-PRICE-UOM
           MOVE SPACES TO PR-PRICE-SOURCE PR-EXPLANATION
           MOVE SPACES TO PR-RIVAL-LIST-CODE
           MOVE "N" TO PR-PRICE-FOUND PR-AMBIGUOUS PR-BLOCK-CONFIRM
           MOVE "N" TO PR-CAND-TRUNC
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 80
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE.
           IF CA-COMPANY OF DFHCOMMAREA = SPACES
               OR PR-ITEM-CODE = SPACES
               MOVE "PRC0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A company and an item are required"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION OF DFHCOMMAREA NOT = "BREAKS"
               AND PR-QUANTITY <= 0
               MOVE "PRC0002" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "The quantity must be greater than zero"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF PR-MANUAL-PRICE NOT = 0
               AND PR-MANUAL-APPROVAL = SPACES
               MOVE "PRC0009" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "A manual price needs an approval reference "
                      "before it can take precedence"
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  SELECT AND EXPLAIN.  BOTH BUILD THE SAME CANDIDATE LIST
      *>       BECAUSE A PRICE THAT CANNOT BE EXPLAINED IS NOT A PRICE
      *>       THE SALES OFFICE CAN DEFEND (SO-004).
      *>----------------------------------------------------------------
       1000-SELECT.
           PERFORM 3000-READ-XCURR-PARM THRU 3000-EXIT
           PERFORM 1100-ADD-MANUAL THRU 1100-EXIT
           PERFORM 1200-READ-LISTS THRU 1200-EXIT
           IF WS-SQL-BAD AND PR-CAND-CNT = 0
               GO TO 1000-EXIT
           END-IF
           PERFORM 1300-RANK THRU 1300-EXIT
           PERFORM 1400-RESULT THRU 1400-EXIT
           IF CA-FUNCTION OF DFHCOMMAREA = "SELECT"
               AND NOT PR-IS-AMBIGUOUS
               MOVE 0 TO PR-CAND-CNT
           END-IF.
       1000-EXIT.
           EXIT.

      *>   LEVEL ONE.  AN APPROVED MANUAL PRICE FOR THIS DOCUMENT
      *>   VERSION OUTRANKS EVERY MASTER FILE PRICE.
       1100-ADD-MANUAL.
           IF PR-MANUAL-PRICE = 0
               GO TO 1100-EXIT
           END-IF
           IF PR-MANUAL-APPROVAL = SPACES
               GO TO 1100-EXIT
           END-IF
           MOVE 1 TO WS-W-LEVEL
           MOVE "MANUAL" TO WS-W-SCOPE
           MOVE PR-MANUAL-APPROVAL TO WS-W-LIST
           MOVE PR-CURRENCY TO WS-W-CURRENCY
           MOVE PR-UOM-CODE TO WS-W-UOM
           MOVE 0 TO WS-W-QTYBRK
           MOVE PR-MANUAL-PRICE TO WS-W-PRICE
           IF PR-MANUAL-PER-QTY > 0
               MOVE PR-MANUAL-PER-QTY TO WS-W-PER
           ELSE
               MOVE 1 TO WS-W-PER
           END-IF
           MOVE 0 TO WS-W-DISC WS-W-MINMGN
           MOVE PR-PRICE-DATE TO WS-W-FROM
           MOVE 0 TO WS-W-TO
           MOVE SPACES TO WS-W-WHY
           STRING "approved manual price "
                  FUNCTION TRIM(PR-MANUAL-APPROVAL)
               DELIMITED BY SIZE INTO WS-W-WHY
           MOVE "Y" TO WS-ELIGIBLE
           PERFORM 1500-ADD-CANDIDATE THRU 1500-EXIT.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1200  LEVELS TWO TO FIVE FROM so.PriceList AND
      *>       so.PriceListLine.  THE QUANTITY BAND IS TESTED IN COBOL
      *>       SO THAT A ROW REJECTED FOR BEING ABOVE THE QUANTITY CAN
      *>       STILL BE SHOWN TO THE USER WITH ITS REASON.
      *>----------------------------------------------------------------
       1200-READ-LISTS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT l.PriceListCode, l.PriceLevel, l.Scope, "
                  "ll.CurrencyCode, ISNULL(ll.UomCode,''), "
                  "ll.QtyBreak, ll.UnitPrice, ll.PricePerQty, "
                  "ll.DiscountPct, ISNULL(ll.MinimumMarginPct,0), "
                  "CONVERT(char(8), ll.ValidFrom, 112), "
                  "ISNULL(CONVERT(char(8), ll.ValidTo, 112),''), "
                  "ISNULL(l.CustomerCode,''), "
                  "ISNULL(l.PriceGroup,''), ISNULL(l.ContractRef,'')"
                  " FROM so.PriceList l JOIN so.PriceListLine ll ON "
                  "ll.CompanyCode = l.CompanyCode AND "
                  "ll.PriceListCode = l.PriceListCode WHERE "
                  "l.CompanyCode = ? AND l.IsActive = 1 AND "
                  "ll.ItemCode = ? AND l.PriceLevel <= ? AND "
                  "l.ValidFrom <= CONVERT(date, ?, 112) AND "
                  "(l.ValidTo IS NULL OR l.ValidTo >= "
                  "CONVERT(date, ?, 112)) AND "
                  "ll.ValidFrom <= CONVERT(date, ?, 112) AND "
                  "(ll.ValidTo IS NULL OR ll.ValidTo >= "
                  "CONVERT(date, ?, 112)) AND (l.Scope = 'STANDARD' "
                  "OR (l.Scope IN ('CONTRACT','CUSTOMER') AND "
                  "l.CustomerCode = ?) OR (l.Scope = 'GROUP' AND "
                  "l.PriceGroup = ?)) ORDER BY l.PriceLevel, "
                  "ll.QtyBreak DESC, l.PriceListCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 9 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE PR-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE PR-MAX-LEVEL TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE PR-PRICE-DATE TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(4)
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(5)
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(6)
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(7)
           MOVE PR-CUSTOMER TO SQLIO-PARM-VAL(8)
           MOVE PR-PRICE-GROUP TO SQLIO-PARM-VAL(9)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 9
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 15 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1200-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROW
               IF SQLIO-ERROR
                   PERFORM 9300-SQL-FAILED THRU 9300-EXIT
                   MOVE "Y" TO WS-DONE
               ELSE
                   IF SQLIO-NOTFOUND
                       MOVE "Y" TO WS-DONE
                   ELSE
                       ADD 1 TO WS-ROWS-SEEN
                       PERFORM 1250-TEST-ROW THRU 1250-EXIT
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       1200-EXIT.
           EXIT.

      *>   ONE FETCHED ROW.  CURRENCY, UNIT AND QUANTITY BAND ARE ALL
      *>   TESTED, AND A ROW THAT FAILS IS STILL RECORDED WITH ITS
      *>   REASON SO THAT EXPLAIN CAN SHOW IT.
       1250-TEST-ROW.
           MOVE WS-R-LIST(1:20) TO WS-W-LIST
           COMPUTE WS-W-LEVEL = FUNCTION NUMVAL(WS-R-LEVEL)
           MOVE WS-R-SCOPE(1:12) TO WS-W-SCOPE
           MOVE WS-R-CURRENCY(1:3) TO WS-W-CURRENCY
           MOVE WS-R-UOM(1:10) TO WS-W-UOM
           COMPUTE WS-W-QTYBRK = FUNCTION NUMVAL(WS-R-QTYBRK)
           COMPUTE WS-W-PRICE = FUNCTION NUMVAL(WS-R-PRICE)
           COMPUTE WS-W-PER = FUNCTION NUMVAL(WS-R-PER)
           COMPUTE WS-W-DISC = FUNCTION NUMVAL(WS-R-DISC)
           COMPUTE WS-W-MINMGN = FUNCTION NUMVAL(WS-R-MINMGN)
           COMPUTE WS-W-FROM = FUNCTION NUMVAL(WS-R-FROM)
           IF WS-R-TO = SPACES
               MOVE 0 TO WS-W-TO
           ELSE
               COMPUTE WS-W-TO = FUNCTION NUMVAL(WS-R-TO)
           END-IF
           IF WS-W-PER <= 0
               MOVE 1 TO WS-W-PER
           END-IF
           MOVE "Y" TO WS-ELIGIBLE
           MOVE SPACES TO WS-W-WHY
           MOVE "candidate" TO WS-W-WHY
      *>   quantity band
           IF WS-W-QTYBRK > PR-QUANTITY
               MOVE "N" TO WS-ELIGIBLE
               ADD 1 TO WS-REJECT-QTY
               MOVE SPACES TO WS-W-WHY
               MOVE WS-W-QTYBRK TO WS-MSG-QTY
               STRING "quantity break "
                      FUNCTION TRIM(WS-MSG-QTY)
                      " is above the order quantity"
                   DELIMITED BY SIZE INTO WS-W-WHY
           END-IF
      *>   currency - no conversion between lists unless permitted
           IF WS-ELIGIBLE = "Y" AND WS-W-CURRENCY NOT = PR-CURRENCY
               IF PR-XCURR-IS-ON
                   MOVE SPACES TO WS-W-WHY
                   STRING "cross currency allowed by "
                          "SO.PRICE.XCURR.ALLOW"
                       DELIMITED BY SIZE INTO WS-W-WHY
               ELSE
                   MOVE "N" TO WS-ELIGIBLE
                   ADD 1 TO WS-REJECT-CUR
                   MOVE SPACES TO WS-W-WHY
                   STRING "priced in " FUNCTION TRIM(WS-W-CURRENCY)
                          " not " FUNCTION TRIM(PR-CURRENCY)
                       DELIMITED BY SIZE INTO WS-W-WHY
               END-IF
           END-IF
      *>   unit of measure - a blank list unit means the item's own
      *>   selling unit, which is the 1998 behaviour kept by CR1077
           IF WS-ELIGIBLE = "Y" AND WS-W-UOM NOT = SPACES
               AND PR-UOM-CODE NOT = SPACES
               AND WS-W-UOM NOT = PR-UOM-CODE
               MOVE "N" TO WS-ELIGIBLE
               ADD 1 TO WS-REJECT-UOM
               MOVE SPACES TO WS-W-WHY
               STRING "priced per " FUNCTION TRIM(WS-W-UOM)
                      " not " FUNCTION TRIM(PR-UOM-CODE)
                   DELIMITED BY SIZE INTO WS-W-WHY
           END-IF
           PERFORM 1500-ADD-CANDIDATE THRU 1500-EXIT.
       1250-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1300  THE RANKING.  LOWEST LEVEL WINS; WITHIN A LEVEL THE
      *>       LARGEST QUANTITY BREAK THAT THE ORDER REACHES WINS;
      *>       TWO SURVIVORS THAT DISAGREE ON PRICE ARE AMBIGUOUS AND
      *>       AMBIGUITY BLOCKS CONFIRMATION.
      *>----------------------------------------------------------------
       1300-RANK.
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > PR-CAND-CNT
               IF PR-C-CHOSEN(WS-IX) NOT = "X"
                   PERFORM 1350-COMPARE THRU 1350-EXIT
               END-IF
           END-PERFORM
           IF WS-BEST-IX > 0
               MOVE "Y" TO PR-C-CHOSEN(WS-BEST-IX)
               MOVE "Y" TO PR-PRICE-FOUND
           END-IF.
       1300-EXIT.
           EXIT.

       1350-COMPARE.
           IF WS-BEST-IX = 0
               PERFORM 1360-TAKE-BEST THRU 1360-EXIT
               GO TO 1350-EXIT
           END-IF
           IF PR-C-LEVEL(WS-IX) < WS-BEST-LEVEL
               PERFORM 1360-TAKE-BEST THRU 1360-EXIT
               GO TO 1350-EXIT
           END-IF
           IF PR-C-LEVEL(WS-IX) > WS-BEST-LEVEL
               MOVE SPACES TO PR-C-WHY(WS-IX)
               STRING "outranked by level " DELIMITED BY SIZE
                   INTO PR-C-WHY(WS-IX)
               MOVE WS-BEST-LEVEL TO WS-MSG-INT
               STRING FUNCTION TRIM(PR-C-WHY(WS-IX)) " "
                      FUNCTION TRIM(WS-MSG-INT)
                   DELIMITED BY SIZE INTO PR-C-WHY(WS-IX)
               GO TO 1350-EXIT
           END-IF
           IF PR-C-QTYBRK(WS-IX) > WS-BEST-BREAK
               PERFORM 1360-TAKE-BEST THRU 1360-EXIT
               GO TO 1350-EXIT
           END-IF
           IF PR-C-QTYBRK(WS-IX) < WS-BEST-BREAK
               MOVE SPACES TO PR-C-WHY(WS-IX)
               STRING "a larger quantity break applies"
                   DELIMITED BY SIZE INTO PR-C-WHY(WS-IX)
               GO TO 1350-EXIT
           END-IF
      *>   same level, same band - identical prices are harmless,
      *>   different prices are an invalid overlap (SO-004)
           IF PR-C-PRICE(WS-IX) = WS-BEST-PRICE
               AND PR-C-PER-QTY(WS-IX) = WS-BEST-PER
               AND PR-C-DISC(WS-IX) = WS-BEST-DISC
               MOVE SPACES TO PR-C-WHY(WS-IX)
               STRING "identical duplicate of the chosen price"
                   DELIMITED BY SIZE INTO PR-C-WHY(WS-IX)
           ELSE
               MOVE "Y" TO PR-AMBIGUOUS
               MOVE PR-C-LIST(WS-IX) TO PR-RIVAL-LIST-CODE
               MOVE SPACES TO PR-C-WHY(WS-IX)
               STRING "equally ranked and disagrees on price"
                   DELIMITED BY SIZE INTO PR-C-WHY(WS-IX)
           END-IF.
       1350-EXIT.
           EXIT.

       1360-TAKE-BEST.
           IF WS-BEST-IX > 0
               MOVE SPACES TO PR-C-WHY(WS-BEST-IX)
               STRING "outranked by a higher precedence price"
                   DELIMITED BY SIZE INTO PR-C-WHY(WS-BEST-IX)
               MOVE "N" TO PR-C-CHOSEN(WS-BEST-IX)
           END-IF
           MOVE WS-IX TO WS-BEST-IX
           MOVE PR-C-LEVEL(WS-IX) TO WS-BEST-LEVEL
           MOVE PR-C-QTYBRK(WS-IX) TO WS-BEST-BREAK
           MOVE PR-C-PRICE(WS-IX) TO WS-BEST-PRICE
           MOVE PR-C-PER-QTY(WS-IX) TO WS-BEST-PER
           MOVE PR-C-DISC(WS-IX) TO WS-BEST-DISC
           MOVE SPACES TO PR-C-WHY(WS-IX)
           STRING "selected - best precedence and quantity band"
               DELIMITED BY SIZE INTO PR-C-WHY(WS-IX).
       1360-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1400  THE ANSWER AND THE SENTENCE THAT EXPLAINS IT.
      *>----------------------------------------------------------------
       1400-RESULT.
           IF WS-BEST-IX = 0
               PERFORM 1450-NOT-FOUND THRU 1450-EXIT
               GO TO 1400-EXIT
           END-IF
           MOVE WS-BEST-IX TO WS-IX
           MOVE PR-C-PRICE(WS-IX) TO PR-UNIT-PRICE
           MOVE PR-C-PER-QTY(WS-IX) TO PR-PRICE-PER-QTY
           MOVE PR-C-DISC(WS-IX) TO PR-DISCOUNT-PCT
           MOVE PR-C-MINMGN(WS-IX) TO PR-MIN-MARGIN-PCT
           MOVE PR-C-LEVEL(WS-IX) TO PR-PRICE-LEVEL
           MOVE PR-C-SCOPE(WS-IX) TO PR-PRICE-SCOPE
           MOVE PR-C-SCOPE(WS-IX) TO PR-PRICE-SOURCE
           MOVE PR-C-LIST(WS-IX) TO PR-PRICE-LIST-CODE
           MOVE PR-C-CURRENCY(WS-IX) TO PR-PRICE-CURRENCY
           MOVE PR-C-UOM(WS-IX) TO PR-PRICE-UOM
           MOVE PR-C-QTYBRK(WS-IX) TO PR-QTY-BREAK
           MOVE PR-C-FROM(WS-IX) TO PR-VALID-FROM
           MOVE PR-C-TO(WS-IX) TO PR-VALID-TO
           IF PR-PRICE-UOM = SPACES
               MOVE PR-UOM-CODE TO PR-PRICE-UOM
           END-IF
           MOVE SPACES TO PR-EXPLANATION
           MOVE PR-UNIT-PRICE TO WS-MSG-NUM
           STRING "Level " DELIMITED BY SIZE INTO PR-EXPLANATION
           MOVE PR-PRICE-LEVEL TO WS-MSG-INT
           STRING FUNCTION TRIM(PR-EXPLANATION)
                  FUNCTION TRIM(WS-MSG-INT) " "
                  FUNCTION TRIM(PR-PRICE-SCOPE) " price from list "
                  FUNCTION TRIM(PR-PRICE-LIST-CODE)
               DELIMITED BY SIZE INTO PR-EXPLANATION
           MOVE PR-UNIT-PRICE TO WS-MSG-NUM
           STRING FUNCTION TRIM(PR-EXPLANATION) ", "
                  FUNCTION TRIM(PR-PRICE-CURRENCY) " "
                  FUNCTION TRIM(WS-MSG-NUM) " per "
               DELIMITED BY SIZE INTO PR-EXPLANATION
           MOVE PR-PRICE-PER-QTY TO WS-MSG-QTY
           STRING FUNCTION TRIM(PR-EXPLANATION)
                  FUNCTION TRIM(WS-MSG-QTY) " "
                  FUNCTION TRIM(PR-PRICE-UOM)
               DELIMITED BY SIZE INTO PR-EXPLANATION
           MOVE PR-QTY-BREAK TO WS-MSG-QTY
           STRING FUNCTION TRIM(PR-EXPLANATION)
                  ", quantity break " FUNCTION TRIM(WS-MSG-QTY)
               DELIMITED BY SIZE INTO PR-EXPLANATION
           MOVE PR-PRICE-DATE TO WS-DATE-N
           STRING FUNCTION TRIM(PR-EXPLANATION)
                  ", effective on " WS-DATE-N
               DELIMITED BY SIZE INTO PR-EXPLANATION
           IF PR-IS-AMBIGUOUS
               PERFORM 1460-AMBIGUOUS THRU 1460-EXIT
           ELSE
               MOVE PR-EXPLANATION TO CA-MESSAGE OF DFHCOMMAREA
           END-IF.
       1400-EXIT.
           EXIT.

       1450-NOT-FOUND.
           MOVE "N" TO PR-PRICE-FOUND
           MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           EVALUATE TRUE
               WHEN WS-REJECT-CUR > 0
                   MOVE "PRC0004" TO CA-REASON-CODE OF DFHCOMMAREA
                   STRING "No effective price for item "
                          FUNCTION TRIM(PR-ITEM-CODE) " in "
                          FUNCTION TRIM(PR-CURRENCY)
                          " - prices are not converted between lists"
                       DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               WHEN WS-REJECT-UOM > 0
                   MOVE "PRC0005" TO CA-REASON-CODE OF DFHCOMMAREA
                   STRING "No effective price for item "
                          FUNCTION TRIM(PR-ITEM-CODE) " per "
                          FUNCTION TRIM(PR-UOM-CODE)
                       DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               WHEN WS-REJECT-QTY > 0
                   MOVE "PRC0006" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE PR-QUANTITY TO WS-MSG-QTY
                   STRING "Quantity " FUNCTION TRIM(WS-MSG-QTY)
                          " is below the lowest quantity break held "
                          "for item " FUNCTION TRIM(PR-ITEM-CODE)
                       DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               WHEN OTHER
                   MOVE "PRC0003" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE PR-PRICE-DATE TO WS-DATE-N
                   STRING "No effective price for item "
                          FUNCTION TRIM(PR-ITEM-CODE) " on "
                          WS-DATE-N
                       DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
           END-EVALUATE
           MOVE CA-MESSAGE OF DFHCOMMAREA TO PR-EXPLANATION.
       1450-EXIT.
           EXIT.

      *>   AN EQUAL PRIORITY OVERLAP IS A DATA FAULT, NOT A CHOICE.
      *>   THE ORDER CANNOT BE CONFIRMED UNTIL THE TWO LISTS ARE
      *>   RECONCILED BY THE PRICE OWNER.
       1460-AMBIGUOUS.
           MOVE "Y" TO PR-BLOCK-CONFIRM
           MOVE "PRC0007" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           STRING "Price lists "
                  FUNCTION TRIM(PR-PRICE-LIST-CODE) " and "
                  FUNCTION TRIM(PR-RIVAL-LIST-CODE)
                  " are equally ranked for item "
                  FUNCTION TRIM(PR-ITEM-CODE)
                  " and disagree - confirmation is blocked"
               DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
           MOVE CA-MESSAGE OF DFHCOMMAREA TO PR-EXPLANATION.
       1460-EXIT.
           EXIT.

       1500-ADD-CANDIDATE.
           IF PR-CAND-CNT >= 40
               MOVE "Y" TO PR-CAND-TRUNC
               GO TO 1500-EXIT
           END-IF
           ADD 1 TO PR-CAND-CNT
           MOVE PR-CAND-CNT TO WS-CNT
           MOVE WS-W-LEVEL TO PR-C-LEVEL(WS-CNT)
           MOVE WS-W-SCOPE TO PR-C-SCOPE(WS-CNT)
           MOVE WS-W-LIST TO PR-C-LIST(WS-CNT)
           MOVE WS-W-CURRENCY TO PR-C-CURRENCY(WS-CNT)
           MOVE WS-W-UOM TO PR-C-UOM(WS-CNT)
           MOVE WS-W-QTYBRK TO PR-C-QTYBRK(WS-CNT)
           MOVE WS-W-PRICE TO PR-C-PRICE(WS-CNT)
           MOVE WS-W-PER TO PR-C-PER-QTY(WS-CNT)
           MOVE WS-W-DISC TO PR-C-DISC(WS-CNT)
           MOVE WS-W-MINMGN TO PR-C-MINMGN(WS-CNT)
           MOVE WS-W-FROM TO PR-C-FROM(WS-CNT)
           MOVE WS-W-TO TO PR-C-TO(WS-CNT)
           MOVE PR-CAND-CNT TO PR-C-RANK(WS-CNT)
           MOVE WS-W-WHY TO PR-C-WHY(WS-CNT)
           IF WS-ELIGIBLE = "Y"
               MOVE "N" TO PR-C-CHOSEN(WS-CNT)
           ELSE
               MOVE "X" TO PR-C-CHOSEN(WS-CNT)
           END-IF.
       1500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  THE QUANTITY BREAKS HELD FOR AN ITEM, EITHER ON ONE
      *>       NAMED LIST OR ON EVERY LIST THE CUSTOMER CAN SEE.
      *>----------------------------------------------------------------
       2000-BREAKS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT l.PriceListCode, l.PriceLevel, l.Scope, "
                  "ll.CurrencyCode, ISNULL(ll.UomCode,''), "
                  "ll.QtyBreak, ll.UnitPrice, ll.PricePerQty, "
                  "ll.DiscountPct, ISNULL(ll.MinimumMarginPct,0), "
                  "CONVERT(char(8), ll.ValidFrom, 112), "
                  "ISNULL(CONVERT(char(8), ll.ValidTo, 112),''), "
                  "ISNULL(l.CustomerCode,''), "
                  "ISNULL(l.PriceGroup,''), ISNULL(l.ContractRef,'')"
                  " FROM so.PriceList l JOIN so.PriceListLine ll ON "
                  "ll.CompanyCode = l.CompanyCode AND "
                  "ll.PriceListCode = l.PriceListCode WHERE "
                  "l.CompanyCode = ? AND ll.ItemCode = ? AND "
                  "(? = '' OR l.PriceListCode = ?) AND "
                  "ll.ValidFrom <= CONVERT(date, ?, 112) AND "
                  "(ll.ValidTo IS NULL OR ll.ValidTo >= "
                  "CONVERT(date, ?, 112)) ORDER BY l.PriceLevel, "
                  "l.PriceListCode, ll.QtyBreak"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE PR-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE PR-LIST-CODE-IN TO SQLIO-PARM-VAL(3)
           MOVE PR-LIST-CODE-IN TO SQLIO-PARM-VAL(4)
           MOVE PR-PRICE-DATE TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(5)
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(6)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 15 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROW
               IF SQLIO-ERROR
                   PERFORM 9300-SQL-FAILED THRU 9300-EXIT
                   MOVE "Y" TO WS-DONE
               ELSE
                   IF SQLIO-NOTFOUND OR PR-BREAK-CNT >= 20
                       MOVE "Y" TO WS-DONE
                   ELSE
                       PERFORM 2100-ADD-BREAK THRU 2100-EXIT
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF PR-BREAK-CNT = 0 AND NOT WS-SQL-BAD
               MOVE "PRC0003" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "No quantity breaks are held for item "
                      FUNCTION TRIM(PR-ITEM-CODE)
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
           END-IF.
       2000-EXIT.
           EXIT.

       2100-ADD-BREAK.
           ADD 1 TO PR-BREAK-CNT
           MOVE PR-BREAK-CNT TO WS-CNT
           COMPUTE PR-B-QTYBRK(WS-CNT) = FUNCTION NUMVAL(WS-R-QTYBRK)
           COMPUTE PR-B-PRICE(WS-CNT) = FUNCTION NUMVAL(WS-R-PRICE)
           COMPUTE PR-B-PER-QTY(WS-CNT) = FUNCTION NUMVAL(WS-R-PER)
           COMPUTE PR-B-DISC(WS-CNT) = FUNCTION NUMVAL(WS-R-DISC)
           MOVE WS-R-CURRENCY(1:3) TO PR-B-CURRENCY(WS-CNT)
           MOVE WS-R-UOM(1:10) TO PR-B-UOM(WS-CNT)
           MOVE WS-R-LIST(1:20) TO PR-B-LIST(WS-CNT)
           COMPUTE PR-B-FROM(WS-CNT) = FUNCTION NUMVAL(WS-R-FROM)
           IF WS-R-TO = SPACES
               MOVE 0 TO PR-B-TO(WS-CNT)
           ELSE
               COMPUTE PR-B-TO(WS-CNT) = FUNCTION NUMVAL(WS-R-TO)
           END-IF.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  THE CROSS CURRENCY POLICY.  THE BASELINE IS "N" SO A
      *>       MISSING PARAMETER CANNOT QUIETLY OPEN A CONVERSION.
      *>----------------------------------------------------------------
       3000-READ-XCURR-PARM.
           IF PR-XCURR-ALLOW = "Y" OR PR-XCURR-ALLOW = "N"
               GO TO 3000-EXIT
           END-IF
           INITIALIZE WS-PARM-CA
           MOVE "GET" TO CA-FUNCTION OF WS-PARM-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-PARM-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-PARM-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "SO.PRICE.XCURR.ALLOW" TO PM-PARAM-CODE
           MOVE PR-PRICE-DATE TO PM-AS-OF
           MOVE "N" TO PM-DEFAULT-TEXT
           MOVE "Y" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARM-CA
           IF CA-RETURN-CODE OF WS-PARM-CA < 8
               AND PM-VALUE-TEXT(1:1) = "Y"
               MOVE "Y" TO PR-XCURR-ALLOW
           ELSE
               MOVE "N" TO PR-XCURR-ALLOW
           END-IF.
       3000-EXIT.
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
           MOVE "PRC0008" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           STRING "Function " FUNCTION TRIM(CA-FUNCTION OF DFHCOMMAREA)
                  " is not one of SELECT, EXPLAIN or BREAKS"
               DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE "Y" TO WS-ERROR
           MOVE "Y" TO WS-SQL-FAILED
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE OF DFHCOMMAREA
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in PRICING"
                   TO CA-MESSAGE OF DFHCOMMAREA
           END-IF
           MOVE "SQL0001" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA.
       9300-EXIT.
           EXIT.
