      *>================================================================
      *> PROGRAM-ID : POPRICE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : DETERMINE THE PURCHASE PRICE OF AN ITEM FROM THE
      *>              EFFECTIVE DATED AGREEMENT, QUOTATION OR LIST
      *>              TERMS THAT APPLIED ON THE PRICING DATE, APPLYING
      *>              QUANTITY BREAKS, DISCOUNTS AND PRICE PER UNITS.
      *>              THE PRICING DATE OF AN ISSUED ORDER IS ITS ORDER
      *>              DATE, SO A LATER SUPPLIER PRICE CHANGE CANNOT
      *>              SILENTLY REPRICE IT (PO-004).
      *> CALLED BY  : POORDER, POREQ, SOURCSEL, .NET PURCHASING FACADE
      *> INTERFACE  : CALL "POPRICE" USING DFHCOMMAREA  (POPRICECA)
      *> CALLS      : SQLIO -> po.PurchasePrice, po.PurchasePriceBreak
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-04-14 RJH  CR0791 PURCHASE PRICE BOOK
      *> 2004-02-09 DJM  CR1203 PRICE PER QUANTITY FOR FASTENERS
      *> 2011-06-30 KMP  CR1688 CONTRACT BEATS QUOTE BEATS LIST
      *> 2026-10-06 STERP PO-004 EFFECTIVE DATED AGREEMENTS
      *>================================================================
      *> REQ: PO-004, PO-002
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POPRICE.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 14/04/1998.
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
           05  WS-FOUND-SW             PIC X VALUE "N".
               88  WS-FOUND                      VALUE "Y".
       01  WS-PRICE-ROW.
           05  WS-P-ID                 PIC X(16).
           05  WS-P-BASIS              PIC X(10).
           05  WS-P-AGREEMENT          PIC X(40).
           05  WS-P-CURRENCY           PIC X(3).
           05  WS-P-UNIT-PRICE         PIC X(24).
           05  WS-P-PRICE-PER          PIC X(24).
           05  WS-P-DISCOUNT           PIC X(16).
           05  WS-P-SETT-DISC          PIC X(16).
           05  WS-P-EFF-FROM           PIC X(8).
           05  WS-P-EFF-TO             PIC X(8).
           05  WS-P-UOM                PIC X(10).
       01  WS-BREAK-ROW.
           05  WS-B-FROM               PIC X(24).
           05  WS-B-PRICE              PIC X(24).
           05  WS-B-DISC               PIC X(16).
       01  WS-HIST-ROW.
           05  WS-H-PRICE              PIC X(24).
           05  WS-H-DATE               PIC X(8).
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(40).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-BEST-PRICE           PIC S9(11)V9(6) VALUE 0.
           05  WS-BEST-DISC            PIC S9(5)V9(4) VALUE 0.
           05  WS-BEST-QTY             PIC S9(13)V9(6) VALUE 0.
           05  WS-GROSS                PIC S9(13)V99 VALUE 0.
           05  WS-DISC-AMT             PIC S9(13)V99 VALUE 0.
           05  WS-QTY-ED               PIC -(13)9.999999.
           05  WS-PRC-ED               PIC -(11)9.999999.
       LINKAGE SECTION.
       COPY POPRICECA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "PRICE"
                   PERFORM 2000-PRICE THRU 2000-EXIT
               WHEN "BREAKS"
                   PERFORM 2000-PRICE THRU 2000-EXIT
                   IF NOT WS-FATAL
                       PERFORM 3000-BREAK-TABLE THRU 3000-EXIT
                   END-IF
               WHEN "HISTORY"
                   PERFORM 4000-HISTORY THRU 4000-EXIT
               WHEN OTHER
                   MOVE "POP1004" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                          " is not supported by POPRICE"
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
           MOVE "N" TO WS-FOUND-SW
           MOVE "POPRICE" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE 0 TO PR-UNIT-PRICE PR-EFFECTIVE-PRICE
           MOVE 0 TO PR-DISCOUNT-PCT PR-SETT-DISC-PCT
           MOVE 0 TO PR-NET-VALUE PR-GROSS-VALUE
           MOVE 0 TO PR-PRICE-ID PR-BREAK-CNT
           MOVE 0 TO PR-EFFECTIVE-FROM PR-EFFECTIVE-TO
           MOVE 0 TO PR-BREAK-QTY-USED
           MOVE 0 TO PR-LAST-PAID-PRICE PR-LAST-PAID-DATE
           MOVE 1 TO PR-PRICE-PER
           MOVE SPACES TO PR-PRICE-BASIS PR-AGREEMENT-REF
           MOVE SPACES TO PR-SOURCE-TEXT
           IF PR-SUPPLIER = SPACES OR PR-ITEM-CODE = SPACES
               MOVE "POP1001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A supplier and an item are needed to price "
                      "a purchase line"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           IF CA-FUNCTION NOT = "HISTORY" AND PR-QUANTITY <= 0
               MOVE "POP1003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A purchase quantity must be greater than zero"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           IF PR-PRICING-DATE = 0
               MOVE CA-BUSINESS-DATE TO PR-PRICING-DATE
           END-IF
           IF PR-CURRENCY = SPACES
               MOVE "GBP" TO PR-CURRENCY
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE PRICE BOOK IS READ IN PRIORITY ORDER: AN EXPLICIT ITEM
      *> AGREEMENT BEATS AN ITEM GROUP AGREEMENT, CONTRACT BEATS QUOTE
      *> BEATS LIST, AND THE LATEST EFFECTIVE FROM DATE WINS.  ONLY
      *> PRICES EFFECTIVE ON THE PRICING DATE ARE CONSIDERED.
      *>----------------------------------------------------------------
       2000-PRICE SECTION.
       2000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 CAST(p.PurchasePriceId AS varchar(16"
                  ")), p.PriceBasis, ISNULL(p.AgreementRef,''), "
                  "p.CurrencyCode, CAST(p.UnitPrice AS varchar(24)), "
                  "CAST(p.PricePerQty AS varchar(24)), "
                  "CAST(p.DiscountPct AS varchar(16)), "
                  "CAST(p.SettlementDiscountPct AS varchar(16)), "
                  "CONVERT(char(8), p.EffectiveFrom, 112), "
                  "ISNULL(CONVERT(char(8), p.EffectiveTo, 112),'') , "
                  "ISNULL(p.UomCode,'') FROM po.PurchasePrice p "
                  "WHERE p.CompanyCode = ? AND p.SupplierCode = ? "
                  "AND p.IsActive = 1 "
                  "AND (p.ItemCode = ? OR (p.ItemCode IS NULL AND "
                  "p.ItemGroupCode = ?)) "
                  "AND p.EffectiveFrom <= CAST(? AS date) "
                  "AND (p.EffectiveTo IS NULL OR p.EffectiveTo >= "
                  "CAST(? AS date)) "
                  "ORDER BY CASE WHEN p.ItemCode IS NULL THEN 1 ELSE "
                  "0 END, CASE p.PriceBasis WHEN 'CONTRACT' THEN 1 "
                  "WHEN 'QUOTE' THEN 2 WHEN 'LIST' THEN 3 ELSE 4 END,"
                  " p.Priority, p.EffectiveFrom DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE PR-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE PR-ITEM-CODE TO SQLIO-PARM-VAL(3)
           MOVE PR-ITEM-GROUP TO SQLIO-PARM-VAL(4)
           MOVE PR-PRICING-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE SQLIO-PARM-VAL(5) TO SQLIO-PARM-VAL(6)
           MOVE 6 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 11 TO SQLIO-COL-CNT
           MOVE 16 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 40 TO SQLIO-COL-WIDTH(3)
           MOVE 3 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 16 TO SQLIO-COL-WIDTH(7)
           MOVE 16 TO SQLIO-COL-WIDTH(8)
           MOVE 8 TO SQLIO-COL-WIDTH(9)
           MOVE 8 TO SQLIO-COL-WIDTH(10)
           MOVE 10 TO SQLIO-COL-WIDTH(11)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PRICE-ROW
           IF SQLIO-ERROR
               PERFORM 9200-SQL-ERROR THRU 9200-EXIT
               GO TO 2000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "POP1002" TO CA-REASON-CODE
               MOVE "NONE" TO PR-PRICE-BASIS
               MOVE SPACES TO CA-MESSAGE
               STRING "No effective purchase price for supplier "
                      FUNCTION TRIM(PR-SUPPLIER) " and item "
                      FUNCTION TRIM(PR-ITEM-CODE)
                      " on the pricing date"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE CA-MESSAGE TO PR-SOURCE-TEXT
               MOVE 16 TO CA-RETURN-CODE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 2000-EXIT
           END-IF
           MOVE "Y" TO WS-FOUND-SW
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-P-ID)
           MOVE WS-NUM TO PR-PRICE-ID
           MOVE WS-P-BASIS TO PR-PRICE-BASIS
      *>     PO-004.  THE BUYER IS ALWAYS TOLD WHERE THE PRICE CAME
      *>     FROM.  A PRICE BOOK ROW WITH NO BASIS ON IT IS A HAND
      *>     KEYED ONE, SO SAY SO RATHER THAN RETURNING SPACES.
           IF PR-PRICE-BASIS = SPACES
               MOVE "MANUAL" TO PR-PRICE-BASIS
           END-IF
           MOVE WS-P-AGREEMENT TO PR-AGREEMENT-REF
           COMPUTE PR-UNIT-PRICE = FUNCTION NUMVAL(WS-P-UNIT-PRICE)
           COMPUTE PR-PRICE-PER = FUNCTION NUMVAL(WS-P-PRICE-PER)
           COMPUTE PR-DISCOUNT-PCT = FUNCTION NUMVAL(WS-P-DISCOUNT)
           COMPUTE PR-SETT-DISC-PCT = FUNCTION NUMVAL(WS-P-SETT-DISC)
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-P-EFF-FROM)
           MOVE WS-NUM TO PR-EFFECTIVE-FROM
           IF WS-P-EFF-TO NOT = SPACES
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-P-EFF-TO)
               MOVE WS-NUM TO PR-EFFECTIVE-TO
           END-IF
           IF PR-PRICE-PER <= 0
               MOVE 1 TO PR-PRICE-PER
           END-IF
           IF WS-P-CURRENCY NOT = PR-CURRENCY
               MOVE "POP1005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The agreed price is held in "
                      FUNCTION TRIM(WS-P-CURRENCY)
                      " and the order is in "
                      FUNCTION TRIM(PR-CURRENCY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE WS-P-CURRENCY TO PR-CURRENCY
               MOVE 4 TO CA-RETURN-CODE
           END-IF
           PERFORM 2200-APPLY-BREAK THRU 2200-EXIT
           PERFORM 2400-VALUE THRU 2400-EXIT
           MOVE SPACES TO PR-SOURCE-TEXT
           STRING FUNCTION TRIM(PR-PRICE-BASIS) " "
                  FUNCTION TRIM(PR-AGREEMENT-REF)
                  " effective from " WS-P-EFF-FROM(7:2) "/"
                  WS-P-EFF-FROM(5:2) "/" WS-P-EFF-FROM(1:4)
               DELIMITED BY SIZE INTO PR-SOURCE-TEXT
           IF CA-RETURN-CODE = 0
               MOVE SPACES TO CA-MESSAGE
               MOVE PR-EFFECTIVE-PRICE TO WS-PRC-ED
               STRING "Price " FUNCTION TRIM(WS-PRC-ED) " from "
                      FUNCTION TRIM(PR-SOURCE-TEXT)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE BREAK THAT APPLIES IS THE HIGHEST FROM QUANTITY THAT THE
      *> ORDER QUANTITY REACHES.  NO BREAK LEAVES THE HEADER PRICE.
      *>----------------------------------------------------------------
       2200-APPLY-BREAK SECTION.
       2200-START.
           MOVE PR-UNIT-PRICE TO WS-BEST-PRICE
           MOVE PR-DISCOUNT-PCT TO WS-BEST-DISC
           MOVE 0 TO WS-BEST-QTY
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 CAST(b.FromQty AS varchar(24)), "
                  "CAST(b.UnitPrice AS varchar(24)), "
                  "CAST(b.DiscountPct AS varchar(16)) "
                  "FROM po.PurchasePriceBreak b WHERE "
                  "b.PurchasePriceId = ? AND b.FromQty <= "
                  "CAST(? AS decimal(19,6)) ORDER BY b.FromQty DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE PR-PRICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE PR-QUANTITY TO WS-QTY-ED
           MOVE FUNCTION TRIM(WS-QTY-ED) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 16 TO SQLIO-COL-WIDTH(3)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BREAK-ROW
           IF SQLIO-ERROR
               PERFORM 9200-SQL-ERROR THRU 9200-EXIT
               GO TO 2200-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 2200-EXIT
           END-IF
           COMPUTE WS-BEST-QTY = FUNCTION NUMVAL(WS-B-FROM)
           COMPUTE WS-BEST-PRICE = FUNCTION NUMVAL(WS-B-PRICE)
           COMPUTE WS-BEST-DISC = FUNCTION NUMVAL(WS-B-DISC)
           MOVE WS-BEST-QTY TO PR-BREAK-QTY-USED
           MOVE WS-BEST-DISC TO PR-DISCOUNT-PCT.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PRICE PER: A PRICE OF 12.500000 PER 100 IS 0.125000 EACH.
      *> THE LINE IS VALUED AT SIX DECIMALS AND ROUNDED ONCE, AT THE
      *> LINE, TO TWO DECIMALS (D-09, COM-011).
      *>----------------------------------------------------------------
       2400-VALUE SECTION.
       2400-START.
           COMPUTE PR-EFFECTIVE-PRICE ROUNDED =
                   WS-BEST-PRICE / PR-PRICE-PER
           COMPUTE WS-GROSS ROUNDED = PR-QUANTITY * WS-BEST-PRICE
                   / PR-PRICE-PER
           MOVE WS-GROSS TO PR-GROSS-VALUE
           COMPUTE WS-DISC-AMT ROUNDED =
                   WS-GROSS * PR-DISCOUNT-PCT / 100
           COMPUTE PR-NET-VALUE = WS-GROSS - WS-DISC-AMT
           MOVE WS-BEST-PRICE TO PR-UNIT-PRICE.
       2400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3000-BREAK-TABLE SECTION.
       3000-START.
           MOVE 0 TO PR-BREAK-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(b.FromQty AS varchar(24)), "
                  "CAST(b.UnitPrice AS varchar(24)), "
                  "CAST(b.DiscountPct AS varchar(16)) "
                  "FROM po.PurchasePriceBreak b WHERE "
                  "b.PurchasePriceId = ? ORDER BY b.FromQty"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE PR-PRICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 16 TO SQLIO-COL-WIDTH(3)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BREAK-ROW
           IF SQLIO-ERROR
               PERFORM 9200-SQL-ERROR THRU 9200-EXIT
               GO TO 3000-EXIT
           END-IF
           PERFORM 3100-FETCH-BREAK THRU 3100-EXIT
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
                         OR PR-BREAK-CNT >= 10
               ADD 1 TO PR-BREAK-CNT
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-B-FROM)
               MOVE WS-NUM TO PR-B-FROM-QTY(PR-BREAK-CNT)
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-B-PRICE)
               MOVE WS-NUM TO PR-B-UNIT-PRICE(PR-BREAK-CNT)
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-B-DISC)
               MOVE WS-NUM TO PR-B-DISC-PCT(PR-BREAK-CNT)
               PERFORM 3100-FETCH-BREAK THRU 3100-EXIT
           END-PERFORM
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BREAK-ROW.
       3000-EXIT.
           EXIT.

       3100-FETCH-BREAK SECTION.
       3100-START.
           MOVE 2 TO SQLIO-CURSOR
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BREAK-ROW.
       3100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PRICE HISTORY FEEDS SOURCE SELECTION (PO-002): THE LAST PRICE
      *> ACTUALLY RECEIVED AGAINST AN APPROVED ORDER, NOT A QUOTED ONE.
      *>----------------------------------------------------------------
       4000-HISTORY SECTION.
       4000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 CAST(l.UnitPrice AS varchar(24)), "
                  "CONVERT(char(8), r.PhysicalDate, 112) "
                  "FROM po.ReceiptLine l JOIN po.Receipt r ON "
                  "r.ReceiptId = l.ReceiptId WHERE r.CompanyCode = ? "
                  "AND r.SupplierCode = ? AND l.ItemCode = ? AND "
                  "r.Status = 'CONFIRMED' ORDER BY r.PhysicalDate "
                  "DESC, l.ReceiptLineId DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE PR-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE PR-ITEM-CODE TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 8 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-HIST-ROW
           IF SQLIO-ERROR
               PERFORM 9200-SQL-ERROR THRU 9200-EXIT
               GO TO 4000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 0 TO PR-LAST-PAID-PRICE PR-LAST-PAID-DATE
               MOVE 4 TO CA-RETURN-CODE
               MOVE "POP1006" TO CA-REASON-CODE
               MOVE "NONE" TO PR-PRICE-BASIS
               MOVE SPACES TO PR-SOURCE-TEXT
               STRING "No purchase history for supplier "
                      FUNCTION TRIM(PR-SUPPLIER) " and item "
                      FUNCTION TRIM(PR-ITEM-CODE)
                   DELIMITED BY SIZE INTO PR-SOURCE-TEXT
               MOVE SPACES TO CA-MESSAGE
               STRING "No purchase history for this supplier and item"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           COMPUTE PR-LAST-PAID-PRICE = FUNCTION NUMVAL(WS-H-PRICE)
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-H-DATE)
           MOVE WS-NUM TO PR-LAST-PAID-DATE
      *>     PO-004.  HISTORY IS A PRICE SOURCE IN ITS OWN RIGHT -
      *>     SOURCSEL RANKS ON IT - SO IT IS LABELLED LIKE ONE.
           MOVE "LASTPAID" TO PR-PRICE-BASIS
           MOVE PR-LAST-PAID-PRICE TO WS-PRC-ED
           MOVE SPACES TO PR-SOURCE-TEXT
           STRING "LASTPAID last price received on "
                  WS-H-DATE(7:2) "/" WS-H-DATE(5:2) "/"
                  WS-H-DATE(1:4)
               DELIMITED BY SIZE INTO PR-SOURCE-TEXT
           MOVE SPACES TO CA-MESSAGE
           STRING "Last price received " FUNCTION TRIM(WS-PRC-ED)
                  " on " WS-H-DATE(7:2) "/" WS-H-DATE(5:2) "/"
                  WS-H-DATE(1:4)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO CA-RETURN-CODE
           MOVE "Y" TO WS-FATAL-SW.
       9000-EXIT.
           EXIT.

       9200-SQL-ERROR SECTION.
       9200-START.
           MOVE 12 TO CA-RETURN-CODE
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           MOVE "Y" TO WS-FATAL-SW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       9200-EXIT.
           EXIT.
