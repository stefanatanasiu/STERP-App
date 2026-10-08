      *>================================================================
      *> PROGRAM-ID : TPOPRICE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : UNIT TESTS FOR POPRICE, THE PURCHASE PRICING
      *>              SERVICE.  THE SUITE MAKES ITS OWN AGREEMENT AND
      *>              ITS OWN QUANTITY BREAKS THROUGH SQLIO, EXERCISES
      *>              THE EFFECTIVE DATING, THE PRICE-PER RULE AND THE
      *>              BREAK LADDER OF SPECIFICATION 06 SECTION 4, AND
      *>              DELETES ITS FIXTURES AGAIN, SO IT MAY BE RUN AS
      *>              OFTEN AS YOU LIKE AGAINST A LIVE DATABASE.
      *>              WITHOUT A CONNECTION THE SUITE SKIPS.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: PO-004   AC: a quantity break and an expired agreement
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TPOPRICE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY POPRICECA REPLACING ==DFHCOMMAREA== BY ==WS-PR==.
       COPY SQLIOCA.
       01  WS-ROW                      PIC X(256).
       01  WS-PRICE-ID                 PIC X(16).
       01  WS-DB-OK                    PIC X VALUE "N".
       01  WS-SUPP                     PIC X(20) VALUE "ZZTESTSUP".
       01  WS-ITEM                 PIC X(40)
                                   VALUE "ZZ-TEST-PRICE-ITEM".
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           MOVE "po POPRICE" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-CONNECT THRU 0100-EXIT
           IF WS-DB-OK = "Y"
               PERFORM 0200-CLEAN THRU 0200-EXIT
               PERFORM 0300-FIXTURES THRU 0300-EXIT
               PERFORM 1000-CURRENT-AGREEMENT THRU 1000-EXIT
               PERFORM 1100-FIRST-BREAK THRU 1100-EXIT
               PERFORM 1200-SECOND-BREAK THRU 1200-EXIT
               PERFORM 1300-EFFECTIVE-DATED THRU 1300-EXIT
               PERFORM 1400-BREAK-LADDER THRU 1400-EXIT
               PERFORM 1500-SOURCE-REPORTED THRU 1500-EXIT
               PERFORM 0200-CLEAN THRU 0200-EXIT
               PERFORM 9000-DISCONNECT THRU 9000-EXIT
           ELSE
               MOVE "no database connection" TO TK-DETAIL
               MOVE "po POPRICE pricing needs the STERP database"
                 TO TK-NAME
               PERFORM TK-SKIP
           END-IF
           PERFORM TK-END
           STOP RUN.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0100-CONNECT SECTION.
       0100-START.
           INITIALIZE SQLIO-CA
           MOVE "CONN" TO SQLIO-FUNC
           MOVE SPACES TO SQLIO-STMT
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-SQLCODE = 0
               MOVE "Y" TO WS-DB-OK
           END-IF.
       0100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE FIXTURES CARRY AN AGREEMENT REFERENCE OF THEIR OWN SO THAT
      *> A FAILED RUN CANNOT LEAVE A PRICE BEHIND FOR THE NEXT ONE.
      *>----------------------------------------------------------------
       0200-CLEAN SECTION.
       0200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE b FROM po.PurchasePriceBreak b JOIN "
                  "po.PurchasePrice p ON p.PurchasePriceId = "
                  "b.PurchasePriceId WHERE p.AgreementRef LIKE "
                  "'ZZ-TEST-PRC%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.PurchasePrice WHERE AgreementRef "
                  "LIKE 'ZZ-TEST-PRC%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT.
       0200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> ONE EXPIRED CONTRACT AT 125.00 PER 100 AND THE CURRENT ONE AT
      *> 140.00 PER 100 WITH BREAKS AT 500 (130.00) AND 1000 (120.00).
      *>----------------------------------------------------------------
       0300-FIXTURES SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.PurchasePrice (CompanyCode, SupplierCode"
                  ", ItemCode, PriceBasis, AgreementRef, CurrencyCode"
                  ", UomCode, PricePerQty, UnitPrice, EffectiveFrom, "
                  "EffectiveTo, CreatedBy) VALUES ('STC', "
                  "'ZZTESTSUP', 'ZZ-TEST-PRICE-ITEM', 'CONTRACT', "
                  "'ZZ-TEST-PRC-OLD', 'GBP', 'EA', 100, 125.000000, "
                  "'2026-01-01', '2026-06-30', 'TEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.PurchasePrice (CompanyCode, SupplierCode"
                  ", ItemCode, PriceBasis, AgreementRef, CurrencyCode"
                  ", UomCode, PricePerQty, UnitPrice, EffectiveFrom, "
                  "CreatedBy) VALUES ('STC', 'ZZTESTSUP', "
                  "'ZZ-TEST-PRICE-ITEM', 'CONTRACT', "
                  "'ZZ-TEST-PRC-CUR', 'GBP', 'EA', 100, 140.000000, "
                  "'2026-07-01', 'TEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(PurchasePriceId AS varchar(16)) FROM "
                  "po.PurchasePrice WHERE AgreementRef = "
                  "'ZZ-TEST-PRC-CUR'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE "SNGL" TO SQLIO-FUNC
           MOVE 8 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 16 TO SQLIO-COL-WIDTH(1)
           MOVE SPACES TO WS-ROW
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE WS-ROW(1:16) TO WS-PRICE-ID
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.PurchasePriceBreak (PurchasePriceId, "
                  "FromQty, UnitPrice) VALUES ("
                  FUNCTION TRIM(WS-PRICE-ID) ", 500, 130.000000), ("
                  FUNCTION TRIM(WS-PRICE-ID) ", 1000, 120.000000)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT.
       0300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0900-EXEC SECTION.
       0900-START.
           MOVE "EXEC" TO SQLIO-FUNC
           MOVE 8 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       0900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0950-ASK SECTION.
       0950-START.
           MOVE "PRICE" TO CA-FUNCTION
           MOVE "STC" TO CA-COMPANY
           MOVE "RDG" TO CA-SITE
           MOVE "BUYER" TO CA-USER
           MOVE 20261006 TO CA-BUSINESS-DATE
           MOVE WS-SUPP TO PR-SUPPLIER
           MOVE WS-ITEM TO PR-ITEM-CODE
           MOVE "EA" TO PR-UOM-CODE
           MOVE "GBP" TO PR-CURRENCY
           CALL "POPRICE" USING WS-PR.
       0950-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 250 AT 140.00 PER 100 IS 350.00 - THE EXPIRED 125.00 CONTRACT
      *> MUST NOT BE USED ON A PRICING DATE AFTER ITS LAST DAY.
      *>----------------------------------------------------------------
       1000-CURRENT-AGREEMENT SECTION.
       1000-START.
           INITIALIZE WS-PR
           MOVE 250 TO PR-QUANTITY
           MOVE 20261006 TO PR-PRICING-DATE
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-004 the agreement in force today is used"
             TO TK-NAME
           MOVE 140 TO TK-EXPECTED-N
           MOVE PR-UNIT-PRICE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-004 the price is per 100, not per each" TO TK-NAME
           MOVE 100 TO TK-EXPECTED-N
           MOVE PR-PRICE-PER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-004 250 at 140.00 per 100 is 350.00" TO TK-NAME
           MOVE 350.00 TO TK-EXPECTED-N
           MOVE PR-NET-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-004 the basis of the price is reported" TO TK-NAME
           MOVE "CONTRACT" TO TK-EXPECTED-X
           MOVE PR-PRICE-BASIS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-004 the agreement reference is reported"
             TO TK-NAME
           MOVE "ZZ-TEST-PRC-CUR" TO TK-EXPECTED-X
           MOVE PR-AGREEMENT-REF TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 500 REACHES THE FIRST BREAK: 130.00 PER 100 IS 650.00.
      *>----------------------------------------------------------------
       1100-FIRST-BREAK SECTION.
       1100-START.
           INITIALIZE WS-PR
           MOVE 500 TO PR-QUANTITY
           MOVE 20261006 TO PR-PRICING-DATE
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-004 500 reaches the 500 break at 130.00"
             TO TK-NAME
           MOVE 130 TO TK-EXPECTED-N
           MOVE PR-UNIT-PRICE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-004 the break quantity used is reported"
             TO TK-NAME
           MOVE 500 TO TK-EXPECTED-N
           MOVE PR-BREAK-QTY-USED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-004 500 at 130.00 per 100 is 650.00" TO TK-NAME
           MOVE 650.00 TO TK-EXPECTED-N
           MOVE PR-NET-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 1200 REACHES THE SECOND BREAK: 120.00 PER 100 IS 1,440.00.
      *>----------------------------------------------------------------
       1200-SECOND-BREAK SECTION.
       1200-START.
           INITIALIZE WS-PR
           MOVE 1200 TO PR-QUANTITY
           MOVE 20261006 TO PR-PRICING-DATE
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-004 1200 reaches the 1000 break at 120.00"
             TO TK-NAME
           MOVE 120 TO TK-EXPECTED-N
           MOVE PR-UNIT-PRICE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-004 1200 at 120.00 per 100 is 1440.00" TO TK-NAME
           MOVE 1440.00 TO TK-EXPECTED-N
           MOVE PR-NET-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PRICED BACK IN MARCH THE EXPIRED CONTRACT IS THE RIGHT ONE:
      *> 250 AT 125.00 PER 100 IS 312.50.
      *>----------------------------------------------------------------
       1300-EFFECTIVE-DATED SECTION.
       1300-START.
           INITIALIZE WS-PR
           MOVE 250 TO PR-QUANTITY
           MOVE 20260301 TO PR-PRICING-DATE
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-004 a March order uses the March agreement"
             TO TK-NAME
           MOVE 125 TO TK-EXPECTED-N
           MOVE PR-UNIT-PRICE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-004 250 at 125.00 per 100 is 312.50" TO TK-NAME
           MOVE 312.50 TO TK-EXPECTED-N
           MOVE PR-NET-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-004 the agreement of the day is named" TO TK-NAME
           MOVE "ZZ-TEST-PRC-OLD" TO TK-EXPECTED-X
           MOVE PR-AGREEMENT-REF TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE BUYER'S SCREEN ASKS FOR THE WHOLE LADDER BEHIND THE PRICE.
      *>----------------------------------------------------------------
       1400-BREAK-LADDER SECTION.
       1400-START.
           INITIALIZE WS-PR
           MOVE 1 TO PR-QUANTITY
           MOVE 20261006 TO PR-PRICING-DATE
           MOVE "BREAKS" TO CA-FUNCTION
           MOVE "STC" TO CA-COMPANY
           MOVE "BUYER" TO CA-USER
           MOVE 20261006 TO CA-BUSINESS-DATE
           MOVE WS-SUPP TO PR-SUPPLIER
           MOVE WS-ITEM TO PR-ITEM-CODE
           CALL "POPRICE" USING WS-PR
           MOVE "PO-004 both quantity breaks are listed" TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE PR-BREAK-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-004 the ladder starts at the 500 break"
             TO TK-NAME
           MOVE 500 TO TK-EXPECTED-N
           MOVE PR-B-FROM-QTY(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-004 the 1000 break is 120.00" TO TK-NAME
           MOVE 120 TO TK-EXPECTED-N
           MOVE PR-B-UNIT-PRICE(2) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-004.  A PRICE THE BUYER CANNOT TRACE IS NO USE, SO EVERY
      *> ANSWER NAMES ITS BASIS AND SPELLS OUT WHERE IT CAME FROM -
      *> INCLUDING THE HISTORY BASIS AND THE "NOTHING FOUND" ANSWER.
      *>----------------------------------------------------------------
       1500-SOURCE-REPORTED SECTION.
       1500-START.
           INITIALIZE WS-PR
           MOVE 250 TO PR-QUANTITY
           MOVE 20261006 TO PR-PRICING-DATE
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-004 the price source is never returned blank"
             TO TK-NAME
           IF PR-SOURCE-TEXT = SPACES
               MOVE "blank" TO TK-ACTUAL-X
           ELSE
               MOVE "stated" TO TK-ACTUAL-X
           END-IF
           MOVE "stated" TO TK-EXPECTED-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-004 the source names the agreement behind it"
             TO TK-NAME
           IF PR-SOURCE-TEXT(1:8) = "CONTRACT"
               MOVE "CONTRACT" TO TK-ACTUAL-X
           ELSE
               MOVE PR-SOURCE-TEXT(1:20) TO TK-ACTUAL-X
           END-IF
           MOVE "CONTRACT" TO TK-EXPECTED-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-PR
           MOVE 1 TO PR-QUANTITY
           MOVE 20261006 TO PR-PRICING-DATE
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "HISTORY" TO CA-FUNCTION
           CALL "POPRICE" USING WS-PR
           MOVE "PO-004 the last price paid is a named source"
             TO TK-NAME
           IF PR-PRICE-BASIS = "LASTPAID" OR PR-PRICE-BASIS = "NONE"
               MOVE "named" TO TK-ACTUAL-X
           ELSE
               MOVE "blank" TO TK-ACTUAL-X
           END-IF
           MOVE "named" TO TK-EXPECTED-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-004 history explains itself as well" TO TK-NAME
           IF PR-SOURCE-TEXT = SPACES
               MOVE "blank" TO TK-ACTUAL-X
           ELSE
               MOVE "stated" TO TK-ACTUAL-X
           END-IF
           MOVE "stated" TO TK-EXPECTED-X
           PERFORM TK-ASSERT-STR.
       1500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-DISCONNECT SECTION.
       9000-START.
           MOVE "DISC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       9000-EXIT.
           EXIT.
       COPY TESTKITP.
       END PROGRAM TPOPRICE.
