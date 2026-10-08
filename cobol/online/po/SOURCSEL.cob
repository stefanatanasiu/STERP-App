      *>================================================================
      *> PROGRAM-ID : SOURCSEL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : APPROVED SOURCE SELECTION.  CHECKS WHETHER A
      *>              SUPPLIER IS AN APPROVED SOURCE FOR AN ITEM,
      *>              RECORDS COMPETING QUOTATIONS AND MAKES THE
      *>              BUYER JUSTIFY ANY CHOICE THAT IS NOT THE
      *>              CHEAPEST ONE (PO-002).
      *> CALLED BY  : POREQ, POORDER, the .NET sourcing form
      *> INTERFACE  : CALL "SOURCSEL" USING DFHCOMMAREA (SOURCSELCA)
      *> CALLS      : NUMGEN, POPRICE (HISTORY), SQLIO ->
      *>              po.SupplierSource, po.SourcingEvent,
      *>              po.SourcingQuote
      *>----------------------------------------------------------------
      *> THE CHEAPEST QUOTATION IS NOT THE AUTOMATIC WINNER.  LEAD
      *> TIME, DELIVERY PERFORMANCE, QUALITY AND COMMERCIAL
      *> RESTRICTIONS ALL COUNT, AND A BUYER MAY PAY MORE FOR A
      *> BETTER SOURCE - BUT THE PREMIUM AND THE REASON ARE RECORDED
      *> SO THAT SOMEBODY CAN ASK ABOUT IT AFTERWARDS.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-05-14 RJH  CR0795 APPROVED SOURCE REGISTER
      *> 2002-02-18 DJM  CR1056 CHEAPEST IS NOT AUTOMATIC
      *> 2026-10-06 STERP PRICE HISTORY AND PERFORMANCE COMPARISON
      *>================================================================
      *> REQ: PO-002, PO-004, PO-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOURCSEL.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 14/05/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==
                      LEADING ==CA-== BY ==NM-==.
       COPY POPRICECA REPLACING ==DFHCOMMAREA== BY ==WS-PRICE-CA==
                      LEADING ==CA-== BY ==PC-==.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-SRC-ROW.
           05  WS-S-ID                 PIC X(12).
           05  WS-S-STATUS             PIC X(12).
           05  WS-S-PART               PIC X(40).
           05  WS-S-CONTRACT           PIC X(40).
           05  WS-S-LEAD               PIC X(12).
           05  WS-S-MINORD             PIC X(24).
           05  WS-S-PACK               PIC X(24).
           05  WS-S-CURRENCY           PIC X(3).
           05  WS-S-RESTRICT           PIC X(200).
           05  WS-S-EFFECTIVE          PIC X(1).
       01  WS-EVT-ROW.
           05  WS-E-ID                 PIC X(12).
           05  WS-E-STATUS             PIC X(12).
           05  WS-E-QTY                PIC X(24).
       01  WS-RANK-ROW.
           05  WS-R-CNT                PIC X(12).
           05  WS-R-SUPPLIER           PIC X(20).
           05  WS-R-VALUE              PIC X(24).
           05  WS-R-SELVALUE           PIC X(24).
           05  WS-R-RANK               PIC X(12).
       01  WS-ONE-COL.
           05  WS-V1                   PIC X(24).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-EFFECTIVE            PIC S9(11)V9(6) VALUE 0.
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-QTY-ED               PIC -(9)9.999999.
       LINKAGE SECTION.
       COPY SOURCSELCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "CHECK"
                   PERFORM 2000-CHECK THRU 2000-EXIT
               WHEN "OPEN"
                   PERFORM 3000-OPEN THRU 3000-EXIT
               WHEN "QUOTE"
                   PERFORM 4000-QUOTE THRU 4000-EXIT
               WHEN "RANK"
                   PERFORM 5000-RANK THRU 5000-EXIT
               WHEN "SELECT"
                   PERFORM 6000-SELECT THRU 6000-EXIT
               WHEN OTHER
                   MOVE "SRC0008" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                          " is not supported by SOURCSEL"
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
           MOVE "SOURCSEL" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE 0 TO SS-EVENT-ID SS-QUOTE-ID SS-SOURCE-ID
           MOVE 0 TO SS-LEAD-TIME-DAYS SS-MIN-ORDER-QTY SS-PACK-QTY
           MOVE 0 TO SS-LINE-VALUE-GBP SS-CHEAPEST-GBP
           MOVE 0 TO SS-SELECTED-GBP SS-PREMIUM-GBP SS-RANK
           MOVE 0 TO SS-QUOTE-CNT SS-APPROVAL-ID
           MOVE 0 TO SS-LAST-PAID-PRICE SS-LAST-PAID-DATE
           MOVE SPACES TO SS-APPROVAL-STATUS SS-SUPPLIER-PART
           MOVE SPACES TO SS-CONTRACT-REF SS-SRC-CURRENCY
           MOVE SPACES TO SS-SRC-RESTRICTION SS-CHEAPEST-SUPP
           MOVE SPACES TO SS-STATUS SS-SOURCE-TEXT
           MOVE "N" TO SS-NEEDS-AUTH SS-ON-PRICE
           IF SS-EVENT-DATE = 0
               MOVE CA-BUSINESS-DATE TO SS-EVENT-DATE
           END-IF
           IF SS-CURRENCY = SPACES
               MOVE "GBP" TO SS-CURRENCY
           END-IF
           IF SS-PRICE-PER <= 0
               MOVE 1 TO SS-PRICE-PER
           END-IF
           IF CA-COMPANY = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "SRC0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A company code is needed for source selection"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> IS THIS SUPPLIER AN APPROVED SOURCE FOR THIS ITEM, ON THIS
      *> DATE?  THE EFFECTIVE DATE MATTERS: APPROVAL EXPIRES.
      *>----------------------------------------------------------------
       2000-CHECK SECTION.
       2000-START.
           IF SS-ITEM-CODE = SPACES OR SS-SUPPLIER = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "SRC0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "An item code and a supplier code are needed "
                      "to check the approved source register"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 CAST(s.SupplierSourceId AS "
                  "varchar(12)), s.ApprovalStatus, "
                  "ISNULL(s.SupplierPartNumber, ''), "
                  "ISNULL(s.ContractRef, ''), CAST(s.LeadTimeDays AS"
                  " varchar(12)), CAST(s.MinOrderQty AS varchar(24))"
                  ", CAST(s.PackQty AS varchar(24)), s.CurrencyCode,"
                  " ISNULL(s.CommercialRestriction, ''), CASE WHEN "
                  "s.EffectiveFrom <= TRY_CONVERT(date, ?) AND "
                  "(s.EffectiveTo IS NULL OR s.EffectiveTo >= "
                  "TRY_CONVERT(date, ?)) THEN 'Y' ELSE 'N' END FROM "
                  "po.SupplierSource s WHERE s.CompanyCode = ? AND "
                  "s.ItemCode = ? AND s.SupplierCode = ? AND "
                  "s.IsActive = 1 ORDER BY s.EffectiveFrom DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SS-EVENT-DATE TO SQLIO-PARM-VAL(1)
           MOVE SS-EVENT-DATE TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE SS-ITEM-CODE TO SQLIO-PARM-VAL(4)
           MOVE SS-SUPPLIER TO SQLIO-PARM-VAL(5)
           MOVE 5 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 40 TO SQLIO-COL-WIDTH(3)
           MOVE 40 TO SQLIO-COL-WIDTH(4)
           MOVE 12 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 3 TO SQLIO-COL-WIDTH(8)
           MOVE 200 TO SQLIO-COL-WIDTH(9)
           MOVE 1 TO SQLIO-COL-WIDTH(10)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SRC-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "UNAPPROVED" TO SS-APPROVAL-STATUS
               MOVE "Y" TO SS-NEEDS-AUTH
               MOVE 16 TO CA-RETURN-CODE
               MOVE "SRC0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Supplier " FUNCTION TRIM(SS-SUPPLIER)
                      " is not an approved source for item "
                      FUNCTION TRIM(SS-ITEM-CODE)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 2500-AUTHORITY THRU 2500-EXIT
               GO TO 2000-EXIT
           END-IF
           COMPUTE SS-SOURCE-ID = FUNCTION NUMVAL(WS-S-ID)
           MOVE WS-S-STATUS TO SS-APPROVAL-STATUS
           MOVE WS-S-PART TO SS-SUPPLIER-PART
           MOVE WS-S-CONTRACT TO SS-CONTRACT-REF
           COMPUTE SS-LEAD-TIME-DAYS = FUNCTION NUMVAL(WS-S-LEAD)
           COMPUTE SS-MIN-ORDER-QTY = FUNCTION NUMVAL(WS-S-MINORD)
           COMPUTE SS-PACK-QTY = FUNCTION NUMVAL(WS-S-PACK)
           MOVE WS-S-CURRENCY TO SS-SRC-CURRENCY
           MOVE WS-S-RESTRICT TO SS-SRC-RESTRICTION
           PERFORM 2600-PRICE-HISTORY THRU 2600-EXIT
           EVALUATE TRUE
               WHEN SS-SRC-SUSPENDED
                   MOVE 8 TO CA-RETURN-CODE
                   MOVE "SRC0003" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Supplier " FUNCTION TRIM(SS-SUPPLIER)
                          " is suspended as a source for item "
                          FUNCTION TRIM(SS-ITEM-CODE)
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   GO TO 2000-EXIT
               WHEN WS-S-EFFECTIVE NOT = "Y"
                   MOVE 6 TO CA-RETURN-CODE
                   MOVE "SRC0004" TO CA-REASON-CODE
                   MOVE "Y" TO SS-NEEDS-AUTH
                   MOVE SPACES TO CA-MESSAGE
                   STRING "The approval of supplier "
                          FUNCTION TRIM(SS-SUPPLIER) " for item "
                          FUNCTION TRIM(SS-ITEM-CODE)
                          " is not effective on this date"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   PERFORM 2500-AUTHORITY THRU 2500-EXIT
                   GO TO 2000-EXIT
               WHEN SS-SRC-UNAPPROVED
                   MOVE 6 TO CA-RETURN-CODE
                   MOVE "SRC0005" TO CA-REASON-CODE
                   MOVE "Y" TO SS-NEEDS-AUTH
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Supplier " FUNCTION TRIM(SS-SUPPLIER)
                          " is an unapproved source and needs "
                          "explicit authorisation before use"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   PERFORM 2500-AUTHORITY THRU 2500-EXIT
                   GO TO 2000-EXIT
               WHEN SS-SRC-CONDITION
                   MOVE 4 TO CA-RETURN-CODE
                   MOVE "SRC0010" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Supplier " FUNCTION TRIM(SS-SUPPLIER)
                          " is a conditional source: "
                          FUNCTION TRIM(SS-SRC-RESTRICTION)
                       DELIMITED BY SIZE INTO CA-MESSAGE
               WHEN OTHER
                   MOVE 0 TO CA-RETURN-CODE
           END-EVALUATE
           IF SS-QUANTITY > 0 AND SS-MIN-ORDER-QTY > 0 AND
              SS-QUANTITY < SS-MIN-ORDER-QTY
               MOVE 4 TO CA-RETURN-CODE
               MOVE "SRC0011" TO CA-REASON-CODE
               MOVE SS-MIN-ORDER-QTY TO WS-QTY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Supplier " FUNCTION TRIM(SS-SUPPLIER)
                      " has a minimum order quantity of "
                      FUNCTION TRIM(WS-QTY-ED)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           MOVE SPACES TO SS-SOURCE-TEXT
           STRING "Approved source " FUNCTION TRIM(SS-SUPPLIER)
                  ", part " FUNCTION TRIM(SS-SUPPLIER-PART)
                  ", contract " FUNCTION TRIM(SS-CONTRACT-REF)
               DELIMITED BY SIZE INTO SS-SOURCE-TEXT.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AN UNAPPROVED SOURCE IS NOT FORBIDDEN - IT IS AUTHORISED.
      *> A NAMED PERSON TAKES THE DECISION AND IT IS RECORDED.
      *>----------------------------------------------------------------
       2500-AUTHORITY SECTION.
       2500-START.
           IF SS-AUTHORISED-BY NOT = SPACES AND
              SS-SELECT-REASON NOT = SPACES
               MOVE "N" TO SS-NEEDS-AUTH
               MOVE 4 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Use of unapproved source "
                      FUNCTION TRIM(SS-SUPPLIER)
                      " authorised by "
                      FUNCTION TRIM(SS-AUTHORISED-BY) ": "
                      FUNCTION TRIM(SS-SELECT-REASON)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       2500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> WHAT DID WE ACTUALLY PAY LAST TIME?  POPRICE OWNS THAT
      *> QUESTION, SO ASK IT RATHER THAN INVENTING A SECOND ANSWER.
      *>----------------------------------------------------------------
       2600-PRICE-HISTORY SECTION.
       2600-START.
           INITIALIZE WS-PRICE-CA
           MOVE "HISTORY" TO PC-FUNCTION
           MOVE CA-COMPANY TO PC-COMPANY
           MOVE CA-SITE TO PC-SITE
           MOVE CA-USER TO PC-USER
           MOVE SS-EVENT-DATE TO PC-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO PC-CORRELATION-ID
           MOVE SS-SUPPLIER TO PR-SUPPLIER
           MOVE SS-ITEM-CODE TO PR-ITEM-CODE
           MOVE SS-EVENT-DATE TO PR-PRICING-DATE
           MOVE SS-QUANTITY TO PR-QUANTITY
           CALL "POPRICE" USING WS-PRICE-CA
           IF PC-RETURN-CODE < 8
               MOVE PR-LAST-PAID-PRICE TO SS-LAST-PAID-PRICE
               MOVE PR-LAST-PAID-DATE TO SS-LAST-PAID-DATE
           END-IF.
       2600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3000-OPEN SECTION.
       3000-START.
           IF SS-SOURCING-REF = SPACES
               PERFORM 9100-NUMBER THRU 9100-EXIT
               IF WS-FATAL
                   GO TO 3000-EXIT
               END-IF
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.SourcingEvent (CompanyCode, "
                  "SourcingRef, RequisitionNumber, ItemCode, "
                  "Description, Quantity, UomCode, NeededByDate, "
                  "BuyerUser, EventDate, Status, CreatedBy) OUTPUT "
                  "CAST(INSERTED.SourcingEventId AS varchar(24)) "
                  "VALUES (?, ?, NULLIF(?, ''), NULLIF(?, ''), "
                  "NULLIF(?, ''), ?, NULLIF(?, ''), "
                  "TRY_CONVERT(date, NULLIF(?, '00000000')), "
                  "NULLIF(?, ''), TRY_CONVERT(date, ?), 'OPEN', ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SS-SOURCING-REF TO SQLIO-PARM-VAL(2)
           MOVE SS-REQ-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE SS-ITEM-CODE TO SQLIO-PARM-VAL(4)
           MOVE SS-DESCRIPTION TO SQLIO-PARM-VAL(5)
           MOVE SS-QUANTITY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE SS-UOM-CODE TO SQLIO-PARM-VAL(7)
           MOVE SS-NEEDED-BY TO SQLIO-PARM-VAL(8)
           MOVE SS-BUYER TO SQLIO-PARM-VAL(9)
           MOVE SS-EVENT-DATE TO SQLIO-PARM-VAL(10)
           MOVE CA-USER TO SQLIO-PARM-VAL(11)
           MOVE 11 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           COMPUTE SS-EVENT-ID = FUNCTION NUMVAL(WS-V1)
           MOVE "OPEN" TO SS-STATUS
           MOVE SPACES TO CA-MESSAGE
           STRING "Sourcing event " FUNCTION TRIM(SS-SOURCING-REF)
                  " opened for item " FUNCTION TRIM(SS-ITEM-CODE)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4000-QUOTE SECTION.
       4000-START.
           PERFORM 8000-READ-EVENT THRU 8000-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           IF SS-QUOTE-VALID-TO NOT = 0 AND
              SS-QUOTE-VALID-TO < SS-EVENT-DATE
               MOVE 4 TO CA-RETURN-CODE
               MOVE "SRC0009" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Quotation " FUNCTION TRIM(SS-QUOTE-REF)
                      " from " FUNCTION TRIM(SS-SUPPLIER)
                      " has expired and may not be relied on"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           COMPUTE WS-EFFECTIVE ROUNDED =
                   (SS-UNIT-PRICE / SS-PRICE-PER) *
                   (1 - (SS-DISCOUNT-PCT / 100))
           COMPUTE SS-LINE-VALUE-GBP ROUNDED =
                   WS-EFFECTIVE * SS-QUANTITY
           IF SS-LINE-NUM = 0
               PERFORM 4100-NEXT-LINE THRU 4100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.SourcingQuote (SourcingEventId, "
                  "LineNum, SupplierCode, ApprovalStatus, QuoteRef, "
                  "CurrencyCode, UnitPrice, PricePerQty, "
                  "DiscountPct, LineValueGBP, LeadTimeDays, "
                  "QuoteValidTo, OnTimePct, QualityPct, Restriction)"
                  " OUTPUT CAST(INSERTED.SourcingQuoteId AS "
                  "varchar(24)) VALUES (?, ?, ?, NULLIF(?, ''), "
                  "NULLIF(?, ''), ?, ?, ?, ?, ?, ?, "
                  "TRY_CONVERT(date, NULLIF(?, '00000000')), "
                  "NULLIF(TRY_CONVERT(decimal(9,4), ?), 0), "
                  "NULLIF(TRY_CONVERT(decimal(9,4), ?), 0), "
                  "NULLIF(?, ''))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SS-EVENT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE SS-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE SS-SUPPLIER TO SQLIO-PARM-VAL(3)
           MOVE SS-APPROVAL-STATUS TO SQLIO-PARM-VAL(4)
           MOVE SS-QUOTE-REF TO SQLIO-PARM-VAL(5)
           MOVE SS-CURRENCY TO SQLIO-PARM-VAL(6)
           MOVE SS-UNIT-PRICE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE SS-PRICE-PER TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE SS-DISCOUNT-PCT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE SS-LINE-VALUE-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE SS-LEAD-TIME-IN TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE SS-QUOTE-VALID-TO TO SQLIO-PARM-VAL(12)
           MOVE SS-ONTIME-PCT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE SS-QUALITY-PCT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(14)
           MOVE SS-RESTRICTION TO SQLIO-PARM-VAL(15)
           MOVE 15 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           COMPUTE SS-QUOTE-ID = FUNCTION NUMVAL(WS-V1)
           MOVE SS-LINE-VALUE-GBP TO WS-AMT-ED
           IF CA-RETURN-CODE = 0
               MOVE SPACES TO CA-MESSAGE
               STRING "Quotation from "
                      FUNCTION TRIM(SS-SUPPLIER) " at "
                      FUNCTION TRIM(WS-AMT-ED) " recorded"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4100-NEXT-LINE SECTION.
       4100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(MAX(LineNum), 0) + 1 AS "
                  "varchar(24)) FROM po.SourcingQuote WHERE "
                  "SourcingEventId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SS-EVENT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 4100-EXIT
           END-IF
           COMPUTE SS-LINE-NUM = FUNCTION NUMVAL(WS-V1).
       4100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> RANK.  THE RANK IS BY PRICE BECAUSE PRICE IS THE ONLY THING
      *> THAT CAN BE RANKED OBJECTIVELY; EVERYTHING ELSE IS SHOWN
      *> ALONGSIDE IT FOR THE BUYER TO WEIGH.
      *>----------------------------------------------------------------
       5000-RANK SECTION.
       5000-START.
           PERFORM 8000-READ-EVENT THRU 8000-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE q SET RankByPrice = r.rk FROM "
                  "po.SourcingQuote q JOIN (SELECT SourcingQuoteId, "
                  "RANK() OVER (ORDER BY LineValueGBP) AS rk FROM "
                  "po.SourcingQuote WHERE SourcingEventId = ?) r ON "
                  "r.SourcingQuoteId = q.SourcingQuoteId"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SS-EVENT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           PERFORM 5100-READ-RANK THRU 5100-EXIT.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       5100-READ-RANK SECTION.
       5100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(a.cnt AS varchar(12)), "
                  "ISNULL((SELECT TOP 1 c.SupplierCode FROM "
                  "po.SourcingQuote c WHERE c.SourcingEventId = ? "
                  "ORDER BY c.LineValueGBP, c.SupplierCode), ''), "
                  "CAST(ISNULL(a.minval, 0) AS varchar(24)), "
                  "CAST(ISNULL(a.selval, 0) AS varchar(24)), "
                  "CAST(ISNULL(a.selrank, 0) AS varchar(12)) FROM "
                  "(SELECT COUNT(*) AS cnt, MIN(q.LineValueGBP) AS "
                  "minval, MIN(CASE WHEN q.SupplierCode = ? THEN "
                  "q.LineValueGBP END) AS selval, MIN(CASE WHEN "
                  "q.SupplierCode = ? THEN q.RankByPrice END) AS "
                  "selrank FROM po.SourcingQuote q WHERE "
                  "q.SourcingEventId = ?) a"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SS-EVENT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE SS-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE SS-SUPPLIER TO SQLIO-PARM-VAL(3)
           MOVE SQLIO-PARM-VAL(1) TO SQLIO-PARM-VAL(4)
           MOVE 4 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 12 TO SQLIO-COL-WIDTH(5)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-RANK-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 5100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 5100-EXIT
           END-IF
           COMPUTE SS-QUOTE-CNT = FUNCTION NUMVAL(WS-R-CNT)
           MOVE WS-R-SUPPLIER TO SS-CHEAPEST-SUPP
           COMPUTE SS-CHEAPEST-GBP = FUNCTION NUMVAL(WS-R-VALUE)
           COMPUTE SS-SELECTED-GBP = FUNCTION NUMVAL(WS-R-SELVALUE)
           COMPUTE SS-RANK = FUNCTION NUMVAL(WS-R-RANK)
           IF SS-SELECTED-GBP > 0
               COMPUTE SS-PREMIUM-GBP =
                       SS-SELECTED-GBP - SS-CHEAPEST-GBP
           END-IF
           IF SS-QUOTE-CNT = 0
               MOVE 16 TO CA-RETURN-CODE
               MOVE "SRC0012" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No quotations have been recorded against "
                      "sourcing event "
                      FUNCTION TRIM(SS-SOURCING-REF)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       5100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> SELECT.  THE CHEAPEST IS NOT AUTOMATIC - AND NEITHER IS
      *> ANYTHING ELSE.  PAYING A PREMIUM IS ALLOWED, UNEXPLAINED
      *> IS NOT.
      *>----------------------------------------------------------------
       6000-SELECT SECTION.
       6000-START.
           PERFORM 8000-READ-EVENT THRU 8000-EXIT
           IF WS-FATAL
               GO TO 6000-EXIT
           END-IF
           IF SS-SUPPLIER = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "SRC0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A supplier must be named to select a source"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 6000-EXIT
           END-IF
           PERFORM 5100-READ-RANK THRU 5100-EXIT
           IF WS-FATAL OR CA-RETURN-CODE = 16
               GO TO 6000-EXIT
           END-IF
           IF SS-SUPPLIER = SS-CHEAPEST-SUPP
               MOVE "Y" TO SS-ON-PRICE
           END-IF
           IF SS-PREMIUM-GBP > 0 AND SS-SELECT-REASON = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "SRC0007" TO CA-REASON-CODE
               MOVE SS-PREMIUM-GBP TO WS-AMT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Selecting " FUNCTION TRIM(SS-SUPPLIER)
                      " costs " FUNCTION TRIM(WS-AMT-ED)
                      " more than the cheapest quotation from "
                      FUNCTION TRIM(SS-CHEAPEST-SUPP)
                      " and needs a reason"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 6000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.SourcingEvent SET SelectedSupplierCode "
                  "= ?, SelectionReason = NULLIF(?, ''), "
                  "SelectedOnPrice = ?, UnapprovedSourceUsed = ?, "
                  "AuthorisedBy = NULLIF(?, ''), Status = 'SELECTED'"
                  ", ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE SourcingEventId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SS-SUPPLIER TO SQLIO-PARM-VAL(1)
           MOVE SS-SELECT-REASON TO SQLIO-PARM-VAL(2)
           IF SS-WON-ON-PRICE
               MOVE "1" TO SQLIO-PARM-VAL(3)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(3)
           END-IF
           IF SS-AUTHORISED-BY NOT = SPACES
               MOVE "1" TO SQLIO-PARM-VAL(4)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(4)
           END-IF
           MOVE SS-AUTHORISED-BY TO SQLIO-PARM-VAL(5)
           MOVE CA-USER TO SQLIO-PARM-VAL(6)
           MOVE SS-EVENT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE 7 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 6000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.SourcingQuote SET IsSelected = CASE "
                  "WHEN SupplierCode = ? THEN 1 ELSE 0 END WHERE "
                  "SourcingEventId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SS-SUPPLIER TO SQLIO-PARM-VAL(1)
           MOVE SS-EVENT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 6000-EXIT
           END-IF
           MOVE "SELECTED" TO SS-STATUS
           MOVE SS-SELECTED-GBP TO WS-AMT-ED
           MOVE SPACES TO CA-MESSAGE
           IF SS-WON-ON-PRICE
               STRING "Supplier " FUNCTION TRIM(SS-SUPPLIER)
                      " selected at " FUNCTION TRIM(WS-AMT-ED)
                      " - the cheapest of "
                      FUNCTION TRIM(SS-SOURCING-REF)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE 4 TO CA-RETURN-CODE
               STRING "Supplier " FUNCTION TRIM(SS-SUPPLIER)
                      " selected at " FUNCTION TRIM(WS-AMT-ED)
                      " over the cheapest quotation: "
                      FUNCTION TRIM(SS-SELECT-REASON)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       6000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8000-READ-EVENT SECTION.
       8000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(e.SourcingEventId AS varchar(12)), "
                  "e.Status, CAST(e.Quantity AS varchar(24)) FROM "
                  "po.SourcingEvent e WHERE e.CompanyCode = ? AND "
                  "e.SourcingRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SS-SOURCING-REF TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-EVT-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 8000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "SRC0006" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Sourcing event "
                      FUNCTION TRIM(SS-SOURCING-REF)
                      " is not on file"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 8000-EXIT
           END-IF
           COMPUTE SS-EVENT-ID = FUNCTION NUMVAL(WS-E-ID)
           MOVE WS-E-STATUS TO SS-STATUS
           IF SS-QUANTITY = 0
               COMPUTE SS-QUANTITY = FUNCTION NUMVAL(WS-E-QTY)
           END-IF.
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
      *>----------------------------------------------------------------
       9100-NUMBER SECTION.
       9100-START.
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO NM-FUNCTION
           MOVE CA-COMPANY TO NM-COMPANY
           MOVE CA-SITE TO NM-SITE
           MOVE CA-USER TO NM-USER
           MOVE SS-EVENT-DATE TO NM-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO NM-CORRELATION-ID
           MOVE "POSRC" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF NM-RETURN-CODE > 4
               MOVE 12 TO CA-RETURN-CODE
               MOVE "SRC0006" TO CA-REASON-CODE
               MOVE NM-MESSAGE TO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 9100-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO SS-SOURCING-REF.
       9100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9300-NO-NULLS SECTION.
       9300-START.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM.
       9300-EXIT.
           EXIT.
       END PROGRAM SOURCSEL.
