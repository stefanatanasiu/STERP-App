      *>================================================================
      *> PROGRAM-ID : ITEMVAL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *> PURPOSE    : ITEM IDENTITY, FULL CODE AND ALIAS SEARCH, THE
      *>              CLASSIFICATION RULES THAT DECIDE WHAT AN ITEM MAY
      *>              TAKE PART IN, AND THE ACTIVATION REVIEW THAT
      *>              DECIDES WHETHER A DRAFT MAY BE RELEASED.  NOTHING
      *>              IS IDENTIFIED BY TRUNCATION AND NOTHING IS
      *>              RELEASED UNTIL THE INFORMATION ITS ENABLED
      *>              PROCESSES NEED IS PRESENT.
      *> CALLED BY  : ITEM MASTER MAINTENANCE, STKMOVE, ITEMCOPY,
      *>              ITMIMPCT, THE .NET CLIENT
      *> INTERFACE  : CALL "ITEMVAL" USING WS-ITEMVAL-CA
      *>              COPYBOOK ITEMVALCA
      *> CALLS      : SQLIO (inv.Item, inv.ItemRevision, inv.ItemUom,
      *>              inv.ItemVatClass, inv.ItemCustoms, inv.ItemPartner
      *>              inv.vw_ItemAlias), AUDITLOG
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-02-11 RJH  CR0412 ORIGINAL - ITEM MASTER VALIDATION
      *> 1996-05-30 RJH  CR0427 CLASSIFICATION TABLE 2100-CLASS-RULES
      *>                 AFTER A PHANTOM TOOK ORDINARY STOCK AT BHM
      *> 1998-09-14 RJH  CR0905 SUPPLIER ALIAS SEARCH
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS ON EFFECTIVITY
      *> 2021-01-01 ANW  CR4410 BREXIT - COMMODITY CODE AND ORIGIN
      *> 2026-10-05 STERP ITM-001 TO ITM-004 ACTIVATION REVIEW
      *>================================================================
      *> REQ: ITM-001, ITM-002, ITM-003, ITM-004
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ITEMVAL.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 11/02/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ITMWORK.
       01  WS-CONTROL.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-ITEM-READ            PIC X VALUE "N".
           05  WS-CREATED-BY           PIC X(20) VALUE SPACES.
           05  WS-INV-ACCT-GRP         PIC X(20) VALUE SPACES.
           05  WS-REV-ACCT-GRP         PIC X(20) VALUE SPACES.
           05  WS-MIN-SHELF            PIC S9(5) VALUE 0.
           05  WS-UKCA                 PIC X VALUE "N".
           05  WS-CONFORM-REF          PIC X(40) VALUE SPACES.
           05  WS-DG-UN                PIC X(8) VALUE SPACES.
           05  WS-DG-CLASS             PIC X(8) VALUE SPACES.
           05  WS-DG-PACK              PIC X(4) VALUE SPACES.
           05  WS-WEEE                 PIC X(20) VALUE SPACES.
           05  WS-BATTERY              PIC X(20) VALUE SPACES.
           05  WS-NET-WEIGHT           PIC S9(9)V9(6) VALUE 0.
           05  WS-COUNT                PIC S9(9) VALUE 0.
           05  WS-LAST-ITEM            PIC X(40) VALUE SPACES.
           05  WS-DISTINCT             PIC S9(4) VALUE 0.
           05  WS-SEVERITY             PIC X(8) VALUE SPACES.
           05  WS-REQ-ID               PIC X(8) VALUE SPACES.
           05  WS-FIELD                PIC X(24) VALUE SPACES.
       LINKAGE SECTION.
       COPY ITEMVALCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "FETCH"    PERFORM 1000-FETCH
               WHEN "SEARCH"   PERFORM 2000-SEARCH
               WHEN "RESOLVE"  PERFORM 2500-RESOLVE
               WHEN "CLASSCHK" PERFORM 3000-CLASS-CHECK
               WHEN "VALIDATE" PERFORM 4000-ACTIVATION-REVIEW
               WHEN "ACTIVATE" PERFORM 5000-ACTIVATE
               WHEN "REVISION" PERFORM 6000-REVISION
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-ITEM-READ
           MOVE "ITEMVAL" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "ITM-003" TO CA-REQ-REF
           MOVE 0 TO IV-FAULT-CNT IV-MATCH-CNT
           MOVE "N" TO IV-ALLOWED IV-READY
           MOVE FUNCTION UPPER-CASE(IV-ITEM-CODE) TO IV-ITEM-CODE
           MOVE FUNCTION UPPER-CASE(IV-ACTION) TO IV-ACTION
           MOVE FUNCTION UPPER-CASE(IV-PARTNER-TYPE) TO IV-PARTNER-TYPE
           IF CA-BUSINESS-DATE = 0
               MOVE 20261005 TO CA-BUSINESS-DATE
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES OR CA-COMPANY = LOW-VALUES
               MOVE "ITM0001" TO CA-REASON-CODE
               MOVE "A company must be supplied" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION = "SEARCH" OR CA-FUNCTION = "RESOLVE"
               IF IV-SEARCH-TEXT = SPACES
                   MOVE "ITM0009" TO CA-REASON-CODE
                   MOVE "Enter a code, a description or a partner "
                       TO CA-MESSAGE
                   PERFORM 9000-REJECT
               END-IF
               GO TO 0200-EXIT
           END-IF
           IF IV-ITEM-CODE = SPACES OR IV-ITEM-CODE = LOW-VALUES
               MOVE "ITM0001" TO CA-REASON-CODE
               MOVE "An item code is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0300  READ THE GOVERNED IDENTITY.  THE FULL CODE IS MATCHED
      *>       EXACTLY: TWO LONG CODES THAT DIFFER ONLY NEAR THEIR END
      *>       ARE TWO ITEMS, NOT ONE.
      *>----------------------------------------------------------------
       0300-READ-ITEM SECTION.
       0300-START.
           IF WS-ITEM-READ = "Y"
               GO TO 0300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Description, ItemClass, ISNULL(Category,'')"
                  ", ISNULL(StewardUser,''), LifecycleStatus, "
                  "StockUomCode, TraceabilityType, "
                  "ISNULL(ShelfLifeDays,0), QuantumQty, "
                  "IsIndivisible, MakeBuy, ValuationGroup, "
                  "CostMethod, CurrentRevision, IsStocked, "
                  "IsPurchasable, IsSaleable, IsManufacturable, "
                  "IsConsumable, RequiresInspection, "
                  "IsDrcSpecifiedGoods, ISNULL(SuccessorItemCode,''),"
                  " ISNULL(PredecessorItemCode,''), "
                  "ISNULL(CreatedBy,'') FROM inv.Item WHERE "
                  "CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE IV-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 24 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               PERFORM 9200-NOT-FOUND
               GO TO 0300-EXIT
           END-IF
           MOVE ITM-COL(1) TO IV-DESCRIPTION
           MOVE ITM-COL(2) TO IV-ITEM-CLASS
           MOVE ITM-COL(3) TO IV-CATEGORY
           MOVE ITM-COL(4) TO IV-STEWARD
           MOVE ITM-COL(5) TO IV-LIFECYCLE
           MOVE ITM-COL(6) TO IV-STOCK-UOM
           MOVE ITM-COL(7) TO IV-TRACEABILITY
           COMPUTE IV-SHELF-LIFE-DAYS = FUNCTION NUMVAL(ITM-COL(8))
           COMPUTE IV-QUANTUM = FUNCTION NUMVAL(ITM-COL(9))
           MOVE ITM-COL(10)(1:1) TO WS-FLAG
           PERFORM 0390-YES-NO
           MOVE WS-FLAG TO IV-INDIVISIBLE
           MOVE ITM-COL(11) TO IV-MAKE-BUY
           MOVE ITM-COL(12) TO IV-VALUATION-GRP
           MOVE ITM-COL(13) TO IV-COST-METHOD
           MOVE ITM-COL(14) TO IV-CURRENT-REV
           MOVE ITM-COL(15)(1:1) TO WS-FLAG
           PERFORM 0390-YES-NO
           MOVE WS-FLAG TO IV-IS-STOCKED
           MOVE ITM-COL(16)(1:1) TO WS-FLAG
           PERFORM 0390-YES-NO
           MOVE WS-FLAG TO IV-IS-PURCHASABLE
           MOVE ITM-COL(17)(1:1) TO WS-FLAG
           PERFORM 0390-YES-NO
           MOVE WS-FLAG TO IV-IS-SALEABLE
           MOVE ITM-COL(18)(1:1) TO WS-FLAG
           PERFORM 0390-YES-NO
           MOVE WS-FLAG TO IV-IS-MANUFACTURED
           MOVE ITM-COL(19)(1:1) TO WS-FLAG
           PERFORM 0390-YES-NO
           MOVE WS-FLAG TO IV-IS-CONSUMABLE
           MOVE ITM-COL(20)(1:1) TO WS-FLAG
           PERFORM 0390-YES-NO
           MOVE WS-FLAG TO IV-REQ-INSPECTION
           MOVE ITM-COL(21)(1:1) TO WS-FLAG
           PERFORM 0390-YES-NO
           MOVE WS-FLAG TO IV-DRC-GOODS
           MOVE ITM-COL(22) TO IV-SUCCESSOR
           MOVE ITM-COL(23) TO IV-PREDECESSOR
           MOVE ITM-COL(24) TO WS-CREATED-BY
           MOVE "Y" TO WS-ITEM-READ.
       0300-EXIT.
           EXIT.

       0390-YES-NO SECTION.
       0390-START.
           IF WS-FLAG = "1"
               MOVE "Y" TO WS-FLAG
           ELSE
               MOVE "N" TO WS-FLAG
           END-IF.
       0390-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0350  THE FINANCE AND COMPLIANCE COLUMNS, READ ONLY WHEN THE
      *>       ACTIVATION REVIEW NEEDS THEM.
      *>----------------------------------------------------------------
       0350-READ-COMPLIANCE SECTION.
       0350-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(InventoryAccountGroup,''), "
                  "ISNULL(RevenueAccountGroup,''), "
                  "ISNULL(MinRemainingShelfLifeDays,0), UkcaMarked, "
                  "ISNULL(ConformityDocRef,''), "
                  "ISNULL(DangerousGoodsUn,''), "
                  "ISNULL(DangerousGoodsClass,''), "
                  "ISNULL(DangerousGoodsPackGroup,''), "
                  "ISNULL(WeeeCategory,''), ISNULL(BatteryCategory,"
                  "''), ISNULL(NetWeightKg,0) FROM inv.Item WHERE "
                  "CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE IV-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 11 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0350-EXIT
           END-IF
           MOVE ITM-COL(1) TO WS-INV-ACCT-GRP
           MOVE ITM-COL(2) TO WS-REV-ACCT-GRP
           COMPUTE WS-MIN-SHELF = FUNCTION NUMVAL(ITM-COL(3))
           MOVE ITM-COL(4)(1:1) TO WS-FLAG
           PERFORM 0390-YES-NO
           MOVE WS-FLAG TO WS-UKCA
           MOVE ITM-COL(5) TO WS-CONFORM-REF
           MOVE ITM-COL(6) TO WS-DG-UN
           MOVE ITM-COL(7) TO WS-DG-CLASS
           MOVE ITM-COL(8) TO WS-DG-PACK
           MOVE ITM-COL(9) TO WS-WEEE
           MOVE ITM-COL(10) TO WS-BATTERY
           COMPUTE WS-NET-WEIGHT = FUNCTION NUMVAL(ITM-COL(11)).
       0350-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-FETCH SECTION.
       1000-START.
           PERFORM 0300-READ-ITEM
           IF CA-FAILED
               GO TO 1000-EXIT
           END-IF
           IF IV-REVISION = SPACES
               MOVE IV-CURRENT-REV TO IV-REVISION
           END-IF
           PERFORM 6000-REVISION
           IF CA-NOT-FOUND
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           END-IF
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(IV-ITEM-CODE) " - "
                  FUNCTION TRIM(IV-DESCRIPTION) " ("
                  FUNCTION TRIM(IV-LIFECYCLE) ")"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  FULL CODE / DESCRIPTION / ALIAS SEARCH (ITM-001).
      *>----------------------------------------------------------------
       2000-SEARCH SECTION.
       2000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ItemCode, Description, MatchSource, "
                  "LifecycleStatus, Alias, PartnerCode FROM "
                  "inv.vw_ItemAlias WHERE CompanyCode = ? AND "
                  "(ItemCode LIKE ? OR Description LIKE ? OR "
                  "Alias LIKE ?) AND (? = '' OR PartnerType = ? OR "
                  "MatchSource = 'ITEM') ORDER BY MatchSource, "
                  "ItemCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SPACES TO WS-TEXT
           STRING "%" FUNCTION TRIM(IV-SEARCH-TEXT) "%"
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE WS-TEXT TO SQLIO-PARM-VAL(2)
           MOVE WS-TEXT TO SQLIO-PARM-VAL(3)
           MOVE WS-TEXT TO SQLIO-PARM-VAL(4)
           MOVE IV-PARTNER-TYPE TO SQLIO-PARM-VAL(5)
           MOVE IV-PARTNER-TYPE TO SQLIO-PARM-VAL(6)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2000-EXIT
           END-IF
           MOVE 0 TO IV-MATCH-CNT WS-COUNT WS-DISTINCT
           MOVE SPACES TO WS-LAST-ITEM
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA ITM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   ADD 1 TO WS-COUNT
                   IF ITM-COL(1) NOT = WS-LAST-ITEM
                       ADD 1 TO WS-DISTINCT
                       MOVE ITM-COL(1) TO WS-LAST-ITEM
                   END-IF
                   IF IV-MATCH-CNT < 15
                       ADD 1 TO IV-MATCH-CNT
                       MOVE ITM-COL(1) TO IV-M-ITEM(IV-MATCH-CNT)
                       MOVE ITM-COL(2) TO IV-M-DESC(IV-MATCH-CNT)
                       MOVE ITM-COL(3) TO IV-M-SOURCE(IV-MATCH-CNT)
                       MOVE ITM-COL(4) TO IV-M-LIFECYCLE(IV-MATCH-CNT)
                       MOVE ITM-COL(5) TO IV-M-ALIAS(IV-MATCH-CNT)
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF IV-MATCH-CNT = 0
               MOVE 16 TO CA-RETURN-CODE
               MOVE "ITM0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Nothing matches "
                      FUNCTION TRIM(IV-SEARCH-TEXT)
                      " in company " FUNCTION TRIM(CA-COMPANY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF
           MOVE WS-COUNT TO WS-INT-ED
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(WS-INT-ED) " match(es) for "
                  FUNCTION TRIM(IV-SEARCH-TEXT)
               DELIMITED BY SIZE INTO CA-MESSAGE
           IF WS-COUNT > 15
               MOVE 4 TO CA-RETURN-CODE
               MOVE "ITM0002" TO CA-REASON-CODE
           END-IF.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2500  RESOLVE EXACTLY ONE ITEM FROM AN ALIAS.  THE SAME
      *>       SUPPLIER ALIAS MAY NOT SELECT TWO ACTIVE INTERNAL ITEMS
      *>       IN THE SAME CONTEXT (ITM-009).
      *>----------------------------------------------------------------
       2500-RESOLVE SECTION.
       2500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ItemCode, Description, MatchSource, "
                  "LifecycleStatus, Alias, PartnerCode FROM "
                  "inv.vw_ItemAlias WHERE CompanyCode = ? AND "
                  "Alias = ? AND (? = '' OR PartnerCode = ?) AND "
                  "(? = '' OR PartnerType = ? OR MatchSource = "
                  "'ITEM') AND LifecycleStatus NOT IN ('RETIRED') "
                  "AND (EffectiveTo IS NULL OR EffectiveTo >= "
                  "CONVERT(date, ?, 112)) ORDER BY ItemCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE FUNCTION UPPER-CASE(IV-SEARCH-TEXT)
               TO SQLIO-PARM-VAL(2)
           MOVE IV-PARTNER-CODE TO SQLIO-PARM-VAL(3)
           MOVE IV-PARTNER-CODE TO SQLIO-PARM-VAL(4)
           MOVE IV-PARTNER-TYPE TO SQLIO-PARM-VAL(5)
           MOVE IV-PARTNER-TYPE TO SQLIO-PARM-VAL(6)
           MOVE CA-BUSINESS-DATE TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(7)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 7
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2500-EXIT
           END-IF
           MOVE 0 TO IV-MATCH-CNT WS-DISTINCT
           MOVE SPACES TO WS-LAST-ITEM
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA ITM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   IF ITM-COL(1) NOT = WS-LAST-ITEM
                       ADD 1 TO WS-DISTINCT
                       MOVE ITM-COL(1) TO WS-LAST-ITEM
                       IF IV-MATCH-CNT < 15
                           ADD 1 TO IV-MATCH-CNT
                           MOVE ITM-COL(1)
                               TO IV-M-ITEM(IV-MATCH-CNT)
                           MOVE ITM-COL(2)
                               TO IV-M-DESC(IV-MATCH-CNT)
                           MOVE ITM-COL(3)
                               TO IV-M-SOURCE(IV-MATCH-CNT)
                           MOVE ITM-COL(4)
                               TO IV-M-LIFECYCLE(IV-MATCH-CNT)
                           MOVE ITM-COL(5)
                               TO IV-M-ALIAS(IV-MATCH-CNT)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF WS-DISTINCT = 0
               PERFORM 9200-NOT-FOUND
               GO TO 2500-EXIT
           END-IF
           IF WS-DISTINCT > 1
               MOVE "ITM0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Reference " FUNCTION TRIM(IV-SEARCH-TEXT)
                      " selects more than one active item; it must "
                      "be made unique before it can be used"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2500-EXIT
           END-IF
           MOVE IV-M-ITEM(1) TO IV-ITEM-CODE
           MOVE "N" TO WS-ITEM-READ
           PERFORM 0300-READ-ITEM.
       2500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  CLASSIFICATION CONTROLS BEHAVIOUR (ITM-002).  CR0427:
      *>       A NON STOCK SERVICE OR A PHANTOM MAY NOT ACQUIRE
      *>       ORDINARY PHYSICAL STOCK, WHATEVER THE SCREEN SAYS.
      *>----------------------------------------------------------------
       3000-CLASS-CHECK SECTION.
       3000-START.
           PERFORM 0300-READ-ITEM
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           MOVE "N" TO IV-ALLOWED
           EVALUATE IV-ACTION
               WHEN "STOCK"    PERFORM 3100-MAY-STOCK
               WHEN "SELL"     PERFORM 3200-MAY-SELL
               WHEN "BUY"      PERFORM 3300-MAY-BUY
               WHEN "MAKE"     PERFORM 3400-MAY-MAKE
               WHEN "BOM"      PERFORM 3500-MAY-BOM
               WHEN "KIT"      PERFORM 3600-MAY-KIT
               WHEN "COST"     MOVE "Y" TO IV-ALLOWED
               WHEN OTHER
                   MOVE "ITM0008" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Action " FUNCTION TRIM(IV-ACTION)
                          " is not a classification rule"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   PERFORM 9000-REJECT
                   GO TO 3000-EXIT
           END-EVALUATE
           IF IV-ALLOWED = "Y"
               MOVE SPACES TO CA-MESSAGE
               STRING "Class " FUNCTION TRIM(IV-ITEM-CLASS)
                      " permits " FUNCTION TRIM(IV-ACTION)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE "ITM0003" TO CA-REASON-CODE
               IF CA-MESSAGE = SPACES
                   STRING "An item of class "
                          FUNCTION TRIM(IV-ITEM-CLASS)
                          " may not take part in "
                          FUNCTION TRIM(IV-ACTION)
                       DELIMITED BY SIZE INTO CA-MESSAGE
               END-IF
               PERFORM 9000-REJECT
           END-IF.
       3000-EXIT.
           EXIT.

       3100-MAY-STOCK SECTION.
       3100-START.
           EVALUATE IV-ITEM-CLASS
               WHEN "BOUGHT"    MOVE "Y" TO IV-ALLOWED
               WHEN "MADE"      MOVE "Y" TO IV-ALLOWED
               WHEN "PACKAGING" MOVE "Y" TO IV-ALLOWED
               WHEN "KIT"       MOVE "Y" TO IV-ALLOWED
               WHEN OTHER       MOVE "N" TO IV-ALLOWED
           END-EVALUATE
           IF IV-ALLOWED = "Y" AND IV-IS-STOCKED NOT = "Y"
               MOVE "N" TO IV-ALLOWED
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(IV-ITEM-CODE)
                      " is not flagged as stocked, so it cannot "
                      "hold a physical balance"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           IF IV-ALLOWED NOT = "Y" AND CA-MESSAGE = SPACES
               STRING "An item of class "
                      FUNCTION TRIM(IV-ITEM-CLASS)
                      " never holds ordinary physical stock"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       3100-EXIT.
           EXIT.

       3200-MAY-SELL SECTION.
       3200-START.
           EVALUATE IV-ITEM-CLASS
               WHEN "COSTONLY"  MOVE "N" TO IV-ALLOWED
               WHEN "PHANTOM"   MOVE "N" TO IV-ALLOWED
               WHEN "PACKAGING" MOVE "N" TO IV-ALLOWED
               WHEN OTHER       MOVE IV-IS-SALEABLE TO IV-ALLOWED
           END-EVALUATE.
       3200-EXIT.
           EXIT.

       3300-MAY-BUY SECTION.
       3300-START.
           EVALUATE IV-ITEM-CLASS
               WHEN "COSTONLY"  MOVE "N" TO IV-ALLOWED
               WHEN "PHANTOM"   MOVE "N" TO IV-ALLOWED
               WHEN "KIT"       MOVE "N" TO IV-ALLOWED
               WHEN OTHER       MOVE IV-IS-PURCHASABLE TO IV-ALLOWED
           END-EVALUATE
           IF IV-ALLOWED = "Y" AND IV-MAKE-BUY = "MAKE"
               MOVE "N" TO IV-ALLOWED
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(IV-ITEM-CODE)
                      " is a make item; change the supply authority "
                      "to BUY or DUAL before purchasing it"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       3300-EXIT.
           EXIT.

       3400-MAY-MAKE SECTION.
       3400-START.
           EVALUATE IV-ITEM-CLASS
               WHEN "MADE"   MOVE IV-IS-MANUFACTURED TO IV-ALLOWED
               WHEN "KIT"    MOVE IV-IS-MANUFACTURED TO IV-ALLOWED
               WHEN OTHER    MOVE "N" TO IV-ALLOWED
           END-EVALUATE.
       3400-EXIT.
           EXIT.

       3500-MAY-BOM SECTION.
       3500-START.
           IF IV-ITEM-CLASS = "SERVICE"
               MOVE "N" TO IV-ALLOWED
           ELSE
               MOVE "Y" TO IV-ALLOWED
           END-IF.
       3500-EXIT.
           EXIT.

       3600-MAY-KIT SECTION.
       3600-START.
           EVALUATE IV-ITEM-CLASS
               WHEN "SERVICE"  MOVE "N" TO IV-ALLOWED
               WHEN "PHANTOM"  MOVE "N" TO IV-ALLOWED
               WHEN "COSTONLY" MOVE "N" TO IV-ALLOWED
               WHEN OTHER      MOVE "Y" TO IV-ALLOWED
           END-EVALUATE.
       3600-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  ACTIVATION REVIEW (ITM-003).  ONLY THE ATTRIBUTES THAT
      *>       THE ITEM'S ENABLED PROCESSES NEED ARE MANDATORY - WEIGHT
      *>       IS NOT AN ARBITRARY PREREQUISITE FOR EVERY SERVICE.
      *>----------------------------------------------------------------
       4000-ACTIVATION-REVIEW SECTION.
       4000-START.
           PERFORM 0300-READ-ITEM
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           PERFORM 0350-READ-COMPLIANCE
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           MOVE 0 TO IV-FAULT-CNT
           PERFORM 4100-CHECK-IDENTITY
           PERFORM 4200-CHECK-UNITS
           PERFORM 4300-CHECK-FINANCE
           PERFORM 4400-CHECK-TRACE
           PERFORM 4500-CHECK-VAT
           PERFORM 4600-CHECK-CUSTOMS
           PERFORM 4700-CHECK-COMPLIANCE
           PERFORM 4800-CHECK-SOURCES
           MOVE "Y" TO IV-READY
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > IV-FAULT-CNT
               IF IV-F-SEVERITY(WS-IX) = "ERROR"
                   MOVE "N" TO IV-READY
               END-IF
           END-PERFORM
           IF IV-READY = "Y"
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(IV-ITEM-CODE)
                      " has the information its enabled processes "
                      "need"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               IF IV-FAULT-CNT > 0
                   MOVE 4 TO CA-RETURN-CODE
                   MOVE "ITM0004" TO CA-REASON-CODE
               END-IF
           ELSE
               MOVE IV-FAULT-CNT TO WS-INT-ED
               MOVE "ITM0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Activation review found "
                      FUNCTION TRIM(WS-INT-ED)
                      " item(s) to resolve before release"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       4000-EXIT.
           EXIT.

       4050-ADD-FAULT SECTION.
       4050-START.
           IF IV-FAULT-CNT < 24
               ADD 1 TO IV-FAULT-CNT
               MOVE WS-REQ-ID TO IV-F-REQ(IV-FAULT-CNT)
               MOVE WS-SEVERITY TO IV-F-SEVERITY(IV-FAULT-CNT)
               MOVE WS-FIELD TO IV-F-FIELD(IV-FAULT-CNT)
               MOVE WS-TEXT TO IV-F-TEXT(IV-FAULT-CNT)
           END-IF
           MOVE SPACES TO WS-TEXT WS-FIELD.
       4050-EXIT.
           EXIT.

       4100-CHECK-IDENTITY SECTION.
       4100-START.
           MOVE "ITM-004" TO WS-REQ-ID
           IF IV-DESCRIPTION = SPACES
               MOVE "ERROR" TO WS-SEVERITY
               MOVE "Description" TO WS-FIELD
               MOVE "An item must carry a description before it is "
                   TO WS-TEXT
               MOVE SPACES TO WS-TEXT2
               STRING FUNCTION TRIM(WS-TEXT) "released"
                   DELIMITED BY SIZE INTO WS-TEXT2
               MOVE WS-TEXT2 TO WS-TEXT
               PERFORM 4050-ADD-FAULT
           END-IF
           IF IV-STEWARD = SPACES
               MOVE "WARNING" TO WS-SEVERITY
               MOVE "StewardUser" TO WS-FIELD
               MOVE "No master data steward owns this item"
                   TO WS-TEXT
               PERFORM 4050-ADD-FAULT
           END-IF
           IF IV-CATEGORY = SPACES
               MOVE "WARNING" TO WS-SEVERITY
               MOVE "Category" TO WS-FIELD
               MOVE "No analysis category is set" TO WS-TEXT
               PERFORM 4050-ADD-FAULT
           END-IF.
       4100-EXIT.
           EXIT.

       4200-CHECK-UNITS SECTION.
       4200-START.
           MOVE "ITM-005" TO WS-REQ-ID
           MOVE "ERROR" TO WS-SEVERITY
           IF IV-STOCK-UOM = SPACES
               MOVE "StockUomCode" TO WS-FIELD
               MOVE "A stock unit is required" TO WS-TEXT
               PERFORM 4050-ADD-FAULT
           END-IF
           MOVE "ITM-006" TO WS-REQ-ID
           IF IV-QUANTUM <= 0
               MOVE "QuantumQty" TO WS-FIELD
               MOVE "The permitted quantity increment must be "
                   TO WS-TEXT
               MOVE SPACES TO WS-TEXT2
               STRING FUNCTION TRIM(WS-TEXT) "greater than zero"
                   DELIMITED BY SIZE INTO WS-TEXT2
               MOVE WS-TEXT2 TO WS-TEXT
               PERFORM 4050-ADD-FAULT
           END-IF
           IF IV-TRACEABILITY = "SERIAL" AND IV-QUANTUM NOT = 1
               MOVE "QuantumQty" TO WS-FIELD
               MOVE "A serialised item must move in whole units of 1"
                   TO WS-TEXT
               PERFORM 4050-ADD-FAULT
           END-IF
           IF IV-IS-PURCHASABLE = "Y"
               MOVE "ITM-007" TO WS-REQ-ID
               MOVE "PURCHASE" TO WS-CODE
               PERFORM 4250-COUNT-UOM
               IF WS-COUNT = 0
                   MOVE "WARNING" TO WS-SEVERITY
                   MOVE "ItemUom PURCHASE" TO WS-FIELD
                   MOVE "No purchase unit conversion is recorded; "
                       TO WS-TEXT
                   MOVE SPACES TO WS-TEXT2
                   STRING FUNCTION TRIM(WS-TEXT)
                          " the stock unit will be assumed"
                       DELIMITED BY SIZE INTO WS-TEXT2
                   MOVE WS-TEXT2 TO WS-TEXT
                   PERFORM 4050-ADD-FAULT
               END-IF
           END-IF.
       4200-EXIT.
           EXIT.

       4250-COUNT-UOM SECTION.
       4250-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT(*) FROM inv.ItemUom WHERE "
                  "CompanyCode = ? AND ItemCode = ? AND UsageType = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE IV-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE WS-CODE TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               MOVE 0 TO WS-COUNT
           ELSE
               COMPUTE WS-COUNT = FUNCTION NUMVAL(ITM-COL(1))
           END-IF.
       4250-EXIT.
           EXIT.

       4300-CHECK-FINANCE SECTION.
       4300-START.
           MOVE "ITM-010" TO WS-REQ-ID
           MOVE "ERROR" TO WS-SEVERITY
           IF IV-IS-STOCKED = "Y" AND IV-VALUATION-GRP = SPACES
               MOVE "ValuationGroup" TO WS-FIELD
               MOVE "Stocked items need a valuation group" TO WS-TEXT
               PERFORM 4050-ADD-FAULT
           END-IF
           IF IV-IS-STOCKED = "Y" AND WS-INV-ACCT-GRP = SPACES
               MOVE "InventoryAccountGroup" TO WS-FIELD
               MOVE "Stocked items need an inventory account mapping"
                   TO WS-TEXT
               PERFORM 4050-ADD-FAULT
           END-IF
           IF IV-IS-SALEABLE = "Y" AND WS-REV-ACCT-GRP = SPACES
               MOVE "WARNING" TO WS-SEVERITY
               MOVE "RevenueAccountGroup" TO WS-FIELD
               MOVE "Saleable items should map to a revenue account"
                   TO WS-TEXT
               PERFORM 4050-ADD-FAULT
           END-IF.
       4300-EXIT.
           EXIT.

       4400-CHECK-TRACE SECTION.
       4400-START.
           MOVE "ITM-011" TO WS-REQ-ID
           MOVE "ERROR" TO WS-SEVERITY
           IF IV-SHELF-LIFE-DAYS > 0
              AND WS-MIN-SHELF > IV-SHELF-LIFE-DAYS
               MOVE "MinRemainingShelfLife" TO WS-FIELD
               MOVE "The minimum remaining shelf life is longer "
                   TO WS-TEXT
               MOVE SPACES TO WS-TEXT2
               STRING FUNCTION TRIM(WS-TEXT) " than the shelf life"
                   DELIMITED BY SIZE INTO WS-TEXT2
               MOVE WS-TEXT2 TO WS-TEXT
               PERFORM 4050-ADD-FAULT
           END-IF
           IF IV-SHELF-LIFE-DAYS > 0 AND IV-TRACEABILITY = "NONE"
               MOVE "WARNING" TO WS-SEVERITY
               MOVE "TraceabilityType" TO WS-FIELD
               MOVE "A shelf life cannot be enforced without lot or "
                   TO WS-TEXT
               MOVE SPACES TO WS-TEXT2
               STRING FUNCTION TRIM(WS-TEXT) " serial control"
                   DELIMITED BY SIZE INTO WS-TEXT2
               MOVE WS-TEXT2 TO WS-TEXT
               PERFORM 4050-ADD-FAULT
           END-IF.
       4400-EXIT.
           EXIT.

       4500-CHECK-VAT SECTION.
       4500-START.
           MOVE "ITM-025" TO WS-REQ-ID
           MOVE "ERROR" TO WS-SEVERITY
           IF IV-IS-SALEABLE = "Y"
               MOVE "SALE" TO WS-CODE
               PERFORM 4550-COUNT-VAT
               IF WS-COUNT = 0
                   MOVE "ItemVatClass SALE" TO WS-FIELD
                   MOVE "A saleable item cannot be invoiced without "
                       TO WS-TEXT
                   MOVE SPACES TO WS-TEXT2
                   STRING FUNCTION TRIM(WS-TEXT)
                          " an effective UK VAT classification"
                       DELIMITED BY SIZE INTO WS-TEXT2
                   MOVE WS-TEXT2 TO WS-TEXT
                   PERFORM 4050-ADD-FAULT
               END-IF
           END-IF
           IF IV-IS-PURCHASABLE = "Y"
               MOVE "PURCH" TO WS-CODE
               PERFORM 4550-COUNT-VAT
               IF WS-COUNT = 0
                   MOVE "WARNING" TO WS-SEVERITY
                   MOVE "ItemVatClass PURCH" TO WS-FIELD
                   MOVE "No purchase VAT liability is recorded"
                       TO WS-TEXT
                   PERFORM 4050-ADD-FAULT
               END-IF
           END-IF.
       4500-EXIT.
           EXIT.

       4550-COUNT-VAT SECTION.
       4550-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT(*) FROM inv.ItemVatClass WHERE "
                  "CompanyCode = ? AND ItemCode = ? AND Direction = ?"
                  " AND EffectiveFrom <= CONVERT(date, ?, 112) AND "
                  "(EffectiveTo IS NULL OR EffectiveTo >= "
                  "CONVERT(date, ?, 112))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE IV-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE WS-CODE TO SQLIO-PARM-VAL(3)
           MOVE CA-BUSINESS-DATE TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(4)
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               MOVE 0 TO WS-COUNT
           ELSE
               COMPUTE WS-COUNT = FUNCTION NUMVAL(ITM-COL(1))
           END-IF.
       4550-EXIT.
           EXIT.

       4600-CHECK-CUSTOMS SECTION.
       4600-START.
           MOVE "ITM-026" TO WS-REQ-ID
           MOVE "WARNING" TO WS-SEVERITY
           IF IV-ITEM-CLASS = "SERVICE"
               GO TO 4600-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT(*) FROM inv.ItemCustoms WHERE "
                  "CompanyCode = ? AND ItemCode = ? AND "
                  "EffectiveFrom <= CONVERT(date, ?, 112) AND "
                  "(EffectiveTo IS NULL OR EffectiveTo >= "
                  "CONVERT(date, ?, 112))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE IV-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE CA-BUSINESS-DATE TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(3)
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 4600-EXIT
           END-IF
           COMPUTE WS-COUNT = FUNCTION NUMVAL(ITM-COL(1))
           IF WS-COUNT = 0
               MOVE "ItemCustoms" TO WS-FIELD
               MOVE "No customs data; an international movement of "
                   TO WS-TEXT
               MOVE SPACES TO WS-TEXT2
               STRING FUNCTION TRIM(WS-TEXT)
                      " this item will be blocked"
                   DELIMITED BY SIZE INTO WS-TEXT2
               MOVE WS-TEXT2 TO WS-TEXT
               PERFORM 4050-ADD-FAULT
           END-IF.
       4600-EXIT.
           EXIT.

       4700-CHECK-COMPLIANCE SECTION.
       4700-START.
           MOVE "ITM-027" TO WS-REQ-ID
           MOVE "ERROR" TO WS-SEVERITY
           IF WS-UKCA = "Y" AND WS-CONFORM-REF = SPACES
               MOVE "ConformityDocRef" TO WS-FIELD
               MOVE "A UKCA marked item needs its declaration of "
                   TO WS-TEXT
               MOVE SPACES TO WS-TEXT2
               STRING FUNCTION TRIM(WS-TEXT) " conformity reference"
                   DELIMITED BY SIZE INTO WS-TEXT2
               MOVE WS-TEXT2 TO WS-TEXT
               PERFORM 4050-ADD-FAULT
           END-IF
           IF WS-DG-UN NOT = SPACES
               IF WS-DG-CLASS = SPACES OR WS-DG-PACK = SPACES
                   MOVE "DangerousGoods" TO WS-FIELD
                   MOVE "Dangerous goods need a class and a packing "
                       TO WS-TEXT
                   MOVE SPACES TO WS-TEXT2
                   STRING FUNCTION TRIM(WS-TEXT)
                          " group as well as the UN number"
                       DELIMITED BY SIZE INTO WS-TEXT2
                   MOVE WS-TEXT2 TO WS-TEXT
                   PERFORM 4050-ADD-FAULT
               END-IF
           END-IF
           MOVE "WARNING" TO WS-SEVERITY
           IF WS-BATTERY NOT = SPACES AND WS-WEEE = SPACES
               MOVE "WeeeCategory" TO WS-FIELD
               MOVE "A battery category without a WEEE category "
                   TO WS-TEXT
               MOVE SPACES TO WS-TEXT2
               STRING FUNCTION TRIM(WS-TEXT) " looks incomplete"
                   DELIMITED BY SIZE INTO WS-TEXT2
               MOVE WS-TEXT2 TO WS-TEXT
               PERFORM 4050-ADD-FAULT
           END-IF.
       4700-EXIT.
           EXIT.

       4800-CHECK-SOURCES SECTION.
       4800-START.
           MOVE "ITM-008" TO WS-REQ-ID
           MOVE "WARNING" TO WS-SEVERITY
           IF IV-IS-PURCHASABLE NOT = "Y"
               GO TO 4800-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT(*) FROM inv.ItemPartner WHERE "
                  "CompanyCode = ? AND ItemCode = ? AND PartnerType ="
                  " 'SUPPLIER' AND IsApprovedSource = 1"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE IV-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 4800-EXIT
           END-IF
           COMPUTE WS-COUNT = FUNCTION NUMVAL(ITM-COL(1))
           IF WS-COUNT = 0
               MOVE "ItemPartner" TO WS-FIELD
               MOVE "No approved supplier source is recorded"
                   TO WS-TEXT
               PERFORM 4050-ADD-FAULT
           END-IF.
       4800-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000  RELEASE THE ITEM AFTER AN AUTHORISED REVIEW.  THE
      *>       REVIEWER MAY NOT BE THE AUTHOR OF THE DRAFT.
      *>----------------------------------------------------------------
       5000-ACTIVATE SECTION.
       5000-START.
           PERFORM 4000-ACTIVATION-REVIEW
           IF CA-FAILED
               GO TO 5000-EXIT
           END-IF
           IF IV-LIFECYCLE NOT = "DRAFT"
              AND IV-LIFECYCLE NOT = "REVIEW"
               MOVE "ITM0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "An item in state "
                      FUNCTION TRIM(IV-LIFECYCLE)
                      " is not waiting to be released"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 5000-EXIT
           END-IF
           IF FUNCTION UPPER-CASE(CA-USER) =
              FUNCTION UPPER-CASE(WS-CREATED-BY)
               MOVE "ITM0006" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A draft must be reviewed by somebody other "
                      "than its author"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 6 TO CA-RETURN-CODE
               MOVE "Y" TO WS-ERROR
               ADD 1 TO CA-MSG-COUNT
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE inv.Item SET LifecycleStatus = 'RELEASED',"
                  " ReleasedBy = ?, ReleasedAt = SYSDATETIME(), "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() WHERE "
                  "CompanyCode = ? AND ItemCode = ? AND "
                  "LifecycleStatus IN ('DRAFT','REVIEW')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE IV-ITEM-CODE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 5000-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           MOVE "RELEASED" TO IV-LIFECYCLE
           MOVE "Y" TO IV-READY
           MOVE SPACES TO CA-MESSAGE
           STRING "Item " FUNCTION TRIM(IV-ITEM-CODE)
                  " released by " FUNCTION TRIM(CA-USER)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       5000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       6000-REVISION SECTION.
       6000-START.
           IF IV-REVISION = SPACES
               PERFORM 0300-READ-ITEM
               MOVE IV-CURRENT-REV TO IV-REVISION
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status, CONVERT(char(8), EffectiveFrom, "
                  "112), ISNULL(CONVERT(char(8), EffectiveTo, 112),"
                  "''), IsInterchangeable, ISNULL(Description,''), "
                  "ISNULL(ChangeReason,'') FROM inv.ItemRevision "
                  "WHERE CompanyCode = ? AND ItemCode = ? AND "
                  "Revision = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE IV-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE IV-REVISION TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 6000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "ITM0007" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Revision " FUNCTION TRIM(IV-REVISION)
                      " of item " FUNCTION TRIM(IV-ITEM-CODE)
                      " is not recorded"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 6000-EXIT
           END-IF
           MOVE ITM-COL(1) TO IV-REV-STATUS
           COMPUTE IV-REV-FROM = FUNCTION NUMVAL(ITM-COL(2))
           IF ITM-COL(3) = SPACES
               MOVE 0 TO IV-REV-TO
           ELSE
               COMPUTE IV-REV-TO = FUNCTION NUMVAL(ITM-COL(3))
           END-IF
           MOVE ITM-COL(4)(1:1) TO WS-FLAG
           PERFORM 0390-YES-NO
           MOVE WS-FLAG TO IV-REV-INTERCHANGE.
       6000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "ITM0008" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "ITEMVAL does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

       9200-NOT-FOUND SECTION.
       9200-START.
           MOVE "ITM0001" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Item " FUNCTION TRIM(IV-ITEM-CODE)
                  " is not held in company "
                  FUNCTION TRIM(CA-COMPANY)
               DELIMITED BY SIZE INTO CA-MESSAGE
           MOVE 16 TO CA-RETURN-CODE
           MOVE "Y" TO WS-ERROR
           ADD 1 TO CA-MSG-COUNT.
       9200-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE "Y" TO WS-ERROR WS-SQL-FAILED
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in ITEMVAL" TO CA-MESSAGE
           END-IF
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE 12 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
