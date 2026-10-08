      *>================================================================
      *> PROGRAM-ID : ITEMENQ
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE
      *> PURPOSE    : SUPPLY RULES, PARTNER REFERENCES, WHERE USED AND
      *>              USAGE ENQUIRY.  PROCUREMENT ROUNDING TO A MINIMUM
      *>              ORDER AND A PURCHASE MULTIPLE IS REPORTED AS
      *>              EXCESS SUPPLY, SEPARATELY FROM THE ENGINEERING
      *>              USAGE THAT CREATED THE DEMAND.  RESTRICTED COST
      *>              FIGURES ARE LABELLED AS WITHHELD RATHER THAN
      *>              SHOWN AS AN INVENTED ZERO.
      *> CALLED BY  : THE ITEM ENQUIRY SCREEN, PLANNING, PURCHASING,
      *>              THE .NET CLIENT
      *> INTERFACE  : CALL "ITEMENQ" USING WS-ITEMENQ-CA
      *>              COPYBOOK ITEMENQCA
      *> CALLS      : SQLIO (inv.Item, inv.ItemSite, inv.ItemPartner,
      *>              inv.usp_ItemWhereUsed, inv.vw_StockAvailability,
      *>              cst.ItemCost, core.usp_CheckPermission)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-08-19 DJM  CR0450 ORIGINAL - WHERE USED ENQUIRY
      *> 1999-04-07 RJH  CR0961 PURCHASE MULTIPLE ROUNDING SHOWN AS
      *>                 EXCESS, NOT FOLDED INTO THE REQUIREMENT
      *> 2015-09-22 ANW  CR3005 COST VISIBILITY BY ROLE
      *> 2026-10-05 STERP ITM-008, ITM-009, ITM-023
      *>================================================================
      *> REQ: ITM-008, ITM-009, ITM-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ITEMENQ.
       AUTHOR. D J MARSH.
       DATE-WRITTEN. 19/08/1996.
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
           05  WS-MULTIPLES            PIC S9(15)V9(6) VALUE 0.
           05  WS-WHOLE                PIC S9(15) VALUE 0.
           05  WS-REMAIN               PIC S9(9)V9(9) VALUE 0.
           05  WS-DISTINCT             PIC S9(4) VALUE 0.
           05  WS-LAST-ITEM            PIC X(40) VALUE SPACES.
           05  WS-MODE                 PIC X(8) VALUE SPACES.
       LINKAGE SECTION.
       COPY ITEMENQCA.
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
               WHEN "SUPPLY"    PERFORM 1000-SUPPLY
               WHEN "PARTNER"   PERFORM 2000-PARTNER
      *> CR2614 CA-FUNCTION is X(8) - "WHEREUSED" never matched
               WHEN "WHEREUSE"  PERFORM 3000-WHERE-USED
               WHEN "USAGE"     PERFORM 3000-WHERE-USED
               WHEN "SUMMARY"   PERFORM 4000-SUMMARY
               WHEN OTHER       PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED
           MOVE "ITEMENQ" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "ITM-023" TO CA-REQ-REF
           MOVE 0 TO IE-LINE-CNT IE-LEAD-TIME-DAYS IE-MIN-ORDER-QTY
           MOVE 0 TO IE-PURCH-MULTIPLE IE-REORDER-POINT
           MOVE 0 TO IE-SAFETY-STOCK IE-MAX-STOCK IE-ROUNDED-QTY
           MOVE 0 TO IE-EXCESS-QTY IE-QTY-ON-HAND IE-QTY-FREE
           MOVE 0 TO IE-QTY-RESERVED IE-QTY-DEMAND IE-QTY-SUPPLY
           MOVE 0 TO IE-QTY-ISSUED IE-QTY-RECEIVED IE-UNIT-COST
           MOVE 0 TO IE-STOCK-VALUE-GBP
           MOVE SPACES TO IE-RESTRICTED-TEXT IE-PREFERRED-SUPP
           MOVE SPACES TO IE-PREFERRED-ALIAS IE-RESOLVED-ITEM
           MOVE SPACES TO IE-DEFAULT-WHS IE-DEFAULT-LOC IE-ABC-CLASS
           MOVE "N" TO IE-AMBIGUOUS IE-COST-VISIBLE
           MOVE FUNCTION UPPER-CASE(IE-ITEM-CODE) TO IE-ITEM-CODE
           MOVE FUNCTION UPPER-CASE(IE-ALIAS) TO IE-ALIAS
           IF IE-PARTNER-TYPE = SPACES
               MOVE "SUPPLIER" TO IE-PARTNER-TYPE
           END-IF
           IF IE-AS-AT = 0
               MOVE CA-BUSINESS-DATE TO IE-AS-AT
           END-IF
           IF IE-AS-AT = 0
               MOVE 20261005 TO IE-AS-AT
           END-IF
           IF IE-FROM-DATE = 0
               MOVE 20000101 TO IE-FROM-DATE
           END-IF
           IF IE-TO-DATE = 0
               MOVE 20991231 TO IE-TO-DATE
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES
               MOVE "ENQ0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF IE-ITEM-CODE = SPACES AND IE-ALIAS = SPACES
               MOVE "ENQ0001" TO CA-REASON-CODE
               MOVE "An item code or a partner reference is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0300  COST VISIBILITY.  AN OPERATIONAL USER DOES NOT GAIN
      *>       ACCESS TO RESTRICTED COSTS JUST BY OPENING AN ENQUIRY.
      *>----------------------------------------------------------------
       0300-COST-PERMISSION SECTION.
       0300-START.
           MOVE "N" TO IE-COST-VISIBLE
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_CheckPermission ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE "CST.COST.VIEW" TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE SPACES TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE "Y" TO SQLIO-PARM-NULL(4)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-OK AND ITM-COL(1)(1:1) = "1"
               MOVE "Y" TO IE-COST-VISIBLE
           END-IF
           IF IE-COST-VISIBLE NOT = "Y"
               MOVE SPACES TO IE-RESTRICTED-TEXT
               STRING "Cost and valuation withheld: "
                      FUNCTION TRIM(CA-USER)
                      " does not hold CST.COST.VIEW"
                   DELIMITED BY SIZE INTO IE-RESTRICTED-TEXT
           END-IF.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  SUPPLY RULES AND PROCUREMENT ROUNDING (ITM-008).
      *>----------------------------------------------------------------
       1000-SUPPLY SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT i.MakeBuy, i.Description, "
                  "ISNULL(s.LeadTimeDays,0), ISNULL(s.MinOrderQty,0),"
                  " ISNULL(s.PurchaseMultipleQty,0), "
                  "ISNULL(s.ReorderPointQty,0), "
                  "ISNULL(s.SafetyStockQty,0), ISNULL(s.MaxStockQty,"
                  "0), ISNULL(s.AbcClass,''), "
                  "ISNULL(s.DefaultWarehouseCode,''), "
                  "ISNULL(s.DefaultLocationCode,''), "
                  "ISNULL(s.MakeBuy, i.MakeBuy) FROM inv.Item i LEFT"
                  " JOIN inv.ItemSite s ON s.CompanyCode = "
                  "i.CompanyCode AND s.ItemCode = i.ItemCode AND "
                  "s.SiteCode = ? WHERE i.CompanyCode = ? AND "
                  "i.ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE IE-SITE TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE IE-ITEM-CODE TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 12 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               PERFORM 9200-NOT-FOUND
               GO TO 1000-EXIT
           END-IF
           MOVE ITM-COL(12) TO IE-MAKE-BUY
           MOVE ITM-COL(2) TO IE-DESCRIPTION
           COMPUTE IE-LEAD-TIME-DAYS = FUNCTION NUMVAL(ITM-COL(3))
           COMPUTE IE-MIN-ORDER-QTY = FUNCTION NUMVAL(ITM-COL(4))
           COMPUTE IE-PURCH-MULTIPLE = FUNCTION NUMVAL(ITM-COL(5))
           COMPUTE IE-REORDER-POINT = FUNCTION NUMVAL(ITM-COL(6))
           COMPUTE IE-SAFETY-STOCK = FUNCTION NUMVAL(ITM-COL(7))
           COMPUTE IE-MAX-STOCK = FUNCTION NUMVAL(ITM-COL(8))
           MOVE ITM-COL(9)(1:1) TO IE-ABC-CLASS
           MOVE ITM-COL(10) TO IE-DEFAULT-WHS
           MOVE ITM-COL(11) TO IE-DEFAULT-LOC
           PERFORM 1100-ROUND-ORDER
           PERFORM 1200-PREFERRED-SOURCE
           MOVE IE-ROUNDED-QTY TO WS-QTY-ED
           MOVE SPACES TO WS-TEXT
           STRING "Order " FUNCTION TRIM(WS-QTY-ED) " ("
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE IE-EXCESS-QTY TO WS-QTY-ED
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(WS-TEXT) FUNCTION TRIM(WS-QTY-ED)
                  " excess from pack rounding), lead time "
               DELIMITED BY SIZE INTO CA-MESSAGE
           MOVE IE-LEAD-TIME-DAYS TO WS-INT-ED
           MOVE SPACES TO WS-TEXT
           STRING FUNCTION TRIM(CA-MESSAGE) FUNCTION TRIM(WS-INT-ED)
                  " day(s)" DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1100  CR0961.  ROUND UP TO THE MINIMUM ORDER AND THEN TO THE
      *>       PURCHASE MULTIPLE.  THE EXCESS IS REPORTED SEPARATELY
      *>       SO THAT NOBODY MISTAKES IT FOR ENGINEERING USAGE.
      *>----------------------------------------------------------------
       1100-ROUND-ORDER SECTION.
       1100-START.
           MOVE IE-ORDER-QTY TO IE-ROUNDED-QTY
           MOVE 0 TO IE-EXCESS-QTY
           IF IE-ORDER-QTY <= 0
               GO TO 1100-EXIT
           END-IF
           IF IE-MIN-ORDER-QTY > 0
              AND IE-ROUNDED-QTY < IE-MIN-ORDER-QTY
               MOVE IE-MIN-ORDER-QTY TO IE-ROUNDED-QTY
           END-IF
           IF IE-PURCH-MULTIPLE > 0
               COMPUTE WS-MULTIPLES =
                   IE-ROUNDED-QTY / IE-PURCH-MULTIPLE
               COMPUTE WS-WHOLE = FUNCTION INTEGER(WS-MULTIPLES)
               COMPUTE WS-REMAIN = WS-MULTIPLES - WS-WHOLE
               IF WS-REMAIN > 0.0000005
                   COMPUTE IE-ROUNDED-QTY =
                       (WS-WHOLE + 1) * IE-PURCH-MULTIPLE
               END-IF
           END-IF
           COMPUTE IE-EXCESS-QTY = IE-ROUNDED-QTY - IE-ORDER-QTY
           IF IE-EXCESS-QTY > 0
               MOVE 4 TO CA-RETURN-CODE
               MOVE "ENQ0004" TO CA-REASON-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       1100-EXIT.
           EXIT.

       1200-PREFERRED-SOURCE SECTION.
       1200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 PartnerCode, PartnerItemCode FROM "
                  "inv.ItemPartner WHERE CompanyCode = ? AND "
                  "ItemCode = ? AND PartnerType = 'SUPPLIER' AND "
                  "IsApprovedSource = 1 AND EffectiveFrom <= "
                  "CONVERT(date, ?, 112) AND (EffectiveTo IS NULL OR"
                  " EffectiveTo >= CONVERT(date, ?, 112)) ORDER BY "
                  "PreferenceRank, PartnerCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE IE-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE IE-AS-AT TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(3)
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-OK
               MOVE ITM-COL(1) TO IE-PREFERRED-SUPP
               MOVE ITM-COL(2) TO IE-PREFERRED-ALIAS
           END-IF.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  PARTNER REFERENCES (ITM-009).  WHEN AN ALIAS IS GIVEN,
      *>       IT MUST SELECT EXACTLY ONE ACTIVE INTERNAL ITEM IN THE
      *>       PERMITTED CONTEXT.
      *>----------------------------------------------------------------
       2000-PARTNER SECTION.
       2000-START.
           IF IE-ALIAS NOT = SPACES
               PERFORM 2100-RESOLVE-ALIAS
               IF CA-FAILED
                   GO TO 2000-EXIT
               END-IF
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT PartnerType, PartnerCode, PartnerItemCode,"
                  " ISNULL(PartnerDescription,''), ISNULL(UomCode,''"
                  "), ISNULL(StockUnitsPerUom,0), ISNULL(UnitPrice,0"
                  "), ISNULL(CurrencyCode,''), CONVERT(char(8), "
                  "EffectiveFrom, 112), ISNULL(CONVERT(char(8), "
                  "EffectiveTo, 112),''), IsApprovedSource, "
                  "PreferenceRank FROM inv.ItemPartner WHERE "
                  "CompanyCode = ? AND ItemCode = ? AND (? = '' OR "
                  "PartnerType = ?) ORDER BY PartnerType, "
                  "PreferenceRank, PartnerCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE IE-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE IE-PARTNER-TYPE TO SQLIO-PARM-VAL(3)
           MOVE IE-PARTNER-TYPE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 12 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2000-EXIT
           END-IF
           MOVE 0 TO IE-LINE-CNT
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA ITM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   IF IE-LINE-CNT < 25
                       ADD 1 TO IE-LINE-CNT
                       MOVE ITM-COL(1) TO IE-L-KIND(IE-LINE-CNT)
                       MOVE ITM-COL(2) TO IE-L-CODE(IE-LINE-CNT)
                       MOVE ITM-COL(4) TO IE-L-DESC(IE-LINE-CNT)
                       MOVE ITM-COL(3) TO IE-L-REF(IE-LINE-CNT)
                       COMPUTE IE-L-QTY(IE-LINE-CNT) =
                           FUNCTION NUMVAL(ITM-COL(6))
                       MOVE ITM-COL(5) TO IE-L-UOM(IE-LINE-CNT)
                       COMPUTE IE-L-VALUE(IE-LINE-CNT) =
                           FUNCTION NUMVAL(ITM-COL(7))
                       COMPUTE IE-L-DATE(IE-LINE-CNT) =
                           FUNCTION NUMVAL(ITM-COL(9))
                       IF ITM-COL(11)(1:1) = "1"
                           MOVE "APPROVED"
                               TO IE-L-STATUS(IE-LINE-CNT)
                           MOVE "Y" TO IE-L-FLAG(IE-LINE-CNT)
                       ELSE
                           MOVE "UNAPPROVED"
                               TO IE-L-STATUS(IE-LINE-CNT)
                           MOVE "N" TO IE-L-FLAG(IE-LINE-CNT)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF IE-LINE-CNT = 0
               MOVE 16 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No partner references for "
                      FUNCTION TRIM(IE-ITEM-CODE)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE IE-LINE-CNT TO WS-INT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-INT-ED)
                      " partner reference(s) for "
                      FUNCTION TRIM(IE-ITEM-CODE)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       2000-EXIT.
           EXIT.

       2100-RESOLVE-ALIAS SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT DISTINCT p.ItemCode FROM inv.ItemPartner p"
                  " JOIN inv.Item i ON i.CompanyCode = p.CompanyCode"
                  " AND i.ItemCode = p.ItemCode WHERE p.CompanyCode "
                  "= ? AND p.PartnerItemCode = ? AND p.PartnerType ="
                  " ? AND (? = '' OR p.PartnerCode = ?) AND "
                  "i.LifecycleStatus NOT IN ('RETIRED','OBSOLETE') "
                  "AND p.EffectiveFrom <= CONVERT(date, ?, 112) AND "
                  "(p.EffectiveTo IS NULL OR p.EffectiveTo >= "
                  "CONVERT(date, ?, 112)) ORDER BY p.ItemCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE IE-ALIAS TO SQLIO-PARM-VAL(2)
           MOVE IE-PARTNER-TYPE TO SQLIO-PARM-VAL(3)
           MOVE IE-PARTNER-CODE TO SQLIO-PARM-VAL(4)
           MOVE IE-PARTNER-CODE TO SQLIO-PARM-VAL(5)
           MOVE IE-AS-AT TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(6)
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(7)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 7
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2100-EXIT
           END-IF
           MOVE 0 TO WS-DISTINCT
           MOVE SPACES TO WS-LAST-ITEM
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA ITM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   ADD 1 TO WS-DISTINCT
                   IF WS-DISTINCT = 1
                       MOVE ITM-COL(1) TO WS-LAST-ITEM
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF WS-DISTINCT = 0
               MOVE "ENQ0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Reference " FUNCTION TRIM(IE-ALIAS)
                      " is not known for that partner"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               MOVE "Y" TO WS-ERROR
               ADD 1 TO CA-MSG-COUNT
               GO TO 2100-EXIT
           END-IF
           IF WS-DISTINCT > 1
               MOVE "Y" TO IE-AMBIGUOUS
               MOVE "ENQ0003" TO CA-REASON-CODE
               MOVE WS-DISTINCT TO WS-INT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Partner reference " FUNCTION TRIM(IE-ALIAS)
                      " selects " FUNCTION TRIM(WS-INT-ED)
                      " active items; it cannot be used until it is"
                      " made unique"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2100-EXIT
           END-IF
           MOVE WS-LAST-ITEM TO IE-RESOLVED-ITEM
           IF IE-ITEM-CODE = SPACES
               MOVE WS-LAST-ITEM TO IE-ITEM-CODE
           END-IF.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  WHERE USED AND USAGE HISTORY (ITM-023).
      *>----------------------------------------------------------------
       3000-WHERE-USED SECTION.
       3000-START.
           PERFORM 0300-COST-PERMISSION
           IF CA-FUNCTION = "USAGE"
               MOVE "USAGE" TO WS-MODE
           ELSE
               MOVE "ALL" TO WS-MODE
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC inv.usp_ItemWhereUsed ?, ?, ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE IE-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE WS-MODE TO SQLIO-PARM-VAL(3)
           MOVE IE-FROM-DATE TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(4)
           MOVE IE-TO-DATE TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(5)
           IF IE-COST-VISIBLE = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(6)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(6)
           END-IF
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 9 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3000-EXIT
           END-IF
           MOVE 0 TO IE-LINE-CNT
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA ITM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 3100-ADD-USED-LINE
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF IE-LINE-CNT = 0
               MOVE 16 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(IE-ITEM-CODE)
                      " is not used anywhere and has no movements "
                      "in that period"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE IE-LINE-CNT TO WS-INT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-INT-ED) " usage link(s)"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               IF IE-COST-VISIBLE NOT = "Y"
                   MOVE 4 TO CA-RETURN-CODE
                   MOVE "ENQ0005" TO CA-REASON-CODE
                   ADD 1 TO CA-MSG-COUNT
               END-IF
           END-IF.
       3000-EXIT.
           EXIT.

       3100-ADD-USED-LINE SECTION.
       3100-START.
           IF IE-LINE-CNT >= 25
               GO TO 3100-EXIT
           END-IF
           ADD 1 TO IE-LINE-CNT
           MOVE ITM-COL(1) TO IE-L-KIND(IE-LINE-CNT)
           MOVE ITM-COL(2) TO IE-L-CODE(IE-LINE-CNT)
           MOVE ITM-COL(3) TO IE-L-DESC(IE-LINE-CNT)
           MOVE ITM-COL(4) TO IE-L-REF(IE-LINE-CNT)
           COMPUTE IE-L-QTY(IE-LINE-CNT) =
               FUNCTION NUMVAL(ITM-COL(5))
           MOVE ITM-COL(6) TO IE-L-UOM(IE-LINE-CNT)
           COMPUTE IE-L-VALUE(IE-LINE-CNT) =
               FUNCTION NUMVAL(ITM-COL(7))
           IF ITM-COL(8) NOT = SPACES
               COMPUTE IE-L-DATE(IE-LINE-CNT) =
                   FUNCTION NUMVAL(ITM-COL(8))
           END-IF
           MOVE ITM-COL(9) TO IE-L-STATUS(IE-LINE-CNT)
           MOVE "Y" TO IE-L-FLAG(IE-LINE-CNT)
           IF IE-L-KIND(IE-LINE-CNT) = "USAGE"
               IF IE-L-QTY(IE-LINE-CNT) < 0
                   COMPUTE IE-QTY-ISSUED = IE-QTY-ISSUED -
                       IE-L-QTY(IE-LINE-CNT)
               ELSE
                   ADD IE-L-QTY(IE-LINE-CNT) TO IE-QTY-RECEIVED
               END-IF
           END-IF.
       3100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  STOCK, DEMAND, SUPPLY AND VALUATION IN ONE CALL.
      *>----------------------------------------------------------------
       4000-SUMMARY SECTION.
       4000-START.
           PERFORM 0300-COST-PERMISSION
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SUM(v.QtyOnHand),0), "
                  "ISNULL(SUM(CASE WHEN v.OwnerType = 'OWNED' AND "
                  "v.QualityStatus = 'ACCEPTED' AND v.IsAvailable = "
                  "1 THEN v.QtyOnHand ELSE 0 END),0), (SELECT "
                  "ISNULL(SUM(r.QtyReserved - r.QtyConsumed),0) FROM"
                  " inv.StockReservation r WHERE r.CompanyCode = ? "
                  "AND r.ItemCode = ? AND r.Status = 'ACTIVE') FROM "
                  "inv.vw_StockAvailability v WHERE v.CompanyCode = "
                  "? AND v.ItemCode = ? AND (? = '' OR v.SiteCode = "
                  "?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE IE-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE IE-ITEM-CODE TO SQLIO-PARM-VAL(4)
           MOVE IE-SITE TO SQLIO-PARM-VAL(5)
           MOVE IE-SITE TO SQLIO-PARM-VAL(6)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 4000-EXIT
           END-IF
           IF SQLIO-OK
               COMPUTE IE-QTY-ON-HAND = FUNCTION NUMVAL(ITM-COL(1))
               COMPUTE WS-QTY = FUNCTION NUMVAL(ITM-COL(2))
               COMPUTE IE-QTY-RESERVED = FUNCTION NUMVAL(ITM-COL(3))
               COMPUTE IE-QTY-FREE = WS-QTY - IE-QTY-RESERVED
               IF IE-QTY-FREE < 0
                   MOVE 0 TO IE-QTY-FREE
               END-IF
           END-IF
           IF IE-COST-VISIBLE = "Y"
               PERFORM 4100-READ-COST
           END-IF
           MOVE IE-QTY-FREE TO WS-QTY-ED
           MOVE SPACES TO CA-MESSAGE
           STRING "On hand " FUNCTION TRIM(WS-QTY-ED)
                  " free for new demand"
               DELIMITED BY SIZE INTO CA-MESSAGE
           IF IE-COST-VISIBLE NOT = "Y"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "ENQ0005" TO CA-REASON-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       4000-EXIT.
           EXIT.

       4100-READ-COST SECTION.
       4100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 ISNULL(AverageUnitCost,0) FROM "
                  "cst.ItemCost WHERE CompanyCode = ? AND ItemCode ="
                  " ? ORDER BY ItemCostId"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE IE-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
      *>       costing may not be deployed; the figure is labelled
               MOVE SPACES TO IE-RESTRICTED-TEXT
               STRING "Cost not available from the costing module"
                   DELIMITED BY SIZE INTO IE-RESTRICTED-TEXT
               GO TO 4100-EXIT
           END-IF
           IF SQLIO-OK
               COMPUTE IE-UNIT-COST = FUNCTION NUMVAL(ITM-COL(1))
               COMPUTE IE-STOCK-VALUE-GBP ROUNDED =
                   IE-QTY-ON-HAND * IE-UNIT-COST
           END-IF.
       4100-EXIT.
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
           MOVE "ENQ0006" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "ITEMENQ does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

       9200-NOT-FOUND SECTION.
       9200-START.
           MOVE "ENQ0001" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Item " FUNCTION TRIM(IE-ITEM-CODE)
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
               MOVE "SQL failure in ITEMENQ" TO CA-MESSAGE
           END-IF
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE 12 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
