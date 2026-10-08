      *>================================================================
      *> PROGRAM-ID : ITEMCOPY
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE
      *> PURPOSE    : ITEM COPY AND BULK MAINTENANCE WITH PREVIEW.  A
      *>              COPY CREATES A NEW IDENTITY: THE DESCRIPTIVE,
      *>              UNIT, SUPPLY, FINANCE, COMPLIANCE AND SUBSTITUTE
      *>              DEFINITIONS MAY BE CARRIED ACROSS, BUT NEVER
      *>              BALANCES, MOVEMENT HISTORY, APPROVALS, THE
      *>              RELEASED STATE OR SERIAL IDENTITIES.  A BULK
      *>              CHANGE PREVIEWS EVERY TARGET AND REPORTS A
      *>              RESULT PER TARGET, SO A MIXED VALID AND INVALID
      *>              SELECTION IS PARTLY APPLIED AND FULLY EXPLAINED.
      *> CALLED BY  : ITEM MASTER MAINTENANCE, THE .NET CLIENT
      *> INTERFACE  : CALL "ITEMCOPY" USING WS-ITEMCOPY-CA
      *>              COPYBOOK ITEMCOPYCA
      *> CALLS      : SQLIO (inv.Item and its detail tables), ITMAUDIT
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-04-17 RJH  CR0855 ORIGINAL - COPY ITEM
      *> 2010-06-11 ANW  CR2402 BULK DESCRIPTIVE MAINTENANCE, PREVIEW
      *> 2026-10-05 STERP ITM-021 PER TARGET RESULTS AND HISTORY
      *> 2026-10-07 STERP ITM-021 THE COPY NOW CREATES THE PARTNER AND
      *>                 SUBSTITUTE GROUPS THE PREVIEW PROMISES, AND
      *>                 BULKAPPLY IS RECOGNISED - CA-FUNCTION IS
      *>                 EIGHT BYTES SO IT ARRIVES AS "BULKAPPL"
      *>================================================================
      *> REQ: ITM-021, ITM-004
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ITEMCOPY.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 17/04/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ITMWORK.
       COPY ITMAUDTW.
       01  WS-CONTROL.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-GROUP                PIC X(24) VALUE SPACES.
           05  WS-GROUP-TEXT           PIC X(80) VALUE SPACES.
           05  WS-ROW-COUNT            PIC S9(9) VALUE 0.
           05  WS-COLUMN               PIC X(24) VALUE SPACES.
           05  WS-DATATYPE             PIC X(4) VALUE SPACES.
           05  WS-TARGET               PIC X(40) VALUE SPACES.
           05  WS-BAD-CHAR             PIC X VALUE "N".
           05  WS-CHAR                 PIC X VALUE SPACE.
       LINKAGE SECTION.
       COPY ITEMCOPYCA.
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
               WHEN "PREVIEW"   PERFORM 1000-PREVIEW
               WHEN "COPY"      PERFORM 2000-COPY
               WHEN "BULKPREV"  PERFORM 3000-BULK-PREVIEW
      *>       CA-FUNCTION IS EIGHT BYTES, SO A CALLER ASKING FOR
      *>       "BULKAPPLY" ARRIVES HERE AS "BULKAPPL"
               WHEN "BULKAPPL"  PERFORM 4000-BULK-APPLY
               WHEN OTHER       PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED
           MOVE "ITEMCOPY" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "ITM-021" TO CA-REQ-REF
           MOVE 0 TO IC-PREVIEW-CNT IC-EXCLUDE-CNT
           MOVE 0 TO IC-COPIED-CNT IC-REJECTED-CNT
           MOVE FUNCTION UPPER-CASE(IC-SOURCE-ITEM) TO IC-SOURCE-ITEM
           MOVE FUNCTION UPPER-CASE(IC-TARGET-ITEM) TO IC-TARGET-ITEM
           MOVE FUNCTION UPPER-CASE(IC-FIELD-NAME) TO IC-FIELD-NAME
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES
               MOVE "CPY0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION = "PREVIEW" OR CA-FUNCTION = "COPY"
               IF IC-SOURCE-ITEM = SPACES
                   MOVE "CPY0001" TO CA-REASON-CODE
                   MOVE "Name the item to copy from" TO CA-MESSAGE
                   PERFORM 9000-REJECT
               END-IF
               GO TO 0200-EXIT
           END-IF
           IF IC-TARGET-CNT = 0
               MOVE "CPY0006" TO CA-REASON-CODE
               MOVE "Select at least one item to maintain"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  PREVIEW A COPY: WHAT WOULD BE CREATED AND WHAT WOULD
      *>       DELIBERATELY BE LEFT BEHIND.
      *>----------------------------------------------------------------
       1000-PREVIEW SECTION.
       1000-START.
           PERFORM 1100-CHECK-SOURCE
           IF CA-FAILED
               GO TO 1000-EXIT
           END-IF
           IF IC-TARGET-ITEM NOT = SPACES
               PERFORM 1200-CHECK-TARGET
               IF CA-FAILED
                   GO TO 1000-EXIT
               END-IF
           END-IF
           MOVE 0 TO IC-PREVIEW-CNT IC-EXCLUDE-CNT
           MOVE "Item master" TO WS-GROUP
           MOVE 1 TO WS-ROW-COUNT
           MOVE "Identity, description, class, units and policy"
               TO WS-GROUP-TEXT
           PERFORM 1300-ADD-PREVIEW
           IF IC-INCLUDE-UOM = "Y"
               MOVE "inv.ItemUom" TO WS-GROUP
               MOVE "Unit conversions by usage with their dates"
                   TO WS-GROUP-TEXT
               PERFORM 1400-COUNT-GROUP
               PERFORM 1300-ADD-PREVIEW
           END-IF
           IF IC-INCLUDE-SITE = "Y"
               MOVE "inv.ItemSite" TO WS-GROUP
               MOVE "Site planning parameters" TO WS-GROUP-TEXT
               PERFORM 1400-COUNT-GROUP
               PERFORM 1300-ADD-PREVIEW
           END-IF
           IF IC-INCLUDE-VAT = "Y"
               MOVE "inv.ItemVatClass" TO WS-GROUP
               MOVE "Effective dated UK VAT liability"
                   TO WS-GROUP-TEXT
               PERFORM 1400-COUNT-GROUP
               PERFORM 1300-ADD-PREVIEW
           END-IF
           IF IC-INCLUDE-CUSTOMS = "Y"
               MOVE "inv.ItemCustoms" TO WS-GROUP
               MOVE "Commodity code, origin and export control"
                   TO WS-GROUP-TEXT
               PERFORM 1400-COUNT-GROUP
               PERFORM 1300-ADD-PREVIEW
           END-IF
           IF IC-INCLUDE-PACKAGE = "Y"
               MOVE "inv.ItemPackaging" TO WS-GROUP
               MOVE "Packaging composition by material"
                   TO WS-GROUP-TEXT
               PERFORM 1400-COUNT-GROUP
               PERFORM 1300-ADD-PREVIEW
           END-IF
           IF IC-INCLUDE-PARTNER = "Y"
               MOVE "inv.ItemPartner" TO WS-GROUP
               MOVE "Supplier and customer references"
                   TO WS-GROUP-TEXT
               PERFORM 1400-COUNT-GROUP
               PERFORM 1300-ADD-PREVIEW
           END-IF
           IF IC-INCLUDE-SUBST = "Y"
               MOVE "inv.ItemSubstitute" TO WS-GROUP
               MOVE "Approved substitutes and their ratios"
                   TO WS-GROUP-TEXT
               PERFORM 1400-COUNT-GROUP
               PERFORM 1300-ADD-PREVIEW
           END-IF
           PERFORM 1500-EXCLUSIONS
           MOVE IC-PREVIEW-CNT TO WS-INT-ED
           MOVE SPACES TO CA-MESSAGE
           STRING "A copy would create "
                  FUNCTION TRIM(WS-INT-ED)
                  " group(s) of master data and no balances, "
                  "history, approvals or serials"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       1000-EXIT.
           EXIT.

       1100-CHECK-SOURCE SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Description, LifecycleStatus FROM inv.Item"
                  " WHERE CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE IC-SOURCE-ITEM TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "CPY0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Source item " FUNCTION TRIM(IC-SOURCE-ITEM)
                      " is not held in company "
                      FUNCTION TRIM(CA-COMPANY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               MOVE "Y" TO WS-ERROR
               ADD 1 TO CA-MSG-COUNT
               GO TO 1100-EXIT
           END-IF
           IF IC-TARGET-DESC = SPACES
               MOVE ITM-COL(1) TO IC-TARGET-DESC
           END-IF.
       1100-EXIT.
           EXIT.

       1200-CHECK-TARGET SECTION.
       1200-START.
      *>   D-21: item codes are upper case and limited to the
      *>   characters A-Z 0-9 and - . / _
           MOVE "N" TO WS-BAD-CHAR
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 40
               MOVE IC-TARGET-ITEM(WS-IX:1) TO WS-CHAR
               IF WS-CHAR NOT = SPACE
                   IF NOT ((WS-CHAR >= "A" AND WS-CHAR <= "Z")
                        OR (WS-CHAR >= "0" AND WS-CHAR <= "9")
                        OR WS-CHAR = "-" OR WS-CHAR = "."
                        OR WS-CHAR = "/" OR WS-CHAR = "_")
                       MOVE "Y" TO WS-BAD-CHAR
                   END-IF
               END-IF
           END-PERFORM
           IF WS-BAD-CHAR = "Y"
               MOVE "CPY0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item code " FUNCTION TRIM(IC-TARGET-ITEM)
                      " may only use A-Z, 0-9 and the characters "
                      "- . / _"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ItemCode FROM inv.Item WHERE CompanyCode ="
                  " ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE IC-TARGET-ITEM TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1200-EXIT
           END-IF
           IF SQLIO-OK
               MOVE "CPY0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(IC-TARGET-ITEM)
                      " already exists; an identity is never reused"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       1200-EXIT.
           EXIT.

       1300-ADD-PREVIEW SECTION.
       1300-START.
           IF IC-PREVIEW-CNT < 20
               ADD 1 TO IC-PREVIEW-CNT
               MOVE WS-GROUP TO IC-P-GROUP(IC-PREVIEW-CNT)
               MOVE WS-ROW-COUNT TO IC-P-ROWS(IC-PREVIEW-CNT)
               MOVE WS-GROUP-TEXT TO IC-P-TEXT(IC-PREVIEW-CNT)
           END-IF
           MOVE 0 TO WS-ROW-COUNT.
       1300-EXIT.
           EXIT.

       1400-COUNT-GROUP SECTION.
       1400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT(*) FROM " FUNCTION TRIM(WS-GROUP)
                  " WHERE CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE IC-SOURCE-ITEM TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-OK
               COMPUTE WS-ROW-COUNT = FUNCTION NUMVAL(ITM-COL(1))
           ELSE
               MOVE 0 TO WS-ROW-COUNT
           END-IF.
       1400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1500  WHAT A COPY NEVER INHERITS.  THE LIST IS SHOWN TO THE
      *>       USER BEFORE THEY CONFIRM (ITM-021).
      *>----------------------------------------------------------------
       1500-EXCLUSIONS SECTION.
       1500-START.
           MOVE 0 TO IC-EXCLUDE-CNT
           PERFORM 1550-ADD-EXCLUSION-1
           PERFORM 1550-ADD-EXCLUSION-2
           PERFORM 1550-ADD-EXCLUSION-3
           PERFORM 1550-ADD-EXCLUSION-4
           PERFORM 1550-ADD-EXCLUSION-5
           PERFORM 1550-ADD-EXCLUSION-6.
       1500-EXIT.
           EXIT.

       1550-ADD-EXCLUSION-1 SECTION.
       1550-START.
           ADD 1 TO IC-EXCLUDE-CNT
           MOVE "Stock balances" TO IC-X-GROUP(IC-EXCLUDE-CNT)
           MOVE SPACES TO IC-X-TEXT(IC-EXCLUDE-CNT)
           STRING "A new identity starts with no physical stock"
               DELIMITED BY SIZE INTO IC-X-TEXT(IC-EXCLUDE-CNT).
       1550-EXIT.
           EXIT.

       1550-ADD-EXCLUSION-2 SECTION.
       1551-START.
           ADD 1 TO IC-EXCLUDE-CNT
           MOVE "Movement history" TO IC-X-GROUP(IC-EXCLUDE-CNT)
           MOVE SPACES TO IC-X-TEXT(IC-EXCLUDE-CNT)
           STRING "Transaction evidence belongs to the item that "
                  "was actually moved"
               DELIMITED BY SIZE INTO IC-X-TEXT(IC-EXCLUDE-CNT).
       1551-EXIT.
           EXIT.

       1550-ADD-EXCLUSION-3 SECTION.
       1552-START.
           ADD 1 TO IC-EXCLUDE-CNT
           MOVE "Approvals and release" TO IC-X-GROUP(IC-EXCLUDE-CNT)
           MOVE SPACES TO IC-X-TEXT(IC-EXCLUDE-CNT)
           STRING "The copy starts as a draft and needs its own "
                  "activation review"
               DELIMITED BY SIZE INTO IC-X-TEXT(IC-EXCLUDE-CNT).
       1552-EXIT.
           EXIT.

       1550-ADD-EXCLUSION-4 SECTION.
       1553-START.
           ADD 1 TO IC-EXCLUDE-CNT
           MOVE "Lots and serials" TO IC-X-GROUP(IC-EXCLUDE-CNT)
           MOVE SPACES TO IC-X-TEXT(IC-EXCLUDE-CNT)
           STRING "A serial identity represents one physical unit "
                  "and is never duplicated"
               DELIMITED BY SIZE INTO IC-X-TEXT(IC-EXCLUDE-CNT).
       1553-EXIT.
           EXIT.

       1550-ADD-EXCLUSION-5 SECTION.
       1554-START.
           ADD 1 TO IC-EXCLUDE-CNT
           MOVE "Reservations" TO IC-X-GROUP(IC-EXCLUDE-CNT)
           MOVE SPACES TO IC-X-TEXT(IC-EXCLUDE-CNT)
           STRING "Reservations belong to the demand that made them"
               DELIMITED BY SIZE INTO IC-X-TEXT(IC-EXCLUDE-CNT).
       1554-EXIT.
           EXIT.

       1550-ADD-EXCLUSION-6 SECTION.
       1555-START.
           ADD 1 TO IC-EXCLUDE-CNT
           MOVE "Costs and valuation" TO IC-X-GROUP(IC-EXCLUDE-CNT)
           MOVE SPACES TO IC-X-TEXT(IC-EXCLUDE-CNT)
           STRING "Carrying values are derived from this item's own"
                  " transactions"
               DELIMITED BY SIZE INTO IC-X-TEXT(IC-EXCLUDE-CNT).
       1555-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  PERFORM THE COPY.  THE NEW ITEM IS A DRAFT.
      *>----------------------------------------------------------------
       2000-COPY SECTION.
       2000-START.
           PERFORM 1000-PREVIEW
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
           IF IC-TARGET-ITEM = SPACES
               MOVE "CPY0003" TO CA-REASON-CODE
               MOVE "Give the new item its own code" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-COPY-MASTER
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
           IF IC-INCLUDE-UOM = "Y"
               PERFORM 2200-COPY-UOM
           END-IF
           IF IC-INCLUDE-SITE = "Y"
               PERFORM 2300-COPY-SITE
           END-IF
           IF IC-INCLUDE-VAT = "Y"
               PERFORM 2400-COPY-VAT
           END-IF
           IF IC-INCLUDE-CUSTOMS = "Y"
               PERFORM 2500-COPY-CUSTOMS
           END-IF
           IF IC-INCLUDE-PACKAGE = "Y"
               PERFORM 2600-COPY-PACKAGING
           END-IF
           IF IC-INCLUDE-PARTNER = "Y"
               PERFORM 2700-COPY-PARTNER
           END-IF
           IF IC-INCLUDE-SUBST = "Y"
               PERFORM 2750-COPY-SUBSTITUTE
           END-IF
           PERFORM 2900-AUDIT-COPY
           MOVE 1 TO IC-COPIED-CNT
           MOVE SPACES TO CA-MESSAGE
           STRING "Item " FUNCTION TRIM(IC-TARGET-ITEM)
                  " created as a draft from "
                  FUNCTION TRIM(IC-SOURCE-ITEM)
                  " with no balances or history"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

       2100-COPY-MASTER SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT inv.Item (CompanyCode, ItemCode, "
                  "Description, LongDescription, ItemClass, "
                  "Category, ProductGroup, StewardUser, "
                  "StockUomCode, PurchaseUomCode, SalesUomCode, "
                  "IssueUomCode, QuantumQty, IsIndivisible, "
                  "NetWeightKg, VolumeM3, DrawingRef, "
                  "SpecificationRef, CurrentRevision, "
                  "TraceabilityType, ShelfLifeDays, "
                  "MinRemainingShelfLifeDays, RequiresInspection, "
                  "MakeBuy, ValuationGroup, CostMethod, "
                  "InventoryAccountGroup, RevenueAccountGroup, "
                  "IsPurchasable, IsSaleable, IsManufacturable, "
                  "IsConsumable, IsStocked, IsDrcSpecifiedGoods, "
                  "UkcaMarked, DangerousGoodsUn, DangerousGoodsClass"
                  ", DangerousGoodsPackGroup, ReachStatus, "
                  "WeeeCategory, BatteryCategory, LifecycleStatus, "
                  "StatusReason, CreatedBy) SELECT CompanyCode, ?, ?"
                  ", LongDescription, ItemClass, Category, "
                  "ProductGroup, ?, StockUomCode, PurchaseUomCode, "
                  "SalesUomCode, IssueUomCode, QuantumQty, "
                  "IsIndivisible, NetWeightKg, VolumeM3, DrawingRef,"
                  " SpecificationRef, 'A', TraceabilityType, "
                  "ShelfLifeDays, MinRemainingShelfLifeDays, "
                  "RequiresInspection, MakeBuy, ValuationGroup, "
                  "CostMethod, InventoryAccountGroup, "
                  "RevenueAccountGroup, IsPurchasable, IsSaleable, "
                  "IsManufacturable, IsConsumable, IsStocked, "
                  "IsDrcSpecifiedGoods, UkcaMarked, "
                  "DangerousGoodsUn, DangerousGoodsClass, "
                  "DangerousGoodsPackGroup, ReachStatus, "
                  "WeeeCategory, BatteryCategory, 'DRAFT', ?, ? FROM"
                  " inv.Item WHERE CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE IC-TARGET-ITEM TO SQLIO-PARM-VAL(1)
           MOVE IC-TARGET-DESC TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE SPACES TO SQLIO-PARM-VAL(4)
           STRING "Copied from " FUNCTION TRIM(IC-SOURCE-ITEM)
                  " - requires its own activation review"
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(4)
           MOVE CA-USER TO SQLIO-PARM-VAL(5)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(6)
           MOVE IC-SOURCE-ITEM TO SQLIO-PARM-VAL(7)
           MOVE 7 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 7
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2100-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW.
       2100-EXIT.
           EXIT.

       2200-COPY-UOM SECTION.
       2200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT inv.ItemUom (CompanyCode, ItemCode, "
                  "UomCode, UsageType, StockUnitsPerUom, "
                  "PackDescription, EffectiveFrom, EffectiveTo, "
                  "CreatedBy) SELECT CompanyCode, ?, UomCode, "
                  "UsageType, StockUnitsPerUom, PackDescription, "
                  "EffectiveFrom, EffectiveTo, ? FROM inv.ItemUom "
                  "WHERE CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 2800-RUN-COPY.
       2200-EXIT.
           EXIT.

       2300-COPY-SITE SECTION.
       2300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT inv.ItemSite (CompanyCode, SiteCode, "
                  "ItemCode, DefaultLocationCode, "
                  "DefaultWarehouseCode, MakeBuy, LeadTimeDays, "
                  "MinOrderQty, PurchaseMultipleQty, ReorderPointQty"
                  ", SafetyStockQty, MaxStockQty, AbcClass, "
                  "CreatedBy) SELECT CompanyCode, SiteCode, ?, "
                  "DefaultLocationCode, DefaultWarehouseCode, "
                  "MakeBuy, LeadTimeDays, MinOrderQty, "
                  "PurchaseMultipleQty, ReorderPointQty, "
                  "SafetyStockQty, MaxStockQty, AbcClass, ? FROM "
                  "inv.ItemSite WHERE CompanyCode = ? AND ItemCode ="
                  " ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 2800-RUN-COPY.
       2300-EXIT.
           EXIT.

       2400-COPY-VAT SECTION.
       2400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT inv.ItemVatClass (CompanyCode, ItemCode, "
                  "Direction, EffectiveFrom, VatCode, EffectiveTo, "
                  "EvidenceRef, CreatedBy) SELECT CompanyCode, ?, "
                  "Direction, EffectiveFrom, VatCode, EffectiveTo, "
                  "EvidenceRef, ? FROM inv.ItemVatClass WHERE "
                  "CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 2800-RUN-COPY.
       2400-EXIT.
           EXIT.

       2500-COPY-CUSTOMS SECTION.
       2500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT inv.ItemCustoms (CompanyCode, ItemCode, "
                  "EffectiveFrom, CommodityCode, CountryOfOrigin, "
                  "PreferentialOrigin, PreferenceEvidenceRef, "
                  "DutyRatePct, ExportControlled, ExportLicenceRef, "
                  "SupplementaryUnit, NetWeightKgPerStockUnit, "
                  "EffectiveTo, CreatedBy) SELECT CompanyCode, ?, "
                  "EffectiveFrom, CommodityCode, CountryOfOrigin, "
                  "PreferentialOrigin, PreferenceEvidenceRef, "
                  "DutyRatePct, ExportControlled, ExportLicenceRef, "
                  "SupplementaryUnit, NetWeightKgPerStockUnit, "
                  "EffectiveTo, ? FROM inv.ItemCustoms WHERE "
                  "CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 2800-RUN-COPY.
       2500-EXIT.
           EXIT.

       2600-COPY-PACKAGING SECTION.
       2600-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT inv.ItemPackaging (CompanyCode, ItemCode, "
                  "PackagingMaterial, PackagingType, WeightGrams, "
                  "RecycledContentPct, NationOfSupply, "
                  "IsPlasticPackagingTax, CreatedBy) SELECT "
                  "CompanyCode, ?, PackagingMaterial, PackagingType,"
                  " WeightGrams, RecycledContentPct, NationOfSupply,"
                  " IsPlasticPackagingTax, ? FROM inv.ItemPackaging "
                  "WHERE CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 2800-RUN-COPY.
       2600-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2700  THE PARTNER REFERENCES AND THE APPROVED SUBSTITUTES ARE
      *>       PART OF THE DEFINITION AND ARE CARRIED ACROSS WHEN THEY
      *>       ARE SELECTED, SO THAT THE PREVIEW TELLS THE TRUTH ABOUT
      *>       WHAT A COPY CREATES (ITM-021).  THE APPROVAL ITSELF IS
      *>       NOT INHERITED: A COPIED SOURCE IS UNAPPROVED AND A
      *>       COPIED SUBSTITUTE CARRIES NO APPROVER UNTIL IT HAS BEEN
      *>       REVIEWED IN ITS OWN RIGHT.
      *>----------------------------------------------------------------
       2700-COPY-PARTNER SECTION.
       2700-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT inv.ItemPartner (CompanyCode, ItemCode, "
                  "PartnerType, PartnerCode, PartnerItemCode, "
                  "PartnerDescription, UomCode, StockUnitsPerUom, "
                  "QtyBreak, CurrencyCode, UnitPrice, PricePerQty, "
                  "LeadTimeDays, MinOrderQty, OrderMultipleQty, "
                  "PreferenceRank, IsApprovedSource, QuoteRef, "
                  "EffectiveFrom, EffectiveTo, CreatedBy) SELECT "
                  "CompanyCode, ?, PartnerType, PartnerCode, "
                  "PartnerItemCode, PartnerDescription, UomCode, "
                  "StockUnitsPerUom, QtyBreak, CurrencyCode, "
                  "UnitPrice, PricePerQty, LeadTimeDays, MinOrderQty"
                  ", OrderMultipleQty, PreferenceRank, 0, QuoteRef, "
                  "EffectiveFrom, EffectiveTo, ? FROM "
                  "inv.ItemPartner WHERE CompanyCode = ? AND "
                  "ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 2800-RUN-COPY.
       2700-EXIT.
           EXIT.

       2750-COPY-SUBSTITUTE SECTION.
       2750-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT inv.ItemSubstitute (CompanyCode, ItemCode,"
                  " SubstituteItemCode, SubstituteRevision, "
                  "ConversionRatio, ContextPurchase, ContextSales, "
                  "ContextKit, ContextBom, "
                  "RequiresCustomerAgreement, Restrictions, "
                  "ValidFrom, ValidTo, CreatedBy) SELECT "
                  "CompanyCode, ?, SubstituteItemCode, "
                  "SubstituteRevision, ConversionRatio, "
                  "ContextPurchase, ContextSales, ContextKit, "
                  "ContextBom, RequiresCustomerAgreement, "
                  "Restrictions, ValidFrom, ValidTo, ? FROM "
                  "inv.ItemSubstitute WHERE CompanyCode = ? AND "
                  "ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 2800-RUN-COPY.
       2750-EXIT.
           EXIT.

       2800-RUN-COPY SECTION.
       2800-START.
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE IC-TARGET-ITEM TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE IC-SOURCE-ITEM TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2800-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW.
       2800-EXIT.
           EXIT.

       2900-AUDIT-COPY SECTION.
       2900-START.
           INITIALIZE ITA-BLOCK
           MOVE CA-COMPANY TO ITA-COMPANY
           MOVE CA-USER TO ITA-USER
           MOVE "inv.Item" TO ITA-ENTITY-TYPE
           MOVE IC-TARGET-ITEM TO ITA-ENTITY-KEY
           MOVE "COPY" TO ITA-ACTION
           MOVE "ItemCode" TO ITA-FIELD-NAME
           MOVE IC-SOURCE-ITEM TO ITA-OLD-VALUE
           MOVE IC-TARGET-ITEM TO ITA-NEW-VALUE
           MOVE SPACES TO ITA-REASON
           STRING "Copied master data only; balances, history, "
                  "approvals and serials were not inherited"
               DELIMITED BY SIZE INTO ITA-REASON
           MOVE CA-CORRELATION-ID TO ITA-CORRELATION
           CALL "ITMAUDIT" USING ITA-BLOCK.
       2900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  BULK MAINTENANCE PREVIEW.  ONLY DESCRIPTIVE AND
      *>       PLANNING ATTRIBUTES MAY BE CHANGED IN BULK; IDENTITY,
      *>       UNITS AND TRACEABILITY MAY NOT.
      *>----------------------------------------------------------------
       3000-BULK-PREVIEW SECTION.
       3000-START.
           PERFORM 3100-FIELD-ALLOWED
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           MOVE 0 TO IC-COPIED-CNT IC-REJECTED-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > IC-TARGET-CNT OR WS-IX > 30
               MOVE FUNCTION UPPER-CASE(IC-T-ITEM(WS-IX))
                   TO IC-T-ITEM(WS-IX)
               MOVE IC-T-ITEM(WS-IX) TO WS-TARGET
               MOVE IC-NEW-VALUE TO IC-T-NEW-VALUE(WS-IX)
               PERFORM 3200-READ-CURRENT
               IF WS-ERROR = "Y"
                   MOVE "N" TO WS-ERROR
                   MOVE "REJECT" TO IC-T-OUTCOME(WS-IX)
                   MOVE SPACES TO IC-T-REASON(WS-IX)
                   STRING "Item not held in this company"
                       DELIMITED BY SIZE INTO IC-T-REASON(WS-IX)
                   ADD 1 TO IC-REJECTED-CNT
               ELSE
                   MOVE WS-TEXT TO IC-T-OLD-VALUE(WS-IX)
                   MOVE "CHANGE" TO IC-T-OUTCOME(WS-IX)
                   MOVE SPACES TO IC-T-REASON(WS-IX)
                   STRING "Descriptive change only; confirmed "
                          "documents keep their own evidence"
                       DELIMITED BY SIZE INTO IC-T-REASON(WS-IX)
                   ADD 1 TO IC-COPIED-CNT
               END-IF
           END-PERFORM
           PERFORM 3900-BULK-MESSAGE.
       3000-EXIT.
           EXIT.

       3100-FIELD-ALLOWED SECTION.
       3100-START.
           EVALUATE IC-FIELD-NAME
               WHEN "DESCRIPTION"
                   MOVE "Description" TO WS-COLUMN
               WHEN "LONGDESCRIPTION"
                   MOVE "LongDescription" TO WS-COLUMN
               WHEN "CATEGORY"
                   MOVE "Category" TO WS-COLUMN
               WHEN "PRODUCTGROUP"
                   MOVE "ProductGroup" TO WS-COLUMN
               WHEN "STEWARDUSER"
                   MOVE "StewardUser" TO WS-COLUMN
               WHEN "DRAWINGREF"
                   MOVE "DrawingRef" TO WS-COLUMN
               WHEN "SPECIFICATIONREF"
                   MOVE "SpecificationRef" TO WS-COLUMN
               WHEN OTHER
                   MOVE SPACES TO WS-COLUMN
           END-EVALUATE
           IF WS-COLUMN = SPACES
               MOVE "CPY0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(IC-FIELD-NAME)
                      " is not available for bulk maintenance; "
                      "identity, units and traceability are changed"
                      " one item at a time"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 3100-EXIT
           END-IF
           IF IC-NEW-VALUE = SPACES AND IC-FIELD-NAME = "DESCRIPTION"
               MOVE "CPY0005" TO CA-REASON-CODE
               MOVE "A description may not be set to blank"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       3100-EXIT.
           EXIT.

       3200-READ-CURRENT SECTION.
       3200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(CAST(" FUNCTION TRIM(WS-COLUMN)
                  " AS varchar(100)),'') FROM inv.Item WHERE "
                  "CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-TARGET TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           MOVE SPACES TO WS-TEXT
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3200-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "Y" TO WS-ERROR
               GO TO 3200-EXIT
           END-IF
           MOVE ITM-COL(1) TO WS-TEXT.
       3200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  APPLY THE BULK CHANGE, TARGET BY TARGET, SO THAT A
      *>       MIXED SELECTION IS PARTLY APPLIED AND FULLY EXPLAINED.
      *>----------------------------------------------------------------
       4000-BULK-APPLY SECTION.
       4000-START.
           PERFORM 3000-BULK-PREVIEW
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           MOVE 0 TO IC-COPIED-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > IC-TARGET-CNT OR WS-IX > 30
               IF IC-T-OUTCOME(WS-IX) = "CHANGE"
                   MOVE IC-T-ITEM(WS-IX) TO WS-TARGET
                   PERFORM 4100-APPLY-ONE
                   IF WS-ERROR = "Y"
                       MOVE "N" TO WS-ERROR
                       MOVE "REJECT" TO IC-T-OUTCOME(WS-IX)
                       ADD 1 TO IC-REJECTED-CNT
                   ELSE
                       MOVE "APPLIED" TO IC-T-OUTCOME(WS-IX)
                       ADD 1 TO IC-COPIED-CNT
                   END-IF
               END-IF
           END-PERFORM
           PERFORM 3900-BULK-MESSAGE.
       4000-EXIT.
           EXIT.

       4100-APPLY-ONE SECTION.
       4100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE inv.Item SET " FUNCTION TRIM(WS-COLUMN)
                  " = ?, ModifiedBy = ?, ModifiedAt = SYSDATETIME()"
                  " WHERE CompanyCode = ? AND ItemCode = ? AND "
                  "LifecycleStatus <> 'RETIRED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE IC-NEW-VALUE TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE WS-TARGET TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               MOVE "Y" TO WS-ERROR
               MOVE SQLIO-ERRMSG(1:80) TO IC-T-REASON(WS-IX)
               GO TO 4100-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           INITIALIZE ITA-BLOCK
           MOVE CA-COMPANY TO ITA-COMPANY
           MOVE CA-USER TO ITA-USER
           MOVE "inv.Item" TO ITA-ENTITY-TYPE
           MOVE WS-TARGET TO ITA-ENTITY-KEY
           MOVE "BULKCHANGE" TO ITA-ACTION
           MOVE WS-COLUMN TO ITA-FIELD-NAME
           MOVE IC-T-OLD-VALUE(WS-IX) TO ITA-OLD-VALUE
           MOVE IC-NEW-VALUE TO ITA-NEW-VALUE
           MOVE SPACES TO ITA-REASON
           STRING "Bulk descriptive maintenance; confirmed "
                  "documents keep the evidence they were posted with"
               DELIMITED BY SIZE INTO ITA-REASON
           MOVE CA-CORRELATION-ID TO ITA-CORRELATION
           CALL "ITMAUDIT" USING ITA-BLOCK.
       4100-EXIT.
           EXIT.

       3900-BULK-MESSAGE SECTION.
       3900-START.
           MOVE IC-COPIED-CNT TO WS-INT-ED
           MOVE SPACES TO WS-TEXT
           STRING FUNCTION TRIM(WS-INT-ED) " target(s) accepted, "
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE IC-REJECTED-CNT TO WS-INT-ED
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(WS-TEXT) FUNCTION TRIM(WS-INT-ED)
                  " rejected; see the result on each line"
               DELIMITED BY SIZE INTO CA-MESSAGE
           IF IC-REJECTED-CNT > 0
               MOVE 4 TO CA-RETURN-CODE
               MOVE "CPY0008" TO CA-REASON-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       3900-EXIT.
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
           MOVE "CPY0007" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "ITEMCOPY does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE "Y" TO WS-ERROR WS-SQL-FAILED
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in ITEMCOPY" TO CA-MESSAGE
           END-IF
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE 12 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
