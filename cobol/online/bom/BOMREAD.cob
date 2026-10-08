>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : BOMREAD
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : Read one selected bill - header plus every
*>              component occurrence - into memory, and resolve at
*>              the same time the item attributes the calculation
*>              needs: stock unit, allowed stock quantum, class,
*>              make or buy, and the purchase multiple of the
*>              planning site.  Holding the bill in one record
*>              lets the recursive explosion keep each level in
*>              LOCAL-STORAGE and never nest a cursor.
*>
*>              Component effectivity is NOT filtered here.  Every
*>              line is returned with its dates so that the caller
*>              can say why a line was left out, which is what
*>              BOM-029 asks for.
*>
*>              Items and units belong to the Items team (module
*>              itm).  Their UOMCONV service was not available when
*>              this was written, so the conversion evidence is
*>              read straight from inv.Item / inv.ItemUom through
*>              SQLIO - see docs\tech-debt\bom.md.
*> CALLED BY  : BOMEXPL, BOMVALID, CYCLECHK, BOMCOMP, BOMJOB,
*>              BOMBULK, BOMCOST
*> INTERFACE  : CALL "BOMREAD" USING BOM-RD-CTL BOM-HDR-REC
*>                                   BOM-CMP-TAB
*>              COPYBOOK BOMBILL  (not LINKable)
*> CALLS      : SQLIO
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2014-04-28 AJS  CR2206 ENGINEERING REWRITE
*> 2016-05-24 AJS  CR2466 OPTION CHOICE REFERENCE
*> 2026-10-05 STERP COST-ONLY RATE, PURCHASE MULTIPLE, ITEM FOUND
*>================================================================
*> REQ: BOM-002, BOM-005, BOM-010, BOM-016, BOM-018, BOM-027
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BOMREAD.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 28/04/2014.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 01  Ws-Work.
     05  Ws-I                    PIC S9(4) VALUE 0.
     05  Ws-More                 PIC X VALUE "Y".
 01  Ws-Hdr-Row.
     05  Hr-Bom-Id               PIC X(12).
     05  Hr-Company              PIC X(10).
     05  Hr-Site                 PIC X(10).
     05  Hr-Bom-Ref              PIC X(30).
     05  Hr-Bom-Rev              PIC X(4).
     05  Hr-Parent-Item          PIC X(40).
     05  Hr-Parent-Rev           PIC X(4).
     05  Hr-Purpose              PIC X(10).
     05  Hr-Job-Ref              PIC X(20).
     05  Hr-Variant              PIC X(20).
     05  Hr-Description          PIC X(60).
     05  Hr-Basis                PIC X(24).
     05  Hr-Yield                PIC X(24).
     05  Hr-Quantum              PIC X(24).
     05  Hr-Batch                PIC X(24).
     05  Hr-Phantom              PIC X(1).
     05  Hr-Consolidate          PIC X(1).
     05  Hr-Costing-Lot          PIC X(24).
     05  Hr-Setup-Cost           PIC X(24).
     05  Hr-Conv-Cost            PIC X(24).
     05  Hr-Eff-From             PIC X(8).
     05  Hr-Eff-To               PIC X(8).
     05  Hr-Status               PIC X(12).
     05  Hr-Validation           PIC X(12).
     05  Hr-Loss-Intent          PIC X(60).
 01  Ws-Cmp-Row.
     05  Cr-Line-Num             PIC X(8).
     05  Cr-Component-Id         PIC X(12).
     05  Cr-Item                 PIC X(40).
     05  Cr-Rev                  PIC X(4).
     05  Cr-Desc                 PIC X(40).
     05  Cr-Usage-Qty            PIC X(24).
     05  Cr-Usage-Basis          PIC X(12).
     05  Cr-Uom                  PIC X(10).
     05  Cr-Stock-Uom            PIC X(10).
     05  Cr-Conv-Fact            PIC X(24).
     05  Cr-Scrap-Pct            PIC X(24).
     05  Cr-Supply               PIC X(10).
     05  Cr-Optional             PIC X(1).
     05  Cr-Selected             PIC X(1).
     05  Cr-Alt-Group            PIC X(10).
     05  Cr-Option-Ref           PIC X(20).
     05  Cr-Eff-From             PIC X(8).
     05  Cr-Eff-To               PIC X(8).
     05  Cr-Need-Offset          PIC X(8).
     05  Cr-Operation            PIC X(20).
     05  Cr-Cost-Rate            PIC X(24).
     05  Cr-Cost-Element         PIC X(10).
     05  Cr-Item-Class           PIC X(12).
     05  Cr-Make-Buy             PIC X(6).
     05  Cr-Quantum              PIC X(24).
     05  Cr-Indivisible          PIC X(1).
     05  Cr-Stocked              PIC X(1).
     05  Cr-Pack-Qty             PIC X(24).
     05  Cr-Item-Found           PIC X(1).
 01  Ws-Item-Row.
     05  Ir-Item                 PIC X(40).
     05  Ir-Desc                 PIC X(40).
     05  Ir-Stock-Uom            PIC X(10).
     05  Ir-Item-Class           PIC X(12).
     05  Ir-Make-Buy             PIC X(6).
     05  Ir-Quantum              PIC X(24).
     05  Ir-Indivisible          PIC X(1).
     05  Ir-Stocked              PIC X(1).
     05  Ir-Pack-Qty             PIC X(24).
     05  Ir-Revision             PIC X(4).
     05  Ir-Lifecycle            PIC X(10).
 LINKAGE SECTION.
 COPY BOMBILL.
 PROCEDURE DIVISION USING BOM-RD-CTL BOM-HDR-REC BOM-CMP-TAB.
 Main-Entry.
     MOVE "OK" TO BR-STATUS
     MOVE SPACES TO BR-REASON BR-MESSAGE
     EVALUATE BR-FUNC
         WHEN "HEADER"  PERFORM Read-Header
         WHEN "BILL"    PERFORM Read-Header
                        IF BR-OK
                            PERFORM Read-Components
                        END-IF
         WHEN "ITEM"    PERFORM Read-One-Item
         WHEN OTHER     MOVE "NOTFOUND" TO BR-STATUS
                        MOVE "BOM0001" TO BR-REASON
                        MOVE "Unknown BOMREAD function"
                            TO BR-MESSAGE
     END-EVALUATE
     GOBACK.

*>----------------------------------------------------------------
 Read-Header.
     INITIALIZE BOM-HDR-REC
     MOVE 0 TO BR-LINE-COUNT
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT h.BomHeaderId, h.CompanyCode, "
            "ISNULL(h.SiteCode,''), h.BomRef, h.BomRevision, "
            "h.ParentItemCode, ISNULL(h.ParentRevision,''), "
            "h.Purpose, ISNULL(h.JobRef,''), "
            "ISNULL(h.VariantCode,''), ISNULL(h.Description,''), "
            "h.NominalBasis, h.YieldFactor, h.ProductionQuantum, "
            "ISNULL(h.BatchSize,0), "
            "CASE WHEN h.IsPhantom=1 THEN 'Y' ELSE 'N' END, "
            "CASE WHEN h.ConsolidateRuns=1 THEN 'Y' ELSE 'N' END, "
            "ISNULL(h.CostingLotSize,0), "
            "ISNULL(h.SetupCostPerRun,0), "
            "ISNULL(h.ConversionCostPerStart,0), "
            "CONVERT(char(8),h.EffectiveFrom,112), "
            "ISNULL(CONVERT(char(8),h.EffectiveTo,112),'00000000'), "
            "h.Status, ISNULL(h.ValidationId,0), "
            "ISNULL(h.LossIntentNote,'') "
            "FROM bom.BomHeader h WHERE h.BomHeaderId = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 1 TO SQLIO-PARM-CNT
     MOVE BR-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE 25 TO SQLIO-COL-CNT
     MOVE 12 TO SQLIO-COL-WIDTH(1)
     MOVE 10 TO SQLIO-COL-WIDTH(2)
     MOVE 10 TO SQLIO-COL-WIDTH(3)
     MOVE 30 TO SQLIO-COL-WIDTH(4)
     MOVE 4 TO SQLIO-COL-WIDTH(5)
     MOVE 40 TO SQLIO-COL-WIDTH(6)
     MOVE 4 TO SQLIO-COL-WIDTH(7)
     MOVE 10 TO SQLIO-COL-WIDTH(8)
     MOVE 20 TO SQLIO-COL-WIDTH(9)
     MOVE 20 TO SQLIO-COL-WIDTH(10)
     MOVE 60 TO SQLIO-COL-WIDTH(11)
     MOVE 24 TO SQLIO-COL-WIDTH(12)
     MOVE 24 TO SQLIO-COL-WIDTH(13)
     MOVE 24 TO SQLIO-COL-WIDTH(14)
     MOVE 24 TO SQLIO-COL-WIDTH(15)
     MOVE 1 TO SQLIO-COL-WIDTH(16)
     MOVE 1 TO SQLIO-COL-WIDTH(17)
     MOVE 24 TO SQLIO-COL-WIDTH(18)
     MOVE 24 TO SQLIO-COL-WIDTH(19)
     MOVE 24 TO SQLIO-COL-WIDTH(20)
     MOVE 8 TO SQLIO-COL-WIDTH(21)
     MOVE 8 TO SQLIO-COL-WIDTH(22)
     MOVE 12 TO SQLIO-COL-WIDTH(23)
     MOVE 12 TO SQLIO-COL-WIDTH(24)
     MOVE 60 TO SQLIO-COL-WIDTH(25)
     MOVE 3 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Hdr-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     IF SQLIO-NOTFOUND
         MOVE "NOTFOUND" TO BR-STATUS
         MOVE "BOM0002" TO BR-REASON
         MOVE "No bill of materials holds that identity"
             TO BR-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Move-Header-Fields.

 Move-Header-Fields.
     COMPUTE BH-BOM-ID = FUNCTION NUMVAL(Hr-Bom-Id)
     MOVE Hr-Company TO BH-COMPANY
     MOVE Hr-Site TO BH-SITE
     MOVE Hr-Bom-Ref TO BH-BOM-REF
     MOVE Hr-Bom-Rev TO BH-BOM-REV
     MOVE Hr-Parent-Item TO BH-PARENT-ITEM
     MOVE Hr-Parent-Rev TO BH-PARENT-REV
     MOVE Hr-Purpose TO BH-PURPOSE
     MOVE Hr-Job-Ref TO BH-JOB-REF
     MOVE Hr-Variant TO BH-VARIANT
     MOVE Hr-Description TO BH-DESCRIPTION
     COMPUTE BH-NOMINAL-BASIS = FUNCTION NUMVAL(Hr-Basis)
     COMPUTE BH-YIELD = FUNCTION NUMVAL(Hr-Yield)
     COMPUTE BH-QUANTUM = FUNCTION NUMVAL(Hr-Quantum)
     COMPUTE BH-BATCH-SIZE = FUNCTION NUMVAL(Hr-Batch)
     MOVE Hr-Phantom TO BH-IS-PHANTOM
     MOVE Hr-Consolidate TO BH-CONSOLIDATE
     COMPUTE BH-COSTING-LOT = FUNCTION NUMVAL(Hr-Costing-Lot)
     COMPUTE BH-SETUP-COST = FUNCTION NUMVAL(Hr-Setup-Cost)
     COMPUTE BH-CONV-COST = FUNCTION NUMVAL(Hr-Conv-Cost)
     IF Hr-Eff-From IS NUMERIC
         MOVE Hr-Eff-From TO BH-EFF-FROM
     ELSE
         MOVE 0 TO BH-EFF-FROM
     END-IF
     IF Hr-Eff-To IS NUMERIC
         MOVE Hr-Eff-To TO BH-EFF-TO
     ELSE
         MOVE 0 TO BH-EFF-TO
     END-IF
     MOVE Hr-Status TO BH-STATUS
     COMPUTE BH-VALIDATION-ID = FUNCTION NUMVAL(Hr-Validation)
     MOVE Hr-Loss-Intent TO BH-LOSS-INTENT.

*>----------------------------------------------------------------
*> Every component line of the bill, in line number order, with
*> the item attributes needed to convert, round and classify it.
 Read-Components.
     MOVE 0 TO BR-LINE-COUNT
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT c.LineNum, c.BomComponentId, "
            "c.ComponentItemCode, ISNULL(c.ComponentRevision,''), "
            "ISNULL(i.Description,''), c.UsageQty, c.UsageBasis, "
            "c.UomCode, ISNULL(i.StockUomCode,''), "
            "c.ConversionFactor, c.ScrapPct, c.SupplyBehaviour, "
            "CASE WHEN c.IsOptional=1 THEN 'Y' ELSE 'N' END, "
            "CASE WHEN c.IsSelected=1 THEN 'Y' ELSE 'N' END, "
            "ISNULL(c.AlternateGroup,''), "
            "ISNULL(c.OptionChoiceRef,''), "
            "ISNULL(CONVERT(char(8),c.EffectiveFrom,112),"
            "'00000000'), "
            "ISNULL(CONVERT(char(8),c.EffectiveTo,112),'00000000'),"
            " c.NeedOffsetDays, ISNULL(c.OperationRef,''), "
            "ISNULL(c.CostOnlyRate,0), "
            "ISNULL(c.CostElementCode,''), ISNULL(i.ItemClass,''), "
            "ISNULL(i.MakeBuy,''), ISNULL(i.QuantumQty,0), "
            "CASE WHEN i.IsIndivisible=1 THEN 'Y' ELSE 'N' END, "
            "CASE WHEN i.IsStocked=1 THEN 'Y' ELSE 'N' END, "
            "ISNULL(s.PurchaseMultipleQty,0), "
            "CASE WHEN i.ItemCode IS NULL THEN 'N' ELSE 'Y' END "
            "FROM bom.BomComponent c "
            "LEFT JOIN inv.Item i ON i.CompanyCode = ? "
            "AND i.ItemCode = c.ComponentItemCode "
            "LEFT JOIN inv.ItemSite s ON s.CompanyCode = ? "
            "AND s.SiteCode = ? AND s.ItemCode = c.ComponentItemCode "
            "WHERE c.BomHeaderId = ? ORDER BY c.LineNum"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 4 TO SQLIO-PARM-CNT
     MOVE BR-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE BR-COMPANY TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE BR-SITE TO SQLIO-PARM-VAL(3)
     MOVE "N" TO SQLIO-PARM-NULL(3)
     MOVE BR-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
     MOVE "N" TO SQLIO-PARM-NULL(4)
     PERFORM Set-Component-Widths
     MOVE 4 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Cmp-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE 4 TO SQLIO-CURSOR
         MOVE "FTCH" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Cmp-Row
         EVALUATE TRUE
             WHEN SQLIO-ERROR
                 PERFORM Report-Sql-Failure
                 MOVE "N" TO Ws-More
             WHEN SQLIO-NOTFOUND
                 MOVE "N" TO Ws-More
             WHEN OTHER
                 IF BR-LINE-COUNT >= BR-MAX-LINES
                     MOVE "TOOMANY" TO BR-STATUS
                     MOVE "BOM0003" TO BR-REASON
                     MOVE SPACES TO BR-MESSAGE
                     STRING "This bill holds more component lines "
                            "than the engine can carry"
                         DELIMITED BY SIZE INTO BR-MESSAGE
                     MOVE "N" TO Ws-More
                 ELSE
                     ADD 1 TO BR-LINE-COUNT
                     MOVE BR-LINE-COUNT TO Ws-I
                     PERFORM Move-Component-Fields
                 END-IF
         END-EVALUATE
     END-PERFORM
     MOVE 4 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Cmp-Row.

 Set-Component-Widths.
     MOVE 29 TO SQLIO-COL-CNT
     MOVE 8 TO SQLIO-COL-WIDTH(1)
     MOVE 12 TO SQLIO-COL-WIDTH(2)
     MOVE 40 TO SQLIO-COL-WIDTH(3)
     MOVE 4 TO SQLIO-COL-WIDTH(4)
     MOVE 40 TO SQLIO-COL-WIDTH(5)
     MOVE 24 TO SQLIO-COL-WIDTH(6)
     MOVE 12 TO SQLIO-COL-WIDTH(7)
     MOVE 10 TO SQLIO-COL-WIDTH(8)
     MOVE 10 TO SQLIO-COL-WIDTH(9)
     MOVE 24 TO SQLIO-COL-WIDTH(10)
     MOVE 24 TO SQLIO-COL-WIDTH(11)
     MOVE 10 TO SQLIO-COL-WIDTH(12)
     MOVE 1 TO SQLIO-COL-WIDTH(13)
     MOVE 1 TO SQLIO-COL-WIDTH(14)
     MOVE 10 TO SQLIO-COL-WIDTH(15)
     MOVE 20 TO SQLIO-COL-WIDTH(16)
     MOVE 8 TO SQLIO-COL-WIDTH(17)
     MOVE 8 TO SQLIO-COL-WIDTH(18)
     MOVE 8 TO SQLIO-COL-WIDTH(19)
     MOVE 20 TO SQLIO-COL-WIDTH(20)
     MOVE 24 TO SQLIO-COL-WIDTH(21)
     MOVE 10 TO SQLIO-COL-WIDTH(22)
     MOVE 12 TO SQLIO-COL-WIDTH(23)
     MOVE 6 TO SQLIO-COL-WIDTH(24)
     MOVE 24 TO SQLIO-COL-WIDTH(25)
     MOVE 1 TO SQLIO-COL-WIDTH(26)
     MOVE 1 TO SQLIO-COL-WIDTH(27)
     MOVE 24 TO SQLIO-COL-WIDTH(28)
     MOVE 1 TO SQLIO-COL-WIDTH(29).

 Move-Component-Fields.
     COMPUTE BC-LINE-NUM(Ws-I) = FUNCTION NUMVAL(Cr-Line-Num)
     COMPUTE BC-COMPONENT-ID(Ws-I) =
         FUNCTION NUMVAL(Cr-Component-Id)
     MOVE Cr-Item TO BC-ITEM(Ws-I)
     MOVE Cr-Rev TO BC-REV(Ws-I)
     MOVE Cr-Desc TO BC-DESC(Ws-I)
     COMPUTE BC-USAGE-QTY(Ws-I) = FUNCTION NUMVAL(Cr-Usage-Qty)
     MOVE Cr-Usage-Basis TO BC-USAGE-BASIS(Ws-I)
     MOVE Cr-Uom TO BC-UOM(Ws-I)
     MOVE Cr-Stock-Uom TO BC-STOCK-UOM(Ws-I)
     COMPUTE BC-CONV-FACT(Ws-I) = FUNCTION NUMVAL(Cr-Conv-Fact)
     COMPUTE BC-SCRAP-PCT(Ws-I) = FUNCTION NUMVAL(Cr-Scrap-Pct)
     MOVE Cr-Supply TO BC-SUPPLY(Ws-I)
     MOVE Cr-Optional TO BC-OPTIONAL(Ws-I)
     MOVE Cr-Selected TO BC-SELECTED(Ws-I)
     MOVE Cr-Alt-Group TO BC-ALT-GROUP(Ws-I)
     MOVE Cr-Option-Ref TO BC-OPTION-REF(Ws-I)
     IF Cr-Eff-From IS NUMERIC
         MOVE Cr-Eff-From TO BC-EFF-FROM(Ws-I)
     ELSE
         MOVE 0 TO BC-EFF-FROM(Ws-I)
     END-IF
     IF Cr-Eff-To IS NUMERIC
         MOVE Cr-Eff-To TO BC-EFF-TO(Ws-I)
     ELSE
         MOVE 0 TO BC-EFF-TO(Ws-I)
     END-IF
     COMPUTE BC-NEED-OFFSET(Ws-I) = FUNCTION NUMVAL(Cr-Need-Offset)
     MOVE Cr-Operation TO BC-OPERATION(Ws-I)
     COMPUTE BC-COST-RATE(Ws-I) = FUNCTION NUMVAL(Cr-Cost-Rate)
     MOVE Cr-Cost-Element TO BC-COST-ELEMENT(Ws-I)
     MOVE Cr-Item-Class TO BC-ITEM-CLASS(Ws-I)
     MOVE Cr-Make-Buy TO BC-MAKE-BUY(Ws-I)
     COMPUTE BC-QUANTUM(Ws-I) = FUNCTION NUMVAL(Cr-Quantum)
     MOVE Cr-Indivisible TO BC-INDIVISIBLE(Ws-I)
     MOVE Cr-Stocked TO BC-STOCKED(Ws-I)
     COMPUTE BC-PACK-QTY(Ws-I) = FUNCTION NUMVAL(Cr-Pack-Qty)
     MOVE Cr-Item-Found TO BC-ITEM-FOUND(Ws-I).

*>----------------------------------------------------------------
*> ITEM - the attributes of one item, returned in line one of the
*> component table.  Used for the root parent of a calculation,
*> which has no component line of its own.
 Read-One-Item.
     MOVE 0 TO BR-LINE-COUNT
     INITIALIZE Ws-Cmp-Row
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT i.ItemCode, ISNULL(i.Description,''), "
            "i.StockUomCode, i.ItemClass, i.MakeBuy, i.QuantumQty, "
            "CASE WHEN i.IsIndivisible=1 THEN 'Y' ELSE 'N' END, "
            "CASE WHEN i.IsStocked=1 THEN 'Y' ELSE 'N' END, "
            "ISNULL(s.PurchaseMultipleQty,0), i.CurrentRevision, "
            "i.LifecycleStatus "
            "FROM inv.Item i LEFT JOIN inv.ItemSite s "
            "ON s.CompanyCode = i.CompanyCode AND s.SiteCode = ? "
            "AND s.ItemCode = i.ItemCode "
            "WHERE i.CompanyCode = ? AND i.ItemCode = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 3 TO SQLIO-PARM-CNT
     MOVE BR-SITE TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE BR-COMPANY TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE BR-ITEM-CODE TO SQLIO-PARM-VAL(3)
     MOVE "N" TO SQLIO-PARM-NULL(3)
     MOVE 11 TO SQLIO-COL-CNT
     MOVE 40 TO SQLIO-COL-WIDTH(1)
     MOVE 40 TO SQLIO-COL-WIDTH(2)
     MOVE 10 TO SQLIO-COL-WIDTH(3)
     MOVE 12 TO SQLIO-COL-WIDTH(4)
     MOVE 6 TO SQLIO-COL-WIDTH(5)
     MOVE 24 TO SQLIO-COL-WIDTH(6)
     MOVE 1 TO SQLIO-COL-WIDTH(7)
     MOVE 1 TO SQLIO-COL-WIDTH(8)
     MOVE 24 TO SQLIO-COL-WIDTH(9)
     MOVE 4 TO SQLIO-COL-WIDTH(10)
     MOVE 10 TO SQLIO-COL-WIDTH(11)
     MOVE 3 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Item-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     IF SQLIO-NOTFOUND
         MOVE "NOTFOUND" TO BR-STATUS
         MOVE "BOM0004" TO BR-REASON
         MOVE SPACES TO BR-MESSAGE
         STRING "Item " FUNCTION TRIM(BR-ITEM-CODE)
                " is not held in the item master"
             DELIMITED BY SIZE INTO BR-MESSAGE
         MOVE "N" TO BC-ITEM-FOUND(1)
         EXIT PARAGRAPH
     END-IF
     MOVE 1 TO BR-LINE-COUNT
     MOVE Ir-Item TO BC-ITEM(1)
     MOVE Ir-Desc TO BC-DESC(1)
     MOVE Ir-Stock-Uom TO BC-STOCK-UOM(1)
     MOVE Ir-Stock-Uom TO BC-UOM(1)
     MOVE Ir-Item-Class TO BC-ITEM-CLASS(1)
     MOVE Ir-Make-Buy TO BC-MAKE-BUY(1)
     COMPUTE BC-QUANTUM(1) = FUNCTION NUMVAL(Ir-Quantum)
     MOVE Ir-Indivisible TO BC-INDIVISIBLE(1)
     MOVE Ir-Stocked TO BC-STOCKED(1)
     COMPUTE BC-PACK-QTY(1) = FUNCTION NUMVAL(Ir-Pack-Qty)
     MOVE Ir-Revision TO BC-REV(1)
     MOVE Ir-Lifecycle TO BC-OPERATION(1)
     MOVE "Y" TO BC-ITEM-FOUND(1).

 Report-Sql-Failure.
     MOVE "SQLFAIL" TO BR-STATUS
     MOVE "SQL0001" TO BR-REASON
     MOVE SQLIO-ERRMSG TO BR-MESSAGE.
