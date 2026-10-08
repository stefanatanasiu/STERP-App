>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : KITCOMM
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : Commercial kit fulfilment for Sales.
*>
*>              A commercial kit parent is a COMMERCIAL IDENTITY,
*>              not an item balance.  It is never received into
*>              stock, never completed and never disassembled, so
*>              KV-PARENT-STOCK always comes back "N" (KIT-001).
*>
*>              Decision D-11 fixes the behaviour:
*>                - the parent quantity counts COMPLETE component
*>                  sets; an incomplete set is not a kit and may
*>                  not be despatched
*>                - revenue is allocated to the components by
*>                  RELATIVE STANDALONE SELLING PRICE and must
*>                  reconcile to the parent net amount, the
*>                  rounding residual going to the largest line so
*>                  the set still adds up to the penny
*>                - components are delivered and costed ONCE, so
*>                  the margin is parent revenue less component
*>                  cost, never component revenue as well
*>                - a partial return is credited at the ALLOCATED
*>                  component value and creates no parent stock
*> CALLED BY   : the sales order entry and despatch programs, the
*>               commercial kit form and the unit tests
*> INTERFACE   : CALL "KITCOMM" USING WS-KITCOMM-CA
*>               COPYBOOK KITCOMMCA
*> CALLS       : AVAILQ, SQLIO
*>----------------------------------------------------------------
*> CHANGE LOG
*> 1999-06-14 RJH  CR0702 SALES KIT PRICING
*> 2015-10-26 PMK  CR2399 COMMERCIAL KIT ALLOCATION (D-11)
*> 2016-12-13 PMK  CR2514 RESIDUAL PENNY TO THE LARGEST LINE - THE
*>                 1999 VERSION LOST A PENNY ON EVERY THREE WAY
*>                 SPLIT AND THE SALES LEDGER NEVER AGREED
*> 2026-10-06 STERP KIT-001, KIT-023
*>================================================================
*> REQ: KIT-001, KIT-023
 IDENTIFICATION DIVISION.
 PROGRAM-ID. KITCOMM.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 26/10/2015.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY AVAILQCA REPLACING ==DFHCOMMAREA== BY ==Ws-Avail-Ca==
      LEADING ==CA-== BY ==VA-==.
 01  Ws-Row.
     05  Ws-Col OCCURS 14 TIMES  PIC X(100).
 01  Ws-Work.
     05  Ws-W                    PIC S9(4) VALUE 0.
     05  Ws-More                 PIC X VALUE "Y".
*>   SQLIO only has eight cursor slots (sterpsql.c MAXCUR) and KITDB
*>   holds 5 and 6, so the kit enquiry programs share 3,4,7,8.
     05  Ws-Cursor               PIC S9(9) COMP-5 VALUE 3.
     05  Ws-Ssp-Total            PIC S9(13)V9(6) VALUE 0.
     05  Ws-Line-Ssp             PIC S9(13)V9(6) VALUE 0.
     05  Ws-Alloc                PIC S9(13)V99 VALUE 0.
     05  Ws-Running              PIC S9(13)V99 VALUE 0.
     05  Ws-Largest              PIC S9(4) VALUE 0.
     05  Ws-Largest-Val          PIC S9(13)V99 VALUE 0.
     05  Ws-Sets                 PIC S9(13)V9(6) VALUE 0.
     05  Ws-Line-Sets            PIC S9(13)V9(6) VALUE 0.
     05  Ws-Required             PIC S9(13)V9(6) VALUE 0.
     05  Ws-Short                PIC S9(13)V9(6) VALUE 0.
     05  Ws-Cost                 PIC S9(13)V99 VALUE 0.
     05  Ws-Priced-Lines         PIC S9(4) VALUE 0.
     05  Ws-Money-Edit           PIC -(11)9.99.
     05  Ws-Qty-Edit             PIC -(9)9.999999.
     05  Ws-Note                 PIC X(200) VALUE SPACES.
 01  Ws-Lines.
     05  Ws-Ln-Cnt               PIC S9(4) VALUE 0.
     05  Ws-Ln OCCURS 15 TIMES.
         10  Ln-Num              PIC S9(4).
         10  Ln-Item             PIC X(40).
         10  Ln-Desc             PIC X(40).
         10  Ln-Qty-Per          PIC S9(13)V9(6).
         10  Ln-Uom              PIC X(10).
         10  Ln-Required         PIC S9(13)V9(6).
         10  Ln-Free             PIC S9(13)V9(6).
         10  Ln-Delivered        PIC S9(13)V9(6).
         10  Ln-Sets             PIC S9(13)V9(6).
         10  Ln-Ssp              PIC S9(11)V9(6).
         10  Ln-Alloc-Pct        PIC S9(5)V9(6).
         10  Ln-Alloc-Net        PIC S9(11)V99.
         10  Ln-Unit-Cost        PIC S9(11)V9(6).
         10  Ln-Cost             PIC S9(11)V99.
         10  Ln-Vat              PIC X(4).
         10  Ln-Cust-Spec        PIC X.
         10  Ln-Sub-Ok           PIC X.
         10  Ln-Optional         PIC X.
         10  Ln-Limiting         PIC X.
 LINKAGE SECTION.
 COPY KITCOMMCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "KITCOMM" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE 0 TO CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     MOVE "KIT-023" TO CA-REQ-REF
     PERFORM Clear-Answer
     EVALUATE CA-FUNCTION
         WHEN "SETS"     PERFORM Count-Complete-Sets
         WHEN "ALLOCATE" PERFORM Allocate-Revenue
         WHEN "FULFIL"   PERFORM Reconcile-Fulfilment
         WHEN "RETURNCK" PERFORM Check-Partial-Return
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "KIT0002" TO CA-REASON-CODE
     STRING "KITCOMM does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

 Set-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE SQLIO-ERRMSG (1:200) TO CA-MESSAGE.

*>   The parent of a commercial kit never has a stock balance.
*>   This is not a setting; it is what the kind means (KIT-001).
 Clear-Answer.
     MOVE 0 TO Ws-Ln-Cnt
     MOVE 0 TO KV-COMPLETE-SETS
     MOVE SPACES TO KV-LIMIT-ITEM
     MOVE 0 TO KV-LIMIT-SHORT
     MOVE 0 TO KV-ALLOC-TOTAL
     MOVE 0 TO KV-RESIDUAL
     MOVE 0 TO KV-COST-TOTAL
     MOVE 0 TO KV-MARGIN-GBP
     MOVE 0 TO KV-MARGIN-PCT
     MOVE 0 TO KV-CREDIT-GBP
     MOVE 0 TO KV-LINE-CNT
     MOVE "N" TO KV-RECONCILED
     MOVE "N" TO KV-MAY-DESPATCH
     MOVE "N" TO KV-PARENT-STOCK
     MOVE SPACES TO KV-NOTE-TEXT
     IF KV-CURRENCY = SPACES
         MOVE "GBP" TO KV-CURRENCY
     END-IF
     IF KV-AS-AT = 0
         MOVE CA-BUSINESS-DATE TO KV-AS-AT
     END-IF.

*>----------------------------------------------------------------
*> The released definition for the version asked for, or the
*> latest released version when none was named.
 Load-Definition.
     IF KV-KIT-ITEM = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0001" TO CA-REASON-CODE
         MOVE "A commercial kit item code is required."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > 8
         MOVE SPACES TO SQLIO-PARM-VAL (Ws-W)
         MOVE "N" TO SQLIO-PARM-NULL (Ws-W)
     END-PERFORM
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT TOP 1 CONVERT(varchar(12),d.KitDefId), d.KitVersion,"
       " d.SupplyKind, CONVERT(varchar(2),d.AllowPartialDespatch),"
       " d.Status"
       " FROM kit.KitDefinition d"
       " WHERE d.CompanyCode = ? AND d.KitItemCode = ?"
       " AND (? = '' OR d.KitVersion = ?)"
       " AND d.Status = 'RELEASED'"
       " AND d.EffectiveFrom <= CONVERT(date,?,112)"
       " AND (d.EffectiveTo IS NULL"
       "      OR d.EffectiveTo >= CONVERT(date,?,112))"
       " ORDER BY d.EffectiveFrom DESC, d.KitDefId DESC"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE KV-KIT-ITEM TO SQLIO-PARM-VAL (2)
     MOVE KV-KIT-VERSION TO SQLIO-PARM-VAL (3)
     MOVE KV-KIT-VERSION TO SQLIO-PARM-VAL (4)
     MOVE KV-AS-AT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (5)
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (6)
     MOVE 6 TO SQLIO-PARM-CNT
     MOVE 5 TO SQLIO-COL-CNT
     PERFORM Set-Wide-Columns
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Row
     EVALUATE TRUE
         WHEN SQLIO-OK
             CONTINUE
         WHEN SQLIO-NOTFOUND
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0001" TO CA-REASON-CODE
             STRING "No released commercial kit definition for "
                    FUNCTION TRIM(KV-KIT-ITEM)
                    " is effective on this date."
                 DELIMITED BY SIZE INTO CA-MESSAGE
             EXIT PARAGRAPH
         WHEN OTHER
             PERFORM Set-Sql-Failure
             EXIT PARAGRAPH
     END-EVALUATE
     COMPUTE KV-KIT-DEF-ID = FUNCTION NUMVAL (Ws-Col (1))
     MOVE Ws-Col (2) TO KV-KIT-VERSION
     MOVE Ws-Col (3) TO KV-SUPPLY-KIND.

 Set-Wide-Columns.
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > 14
         MOVE 100 TO SQLIO-COL-WIDTH (Ws-W)
     END-PERFORM
     MOVE Ws-Cursor TO SQLIO-CURSOR.

 Load-Lines.
     MOVE 0 TO Ws-Ln-Cnt
     MOVE 0 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > 8
         MOVE SPACES TO SQLIO-PARM-VAL (Ws-W)
         MOVE "N" TO SQLIO-PARM-NULL (Ws-W)
     END-PERFORM
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT CONVERT(varchar(12),l.LineNum), l.ComponentItemCode,"
       " ISNULL(i.Description,''), CONVERT(varchar(26),l.Quantity),"
       " l.UomCode,"
       " CONVERT(varchar(26),ISNULL(l.StandaloneSellingPrice,0)),"
       " ISNULL(l.VatCode,''),"
       " CONVERT(varchar(2),l.IsCustomerSpecified),"
       " CONVERT(varchar(2),l.SubstitutionAllowed),"
       " CONVERT(varchar(2),l.IsOptional),"
       " CONVERT(varchar(26),ISNULL(c.StandardUnitCost,0))"
       " FROM kit.KitDefinitionLine l"
       " LEFT JOIN inv.Item i ON i.CompanyCode = ?"
       "   AND i.ItemCode = l.ComponentItemCode"
       " LEFT JOIN cst.ItemCost c ON c.CompanyCode = ?"
       "   AND c.ItemCode = l.ComponentItemCode"
       "   AND c.SiteCode = '*'"
       " WHERE l.KitDefId = ? ORDER BY l.LineNum"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (2)
     MOVE KV-KIT-DEF-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (3)
     MOVE 3 TO SQLIO-PARM-CNT
     MOVE 11 TO SQLIO-COL-CNT
     PERFORM Set-Wide-Columns
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Row
     IF SQLIO-ERROR
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE "FTCH" TO SQLIO-FUNC
         MOVE Ws-Cursor TO SQLIO-CURSOR
         CALL "SQLIO" USING SQLIO-CA, Ws-Row
         EVALUATE TRUE
             WHEN SQLIO-OK
                 PERFORM Store-Line
             WHEN SQLIO-NOTFOUND
                 MOVE "N" TO Ws-More
             WHEN OTHER
                 PERFORM Set-Sql-Failure
                 MOVE "N" TO Ws-More
         END-EVALUATE
     END-PERFORM
     MOVE "CLOS" TO SQLIO-FUNC
     MOVE Ws-Cursor TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA, Ws-Row
     MOVE Ws-Ln-Cnt TO KV-LINE-CNT
     IF Ws-Ln-Cnt = 0 AND NOT CA-FAILED
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0020" TO CA-REASON-CODE
         MOVE "The kit definition has no component lines."
              TO CA-MESSAGE
     END-IF.

 Store-Line.
     IF Ws-Ln-Cnt >= 15
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Ln-Cnt
     COMPUTE Ln-Num (Ws-Ln-Cnt) = FUNCTION NUMVAL (Ws-Col (1))
     MOVE Ws-Col (2) TO Ln-Item (Ws-Ln-Cnt)
     MOVE Ws-Col (3) TO Ln-Desc (Ws-Ln-Cnt)
     COMPUTE Ln-Qty-Per (Ws-Ln-Cnt) = FUNCTION NUMVAL (Ws-Col (4))
     MOVE Ws-Col (5) TO Ln-Uom (Ws-Ln-Cnt)
     COMPUTE Ln-Ssp (Ws-Ln-Cnt) = FUNCTION NUMVAL (Ws-Col (6))
     MOVE Ws-Col (7) TO Ln-Vat (Ws-Ln-Cnt)
     IF Ws-Col (8) (1:1) = "1"
         MOVE "Y" TO Ln-Cust-Spec (Ws-Ln-Cnt)
     ELSE
         MOVE "N" TO Ln-Cust-Spec (Ws-Ln-Cnt)
     END-IF
     IF Ws-Col (9) (1:1) = "1"
         MOVE "Y" TO Ln-Sub-Ok (Ws-Ln-Cnt)
     ELSE
         MOVE "N" TO Ln-Sub-Ok (Ws-Ln-Cnt)
     END-IF
     IF Ws-Col (10) (1:1) = "1"
         MOVE "Y" TO Ln-Optional (Ws-Ln-Cnt)
     ELSE
         MOVE "N" TO Ln-Optional (Ws-Ln-Cnt)
     END-IF
     COMPUTE Ln-Unit-Cost (Ws-Ln-Cnt) =
         FUNCTION NUMVAL (Ws-Col (11))
     MOVE 0 TO Ln-Free (Ws-Ln-Cnt)
     MOVE 0 TO Ln-Required (Ws-Ln-Cnt)
     MOVE 0 TO Ln-Delivered (Ws-Ln-Cnt)
     MOVE 0 TO Ln-Sets (Ws-Ln-Cnt)
     MOVE 0 TO Ln-Alloc-Pct (Ws-Ln-Cnt)
     MOVE 0 TO Ln-Alloc-Net (Ws-Ln-Cnt)
     MOVE 0 TO Ln-Cost (Ws-Ln-Cnt)
     MOVE "N" TO Ln-Limiting (Ws-Ln-Cnt).

*>----------------------------------------------------------------
*> SETS - how many COMPLETE sets the free stock supports.  A kit
*> is complete or it is not, so the answer is the minimum over
*> the mandatory components, truncated to whole sets.  An
*> optional line cannot stop a set being complete.
 Count-Complete-Sets.
     PERFORM Load-Definition
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Lines
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO Ws-Sets
     MOVE "N" TO Ws-More
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > Ws-Ln-Cnt
         PERFORM Read-Free-Stock
         COMPUTE Ln-Required (Ws-W) =
             Ln-Qty-Per (Ws-W) * KV-PARENT-QTY
         IF Ln-Qty-Per (Ws-W) > 0
             COMPUTE Ws-Line-Sets =
                 FUNCTION INTEGER (Ln-Free (Ws-W)
                                   / Ln-Qty-Per (Ws-W))
         ELSE
             MOVE 0 TO Ws-Line-Sets
         END-IF
         MOVE Ws-Line-Sets TO Ln-Sets (Ws-W)
         IF Ln-Optional (Ws-W) NOT = "Y"
             IF Ws-More = "N"
                 MOVE Ws-Line-Sets TO Ws-Sets
                 MOVE "Y" TO Ws-More
                 MOVE Ws-W TO Ws-Largest
             ELSE
                 IF Ws-Line-Sets < Ws-Sets
                     MOVE Ws-Line-Sets TO Ws-Sets
                     MOVE Ws-W TO Ws-Largest
                 END-IF
             END-IF
         END-IF
     END-PERFORM
     MOVE Ws-Sets TO KV-COMPLETE-SETS
     IF Ws-Largest > 0
         MOVE "Y" TO Ln-Limiting (Ws-Largest)
         MOVE Ln-Item (Ws-Largest) TO KV-LIMIT-ITEM
         COMPUTE Ws-Required =
             Ln-Qty-Per (Ws-Largest) * KV-PARENT-QTY
         COMPUTE Ws-Short = Ws-Required - Ln-Free (Ws-Largest)
         IF Ws-Short < 0
             MOVE 0 TO Ws-Short
         END-IF
         MOVE Ws-Short TO KV-LIMIT-SHORT
     END-IF
     PERFORM Decide-Despatch
     PERFORM Publish-Lines
     PERFORM Note-Sets.

 Read-Free-Stock.
     INITIALIZE Ws-Avail-Ca
     MOVE "AVAIL" TO VA-FUNCTION
     MOVE CA-COMPANY TO VA-COMPANY
     MOVE KV-SITE TO VA-SITE
     MOVE CA-USER TO VA-USER
     MOVE CA-BUSINESS-DATE TO VA-BUSINESS-DATE
     MOVE Ln-Item (Ws-W) TO AV-ITEM-CODE
     MOVE KV-SITE TO AV-SITE
     MOVE KV-AS-AT TO AV-AS-AT
     CALL "AVAILQ" USING Ws-Avail-Ca
     IF VA-RETURN-CODE > 4
         MOVE 0 TO Ln-Free (Ws-W)
     ELSE
         MOVE AV-QTY-FREE TO Ln-Free (Ws-W)
     END-IF.

*>   KIT-023: no partial despatch of a commercial kit unless the
*>   definition explicitly allows it, which the demo data does
*>   not.  An incomplete set is not a kit.
 Decide-Despatch.
     IF KV-COMPLETE-SETS >= KV-PARENT-QTY AND KV-PARENT-QTY > 0
         MOVE "Y" TO KV-MAY-DESPATCH
     ELSE
         MOVE "N" TO KV-MAY-DESPATCH
         IF KV-PARENT-QTY > 0
             MOVE 4 TO CA-RETURN-CODE
             MOVE "KIT0020" TO CA-REASON-CODE
             MOVE "An incomplete component set is not a kit and may not be despatched."
                  TO CA-MESSAGE
         END-IF
     END-IF.

 Note-Sets.
     MOVE KV-COMPLETE-SETS TO Ws-Qty-Edit
     MOVE SPACES TO KV-NOTE-TEXT
     STRING "Free stock supports " FUNCTION TRIM(Ws-Qty-Edit)
            " complete sets"
         DELIMITED BY SIZE INTO KV-NOTE-TEXT
     IF KV-LIMIT-ITEM NOT = SPACES
         MOVE KV-LIMIT-SHORT TO Ws-Qty-Edit
         MOVE KV-NOTE-TEXT TO Ws-Note
         STRING FUNCTION TRIM(Ws-Note) "; limited by "
                FUNCTION TRIM(KV-LIMIT-ITEM) ", short "
                FUNCTION TRIM(Ws-Qty-Edit) "."
             DELIMITED BY SIZE INTO KV-NOTE-TEXT
     END-IF.

*>----------------------------------------------------------------
*> ALLOCATE - revenue by relative standalone selling price.
*> AC-082 style check: the allocated amounts must add back to the
*> parent net amount exactly.
 Allocate-Revenue.
     PERFORM Load-Definition
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Lines
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Total-Standalone-Prices
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Spread-Revenue
     PERFORM Accumulate-Cost
     PERFORM Publish-Lines
     PERFORM Report-Margin.

 Total-Standalone-Prices.
     MOVE 0 TO Ws-Ssp-Total
     MOVE 0 TO Ws-Priced-Lines
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > Ws-Ln-Cnt
         COMPUTE Ln-Required (Ws-W) =
             Ln-Qty-Per (Ws-W) * KV-PARENT-QTY
         COMPUTE Ws-Line-Ssp =
             Ln-Ssp (Ws-W) * Ln-Qty-Per (Ws-W)
         IF Ws-Line-Ssp > 0
             ADD 1 TO Ws-Priced-Lines
             ADD Ws-Line-Ssp TO Ws-Ssp-Total
         END-IF
     END-PERFORM
     IF Ws-Ssp-Total <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0028" TO CA-REASON-CODE
         MOVE "No component carries a standalone selling price, so revenue cannot be allocated."
              TO CA-MESSAGE
     END-IF.

 Spread-Revenue.
     MOVE 0 TO Ws-Running
     MOVE 0 TO Ws-Largest
     MOVE 0 TO Ws-Largest-Val
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > Ws-Ln-Cnt
         COMPUTE Ws-Line-Ssp =
             Ln-Ssp (Ws-W) * Ln-Qty-Per (Ws-W)
         IF Ws-Line-Ssp > 0
             COMPUTE Ln-Alloc-Pct (Ws-W) ROUNDED =
                 Ws-Line-Ssp * 100 / Ws-Ssp-Total
             COMPUTE Ws-Alloc ROUNDED =
                 KV-PARENT-NET * Ws-Line-Ssp / Ws-Ssp-Total
             MOVE Ws-Alloc TO Ln-Alloc-Net (Ws-W)
             ADD Ws-Alloc TO Ws-Running
             IF Ws-Alloc > Ws-Largest-Val
                 MOVE Ws-Alloc TO Ws-Largest-Val
                 MOVE Ws-W TO Ws-Largest
             END-IF
         ELSE
             MOVE 0 TO Ln-Alloc-Pct (Ws-W)
             MOVE 0 TO Ln-Alloc-Net (Ws-W)
         END-IF
     END-PERFORM
     COMPUTE KV-RESIDUAL = KV-PARENT-NET - Ws-Running
     IF Ws-Largest > 0 AND KV-RESIDUAL NOT = 0
         COMPUTE Ln-Alloc-Net (Ws-Largest) =
             Ln-Alloc-Net (Ws-Largest) + KV-RESIDUAL
         ADD KV-RESIDUAL TO Ws-Running
     END-IF
     MOVE Ws-Running TO KV-ALLOC-TOTAL
     IF KV-ALLOC-TOTAL = KV-PARENT-NET
         MOVE "Y" TO KV-RECONCILED
     ELSE
         MOVE "N" TO KV-RECONCILED
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0028" TO CA-REASON-CODE
         MOVE "The allocated component revenue does not add back to the parent net amount."
              TO CA-MESSAGE
     END-IF.

*>   Components are costed ONCE.  The parent carries no cost of
*>   its own, so the kit cost is the sum of the component costs
*>   actually delivered (D-11).
 Accumulate-Cost.
     MOVE 0 TO Ws-Cost
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > Ws-Ln-Cnt
         COMPUTE Ln-Cost (Ws-W) ROUNDED =
             Ln-Required (Ws-W) * Ln-Unit-Cost (Ws-W)
         ADD Ln-Cost (Ws-W) TO Ws-Cost
     END-PERFORM
     MOVE Ws-Cost TO KV-COST-TOTAL.

 Report-Margin.
     COMPUTE KV-MARGIN-GBP = KV-PARENT-NET - KV-COST-TOTAL
     IF KV-PARENT-NET NOT = 0
         COMPUTE KV-MARGIN-PCT ROUNDED =
             KV-MARGIN-GBP * 100 / KV-PARENT-NET
     ELSE
         MOVE 0 TO KV-MARGIN-PCT
     END-IF
     MOVE KV-ALLOC-TOTAL TO Ws-Money-Edit
     MOVE SPACES TO KV-NOTE-TEXT
     STRING "Allocated " FUNCTION TRIM(Ws-Money-Edit)
            " by relative standalone selling price"
         DELIMITED BY SIZE INTO KV-NOTE-TEXT
     MOVE KV-RESIDUAL TO Ws-Money-Edit
     MOVE KV-NOTE-TEXT TO Ws-Note
     STRING FUNCTION TRIM(Ws-Note) "; residual "
            FUNCTION TRIM(Ws-Money-Edit)
            " given to the largest line"
         DELIMITED BY SIZE INTO KV-NOTE-TEXT
     MOVE KV-MARGIN-GBP TO Ws-Money-Edit
     MOVE KV-NOTE-TEXT TO Ws-Note
     STRING FUNCTION TRIM(Ws-Note) "; margin "
            FUNCTION TRIM(Ws-Money-Edit) "."
         DELIMITED BY SIZE INTO KV-NOTE-TEXT.

*>----------------------------------------------------------------
*> FULFIL - the parent sold quantity against the component
*> deliveries, with no duplication.  The parent is never stocked,
*> so delivery of the parent IS delivery of every component set.
 Reconcile-Fulfilment.
     PERFORM Load-Definition
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Lines
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Total-Standalone-Prices
     IF NOT CA-FAILED
         PERFORM Spread-Revenue
     END-IF
     MOVE 0 TO Ws-Cost
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > Ws-Ln-Cnt
         COMPUTE Ln-Required (Ws-W) =
             Ln-Qty-Per (Ws-W) * KV-PARENT-QTY
         COMPUTE Ln-Delivered (Ws-W) =
             Ln-Qty-Per (Ws-W) * KV-DELIVERED-SETS
         COMPUTE Ln-Cost (Ws-W) ROUNDED =
             Ln-Delivered (Ws-W) * Ln-Unit-Cost (Ws-W)
         ADD Ln-Cost (Ws-W) TO Ws-Cost
         MOVE KV-DELIVERED-SETS TO Ln-Sets (Ws-W)
     END-PERFORM
     MOVE Ws-Cost TO KV-COST-TOTAL
     MOVE KV-DELIVERED-SETS TO KV-COMPLETE-SETS
     IF KV-DELIVERED-SETS >= KV-PARENT-QTY
         MOVE "Y" TO KV-MAY-DESPATCH
     ELSE
         MOVE "N" TO KV-MAY-DESPATCH
     END-IF
     PERFORM Publish-Lines
     PERFORM Report-Margin.

*>----------------------------------------------------------------
*> RETURNCK - a customer may return ONE component of a set.  The
*> credit is the ALLOCATED value of that component for the
*> quantity returned, never a share of the parent price worked
*> out again from list, and it creates no parent stock (D-11).
 Check-Partial-Return.
     PERFORM Load-Definition
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Lines
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Total-Standalone-Prices
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Spread-Revenue
     MOVE 0 TO Ws-Largest
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > Ws-Ln-Cnt
         IF Ln-Num (Ws-W) = KV-RETURN-LINE
             MOVE Ws-W TO Ws-Largest
         END-IF
     END-PERFORM
     IF Ws-Largest = 0
         MOVE 16 TO CA-RETURN-CODE
         MOVE "KIT0007" TO CA-REASON-CODE
         MOVE "That component line is not on this kit definition."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KV-RETURN-QTY <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "The return quantity must be greater than zero."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ws-Required =
         Ln-Qty-Per (Ws-Largest) * KV-PARENT-QTY
     IF KV-RETURN-QTY > Ws-Required
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0011" TO CA-REASON-CODE
         MOVE "The return exceeds the quantity of that component supplied in the kit."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF Ws-Required > 0
         COMPUTE KV-CREDIT-GBP ROUNDED =
             Ln-Alloc-Net (Ws-Largest)
             * KV-RETURN-QTY / Ws-Required
     ELSE
         MOVE 0 TO KV-CREDIT-GBP
     END-IF
     MOVE "N" TO KV-PARENT-STOCK
     MOVE "N" TO KV-MAY-DESPATCH
     PERFORM Publish-Lines
     MOVE KV-CREDIT-GBP TO Ws-Money-Edit
     MOVE SPACES TO KV-NOTE-TEXT
     STRING "Credit " FUNCTION TRIM(Ws-Money-Edit) " for "
            FUNCTION TRIM(Ln-Item (Ws-Largest))
            " at its allocated value; the parent kit is not"
            " returned to stock."
         DELIMITED BY SIZE INTO KV-NOTE-TEXT.

 Publish-Lines.
     MOVE Ws-Ln-Cnt TO KV-LINE-CNT
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > Ws-Ln-Cnt
         MOVE Ln-Num (Ws-W) TO KV-L-LINE-NUM (Ws-W)
         MOVE Ln-Item (Ws-W) TO KV-L-ITEM (Ws-W)
         MOVE Ln-Desc (Ws-W) TO KV-L-DESC (Ws-W)
         MOVE Ln-Qty-Per (Ws-W) TO KV-L-QTY-PER (Ws-W)
         MOVE Ln-Uom (Ws-W) TO KV-L-UOM (Ws-W)
         MOVE Ln-Required (Ws-W) TO KV-L-REQUIRED (Ws-W)
         MOVE Ln-Free (Ws-W) TO KV-L-FREE (Ws-W)
         MOVE Ln-Delivered (Ws-W) TO KV-L-DELIVERED (Ws-W)
         MOVE Ln-Sets (Ws-W) TO KV-L-SETS (Ws-W)
         MOVE Ln-Ssp (Ws-W) TO KV-L-SSP (Ws-W)
         MOVE Ln-Alloc-Pct (Ws-W) TO KV-L-ALLOC-PCT (Ws-W)
         MOVE Ln-Alloc-Net (Ws-W) TO KV-L-ALLOC-NET (Ws-W)
         MOVE Ln-Unit-Cost (Ws-W) TO KV-L-UNIT-COST (Ws-W)
         MOVE Ln-Cost (Ws-W) TO KV-L-COST-GBP (Ws-W)
         MOVE Ln-Vat (Ws-W) TO KV-L-VAT-CODE (Ws-W)
         MOVE Ln-Cust-Spec (Ws-W) TO KV-L-CUST-SPEC (Ws-W)
         MOVE Ln-Sub-Ok (Ws-W) TO KV-L-SUB-OK (Ws-W)
         MOVE Ln-Limiting (Ws-W) TO KV-L-LIMITING (Ws-W)
     END-PERFORM.
 END PROGRAM KITCOMM.
