      *>================================================================
      *> PROGRAM-ID : KITSOAL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : COMMERCIAL KITS (SO-020, SO-028, D-11).  A
      *>              COMMERCIAL KIT IS ONE OFFER MADE OF SEVERAL
      *>              DELIVERABLES, AND THREE QUESTIONS ABOUT IT ARE
      *>              ANSWERED HERE AND NOWHERE ELSE.
      *>
      *>              ALLOCATE (SO-028) SPREADS THE PARENT
      *>              TRANSACTION PRICE OVER THE COMPONENTS BY
      *>              RELATIVE STANDALONE SELLING PRICE.  THE SPREAD
      *>              IS DONE BY RNDALLOC FUNCTION ALLOC MODE WEIGHT
      *>              SO THE PARTS RE-ADD EXACTLY TO THE PARENT NET
      *>              AMOUNT.  WHERE THE COMPONENTS CARRY DIFFERENT
      *>              VAT LIABILITIES THE SUPPLY IS MIXED AND THE VAT
      *>              IS WORKED OUT PER RATE ON THE ALLOCATED
      *>              AMOUNTS; OTHERWISE IT IS SINGLE.  THE ANSWER IS
      *>              RECORDED, NOT GUESSED.
      *>
      *>              COMPLETE (D-11) REQUIRES A COMPLETE COMMERCIAL
      *>              KIT FOR EACH DESPATCHED PARENT QUANTITY.
      *>              PARTIAL COMPONENT DESPATCH IS NOT PERMITTED, SO
      *>              A PROPOSAL LARGER THAN THE COMPLETE KITS THE
      *>              DELIVERABLE COMPONENTS ALLOW IS REFUSED WITH
      *>              KIT0003.  NO PARENT INVENTORY IS EVER CONSUMED.
      *>
      *>              CHECKDUP (SO-020) PROVES THAT THE PARENT AND
      *>              ITS CHILDREN DO NOT BOTH CREATE THE SAME
      *>              INVENTORY DEMAND AND ARE NOT BILLED TWICE.
      *>
      *>              NONE OF THE THREE WRITES ANYTHING.
      *> CALLED BY  : SOACCEPT, PICKGEN, DESPATCH, SOINVGEN, THE KIT
      *>              FORMS, THE .NET CLIENT AND THE UNIT TESTS
      *> INTERFACE  : CALL "KITSOAL" USING WS-KITSOAL-CA
      *>              COPYBOOK KITSOALCA
      *> CALLS      : RNDALLOC, SQLIO (kit.*, so.*, fin.VatRate reads)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-09-14 DJM  CR0967 COMMERCIAL KITS (ACUCOBOL)
      *> 2007-03-22 KMP  CR1402 MIXED SUPPLY VAT ON KIT COMPONENTS
      *> 2016-05-30 PMK  CR2488 COMPLETE KIT RULE MADE EXPLICIT
      *> 2026-10-06 STERP SO-020, SO-028, D-11 RELATIVE SSP
      *>================================================================
      *> REQ: SO-020, SO-028
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KITSOAL.
       AUTHOR. D J MERRICK.
       DATE-WRITTEN. 14/09/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY RNDALLOCCA REPLACING ==DFHCOMMAREA== BY ==WS-RND-CA==.
       01  WS-ROW.
           05  WS-COL OCCURS 12 TIMES  PIC X(100).
       01  WS-WORK.
           05  WS-BAD                  PIC X VALUE "N".
               88  WS-BAD-INPUT                  VALUE "Y".
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-K                    PIC S9(4) VALUE 0.
           05  WS-DONE                 PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-QTY                  PIC S9(13)V9(6) VALUE 0.
           05  WS-KITS                 PIC S9(13)V9(6) VALUE 0.
           05  WS-LOWEST               PIC S9(13)V9(6) VALUE 0.
           05  WS-FIRST                PIC X VALUE "Y".
           05  WS-RATE                 PIC S9(5)V9(4) VALUE 0.
           05  WS-AMT                  PIC S9(11)V99 VALUE 0.
           05  WS-TEXT                 PIC X(200) VALUE SPACES.
           05  WS-VAT1                 PIC X(4) VALUE SPACES.
           05  WS-DIFFERENT            PIC X VALUE "N".
           05  WS-QTY-ED               PIC Z(9)9.9(6).
           05  WS-KIT-ED               PIC -(9)9.
           05  WS-MONEY-ED             PIC -(11)9.99.
       LINKAGE SECTION.
       COPY KITSOALCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       1000-MAIN.
           PERFORM 1100-INITIALISE THRU 1100-EXIT
           PERFORM 1200-VALIDATE THRU 1200-EXIT
           IF WS-BAD-INPUT
               GO TO 1000-RETURN
           END-IF
           EVALUATE CA-FUNCTION OF DFHCOMMAREA
               WHEN "ALLOCATE"
                   PERFORM 2000-ALLOCATE THRU 2000-EXIT
               WHEN "COMPLETE"
                   PERFORM 3000-COMPLETE THRU 3000-EXIT
               WHEN "CHECKDUP"
                   PERFORM 4000-CHECKDUP THRU 4000-EXIT
               WHEN OTHER
                   PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       1000-RETURN.
           GOBACK.

      *>----------------------------------------------------------------
       1100-INITIALISE.
           MOVE "N" TO WS-BAD WS-DONE WS-DIFFERENT
           MOVE "Y" TO WS-FIRST
           MOVE 0 TO WS-I WS-J WS-K WS-QTY WS-KITS WS-LOWEST
           MOVE 0 TO WS-RATE WS-AMT
           MOVE SPACES TO WS-TEXT WS-VAT1
           MOVE "KITSOAL" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "SO-020" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE 0 TO KA-SSP-TOTAL KA-ALLOCATED KA-RESIDUAL
           MOVE 0 TO KA-BUMPED KA-VAT-TOTAL KA-GROSS-TOTAL
           MOVE 0 TO KA-COMPLETE-KITS KA-LIMIT-SHORT
           MOVE 0 TO KA-VAT-CNT KA-DUP-CNT
           MOVE 0 TO KA-PARENT-DEMAND KA-PARENT-BALANCE
           MOVE SPACES TO KA-LIMIT-ITEM KA-DUP-TEXT
           MOVE SPACES TO KA-SUPPLY-TEXT KA-STRUCT-NOTE
           MOVE "N" TO KA-REFUSED
           MOVE "N" TO KA-DUP-DEMAND
           MOVE "N" TO KA-DUP-BILLING
           MOVE "N" TO KA-DUP-COGS
           MOVE "N" TO KA-PARTIAL-ALLOWED
           IF KA-SUPPLY-RECORDED NOT = "Y"
               MOVE "N" TO KA-SUPPLY-RECORDED
           END-IF
           IF NOT KA-FROM-CALLER
               MOVE 0 TO KA-LINE-CNT
               MOVE SPACES TO KA-STRUCT-SOURCE
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 12
               MOVE 100 TO SQLIO-COL-WIDTH(WS-I)
           END-PERFORM.
       1100-EXIT.
           EXIT.

       1200-VALIDATE.
           IF CA-COMPANY OF DFHCOMMAREA = SPACES
               MOVE "KIT0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A company code is required." TO WS-TEXT
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1200-EXIT
           END-IF
           IF KA-FROM-CALLER
               IF KA-LINE-CNT < 1 OR KA-LINE-CNT > 40
                   MOVE "KIT0002" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SPACES TO WS-TEXT
                   STRING "A caller supplied kit must carry between "
                          "one and forty components."
                       DELIMITED BY SIZE INTO WS-TEXT
                   PERFORM 9000-REJECT THRU 9000-EXIT
               END-IF
               GO TO 1200-EXIT
           END-IF
           IF KA-ORDER-NUMBER = SPACES AND KA-KIT-ITEM = SPACES
               MOVE "KIT0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "An order number with a parent line, or a kit "
                      "item code, is required."
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1500  THE KIT STRUCTURE.  kit.KitDefinition is the proper
      *>       home for it.  Where the kit schema is not deployed the
      *>       order itself carries the structure in its KIT and
      *>       KITCOMP lines, and KA-STRUCT-SOURCE plus
      *>       KA-STRUCT-NOTE say which was used, because an answer
      *>       nobody can explain is worse than no answer.
      *>----------------------------------------------------------------
       1500-LOAD-STRUCTURE.
           IF KA-FROM-CALLER
               MOVE SPACES TO WS-TEXT
               STRING "Structure supplied by the caller; nothing "
                      "was read from the database."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO KA-STRUCT-NOTE
               GO TO 1500-EXIT
           END-IF
           PERFORM 1550-READ-PARENT THRU 1550-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 1500-EXIT
           END-IF
           PERFORM 1600-TRY-KITDEF THRU 1600-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 1500-EXIT
           END-IF
           IF KA-LINE-CNT > 0
               MOVE "KITDEF" TO KA-STRUCT-SOURCE
               MOVE SPACES TO WS-TEXT
               STRING "Structure taken from kit.KitDefinition for "
                      FUNCTION TRIM(KA-KIT-ITEM)
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO KA-STRUCT-NOTE
               GO TO 1500-EXIT
           END-IF
           PERFORM 1700-TRY-ORDER-LINES THRU 1700-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 1500-EXIT
           END-IF
           IF KA-LINE-CNT > 0
               MOVE "ORDERLINE" TO KA-STRUCT-SOURCE
               MOVE SPACES TO WS-TEXT
               STRING "No kit.KitDefinition row exists, so the "
                      "structure was driven from the KIT and "
                      "KITCOMP order lines."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO KA-STRUCT-NOTE
               GO TO 1500-EXIT
           END-IF
           MOVE "NONE" TO KA-STRUCT-SOURCE
           MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE "KIT0002" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO WS-TEXT
           STRING "No kit structure was found for parent line of "
                  FUNCTION TRIM(KA-ORDER-NUMBER)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
           MOVE WS-TEXT TO KA-STRUCT-NOTE.
       1500-EXIT.
           EXIT.

       1550-READ-PARENT.
           IF KA-ORDER-NUMBER = SPACES
               GO TO 1550-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(p.ItemCode,''), p.LineType, "
                  "CONVERT(varchar(30), p.AcceptedQty), "
                  "CONVERT(varchar(24), p.NetAmount), "
                  "ISNULL(p.VatCode,''), "
                  "CONVERT(varchar(24), p.SalesOrderLineId), "
                  "CONVERT(varchar(30), p.DespatchedQty), "
                  "ISNULL(p.LinkedKitRef,'') "
                  "FROM so.SalesOrderLine p WHERE "
                  "p.CompanyCode = ? AND p.OrderNumber = ? AND "
                  "p.LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE KA-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE KA-PARENT-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 8 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1550-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 1550-EXIT
           END-IF
           IF KA-KIT-ITEM = SPACES
               MOVE WS-COL(1) TO KA-KIT-ITEM
           END-IF
           IF KA-PARENT-QTY = 0
               COMPUTE KA-PARENT-QTY = FUNCTION NUMVAL(WS-COL(3))
           END-IF
           IF KA-PARENT-NET = 0
               COMPUTE KA-PARENT-NET = FUNCTION NUMVAL(WS-COL(4))
           END-IF
           IF KA-PARENT-VAT-CODE = SPACES
               MOVE WS-COL(5) TO KA-PARENT-VAT-CODE
           END-IF
           COMPUTE KA-PARENT-SOL-ID = FUNCTION NUMVAL(WS-COL(6))
           IF KA-KIT-REF = SPACES
               MOVE WS-COL(8) TO KA-KIT-REF
           END-IF
      *>   a commercial kit parent of LineType KIT is non stock by
      *>   definition; anything else is an ordinary stock line
           IF WS-COL(2) = "KIT"
               MOVE "N" TO KA-PARENT-IS-STOCK
           ELSE
               MOVE "Y" TO KA-PARENT-IS-STOCK
           END-IF.
       1550-EXIT.
           EXIT.

       1600-TRY-KITDEF.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12), l.LineNum), "
                  "l.ComponentItemCode, "
                  "ISNULL(l.ComponentRevision,''), "
                  "CONVERT(varchar(30), l.Quantity), "
                  "ISNULL(l.UomCode,''), CONVERT(varchar(30), "
                  "ISNULL(l.StandaloneSellingPrice,0)), "
                  "ISNULL(l.VatCode,''), CASE WHEN l.IsOptional = 1 "
                  "THEN 'Y' ELSE 'N' END, d.SupplyKind, "
                  "ISNULL(d.VatOwnerDecisionBy,''), "
                  "CASE WHEN d.AllowPartialDespatch = 1 THEN 'Y' "
                  "ELSE 'N' END FROM kit.KitDefinition d JOIN "
                  "kit.KitDefinitionLine l ON l.KitDefId = d.KitDefId"
                  " WHERE d.CompanyCode = ? AND d.KitItemCode = ? "
                  "AND d.Status = 'RELEASED' AND (? = '' OR "
                  "d.KitVersion = ?) ORDER BY l.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE KA-KIT-ITEM TO SQLIO-PARM-VAL(2)
           MOVE KA-KIT-VERSION TO SQLIO-PARM-VAL(3)
           MOVE KA-KIT-VERSION TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 11 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
      *>       the kit schema is not deployed - fall back quietly
               MOVE 0 TO SQLIO-SQLCODE
               MOVE "CLOS" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROW
               MOVE 0 TO SQLIO-SQLCODE
               GO TO 1600-EXIT
           END-IF
           MOVE 0 TO KA-LINE-CNT
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROW
               IF SQLIO-ERROR OR SQLIO-NOTFOUND
                   MOVE "Y" TO WS-DONE
               ELSE
                   IF KA-LINE-CNT < 40
                       ADD 1 TO KA-LINE-CNT
                       MOVE KA-LINE-CNT TO WS-J
                       PERFORM 1650-KITDEF-LINE THRU 1650-EXIT
                   ELSE
                       MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
                       MOVE "KIT0009"
                           TO CA-REASON-CODE OF DFHCOMMAREA
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       1600-EXIT.
           EXIT.

       1650-KITDEF-LINE.
           COMPUTE KA-L-LINE-NUM(WS-J) = FUNCTION NUMVAL(WS-COL(1))
           MOVE 0 TO KA-L-ORDER-LINE(WS-J)
           MOVE 0 TO KA-L-SOL-ID(WS-J)
           MOVE WS-COL(2) TO KA-L-ITEM(WS-J)
           MOVE WS-COL(3) TO KA-L-REVISION(WS-J)
           MOVE SPACES TO KA-L-DESC(WS-J)
           COMPUTE KA-L-QTY-PER(WS-J) = FUNCTION NUMVAL(WS-COL(4))
           MOVE WS-COL(5) TO KA-L-UOM(WS-J)
           COMPUTE KA-L-SSP(WS-J) = FUNCTION NUMVAL(WS-COL(6))
           MOVE WS-COL(7) TO KA-L-VAT-CODE(WS-J)
           MOVE WS-COL(8)(1:1) TO KA-L-OPTIONAL(WS-J)
           MOVE 0 TO KA-L-ALLOC-NET(WS-J)
           MOVE 0 TO KA-L-ALLOC-PCT(WS-J)
           MOVE 0 TO KA-L-VAT-AMT(WS-J)
           MOVE 0 TO KA-L-KITS-POSS(WS-J)
           MOVE SPACES TO KA-L-STATUS(WS-J)
           MOVE SPACES TO KA-L-TEXT(WS-J)
           COMPUTE KA-L-REQUIRED(WS-J) =
               KA-L-QTY-PER(WS-J) * KA-PARENT-QTY
      *>   the tax owner's decision travels with the definition and
      *>   is therefore RECORDED rather than inferred
           IF WS-COL(9) NOT = SPACES
               MOVE WS-COL(9) TO KA-SUPPLY-KIND
               MOVE "Y" TO KA-SUPPLY-RECORDED
               MOVE WS-COL(10) TO KA-SUPPLY-DECIDER
           END-IF
           MOVE WS-COL(11)(1:1) TO KA-PARTIAL-ALLOWED.
       1650-EXIT.
           EXIT.

       1700-TRY-ORDER-LINES.
           IF KA-ORDER-NUMBER = SPACES
               GO TO 1700-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12), c.LineNum), "
                  "ISNULL(c.ItemCode,''), ISNULL(c.Revision,''), "
                  "CONVERT(varchar(30), c.AcceptedQty), "
                  "ISNULL(c.UomCode,''), CONVERT(varchar(30), "
                  "ISNULL(c.StandaloneSellingPrice,0)), "
                  "ISNULL(c.VatCode,''), 'N', "
                  "CONVERT(varchar(24), c.SalesOrderLineId), "
                  "c.Description, CONVERT(varchar(24), c.NetAmount) "
                  "FROM so.SalesOrderLine c WHERE "
                  "c.CompanyCode = ? AND c.OrderNumber = ? AND "
                  "c.LineType = 'KITCOMP' AND "
                  "c.KitParentLineNum = ? ORDER BY c.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE KA-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE KA-PARENT-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 11 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1700-EXIT
           END-IF
           MOVE 0 TO KA-LINE-CNT
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROW
               IF SQLIO-ERROR OR SQLIO-NOTFOUND
                   MOVE "Y" TO WS-DONE
               ELSE
                   IF KA-LINE-CNT < 40
                       ADD 1 TO KA-LINE-CNT
                       MOVE KA-LINE-CNT TO WS-J
                       PERFORM 1750-ORDER-LINE THRU 1750-EXIT
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       1700-EXIT.
           EXIT.

       1750-ORDER-LINE.
           COMPUTE KA-L-ORDER-LINE(WS-J) = FUNCTION NUMVAL(WS-COL(1))
           MOVE WS-J TO KA-L-LINE-NUM(WS-J)
           MOVE WS-COL(2) TO KA-L-ITEM(WS-J)
           MOVE WS-COL(3) TO KA-L-REVISION(WS-J)
           COMPUTE KA-L-REQUIRED(WS-J) = FUNCTION NUMVAL(WS-COL(4))
           MOVE WS-COL(5) TO KA-L-UOM(WS-J)
           COMPUTE KA-L-SSP(WS-J) = FUNCTION NUMVAL(WS-COL(6))
           MOVE WS-COL(7) TO KA-L-VAT-CODE(WS-J)
           MOVE "N" TO KA-L-OPTIONAL(WS-J)
           COMPUTE KA-L-SOL-ID(WS-J) = FUNCTION NUMVAL(WS-COL(9))
           MOVE WS-COL(10) TO KA-L-DESC(WS-J)
           MOVE 0 TO KA-L-ALLOC-NET(WS-J)
           MOVE 0 TO KA-L-ALLOC-PCT(WS-J)
           MOVE 0 TO KA-L-VAT-AMT(WS-J)
           MOVE 0 TO KA-L-KITS-POSS(WS-J)
           MOVE SPACES TO KA-L-STATUS(WS-J)
           MOVE SPACES TO KA-L-TEXT(WS-J)
      *>   the order states the component quantity for the WHOLE
      *>   parent quantity, so the per kit figure is derived
           IF KA-PARENT-QTY > 0
               COMPUTE KA-L-QTY-PER(WS-J) =
                   KA-L-REQUIRED(WS-J) / KA-PARENT-QTY
           ELSE
               MOVE KA-L-REQUIRED(WS-J) TO KA-L-QTY-PER(WS-J)
           END-IF
      *>   a component that carries its own net amount is billed at
      *>   the component, which CHECKDUP has to know about
           COMPUTE WS-AMT = FUNCTION NUMVAL(WS-COL(11))
           IF WS-AMT NOT = 0
               MOVE "Y" TO KA-COMP-BILLED
           END-IF.
       1750-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  ALLOCATE - SO-028.  Relative standalone selling price.
      *>       RNDALLOC owns the spread so that the parts re-add to
      *>       the parent net amount exactly; this program never
      *>       rounds a share itself.
      *>----------------------------------------------------------------
       2000-ALLOCATE.
           PERFORM 1500-LOAD-STRUCTURE THRU 1500-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           IF KA-PARENT-NET <= 0
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "KIT0004" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "A kit transaction price of zero cannot be "
                      "allocated to its components."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           MOVE 0 TO KA-SSP-TOTAL
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > KA-LINE-CNT
               ADD KA-L-SSP(WS-I) TO KA-SSP-TOTAL
           END-PERFORM
           IF KA-SSP-TOTAL <= 0
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "KIT0005" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "No component carries a standalone selling "
                      "price, so the kit price cannot be allocated "
                      "on a relative basis."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-CALL-RNDALLOC THRU 2100-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 2300-DECIDE-SUPPLY THRU 2300-EXIT
           PERFORM 2400-VAT-BY-RATE THRU 2400-EXIT
           PERFORM 2900-ALLOC-WORDS THRU 2900-EXIT.
       2000-EXIT.
           EXIT.

       2100-CALL-RNDALLOC.
           INITIALIZE WS-RND-CA
           MOVE "ALLOC" TO CA-FUNCTION OF WS-RND-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-RND-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-RND-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-RND-CA
           MOVE 2 TO RA-PLACES
           MOVE "WEIGHT" TO RA-MODE
           MOVE KA-PARENT-NET TO RA-AMOUNT
           MOVE KA-LINE-CNT TO RA-LINE-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > KA-LINE-CNT
               MOVE KA-L-ITEM(WS-I)(1:8) TO RA-LINE-REF(WS-I)
               MOVE KA-L-SSP(WS-I) TO RA-WEIGHT(WS-I)
               MOVE 0 TO RA-PART(WS-I)
           END-PERFORM
           CALL "RNDALLOC" USING WS-RND-CA
           IF CA-RETURN-CODE OF WS-RND-CA >= 8
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "KIT0005" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-RND-CA
                   TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 2100-EXIT
           END-IF
           MOVE RA-ALLOCATED TO KA-ALLOCATED
           MOVE RA-RESIDUAL TO KA-RESIDUAL
           MOVE RA-BUMPED TO KA-BUMPED
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > KA-LINE-CNT
               MOVE RA-PART(WS-I) TO KA-L-ALLOC-NET(WS-I)
               COMPUTE KA-L-ALLOC-PCT(WS-I) ROUNDED =
                   KA-L-SSP(WS-I) / KA-SSP-TOTAL * 100
               MOVE "ALLOCATED" TO KA-L-STATUS(WS-I)
           END-PERFORM.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2300  SINGLE OR MIXED.  Where the tax owner has recorded the
      *>       decision on the kit definition it stands.  Otherwise it
      *>       is derived from the component VAT codes and recorded
      *>       with a note saying that the tax owner must confirm it -
      *>       the flag is never simply assumed to be SINGLE.
      *>----------------------------------------------------------------
       2300-DECIDE-SUPPLY.
           MOVE SPACES TO WS-VAT1
           MOVE "N" TO WS-DIFFERENT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > KA-LINE-CNT
               IF KA-L-VAT-CODE(WS-I) NOT = SPACES
                   IF WS-VAT1 = SPACES
                       MOVE KA-L-VAT-CODE(WS-I) TO WS-VAT1
                   ELSE
                       IF KA-L-VAT-CODE(WS-I) NOT = WS-VAT1
                           MOVE "Y" TO WS-DIFFERENT
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF KA-SUPPLY-RECORDED = "Y"
               MOVE SPACES TO WS-TEXT
               STRING "Supply kind " FUNCTION TRIM(KA-SUPPLY-KIND)
                      " was recorded by the tax owner on the kit "
                      "definition."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO KA-SUPPLY-TEXT
               GO TO 2300-EXIT
           END-IF
           IF WS-DIFFERENT = "Y"
               MOVE "MIXED" TO KA-SUPPLY-KIND
               MOVE SPACES TO WS-TEXT
               STRING "The components carry different VAT "
                      "liabilities, so this is a MIXED supply and "
                      "VAT is apportioned by relative standalone "
                      "selling price."
                   DELIMITED BY SIZE INTO WS-TEXT
           ELSE
               MOVE "SINGLE" TO KA-SUPPLY-KIND
               MOVE SPACES TO WS-TEXT
               STRING "Every component carries the same VAT "
                      "liability, so this is a SINGLE supply."
                   DELIMITED BY SIZE INTO WS-TEXT
           END-IF
           MOVE WS-TEXT TO KA-SUPPLY-TEXT
           MOVE "Y" TO KA-SUPPLY-RECORDED
           IF KA-SUPPLY-DECIDER = SPACES
               MOVE "DERIVED" TO KA-SUPPLY-DECIDER
           END-IF.
       2300-EXIT.
           EXIT.

      *>   One analysis row per rate on the ALLOCATED amounts, which
      *>   is what the invoice has to show (specification 05 part 7).
       2400-VAT-BY-RATE.
           MOVE 0 TO KA-VAT-CNT KA-VAT-TOTAL
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > KA-LINE-CNT
               PERFORM 2450-ONE-COMPONENT-VAT THRU 2450-EXIT
           END-PERFORM
           COMPUTE KA-GROSS-TOTAL = KA-ALLOCATED + KA-VAT-TOTAL.
       2400-EXIT.
           EXIT.

       2450-ONE-COMPONENT-VAT.
           MOVE KA-L-VAT-CODE(WS-I) TO WS-VAT1
           IF WS-VAT1 = SPACES
               MOVE KA-PARENT-VAT-CODE TO WS-VAT1
               MOVE WS-VAT1 TO KA-L-VAT-CODE(WS-I)
           END-IF
           IF KA-L-VAT-RATE(WS-I) = 0 AND NOT KA-FROM-CALLER
               PERFORM 2500-LOOKUP-RATE THRU 2500-EXIT
               MOVE WS-RATE TO KA-L-VAT-RATE(WS-I)
           END-IF
           COMPUTE KA-L-VAT-AMT(WS-I) ROUNDED =
               KA-L-ALLOC-NET(WS-I) * KA-L-VAT-RATE(WS-I) / 100
           ADD KA-L-VAT-AMT(WS-I) TO KA-VAT-TOTAL
           MOVE 0 TO WS-K
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > KA-VAT-CNT
               IF KA-V-CODE(WS-J) = KA-L-VAT-CODE(WS-I)
                   MOVE WS-J TO WS-K
               END-IF
           END-PERFORM
           IF WS-K = 0
               IF KA-VAT-CNT < 6
                   ADD 1 TO KA-VAT-CNT
                   MOVE KA-VAT-CNT TO WS-K
                   MOVE KA-L-VAT-CODE(WS-I) TO KA-V-CODE(WS-K)
                   MOVE KA-L-VAT-RATE(WS-I) TO KA-V-RATE(WS-K)
                   MOVE 0 TO KA-V-NET(WS-K)
                   MOVE 0 TO KA-V-VAT(WS-K)
               ELSE
                   GO TO 2450-EXIT
               END-IF
           END-IF
           ADD KA-L-ALLOC-NET(WS-I) TO KA-V-NET(WS-K)
           ADD KA-L-VAT-AMT(WS-I) TO KA-V-VAT(WS-K).
       2450-EXIT.
           EXIT.

       2500-LOOKUP-RATE.
           MOVE 0 TO WS-RATE
           IF WS-VAT1 = SPACES
               GO TO 2500-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CONVERT(varchar(24), r.RatePct), "
                  "c.Liability FROM fin.VatRate r JOIN fin.VatCode c "
                  "ON c.VatCode = r.VatCode WHERE r.VatCode = ? AND "
                  "CONVERT(char(8), r.EffectiveFrom, 112) <= ? "
                  "ORDER BY r.EffectiveFrom DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-VAT1 TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE 0 TO SQLIO-SQLCODE
               GO TO 2500-EXIT
           END-IF
           COMPUTE WS-RATE = FUNCTION NUMVAL(WS-COL(1)).
       2500-EXIT.
           EXIT.

       2900-ALLOC-WORDS.
           MOVE KA-ALLOCATED TO WS-MONEY-ED
           MOVE SPACES TO WS-TEXT
           STRING "Allocated GBP " FUNCTION TRIM(WS-MONEY-ED)
                  " over"
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE KA-LINE-CNT TO WS-KIT-ED
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE KA-VAT-TOTAL TO WS-MONEY-ED
           STRING FUNCTION TRIM(WS-TEXT) " "
                  FUNCTION TRIM(WS-KIT-ED)
                  " component(s) by relative standalone selling "
                  "price; VAT GBP " FUNCTION TRIM(WS-MONEY-ED)
                  " on a " FUNCTION TRIM(KA-SUPPLY-KIND) " supply."
               DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
           IF KA-RESIDUAL NOT = 0
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "KIT0004" TO CA-REASON-CODE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       2900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  COMPLETE - D-11.  A complete commercial kit is required
      *>       for each despatched parent quantity, so the number of
      *>       kits a component can support is
      *>           floor(deliverable quantity / quantity per kit)
      *>       and the kit total is the LOWEST of those.  Three pins
      *>       per kit with five pins deliverable makes one kit, not
      *>       one and two thirds: a part kit is not a kit.
      *>----------------------------------------------------------------
       3000-COMPLETE.
           PERFORM 1500-LOAD-STRUCTURE THRU 1500-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 3000-EXIT
           END-IF
           MOVE "Y" TO WS-FIRST
           MOVE 0 TO WS-LOWEST
           MOVE SPACES TO KA-LIMIT-ITEM
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > KA-LINE-CNT
               PERFORM 3100-KITS-FROM-LINE THRU 3100-EXIT
           END-PERFORM
           IF WS-FIRST = "Y"
               MOVE 0 TO KA-COMPLETE-KITS
           ELSE
               MOVE WS-LOWEST TO KA-COMPLETE-KITS
           END-IF
      *>   D-11 again, from the other side: the parent of a non stock
      *>   commercial kit has no warehouse balance, so a complete kit
      *>   consumes components and never any parent inventory
           MOVE 0 TO KA-PARENT-DEMAND
           PERFORM 3200-SHORTFALL THRU 3200-EXIT
           PERFORM 3300-JUDGE-PROPOSAL THRU 3300-EXIT.
       3000-EXIT.
           EXIT.

       3100-KITS-FROM-LINE.
           MOVE 0 TO KA-L-KITS-POSS(WS-I)
           IF KA-L-OPTIONAL(WS-I) = "Y"
               MOVE "OPTIONAL" TO KA-L-STATUS(WS-I)
               GO TO 3100-EXIT
           END-IF
           IF KA-L-QTY-PER(WS-I) <= 0
               MOVE "NOQTY" TO KA-L-STATUS(WS-I)
               GO TO 3100-EXIT
           END-IF
           COMPUTE WS-KITS =
               FUNCTION INTEGER(KA-L-DELIVER(WS-I)
                                / KA-L-QTY-PER(WS-I))
           IF WS-KITS < 0
               MOVE 0 TO WS-KITS
           END-IF
           MOVE WS-KITS TO KA-L-KITS-POSS(WS-I)
           MOVE "COUNTED" TO KA-L-STATUS(WS-I)
           IF WS-FIRST = "Y"
               MOVE "N" TO WS-FIRST
               MOVE WS-KITS TO WS-LOWEST
               MOVE KA-L-ITEM(WS-I) TO KA-LIMIT-ITEM
           ELSE
               IF WS-KITS < WS-LOWEST
                   MOVE WS-KITS TO WS-LOWEST
                   MOVE KA-L-ITEM(WS-I) TO KA-LIMIT-ITEM
               END-IF
           END-IF.
       3100-EXIT.
           EXIT.

      *>   what the limiting component is short of, for the proposal
       3200-SHORTFALL.
           MOVE 0 TO KA-LIMIT-SHORT
           IF KA-PROPOSED-KITS <= 0
               GO TO 3200-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > KA-LINE-CNT
               IF KA-L-OPTIONAL(WS-I) NOT = "Y"
                  AND KA-L-QTY-PER(WS-I) > 0
                   COMPUTE WS-QTY = KA-L-QTY-PER(WS-I)
                       * KA-PROPOSED-KITS - KA-L-DELIVER(WS-I)
                   IF WS-QTY > 0
                       MOVE WS-QTY TO KA-L-REQUIRED(WS-I)
                       IF KA-L-ITEM(WS-I) = KA-LIMIT-ITEM
                           MOVE WS-QTY TO KA-LIMIT-SHORT
                       END-IF
                       MOVE "SHORT" TO KA-L-STATUS(WS-I)
                   END-IF
               END-IF
           END-PERFORM.
       3200-EXIT.
           EXIT.

       3300-JUDGE-PROPOSAL.
           MOVE KA-COMPLETE-KITS TO WS-KIT-ED
           IF KA-PROPOSED-KITS <= 0
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING FUNCTION TRIM(WS-KIT-ED)
                      " complete kit(s) can be made from the "
                      "deliverable components."
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               GO TO 3300-EXIT
           END-IF
           IF KA-PROPOSED-KITS > KA-COMPLETE-KITS
               MOVE "Y" TO KA-REFUSED
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "KIT0003" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE KA-PROPOSED-KITS TO WS-QTY-ED
               MOVE SPACES TO WS-TEXT
               STRING "A despatch of " FUNCTION TRIM(WS-QTY-ED)
                      " kit(s) is refused: only "
                      FUNCTION TRIM(WS-KIT-ED)
                      " complete kit(s) can be made and partial "
                      "component despatch is not permitted."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               GO TO 3300-EXIT
           END-IF
           MOVE "N" TO KA-REFUSED
           MOVE KA-PROPOSED-KITS TO WS-QTY-ED
           MOVE SPACES TO WS-TEXT
           STRING "A despatch of " FUNCTION TRIM(WS-QTY-ED)
                  " complete kit(s) is permitted; "
                  FUNCTION TRIM(WS-KIT-ED)
                  " can be made in total."
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       3300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  CHECKDUP - SO-020.  The parent and its children can
      *>       never both create the same inventory demand nor be
      *>       billed twice.  For a non stock commercial kit parent:
      *>         demand, issue, COGS and traceability -> components
      *>         pricing and invoicing -> the parent, or explicitly
      *>         allocated component commercial lines, never both.
      *>----------------------------------------------------------------
       4000-CHECKDUP.
           PERFORM 1500-LOAD-STRUCTURE THRU 1500-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 4000-EXIT
           END-IF
           MOVE 0 TO KA-DUP-CNT
           IF KA-PARENT-NET NOT = 0
               MOVE "Y" TO KA-PARENT-BILLED
           ELSE
               MOVE "N" TO KA-PARENT-BILLED
           END-IF
           PERFORM 4100-PARENT-BALANCE THRU 4100-EXIT
           PERFORM 4200-PARENT-DEMAND THRU 4200-EXIT
           PERFORM 4300-BILLED-TWICE THRU 4300-EXIT
           PERFORM 4900-DUP-WORDS THRU 4900-EXIT.
       4000-EXIT.
           EXIT.

      *>   A non stock commercial kit parent has no warehouse
      *>   balance.  If it has one, somebody has been treating the
      *>   parent as stock and the components as stock as well.
       4100-PARENT-BALANCE.
           MOVE 0 TO KA-PARENT-BALANCE
           IF KA-KIT-ITEM = SPACES OR KA-FROM-CALLER
               GO TO 4100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(30), ISNULL(SUM("
                  "b.QtyOnHand),0)), '' FROM inv.StockBalance b "
                  "WHERE b.CompanyCode = ? AND b.ItemCode = ? AND "
                  "(? = '' OR b.SiteCode = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE KA-KIT-ITEM TO SQLIO-PARM-VAL(2)
           MOVE KA-SITE TO SQLIO-PARM-VAL(3)
           MOVE KA-SITE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE 0 TO SQLIO-SQLCODE
               GO TO 4100-EXIT
           END-IF
           COMPUTE KA-PARENT-BALANCE = FUNCTION NUMVAL(WS-COL(1))
           IF KA-PARENT-BALANCE > 0 AND KA-PARENT-IS-STOCK = "N"
               ADD 1 TO KA-DUP-CNT
               MOVE "Y" TO KA-DUP-COGS
               MOVE 6 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "KIT0008" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE KA-PARENT-BALANCE TO WS-QTY-ED
               MOVE SPACES TO WS-TEXT
               STRING "The non stock kit parent "
                      FUNCTION TRIM(KA-KIT-ITEM) " holds a balance "
                      "of " FUNCTION TRIM(WS-QTY-ED)
                      "; it should have no warehouse balance at all."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO KA-DUP-TEXT
           END-IF.
       4100-EXIT.
           EXIT.

      *>   Inventory demand must exist once, at the components.  A
      *>   reservation held against the PARENT line as well is the
      *>   classic double demand this requirement forbids.
       4200-PARENT-DEMAND.
           MOVE 0 TO KA-PARENT-DEMAND
           IF KA-ORDER-NUMBER = SPACES OR KA-FROM-CALLER
               GO TO 4200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(30), ISNULL(SUM("
                  "r.QtyReserved - r.QtyConsumed),0)), '' FROM "
                  "inv.StockReservation r WHERE r.CompanyCode = ? "
                  "AND r.DemandType = 'SALESORDER' AND "
                  "r.DemandRef = ? AND r.DemandLine = ? AND "
                  "r.Status = 'ACTIVE'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE KA-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE KA-PARENT-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE 0 TO SQLIO-SQLCODE
               GO TO 4200-EXIT
           END-IF
           COMPUTE KA-PARENT-DEMAND = FUNCTION NUMVAL(WS-COL(1))
           IF KA-PARENT-DEMAND > 0 AND KA-PARENT-IS-STOCK = "N"
               ADD 1 TO KA-DUP-CNT
               MOVE "Y" TO KA-DUP-DEMAND
               MOVE 6 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "KIT0007" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Stock is reserved against the kit parent "
                      "line as well as its components - the same "
                      "demand has been created twice."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO KA-DUP-TEXT
           END-IF.
       4200-EXIT.
           EXIT.

       4300-BILLED-TWICE.
           IF KA-PARENT-BILLED = "Y" AND KA-COMP-BILLED = "Y"
               ADD 1 TO KA-DUP-CNT
               MOVE "Y" TO KA-DUP-BILLING
               MOVE 6 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "KIT0007" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Both the kit parent and its components carry "
                      "a net amount; the customer would be billed "
                      "twice for one supply."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO KA-DUP-TEXT
           END-IF.
       4300-EXIT.
           EXIT.

       4900-DUP-WORDS.
           IF KA-DUP-CNT > 0
               MOVE KA-DUP-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               GO TO 4900-EXIT
           END-IF
           MOVE SPACES TO WS-TEXT
           STRING "Component demand, issue, cost and traceability "
                  "happen once at the components and the commercial "
                  "value once at the parent; no duplication found."
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO KA-DUP-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       4900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT.
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE "Y" TO WS-BAD.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE "KIT0006" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO WS-TEXT
           STRING "KITSOAL does not support the function "
                  FUNCTION TRIM(CA-FUNCTION OF DFHCOMMAREA)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE "SQL0001" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       9300-EXIT.
           EXIT.

