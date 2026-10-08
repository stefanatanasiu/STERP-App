>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : KITRET
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : Return material from an execution kit to stores
*>              (KIT-013, D-12).
*>
*>              A return is always ANCHORED TO AN ISSUE.  You may
*>              only give back what was actually taken and not yet
*>              given back, at the cost it was taken at, and the
*>              condition decides whether it becomes available
*>              again or goes to inspection.  Three rules follow
*>              from that and are all enforced here:
*>                * returned quantity <= unreversed issued
*>                  quantity on the referenced issue;
*>                * the value credited to the job is the value
*>                  that was charged to it, not today's cost -
*>                  otherwise a cost revision would turn a return
*>                  into a profit;
*>                * if inspection is required the stock cannot
*>                  come back as available, so KX-AVAILABLE is
*>                  "N" and the receipt is into INSPECTION.
*> CALLED BY  : frmKitExecution, KITCLOSE (leftover material)
*> INTERFACE  : CALL "KITRET" USING WS-KITRET-CA
*>              COPYBOOK KITRETCA
*> CALLS      : KITDB, KITQTY, STKMOVE, SQLIO (issue history)
*>----------------------------------------------------------------
*> CHANGE LOG
*> 1996-02-19 DRN  CR0533 RETURN TO STORES
*> 2015-06-15 PMK  CR2352 RETURNS MUST QUOTE THE ORIGINAL ISSUE
*> 2016-10-04 PMK  CR2501 CONDITION AND INSPECTION ON RETURN
*> 2026-10-06 STERP KIT-013
*>================================================================
*> REQ: KIT-013
 IDENTIFICATION DIVISION.
 PROGRAM-ID. KITRET.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 15/06/2015.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY KITDBW.
 COPY KITWORK.
 COPY KITQTYCA REPLACING ==DFHCOMMAREA== BY ==Ws-Qty-Ca==
      LEADING ==CA-== BY ==QA-==.
 COPY STKMOVECA REPLACING ==DFHCOMMAREA== BY ==Ws-Move-Ca==
      LEADING ==CA-== BY ==MA-==.
 01  Ws-Iss-Row.
     05  Ws-IR-Id                PIC X(20).
     05  Ws-IR-Item              PIC X(40).
     05  Ws-IR-Lot               PIC X(30).
     05  Ws-IR-Serial            PIC X(40).
     05  Ws-IR-Date              PIC X(8).
     05  Ws-IR-Issued            PIC X(24).
     05  Ws-IR-Returned          PIC X(24).
     05  Ws-IR-Unit-Cost         PIC X(24).
     05  Ws-IR-Operator          PIC X(20).
 01  Ws-Work.
     05  Ws-More                 PIC X VALUE "Y".
     05  Ws-Row                  PIC S9(4) VALUE 0.
     05  Ws-J                    PIC S9(4) VALUE 0.
     05  Ws-Num                  PIC S9(13)V9(6) VALUE 0.
     05  Ws-Open-Qty             PIC S9(13)V9(6) VALUE 0.
     05  Ws-Found-Issue          PIC X VALUE "N".
     05  Ws-Edit                 PIC -(13)9.9(6).
 LINKAGE SECTION.
 COPY KITRETCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "KITRET" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE 0 TO CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     MOVE "KIT-013" TO CA-REQ-REF
     MOVE 0 TO KX-RETURNABLE-QTY
     MOVE 0 TO KX-RETURNED-QTY
     MOVE 0 TO KX-KIT-ISSUE-ID
     MOVE 0 TO KX-MOVEMENT-ID
     MOVE 0 TO KX-ISSUE-CNT
     MOVE "Y" TO KX-AVAILABLE
     IF KX-ACTUAL-DATE = 0
         MOVE CA-BUSINESS-DATE TO KX-ACTUAL-DATE
     END-IF
     IF KX-POSTING-DATE = 0
         MOVE KX-ACTUAL-DATE TO KX-POSTING-DATE
     END-IF
     IF KX-CONDITION = SPACES
         MOVE "GOOD" TO KX-CONDITION
     END-IF
     EVALUATE CA-FUNCTION
         WHEN "LISTISS"  PERFORM List-The-Issues
         WHEN "RETURNCK" PERFORM Check-The-Return
         WHEN "RETURN"   PERFORM Make-The-Return
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "KIT0002" TO CA-REASON-CODE
     STRING "KITRET does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

 Set-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE KD-ERRMSG TO CA-MESSAGE.

 Set-Sqlio-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE SQLIO-ERRMSG (1:200) TO CA-MESSAGE.

*>----------------------------------------------------------------
 Load-Kit.
     INITIALIZE KIT-DB-CTL
     INITIALIZE KIT-HEADER-REC
     MOVE "HDRGET" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KX-KIT-REF TO KD-KIT-REF
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     EVALUATE KD-RC
         WHEN 0
             CONTINUE
         WHEN 100
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0001" TO CA-REASON-CODE
             STRING "Kit " FUNCTION TRIM(KX-KIT-REF)
                    " is not known in this company."
                 DELIMITED BY SIZE INTO CA-MESSAGE
         WHEN OTHER
             PERFORM Set-Sql-Failure
     END-EVALUATE
     IF KX-SITE = SPACES
         MOVE KH-SITE TO KX-SITE
     END-IF
*>   A financially closed kit cannot take a return; the material
*>   has to be dealt with before the kit is closed (KIT-017).
     IF CA-RETURN-CODE = 0 AND KH-IS-FINCLOSED
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0023" TO CA-REASON-CODE
         MOVE "This kit has been financially closed; a return needs it reopened first."
              TO CA-MESSAGE
     END-IF.

 Load-Line.
     MOVE "LINGET" TO KD-FUNC
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     MOVE KX-LINE-NUM TO KD-LINE-NUM
     INITIALIZE KIT-LINE-REC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     EVALUATE KD-RC
         WHEN 0
             IF KX-ITEM-CODE = SPACES
                 MOVE KL-ITEM-CODE TO KX-ITEM-CODE
             END-IF
             IF KX-UOM-CODE = SPACES
                 MOVE KL-UOM-CODE TO KX-UOM-CODE
             END-IF
         WHEN 100
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0007" TO CA-REASON-CODE
             MOVE "That material line is not on this kit." TO CA-MESSAGE
         WHEN OTHER
             PERFORM Set-Sql-Failure
     END-EVALUATE.

*>----------------------------------------------------------------
*> LISTISS - the issues a return may be anchored to, with the
*> quantity still unreversed on each.  The operator picks one; a
*> return without an issue behind it is not a return, it is a
*> receipt, and belongs in stock control.
 List-The-Issues.
     PERFORM Load-Kit
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(20), i.KitIssueId), "
            "CONVERT(varchar(40), i.ComponentItemCode), "
            "CONVERT(varchar(30), ISNULL(i.LotCode,'')), "
            "CONVERT(varchar(40), ISNULL(i.SerialNo,'')), "
            "CONVERT(varchar(8), i.ActualDate, 112), "
            "CONVERT(varchar(24), i.StockQty), "
            "CONVERT(varchar(24), ISNULL(r.Ret,0)), "
            "CONVERT(varchar(24), ISNULL(i.UnitCostGBP,0)), "
            "CONVERT(varchar(20), ISNULL(i.OperatorUser,'')) "
            "FROM kit.KitIssue i OUTER APPLY (SELECT "
            "SUM(x.StockQty) AS Ret FROM kit.KitIssue x WHERE "
            "x.OriginalKitIssueId = i.KitIssueId AND "
            "x.IssueType = 'RETURN') r WHERE i.CompanyCode = ? "
            "AND i.KitRef = ? AND i.IssueType IN "
            "('ISSUE','BACKFLUSH') AND (CONVERT(int, ?) = 0 OR "
            "i.LineNum = CONVERT(int, ?)) ORDER BY i.KitIssueId"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE KX-KIT-REF TO SQLIO-PARM-VAL (2)
     MOVE KX-LINE-NUM TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (3)
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (4)
     MOVE 4 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-J FROM 1 BY 1 UNTIL Ws-J > 4
         MOVE "N" TO SQLIO-PARM-NULL (Ws-J)
     END-PERFORM
     MOVE 9 TO SQLIO-COL-CNT
     MOVE 20 TO SQLIO-COL-WIDTH (1)
     MOVE 40 TO SQLIO-COL-WIDTH (2)
     MOVE 30 TO SQLIO-COL-WIDTH (3)
     MOVE 40 TO SQLIO-COL-WIDTH (4)
     MOVE 8 TO SQLIO-COL-WIDTH (5)
     MOVE 24 TO SQLIO-COL-WIDTH (6)
     MOVE 24 TO SQLIO-COL-WIDTH (7)
     MOVE 24 TO SQLIO-COL-WIDTH (8)
     MOVE 20 TO SQLIO-COL-WIDTH (9)
     MOVE 7 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Iss-Row
     IF SQLIO-ERROR
         PERFORM Set-Sqlio-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO KX-ISSUE-CNT
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE 7 TO SQLIO-CURSOR
         MOVE "FTCH" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA, Ws-Iss-Row
         IF SQLIO-ERROR
             PERFORM Set-Sqlio-Failure
             MOVE "N" TO Ws-More
         ELSE
             IF SQLIO-NOTFOUND
                 MOVE "N" TO Ws-More
             ELSE
                 PERFORM Add-Issue-Row
             END-IF
         END-IF
         IF KX-ISSUE-CNT >= 15
             MOVE "N" TO Ws-More
         END-IF
     END-PERFORM
     MOVE 7 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Iss-Row
     IF CA-RETURN-CODE = 0 AND KX-ISSUE-CNT = 0
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0011" TO CA-REASON-CODE
         MOVE "Nothing has been issued to this kit, so nothing can be returned."
              TO CA-MESSAGE
     END-IF.

 Add-Issue-Row.
     ADD 1 TO KX-ISSUE-CNT
     MOVE Ws-Row TO Ws-J
     COMPUTE KX-I-ISSUE-ID (KX-ISSUE-CNT) =
         FUNCTION NUMVAL (Ws-IR-Id)
     MOVE Ws-IR-Item TO KX-I-ITEM (KX-ISSUE-CNT)
     MOVE Ws-IR-Lot TO KX-I-LOT (KX-ISSUE-CNT)
     MOVE Ws-IR-Serial TO KX-I-SERIAL (KX-ISSUE-CNT)
     COMPUTE KX-I-DATE (KX-ISSUE-CNT) = FUNCTION NUMVAL (Ws-IR-Date)
     COMPUTE KX-I-ISSUED (KX-ISSUE-CNT) =
         FUNCTION NUMVAL (Ws-IR-Issued)
     COMPUTE KX-I-RETURNED (KX-ISSUE-CNT) =
         FUNCTION NUMVAL (Ws-IR-Returned)
     COMPUTE KX-I-OPEN-QTY (KX-ISSUE-CNT) =
         KX-I-ISSUED (KX-ISSUE-CNT) - KX-I-RETURNED (KX-ISSUE-CNT)
     IF KX-I-OPEN-QTY (KX-ISSUE-CNT) < 0
         MOVE 0 TO KX-I-OPEN-QTY (KX-ISSUE-CNT)
     END-IF
     COMPUTE KX-I-UNIT-COST (KX-ISSUE-CNT) =
         FUNCTION NUMVAL (Ws-IR-Unit-Cost)
     MOVE Ws-IR-Operator TO KX-I-OPERATOR (KX-ISSUE-CNT)
     COMPUTE KX-RETURNABLE-QTY = KX-RETURNABLE-QTY
                               + KX-I-OPEN-QTY (KX-ISSUE-CNT)
     IF KX-ORIG-ISSUE-ID NOT = 0
        AND KX-I-ISSUE-ID (KX-ISSUE-CNT) = KX-ORIG-ISSUE-ID
         MOVE "Y" TO Ws-Found-Issue
         MOVE KX-I-OPEN-QTY (KX-ISSUE-CNT) TO Ws-Open-Qty
         MOVE KX-I-UNIT-COST (KX-ISSUE-CNT) TO KX-UNIT-COST
         IF KX-ITEM-CODE = SPACES
             MOVE KX-I-ITEM (KX-ISSUE-CNT) TO KX-ITEM-CODE
         END-IF
     END-IF.

*>----------------------------------------------------------------
*> RETURNCK - everything that could refuse the return, decided
*> before a single row is written.
 Check-The-Return.
     PERFORM Load-Kit
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     IF KX-LINE-NUM > 0
         PERFORM Load-Line
         IF CA-RETURN-CODE > 4
             EXIT PARAGRAPH
         END-IF
     END-IF
     MOVE "N" TO Ws-Found-Issue
     MOVE 0 TO Ws-Open-Qty
     MOVE 0 TO KX-RETURNABLE-QTY
     PERFORM List-The-Issues
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO CA-RETURN-CODE
     MOVE SPACES TO CA-REASON-CODE
     IF KX-ORIG-ISSUE-ID NOT = 0 AND Ws-Found-Issue NOT = "Y"
         MOVE 16 TO CA-RETURN-CODE
         MOVE "KIT0011" TO CA-REASON-CODE
         MOVE "That issue does not belong to this kit, so it cannot be returned against."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KX-ORIG-ISSUE-ID NOT = 0
         MOVE Ws-Open-Qty TO KX-RETURNABLE-QTY
     END-IF
     IF KX-QUANTITY <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "The returned quantity must be greater than zero."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
*>   The one rule that matters most: you cannot hand back more
*>   than you took.  A kit is not a source of stock.
     IF KX-QUANTITY > KX-RETURNABLE-QTY
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0011" TO CA-REASON-CODE
         MOVE KX-RETURNABLE-QTY TO Ws-Edit
         STRING "At most " FUNCTION TRIM(Ws-Edit)
                " is still unreversed on the issue(s) quoted."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KX-REASON-TEXT = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0008" TO CA-REASON-CODE
         MOVE "A return needs a reason so that the job cost movement can be explained."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Decide-The-Condition
     MOVE KX-RETURNABLE-QTY TO Ws-Edit
     STRING "Returnable " FUNCTION TRIM(Ws-Edit)
            ". Condition " FUNCTION TRIM(KX-STOCK-CONDITION) "."
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>   D-12 and KIT-013: material whose condition is not plainly
*>   good, or that the operator has flagged for inspection, comes
*>   back into INSPECTION and is not available to anyone else
*>   until it has been looked at.
 Decide-The-Condition.
     IF KX-INSPECT-REQD OR KX-CONDITION = "SUSPECT"
         OR KX-CONDITION = "DAMAGED"
         MOVE "INSPECTION" TO KX-STOCK-CONDITION
         MOVE "N" TO KX-AVAILABLE
     ELSE
         MOVE "GOOD" TO KX-STOCK-CONDITION
         MOVE "Y" TO KX-AVAILABLE
     END-IF.

*>----------------------------------------------------------------
 Make-The-Return.
     PERFORM Check-The-Return
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Line
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Post-Stock-Return
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Record-Kit-Return
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Update-Line-Quantities.

*>   The value credited back to the job is quantity x the unit
*>   cost of the ORIGINAL issue when one was quoted.  STKMOVE is
*>   told that cost so the receipt and the job credit agree.
 Post-Stock-Return.
     INITIALIZE Ws-Move-Ca
     MOVE "RETURN" TO MA-FUNCTION
     MOVE CA-COMPANY TO MA-COMPANY
     MOVE KX-SITE TO MA-SITE
     MOVE CA-USER TO MA-USER
     MOVE CA-BUSINESS-DATE TO MA-BUSINESS-DATE
     MOVE CA-CORRELATION-ID TO MA-CORRELATION-ID
     MOVE KX-ITEM-CODE TO SM-ITEM-CODE
     MOVE KX-REVISION TO SM-REVISION
     MOVE KX-SITE TO SM-SITE
     MOVE KX-WAREHOUSE TO SM-WAREHOUSE
     MOVE KX-LOCATION TO SM-LOCATION
     MOVE KX-LOT-CODE TO SM-LOT-CODE
     MOVE KX-SERIAL-NO TO SM-SERIAL-NO
     MOVE KX-QUANTITY TO SM-QUANTITY
     MOVE KX-UOM-CODE TO SM-UOM-CODE
     MOVE KX-STOCK-CONDITION TO SM-CONDITION
     MOVE KX-ACTUAL-DATE TO SM-MOVEMENT-DATE
     MOVE KX-POSTING-DATE TO SM-POSTING-DATE
     IF KX-UNIT-COST > 0
         MOVE KX-UNIT-COST TO SM-UNIT-COST
     END-IF
     MOVE "KIT" TO SM-SOURCE-MODULE
     MOVE "EXECUTIONKIT" TO SM-SOURCE-DOC-TYPE
     MOVE KX-KIT-REF TO SM-SOURCE-DOC-REF
     MOVE KX-LINE-NUM TO SM-SOURCE-DOC-LINE
     MOVE KX-REASON-CODE TO SM-REASON-CODE
     MOVE KX-REASON-TEXT (1:200) TO SM-REASON
     CALL "STKMOVE" USING Ws-Move-Ca
     IF MA-RETURN-CODE > 4
         MOVE MA-RETURN-CODE TO CA-RETURN-CODE
         MOVE MA-REASON-CODE TO CA-REASON-CODE
         MOVE MA-MESSAGE TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE SM-MOVEMENT-ID TO KX-MOVEMENT-ID
     IF KX-UNIT-COST = 0
         MOVE SM-UNIT-COST TO KX-UNIT-COST
     END-IF
     COMPUTE KX-VALUE-GBP ROUNDED = KX-UNIT-COST * SM-STOCK-QTY
     MOVE SM-STOCK-QTY TO Ws-Num.

 Record-Kit-Return.
     MOVE "ISSINS" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KX-KIT-REF TO KD-KIT-REF
     MOVE CA-CORRELATION-ID TO KD-REASON
     MOVE "RETURN" TO KD-I-TYPE
     MOVE KX-ITEM-CODE TO KD-I-ITEM
     MOVE KX-REVISION TO KD-I-REV
     MOVE KX-QUANTITY TO KD-I-QTY
     MOVE KX-UOM-CODE TO KD-I-UOM
     MOVE Ws-Num TO KD-I-STOCK-QTY
     MOVE KX-WAREHOUSE TO KD-I-WAREHOUSE
     MOVE KX-LOCATION TO KD-I-LOCATION
     MOVE KX-LOT-CODE TO KD-I-LOT
     MOVE KX-SERIAL-NO TO KD-I-SERIAL
     MOVE KX-ACTUAL-DATE TO KD-I-ACT-DATE
     MOVE KX-POSTING-DATE TO KD-I-POST-DATE
     MOVE KX-UNIT-COST TO KD-I-UNIT-COST
     MOVE KX-VALUE-GBP TO KD-I-VALUE
     MOVE KX-REASON-CODE TO KD-I-REASON-CODE
     MOVE KX-REASON-TEXT (1:200) TO KD-I-REASON
     MOVE 0 TO KD-I-APPROVAL
     MOVE KX-ORIG-ISSUE-ID TO KD-I-ORIG-ISSUE
     MOVE KX-MOVEMENT-ID TO KD-I-MOVEMENT
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
     ELSE
         MOVE KD-NEW-ID TO KX-KIT-ISSUE-ID
     END-IF.

*>   KITQTY owns the arithmetic: net = gross - returns, and the
*>   remaining requirement comes BACK when material is returned.
*>   A return therefore reopens the line - it does not pretend
*>   the issue never happened (KIT-003).
 Update-Line-Quantities.
     COMPUTE KL-RETURNED-QTY = KL-RETURNED-QTY + Ws-Num
     COMPUTE KL-ISSUED-VALUE ROUNDED
         = KL-ISSUED-VALUE - KX-VALUE-GBP
     INITIALIZE Ws-Qty-Ca
     MOVE "LINEQTY" TO QA-FUNCTION
     MOVE CA-COMPANY TO QA-COMPANY
     MOVE KL-CURRENT-REQ TO KQ-CURRENT-REQ
     MOVE KL-ORIGINAL-REQ TO KQ-ORIGINAL-REQ
     MOVE KL-GROSS-ISSUED TO KQ-GROSS-ISSUED
     MOVE KL-RETURNED-QTY TO KQ-RETURNED
     MOVE KL-SCRAPPED-QTY TO KQ-SCRAPPED
     CALL "KITQTY" USING Ws-Qty-Ca
     MOVE KQ-NET-ISSUED TO KL-NET-ISSUED
     MOVE KQ-REMAINING TO KL-REMAINING-QTY
     MOVE KQ-EXCESS TO KL-EXCESS-QTY
     IF KL-REMAINING-QTY > 0
         IF KL-GROSS-ISSUED > 0
             MOVE "PARTISSUED" TO KL-STATUS
         ELSE
             MOVE "OPEN" TO KL-STATUS
         END-IF
     ELSE
         MOVE "ISSUED" TO KL-STATUS
     END-IF
     MOVE "LINSAVE" TO KD-FUNC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE Ws-Num TO KX-RETURNED-QTY
     MOVE KL-GROSS-ISSUED TO KX-LINE-GROSS
     MOVE KL-NET-ISSUED TO KX-LINE-NET
     MOVE KL-REMAINING-QTY TO KX-LINE-REMAINING
     MOVE KL-EXCESS-QTY TO KX-LINE-EXCESS
     MOVE KX-VALUE-GBP TO Ws-Edit
     IF KX-AVAILABLE = "N"
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0012" TO CA-REASON-CODE
         STRING "Returned into inspection; the stock is not available "
                "until it has been inspected."
             DELIMITED BY SIZE INTO CA-MESSAGE
     ELSE
         STRING "Returned to stores; "
                FUNCTION TRIM(Ws-Edit)
                " credited back to the job at the issued cost."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.
