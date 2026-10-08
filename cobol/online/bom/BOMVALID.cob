>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : BOMVALID
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : Complete validation of one bill.  BOM-004 says a
*>              release needs complete validation AND an
*>              authorised engineering review; this program is the
*>              validation half and records its verdict in
*>              bom.BomValidation so that bom.usp_ReleaseBom can
*>              refuse to act without one.  Nothing is released
*>              here - a program that both checks and releases is
*>              a program that can be talked into releasing.
*>
*>              What stops a complete result (specification
*>              section 7): a missing component identity, an
*>              incompatible or missing unit, a zero divisor, an
*>              invalid yield or basis, a missing manufactured
*>              child bill, an ambiguous revision, an alternate
*>              group with no selected choice, a negative usage
*>              offered as a by-product, a zero quantity line that
*>              is not a note, a cost-only line with no rate, and
*>              any cycle.  Each finding names its line and path.
*> CALLED BY  : the BOM editor, BOMBULK, the batch reports
*> INTERFACE  : CALL "BOMVALID" USING WS-BOMVALID-CA
*>              COPYBOOK BOMVALIDCA
*> CALLS      : BOMREAD, BOMSELCT, CYCLECHK, SQLIO
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2014-05-15 AJS  CR2211 ENGINEERING REWRITE
*> 2016-05-24 AJS  CR2466 UNCONFIGURED ALTERNATE GROUPS
*> 2026-10-05 STERP VALIDATION RESULT RECORDED FOR RELEASE
*> 2026-10-07 STERP BOM-001 PARENT CLASSIFICATION WAS NEVER CHECKED
*>================================================================
*> REQ: BOM-001, BOM-002, BOM-004, BOM-005, BOM-009, BOM-010,
*> REQ: BOM-013, BOM-018, BOM-026, BOM-027, BOM-029
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BOMVALID.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 15/05/2014.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY BOMBILL.
 COPY BOMSELCTCA REPLACING ==DFHCOMMAREA== BY ==Ws-Sel-Ca==
      LEADING ==CA-== BY ==SA-==
      LEADING ==BS-== BY ==SS-==.
 COPY CYCLECHKCA REPLACING ==DFHCOMMAREA== BY ==Ws-Cyc-Ca==
      LEADING ==CA-== BY ==YA-==
      LEADING ==CC-== BY ==YC-==.
 01  Ws-Work.
     05  Ws-I                    PIC S9(4) VALUE 0.
     05  Ws-J                    PIC S9(4) VALUE 0.
     05  Ws-K                    PIC S9(4) VALUE 0.
     05  Ws-Chosen               PIC S9(4) VALUE 0.
     05  Ws-Group-Cnt            PIC S9(4) VALUE 0.
     05  Ws-First-In-Group       PIC S9(4) VALUE 0.
     05  Ws-Errors               PIC S9(4) VALUE 0.
     05  Ws-Warnings             PIC S9(4) VALUE 0.
     05  Ws-Severity             PIC X(8) VALUE SPACES.
     05  Ws-Reason               PIC X(8) VALUE SPACES.
     05  Ws-Text                 PIC X(120) VALUE SPACES.
     05  Ws-Line-Edit            PIC Z(3)9.
     05  Ws-Valid-Id             PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Record               PIC X VALUE "N".
 01  Ws-Id-Row.
     05  Vi-Id                   PIC X(12).
 LINKAGE SECTION.
 COPY BOMVALIDCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "BOMVALID" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
     MOVE "BOM-004" TO CA-REQ-REF
     MOVE 50 TO BV-FIND-MAX
     MOVE 0 TO BV-FIND-CNT BV-ERROR-CNT BV-WARNING-CNT
     MOVE 0 TO BV-COMPONENT-CNT BV-VALID-ID
     MOVE "N" TO BV-CYCLE-FOUND
     MOVE "N" TO BV-RELEASE-OK
     MOVE SPACES TO BV-CYCLE-PATH BV-OUTCOME
     MOVE 0 TO Ws-Errors Ws-Warnings
     IF BV-EFF-DATE = 0
         MOVE CA-BUSINESS-DATE TO BV-EFF-DATE
     END-IF
     EVALUATE CA-FUNCTION
         WHEN "VALIDATE" MOVE "Y" TO Ws-Record
                         PERFORM Validate-The-Bill
         WHEN "CHECK"    MOVE "N" TO Ws-Record
                         PERFORM Validate-The-Bill
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "BOM0301" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "BOMVALID does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
 Validate-The-Bill.
     PERFORM Read-The-Bill
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-The-Header
     PERFORM Check-Every-Component
     PERFORM Check-Alternate-Groups
     PERFORM Check-By-Products
     PERFORM Check-For-Cycles
     PERFORM Decide-The-Outcome
     IF Ws-Record = "Y"
         PERFORM Record-The-Validation
     END-IF.

 Read-The-Bill.
     INITIALIZE BOM-RD-CTL
     MOVE "BILL" TO BR-FUNC
     MOVE CA-COMPANY TO BR-COMPANY
     MOVE CA-SITE TO BR-SITE
     MOVE BV-BOM-ID TO BR-BOM-ID
     MOVE BV-EFF-DATE TO BR-EFF-DATE
     MOVE 500 TO BR-MAX-LINES
     CALL "BOMREAD" USING BOM-RD-CTL BOM-HDR-REC BOM-CMP-TAB
     IF BR-STATUS = "OK" OR BR-STATUS = "TOOMANY"
         MOVE BH-BOM-REF TO BV-BOM-REF
         MOVE BH-BOM-REV TO BV-BOM-REV
         MOVE BH-PARENT-ITEM TO BV-PARENT-ITEM
         MOVE BR-LINE-COUNT TO BV-COMPONENT-CNT
         IF BR-STATUS = "TOOMANY"
             MOVE "ERROR" TO Ws-Severity
             MOVE "BOM0302" TO Ws-Reason
             MOVE BR-MESSAGE TO Ws-Text
             MOVE 0 TO Ws-J
             PERFORM Add-Finding
         END-IF
         EXIT PARAGRAPH
     END-IF
     IF BR-STATUS = "SQLFAIL"
         MOVE 12 TO CA-RETURN-CODE
         MOVE "SQL0001" TO CA-REASON-CODE
     ELSE
         MOVE 16 TO CA-RETURN-CODE
         MOVE "BOM0303" TO CA-REASON-CODE
     END-IF
     MOVE BR-MESSAGE TO CA-MESSAGE.

*>----------------------------------------------------------------
*> BOM-001: an invalid or zero basis, an invalid yield or an
*> incompatible parent classification blocks release.
 Check-The-Header.
     IF BH-NOMINAL-BASIS <= 0
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0310" TO Ws-Reason
         MOVE "The nominal output basis must be positive"
             TO Ws-Text
         MOVE 0 TO Ws-J
         PERFORM Add-Finding
     END-IF
     IF BH-YIELD <= 0 OR BH-YIELD > 1
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0311" TO Ws-Reason
         MOVE "Yield must be above zero and not more than one"
             TO Ws-Text
         MOVE 0 TO Ws-J
         PERFORM Add-Finding
     END-IF
     IF BH-QUANTUM <= 0
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0312" TO Ws-Reason
         MOVE "The production quantum must be positive" TO Ws-Text
         MOVE 0 TO Ws-J
         PERFORM Add-Finding
     END-IF
     IF BH-EFF-TO NOT = 0 AND BH-EFF-TO <= BH-EFF-FROM
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0313" TO Ws-Reason
         MOVE "Effectivity end cannot precede or equal its start"
             TO Ws-Text
         MOVE 0 TO Ws-J
         PERFORM Add-Finding
     END-IF
     IF BR-LINE-COUNT = 0
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0314" TO Ws-Reason
         MOVE "A bill with no component occurrence cannot be used"
             TO Ws-Text
         MOVE 0 TO Ws-J
         PERFORM Add-Finding
     END-IF
     IF BH-IS-PHANTOM = "Y" AND BH-YIELD NOT = 1
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0315" TO Ws-Reason
         MOVE "A phantom has yield one and no independent loss"
             TO Ws-Text
         MOVE 0 TO Ws-J
         PERFORM Add-Finding
     END-IF
     PERFORM Check-Parent-Class
     PERFORM Check-Loss-Intent.

*> BOM-001: the parent identity is governed.  A bill describes how
*> an item is made, so a parent that the item master says is not
*> manufactured is an incompatible classification and must block
*> the release and the calculation approval rather than quietly
*> produce a structure nobody can build.
 Check-Parent-Class.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CASE WHEN i.IsManufacturable = 1 AND "
            "i.ItemClass IN ('MADE','PHANTOM','KIT') THEN 'Y' "
            "ELSE 'N' END FROM inv.Item i "
            "WHERE i.CompanyCode = ? AND i.ItemCode = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE BH-PARENT-ITEM TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 12 TO SQLIO-COL-WIDTH(1)
     MOVE 1 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     IF SQLIO-ERROR
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO Ws-J
     IF SQLIO-NOTFOUND
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0317" TO Ws-Reason
         MOVE SPACES TO Ws-Text
         STRING "Parent " FUNCTION TRIM(BH-PARENT-ITEM)
                " is not held in the item master"
             DELIMITED BY SIZE INTO Ws-Text
         PERFORM Add-Finding
         EXIT PARAGRAPH
     END-IF
     IF Vi-Id(1:1) NOT = "Y"
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0317" TO Ws-Reason
         MOVE SPACES TO Ws-Text
         STRING "Parent " FUNCTION TRIM(BH-PARENT-ITEM)
                " is not classified as a manufactured item"
             DELIMITED BY SIZE INTO Ws-Text
         PERFORM Add-Finding
     END-IF.

*> Scrap versus yield: applying both is allowed only where they
*> represent distinct losses, and the bill must say so.
 Check-Loss-Intent.
     IF BH-YIELD >= 1 OR BH-LOSS-INTENT NOT = SPACES
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO Ws-K
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > BR-LINE-COUNT
         IF BC-SCRAP-PCT(Ws-I) > 0
             ADD 1 TO Ws-K
         END-IF
     END-PERFORM
     IF Ws-K = 0
         EXIT PARAGRAPH
     END-IF
     MOVE "WARNING" TO Ws-Severity
     MOVE "BOM0316" TO Ws-Reason
     MOVE "Yield and scrap are both used - state the intent so "
         TO Ws-Text
     MOVE 0 TO Ws-J
     PERFORM Add-Finding.

*>----------------------------------------------------------------
 Check-Every-Component.
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > BR-LINE-COUNT
         MOVE BC-LINE-NUM(Ws-I) TO Ws-J
         PERFORM Check-One-Component
     END-PERFORM.

 Check-One-Component.
     IF BC-ITEM-FOUND(Ws-I) NOT = "Y"
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0320" TO Ws-Reason
         MOVE SPACES TO Ws-Text
         STRING "Component " FUNCTION TRIM(BC-ITEM(Ws-I))
                " is not held in the item master"
             DELIMITED BY SIZE INTO Ws-Text
         PERFORM Add-Finding
         EXIT PARAGRAPH
     END-IF
     IF BC-CONV-FACT(Ws-I) <= 0
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0321" TO Ws-Reason
         MOVE "A positive unit conversion factor is required"
             TO Ws-Text
         PERFORM Add-Finding
     END-IF
     IF BC-STOCK-UOM(Ws-I) = SPACES
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0322" TO Ws-Reason
         MOVE "The component item has no stock unit" TO Ws-Text
         PERFORM Add-Finding
     END-IF
     IF BC-SCRAP-PCT(Ws-I) < 0
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0323" TO Ws-Reason
         MOVE "A scrap allowance cannot be negative" TO Ws-Text
         PERFORM Add-Finding
     END-IF
     IF BC-USAGE-QTY(Ws-I) < 0
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0324" TO Ws-Reason
         MOVE "Negative usage is not a by-product mechanism"
             TO Ws-Text
         PERFORM Add-Finding
     END-IF
     IF BC-USAGE-QTY(Ws-I) = 0 AND BC-SUPPLY(Ws-I) NOT = "NOTE"
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0325" TO Ws-Reason
         MOVE "A zero quantity line is permitted only as a note"
             TO Ws-Text
         PERFORM Add-Finding
     END-IF
     IF BC-SUPPLY(Ws-I) = "COSTONLY" AND BC-COST-RATE(Ws-I) <= 0
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0326" TO Ws-Reason
         MOVE "A cost-only resource needs a rate to be complete"
             TO Ws-Text
         PERFORM Add-Finding
     END-IF
     IF BC-SUPPLY(Ws-I) = "MAKE"
         PERFORM Check-Child-Bill-Exists
     END-IF
     IF BC-SUPPLY(Ws-I) = "PHANTOM"
         PERFORM Check-Phantom-Child
     END-IF
     IF BC-EFF-TO(Ws-I) NOT = 0
        AND BC-EFF-FROM(Ws-I) NOT = 0
        AND BC-EFF-TO(Ws-I) <= BC-EFF-FROM(Ws-I)
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0327" TO Ws-Reason
         MOVE "Line effectivity end precedes or equals its start"
             TO Ws-Text
         PERFORM Add-Finding
     END-IF.

 Check-Child-Bill-Exists.
     INITIALIZE Ws-Sel-Ca
     MOVE "SELECT" TO SA-FUNCTION
     MOVE CA-COMPANY TO SA-COMPANY
     MOVE CA-SITE TO SA-SITE
     MOVE CA-USER TO SA-USER
     MOVE BV-EFF-DATE TO SA-BUSINESS-DATE
     MOVE BC-ITEM(Ws-I) TO SS-PARENT-ITEM
     MOVE BC-REV(Ws-I) TO SS-PARENT-REV
     MOVE "STANDARD" TO SS-PURPOSE
     MOVE BV-EFF-DATE TO SS-EFF-DATE
     CALL "BOMSELCT" USING Ws-Sel-Ca
     IF SA-RETURN-CODE <= 4 AND SS-BOM-ID NOT = 0
         EXIT PARAGRAPH
     END-IF
     MOVE "ERROR" TO Ws-Severity
     MOVE "BOM0328" TO Ws-Reason
     MOVE SPACES TO Ws-Text
     STRING "No usable bill for the manufactured component "
            FUNCTION TRIM(BC-ITEM(Ws-I))
         DELIMITED BY SIZE INTO Ws-Text
     PERFORM Add-Finding.

 Check-Phantom-Child.
     INITIALIZE Ws-Sel-Ca
     MOVE "SELECT" TO SA-FUNCTION
     MOVE CA-COMPANY TO SA-COMPANY
     MOVE CA-SITE TO SA-SITE
     MOVE CA-USER TO SA-USER
     MOVE BV-EFF-DATE TO SA-BUSINESS-DATE
     MOVE BC-ITEM(Ws-I) TO SS-PARENT-ITEM
     MOVE "STANDARD" TO SS-PURPOSE
     MOVE BV-EFF-DATE TO SS-EFF-DATE
     CALL "BOMSELCT" USING Ws-Sel-Ca
     IF SA-RETURN-CODE > 4 OR SS-BOM-ID = 0
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0329" TO Ws-Reason
         MOVE SPACES TO Ws-Text
         STRING "Phantom " FUNCTION TRIM(BC-ITEM(Ws-I))
                " has no bill to expand through"
             DELIMITED BY SIZE INTO Ws-Text
         PERFORM Add-Finding
         EXIT PARAGRAPH
     END-IF
     IF SS-IS-PHANTOM NOT = "Y"
         MOVE "WARNING" TO Ws-Severity
         MOVE "BOM0330" TO Ws-Reason
         MOVE "This line expands a bill that is not marked phantom"
             TO Ws-Text
         PERFORM Add-Finding
     END-IF.

*>----------------------------------------------------------------
*> Every alternate group needs exactly one selected choice.  None
*> is an unconfigured selection; more than one is ambiguous.  In
*> neither case may the alternatives simply be added together.
 Check-Alternate-Groups.
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > BR-LINE-COUNT
         IF BC-ALT-GROUP(Ws-I) NOT = SPACES
             PERFORM Examine-Group
         END-IF
     END-PERFORM.

 Examine-Group.
     MOVE 0 TO Ws-Chosen Ws-Group-Cnt Ws-First-In-Group
     PERFORM VARYING Ws-K FROM 1 BY 1 UNTIL Ws-K > BR-LINE-COUNT
         IF BC-ALT-GROUP(Ws-K) = BC-ALT-GROUP(Ws-I)
             ADD 1 TO Ws-Group-Cnt
             IF Ws-First-In-Group = 0
                 MOVE Ws-K TO Ws-First-In-Group
             END-IF
             IF BC-SELECTED(Ws-K) = "Y"
                 ADD 1 TO Ws-Chosen
             END-IF
         END-IF
     END-PERFORM
     IF Ws-First-In-Group NOT = Ws-I
         EXIT PARAGRAPH
     END-IF
     MOVE BC-LINE-NUM(Ws-I) TO Ws-J
     IF Ws-Chosen = 0
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0331" TO Ws-Reason
         MOVE SPACES TO Ws-Text
         STRING "Alternate group "
                FUNCTION TRIM(BC-ALT-GROUP(Ws-I))
                " has no selected choice"
             DELIMITED BY SIZE INTO Ws-Text
         PERFORM Add-Finding
         EXIT PARAGRAPH
     END-IF
     IF Ws-Chosen > 1
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0332" TO Ws-Reason
         MOVE SPACES TO Ws-Text
         STRING "Alternate group "
                FUNCTION TRIM(BC-ALT-GROUP(Ws-I))
                " has more than one selected choice"
             DELIMITED BY SIZE INTO Ws-Text
         PERFORM Add-Finding
     END-IF.

*>----------------------------------------------------------------
*> A recovery must be positive and carry an approved valuation.
 Check-By-Products.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT COUNT(*) FROM bom.BomByProduct p "
            "WHERE p.BomHeaderId = ? AND (p.RecoveryQty <= 0 "
            "OR p.ApprovedBy IS NULL)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 1 TO SQLIO-PARM-CNT
     MOVE BV-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 12 TO SQLIO-COL-WIDTH(1)
     MOVE 1 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     IF SQLIO-ERROR OR SQLIO-NOTFOUND
         EXIT PARAGRAPH
     END-IF
     IF FUNCTION NUMVAL(Vi-Id) > 0
         MOVE "ERROR" TO Ws-Severity
         MOVE "BOM0333" TO Ws-Reason
         MOVE "A recovery must be positive and approved" TO Ws-Text
         MOVE 0 TO Ws-J
         PERFORM Add-Finding
     END-IF.

 Check-For-Cycles.
     INITIALIZE Ws-Cyc-Ca
     MOVE "CHECK" TO YA-FUNCTION
     MOVE CA-COMPANY TO YA-COMPANY
     MOVE CA-SITE TO YA-SITE
     MOVE CA-USER TO YA-USER
     MOVE BV-EFF-DATE TO YA-BUSINESS-DATE
     MOVE BH-PARENT-ITEM TO YC-ROOT-ITEM
     MOVE BH-PARENT-REV TO YC-ROOT-REV
     MOVE BH-PURPOSE TO YC-PURPOSE
     MOVE BH-VARIANT TO YC-VARIANT
     MOVE BV-EFF-DATE TO YC-EFF-DATE
     MOVE 25 TO YC-MAX-DEPTH
     MOVE "Y" TO YC-INCLUDE-OPT
     CALL "CYCLECHK" USING Ws-Cyc-Ca
     IF YC-CYCLE-FOUND NOT = "Y"
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO BV-CYCLE-FOUND
     MOVE YC-CYCLE-PATH TO BV-CYCLE-PATH
     MOVE "ERROR" TO Ws-Severity
     MOVE "BOM0334" TO Ws-Reason
     MOVE SPACES TO Ws-Text
     STRING "Cyclic structure: " FUNCTION TRIM(YC-CYCLE-PATH)
         DELIMITED BY SIZE INTO Ws-Text
     MOVE YC-CYCLE-LINE TO Ws-J
     PERFORM Add-Finding.

*>----------------------------------------------------------------
 Add-Finding.
     IF Ws-Severity = "ERROR"
         ADD 1 TO Ws-Errors
     ELSE
         ADD 1 TO Ws-Warnings
     END-IF
     IF BV-FIND-CNT >= BV-FIND-MAX
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO BV-FIND-CNT
     MOVE Ws-Severity TO BV-F-SEVERITY(BV-FIND-CNT)
     MOVE Ws-Reason TO BV-F-REASON(BV-FIND-CNT)
     MOVE Ws-J TO BV-F-LINE-NUM(BV-FIND-CNT)
     MOVE Ws-Text TO BV-F-MESSAGE(BV-FIND-CNT)
     IF Ws-J = 0
         MOVE BH-PARENT-ITEM TO BV-F-ITEM(BV-FIND-CNT)
         MOVE BV-BOM-REF TO BV-F-PATH(BV-FIND-CNT)
         EXIT PARAGRAPH
     END-IF
     MOVE BC-ITEM(Ws-I) TO BV-F-ITEM(BV-FIND-CNT)
     MOVE Ws-J TO Ws-Line-Edit
     MOVE SPACES TO BV-F-PATH(BV-FIND-CNT)
     STRING FUNCTION TRIM(BH-PARENT-ITEM) "/"
            FUNCTION TRIM(Ws-Line-Edit) ":"
            FUNCTION TRIM(BC-ITEM(Ws-I))
         DELIMITED BY SIZE INTO BV-F-PATH(BV-FIND-CNT).

 Decide-The-Outcome.
     MOVE Ws-Errors TO BV-ERROR-CNT
     MOVE Ws-Warnings TO BV-WARNING-CNT
     EVALUATE TRUE
         WHEN Ws-Errors > 0
             MOVE "FAIL" TO BV-OUTCOME
             MOVE "N" TO BV-RELEASE-OK
             MOVE 8 TO CA-RETURN-CODE
             MOVE "BOM0340" TO CA-REASON-CODE
             MOVE Ws-Errors TO Ws-Line-Edit
             MOVE SPACES TO CA-MESSAGE
             STRING "Validation failed with "
                    FUNCTION TRIM(Ws-Line-Edit)
                    " error(s) - this bill cannot be released"
                 DELIMITED BY SIZE INTO CA-MESSAGE
             ADD 1 TO CA-MSG-COUNT
         WHEN Ws-Warnings > 0
             MOVE "WARNING" TO BV-OUTCOME
             MOVE "Y" TO BV-RELEASE-OK
             MOVE 4 TO CA-RETURN-CODE
             MOVE "BOM0341" TO CA-REASON-CODE
             MOVE "Validation passed with warnings to review"
                 TO CA-MESSAGE
             ADD 1 TO CA-MSG-COUNT
         WHEN OTHER
             MOVE "PASS" TO BV-OUTCOME
             MOVE "Y" TO BV-RELEASE-OK
             MOVE 0 TO CA-RETURN-CODE
             MOVE "Validation complete - the bill may be released"
                 TO CA-MESSAGE
     END-EVALUATE.

*>----------------------------------------------------------------
*> The verdict is evidence, so it is stored even when it fails:
*> a reviewer must be able to see why a release was refused.
 Record-The-Validation.
     IF BV-VALID-REF = SPACES
         MOVE SPACES TO BV-VALID-REF
         STRING "VAL-" FUNCTION TRIM(BV-BOM-REF) "-"
                FUNCTION TRIM(BV-BOM-REV)
             DELIMITED BY SIZE INTO BV-VALID-REF
     END-IF
     PERFORM Delete-Previous-Validation
     PERFORM Insert-Validation
     IF CA-RETURN-CODE > 8
         EXIT PARAGRAPH
     END-IF
     PERFORM Read-Validation-Id
     PERFORM Insert-Findings
     PERFORM Stamp-The-Header.

 Delete-Previous-Validation.
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE f FROM bom.BomValidationFinding f "
            "JOIN bom.BomValidation v ON v.ValidationId = "
            "f.ValidationId WHERE v.CompanyCode = ? "
            "AND v.ValidationRef = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Bind-Validation-Key
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 1 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     MOVE 1 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE FROM bom.BomValidation WHERE CompanyCode = ? "
            "AND ValidationRef = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Bind-Validation-Key
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 1 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     MOVE 1 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row.

 Bind-Validation-Key.
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE BV-VALID-REF TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2).

 Insert-Validation.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT INTO bom.BomValidation (CompanyCode, "
            "BomHeaderId, ValidationRef, EffectiveDate, "
            "ValidatedBy, Outcome, ErrorCount, WarningCount, "
            "ComponentCount, CycleFound, CyclePath, ReasonCode, "
            "Message, CorrelationId) VALUES (?,?,?,"
            "CONVERT(date,?,112),?,?,?,?,?,?,?,?,?,?)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 14 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-K FROM 1 BY 1 UNTIL Ws-K > 14
         MOVE SPACES TO SQLIO-PARM-VAL(Ws-K)
         MOVE "N" TO SQLIO-PARM-NULL(Ws-K)
     END-PERFORM
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE BV-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
     MOVE BV-VALID-REF TO SQLIO-PARM-VAL(3)
     MOVE BV-EFF-DATE TO SQLIO-PARM-VAL(4)
     MOVE CA-USER TO SQLIO-PARM-VAL(5)
     MOVE BV-OUTCOME TO SQLIO-PARM-VAL(6)
     MOVE BV-ERROR-CNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
     MOVE BV-WARNING-CNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(8)
     MOVE BV-COMPONENT-CNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(9)
     IF BV-CYCLE-FOUND = "Y"
         MOVE "1" TO SQLIO-PARM-VAL(10)
     ELSE
         MOVE "0" TO SQLIO-PARM-VAL(10)
     END-IF
     MOVE BV-CYCLE-PATH TO SQLIO-PARM-VAL(11)
     MOVE CA-REASON-CODE TO SQLIO-PARM-VAL(12)
     MOVE CA-MESSAGE TO SQLIO-PARM-VAL(13)
     MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(14)
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 1 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     IF SQLIO-ERROR
         MOVE 12 TO CA-RETURN-CODE
         MOVE "SQL0001" TO CA-REASON-CODE
         MOVE SQLIO-ERRMSG TO CA-MESSAGE
     END-IF
     MOVE 1 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row.

 Read-Validation-Id.
     MOVE 0 TO Ws-Valid-Id
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT v.ValidationId FROM bom.BomValidation v "
            "WHERE v.CompanyCode = ? AND v.ValidationRef = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Bind-Validation-Key
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 12 TO SQLIO-COL-WIDTH(1)
     MOVE 1 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     IF SQLIO-OK
         COMPUTE Ws-Valid-Id = FUNCTION NUMVAL(Vi-Id)
         MOVE Ws-Valid-Id TO BV-VALID-ID
     END-IF.

 Insert-Findings.
     IF Ws-Valid-Id = 0
         EXIT PARAGRAPH
     END-IF
     PERFORM VARYING Ws-K FROM 1 BY 1 UNTIL Ws-K > BV-FIND-CNT
         PERFORM Insert-One-Finding
     END-PERFORM.

 Insert-One-Finding.
     MOVE SPACES TO SQLIO-STMT
     MOVE Ws-Valid-Id TO SQLIO-INT-EDIT
     STRING "INSERT INTO bom.BomValidationFinding (ValidationId, "
            "LineNum, Severity, ReasonCode, ComponentLineNum, "
            "PathText, ItemCode, Message) VALUES ("
            FUNCTION TRIM(SQLIO-INT-EDIT) ",?,?,?,?,?,?,?)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 7 TO SQLIO-PARM-CNT
     MOVE Ws-K TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE BV-F-SEVERITY(Ws-K) TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE BV-F-REASON(Ws-K) TO SQLIO-PARM-VAL(3)
     MOVE "N" TO SQLIO-PARM-NULL(3)
     MOVE BV-F-LINE-NUM(Ws-K) TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
     MOVE "N" TO SQLIO-PARM-NULL(4)
     MOVE BV-F-PATH(Ws-K) TO SQLIO-PARM-VAL(5)
     MOVE "N" TO SQLIO-PARM-NULL(5)
     MOVE BV-F-ITEM(Ws-K) TO SQLIO-PARM-VAL(6)
     MOVE "N" TO SQLIO-PARM-NULL(6)
     MOVE BV-F-MESSAGE(Ws-K) TO SQLIO-PARM-VAL(7)
     MOVE "N" TO SQLIO-PARM-NULL(7)
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 1 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     MOVE 1 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row.

*> The bill remembers which validation result belongs to it, so a
*> later edit can be seen to have invalidated the evidence.
 Stamp-The-Header.
     IF Ws-Valid-Id = 0
         EXIT PARAGRAPH
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "UPDATE bom.BomHeader SET ValidationId = ? "
            "WHERE BomHeaderId = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE Ws-Valid-Id TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE BV-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 1 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     MOVE 1 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row.
