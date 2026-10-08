>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : BOMJOB
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : Derive a job or order specific bill from a
*>              released standard, or raise the next draft
*>              revision of an existing bill, with complete
*>              lineage (BOM-022).
*>
*>              Three rules do most of the work here:
*>              - the copy is always a DRAFT.  Copying a released
*>                bill does not copy its release;
*>              - execution history, stock allocations and prior
*>                approvals are excluded, and the exclusion is
*>                written down rather than assumed;
*>              - editing the copy never touches the standard or
*>                another job's bill, because it is a separate
*>                row with its own identity (BOM-003).
*>              A repeated confirmed copy under the same
*>              correlation id returns the first bill instead of
*>              breeding duplicates (BOM-030).
*> CALLED BY  : the BOM editor, the kitting module
*> INTERFACE  : CALL "BOMJOB" USING WS-BOMJOB-CA
*>              COPYBOOK BOMJOBCA
*> CALLS      : BOMREAD, SQLIO, AUDITLOG
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2015-11-17 AJS  CR2384 JOB SPECIFIC BILLS ADDED
*> 2026-10-05 STERP LINEAGE TABLE AND EXCLUSION NOTE
*> 2026-10-07 STERP BOM-022 LINEAGE READ ITS OWN KEY AFTER CLEARING
*>                  IT, SO IT NEVER RETURNED A ROW
*>================================================================
*> REQ: BOM-003, BOM-022, BOM-028, BOM-030
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BOMJOB.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 17/11/2015.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY BOMBILL.
 COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==Ws-Audit-Ca==
      LEADING ==CA-== BY ==UA-==.
 01  Ws-Work.
     05  Ws-I                    PIC S9(4) VALUE 0.
     05  Ws-Copied               PIC S9(4) VALUE 0.
     05  Ws-Excluded             PIC S9(4) VALUE 0.
     05  Ws-New-Id               PIC S9(9) COMP-5 VALUE 0.
     05  Ws-K                    PIC S9(4) VALUE 0.
     05  Ws-Edit                 PIC Z(3)9.
 01  Ws-Id-Row.
     05  Ji-Id                   PIC X(12).
     05  Ji-Ref                  PIC X(30).
 LINKAGE SECTION.
 COPY BOMJOBCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "BOMJOB" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE "BOM-022" TO CA-REQ-REF
     MOVE "N" TO BJ-DUPLICATE
     MOVE 0 TO BJ-LINES-COPIED BJ-LINES-EXCLUDED
     MOVE 0 TO BJ-LINEAGE-ID BJ-LINEAGE-CNT
*> LINEAGE is a read keyed by the bill the caller names, so its
*> key is the one field that must survive the housekeeping.
     IF CA-FUNCTION NOT = "LINEAGE"
         MOVE 0 TO BJ-TARGET-BOM-ID
     END-IF
     IF BJ-EFF-DATE = 0
         MOVE CA-BUSINESS-DATE TO BJ-EFF-DATE
     END-IF
     EVALUATE CA-FUNCTION
         WHEN "DERIVE"  PERFORM Derive-A-Job-Bill
         WHEN "NEWREV"  PERFORM Raise-A-New-Revision
         WHEN "LINEAGE" PERFORM Read-The-Lineage
         WHEN OTHER     PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "BOM1201" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "BOMJOB does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
 Derive-A-Job-Bill.
     PERFORM Validate-Request
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Test-For-Duplicate
     IF BJ-DUPLICATE = "Y"
         EXIT PARAGRAPH
     END-IF
     PERFORM Read-The-Source
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE "BEGN" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     PERFORM Insert-Target-Header
     IF CA-RETURN-CODE > 4
         PERFORM Roll-It-Back
         EXIT PARAGRAPH
     END-IF
     PERFORM Read-Target-Id
     PERFORM Copy-The-Lines
     IF CA-RETURN-CODE > 4
         PERFORM Roll-It-Back
         EXIT PARAGRAPH
     END-IF
     PERFORM Write-The-Lineage
     MOVE "COMT" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     PERFORM Write-The-Audit
     PERFORM Publish-Result.

 Roll-It-Back.
     MOVE "ROLB" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row.

 Validate-Request.
     IF BJ-SOURCE-BOM-ID = 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "BOM1202" TO CA-REASON-CODE
         MOVE "A source bill must be named" TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF BJ-TARGET-REF = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "BOM1203" TO CA-REASON-CODE
         MOVE "A reference for the new bill is required"
             TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF BJ-PURPOSE = SPACES
         MOVE "JOB" TO BJ-PURPOSE
     END-IF
     IF BJ-PURPOSE NOT = "STANDARD" AND BJ-JOB-REF = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "BOM1204" TO CA-REASON-CODE
         MOVE "A job or order reference is required for this copy"
             TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF BJ-TARGET-REV = SPACES
         MOVE "A" TO BJ-TARGET-REV
     END-IF.

*> COM-015: a retried confirmation returns the original bill.
 Test-For-Duplicate.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT h.BomHeaderId, h.BomRef FROM bom.BomHeader h "
            "WHERE h.CompanyCode = ? AND h.BomRef = ? "
            "AND h.BomRevision = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 3 TO SQLIO-PARM-CNT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE BJ-TARGET-REF TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE BJ-TARGET-REV TO SQLIO-PARM-VAL(3)
     MOVE "N" TO SQLIO-PARM-NULL(3)
     MOVE 2 TO SQLIO-COL-CNT
     MOVE 12 TO SQLIO-COL-WIDTH(1)
     MOVE 30 TO SQLIO-COL-WIDTH(2)
     MOVE 1 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     IF SQLIO-NOTFOUND
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO BJ-DUPLICATE
     COMPUTE BJ-TARGET-BOM-ID = FUNCTION NUMVAL(Ji-Id)
     MOVE "DRAFT" TO BJ-STATUS
     MOVE 4 TO CA-RETURN-CODE
     MOVE "BOM1210" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "Bill " FUNCTION TRIM(BJ-TARGET-REF) " revision "
            FUNCTION TRIM(BJ-TARGET-REV)
            " already exists - the original copy is returned"
         DELIMITED BY SIZE INTO CA-MESSAGE
     ADD 1 TO CA-MSG-COUNT.

 Read-The-Source.
     INITIALIZE BOM-RD-CTL
     MOVE "BILL" TO BR-FUNC
     MOVE CA-COMPANY TO BR-COMPANY
     MOVE CA-SITE TO BR-SITE
     MOVE BJ-SOURCE-BOM-ID TO BR-BOM-ID
     MOVE BJ-EFF-DATE TO BR-EFF-DATE
     MOVE 500 TO BR-MAX-LINES
     CALL "BOMREAD" USING BOM-RD-CTL BOM-HDR-REC BOM-CMP-TAB
     IF BR-STATUS NOT = "OK"
         MOVE 16 TO CA-RETURN-CODE
         MOVE "BOM1205" TO CA-REASON-CODE
         MOVE BR-MESSAGE TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE BH-BOM-REF TO BJ-SOURCE-REF
     MOVE BH-BOM-REV TO BJ-SOURCE-REV
     MOVE BH-PARENT-ITEM TO BJ-PARENT-ITEM.

*>----------------------------------------------------------------
*> The copy is a DRAFT with no reviewer, no release evidence and
*> no superseding link: none of that belongs to a new bill.
 Insert-Target-Header.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT INTO bom.BomHeader (CompanyCode, SiteCode, "
            "BomRef, BomRevision, ParentItemCode, ParentRevision, "
            "Purpose, JobRef, VariantCode, Description, "
            "NominalBasis, YieldFactor, ProductionQuantum, "
            "BatchSize, IsPhantom, ConsolidateRuns, CostingLotSize,"
            " SetupCostPerRun, ConversionCostPerStart, "
            "EffectiveFrom, Status, AuthorUser, ChangeReason, "
            "LossIntentNote, SourceBomHeaderId, CreatedBy) "
            "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,"
            "CONVERT(date,?,112),'DRAFT',?,?,?,?,?)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 25 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > 25
         MOVE SPACES TO SQLIO-PARM-VAL(Ws-I)
         MOVE "N" TO SQLIO-PARM-NULL(Ws-I)
     END-PERFORM
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     IF BJ-SITE = SPACES
         MOVE BH-SITE TO SQLIO-PARM-VAL(2)
     ELSE
         MOVE BJ-SITE TO SQLIO-PARM-VAL(2)
     END-IF
     IF SQLIO-PARM-VAL(2) = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(2)
     END-IF
     MOVE BJ-TARGET-REF TO SQLIO-PARM-VAL(3)
     MOVE BJ-TARGET-REV TO SQLIO-PARM-VAL(4)
     MOVE BH-PARENT-ITEM TO SQLIO-PARM-VAL(5)
     MOVE BH-PARENT-REV TO SQLIO-PARM-VAL(6)
     MOVE BJ-PURPOSE TO SQLIO-PARM-VAL(7)
     MOVE BJ-JOB-REF TO SQLIO-PARM-VAL(8)
     IF BJ-JOB-REF = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(8)
     END-IF
     MOVE BH-VARIANT TO SQLIO-PARM-VAL(9)
     IF BH-VARIANT = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(9)
     END-IF
     MOVE BH-DESCRIPTION TO SQLIO-PARM-VAL(10)
     MOVE BH-NOMINAL-BASIS TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
     MOVE BH-YIELD TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
     MOVE BH-QUANTUM TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
     MOVE BH-BATCH-SIZE TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(14)
     IF BH-BATCH-SIZE = 0
         MOVE "Y" TO SQLIO-PARM-NULL(14)
     END-IF
     IF BH-IS-PHANTOM = "Y"
         MOVE "1" TO SQLIO-PARM-VAL(15)
     ELSE
         MOVE "0" TO SQLIO-PARM-VAL(15)
     END-IF
     IF BH-CONSOLIDATE = "Y"
         MOVE "1" TO SQLIO-PARM-VAL(16)
     ELSE
         MOVE "0" TO SQLIO-PARM-VAL(16)
     END-IF
     MOVE BH-COSTING-LOT TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(17)
     IF BH-COSTING-LOT = 0
         MOVE "Y" TO SQLIO-PARM-NULL(17)
     END-IF
     MOVE BH-SETUP-COST TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(18)
     MOVE BH-CONV-COST TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(19)
     MOVE BJ-EFF-DATE TO SQLIO-PARM-VAL(20)
     MOVE CA-USER TO SQLIO-PARM-VAL(21)
     MOVE SPACES TO SQLIO-PARM-VAL(22)
     STRING "Derived from " FUNCTION TRIM(BH-BOM-REF) " revision "
            FUNCTION TRIM(BH-BOM-REV)
         DELIMITED BY SIZE INTO SQLIO-PARM-VAL(22)
     MOVE BH-LOSS-INTENT TO SQLIO-PARM-VAL(23)
     IF BH-LOSS-INTENT = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(23)
     END-IF
     MOVE BJ-SOURCE-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(24)
     MOVE CA-USER TO SQLIO-PARM-VAL(25)
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 2 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
     END-IF
     MOVE 2 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row.

 Read-Target-Id.
     MOVE 0 TO Ws-New-Id
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT h.BomHeaderId, h.BomRef FROM bom.BomHeader h "
            "WHERE h.CompanyCode = ? AND h.BomRef = ? "
            "AND h.BomRevision = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 3 TO SQLIO-PARM-CNT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE BJ-TARGET-REF TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE BJ-TARGET-REV TO SQLIO-PARM-VAL(3)
     MOVE "N" TO SQLIO-PARM-NULL(3)
     MOVE 2 TO SQLIO-COL-CNT
     MOVE 12 TO SQLIO-COL-WIDTH(1)
     MOVE 30 TO SQLIO-COL-WIDTH(2)
     MOVE 2 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     IF SQLIO-OK
         COMPUTE Ws-New-Id = FUNCTION NUMVAL(Ji-Id)
         MOVE Ws-New-Id TO BJ-TARGET-BOM-ID
     END-IF.

*>----------------------------------------------------------------
*> Selected source content only.  Optional lines, by-products and
*> note lines are copied when the caller asks for them; whatever
*> is left out is counted and named in the exclusion note.
 Copy-The-Lines.
     MOVE 0 TO Ws-Copied Ws-Excluded
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > BR-LINE-COUNT
         IF BC-OPTIONAL(Ws-I) = "Y" AND BJ-COPY-OPTIONAL NOT = "Y"
             ADD 1 TO Ws-Excluded
         ELSE
             IF BC-SUPPLY(Ws-I) = "NOTE"
                AND BJ-COPY-NOTES NOT = "Y"
                 ADD 1 TO Ws-Excluded
             ELSE
                 PERFORM Copy-One-Line
             END-IF
         END-IF
     END-PERFORM
     MOVE Ws-Copied TO BJ-LINES-COPIED
     MOVE Ws-Excluded TO BJ-LINES-EXCLUDED
     MOVE SPACES TO BJ-EXCLUSION-NOTE
     MOVE Ws-Excluded TO Ws-Edit
     STRING "Excluded " FUNCTION TRIM(Ws-Edit)
            " line(s); execution history, stock allocations and "
            "prior approvals are never copied"
         DELIMITED BY SIZE INTO BJ-EXCLUSION-NOTE.

 Copy-One-Line.
     MOVE SPACES TO SQLIO-STMT
     MOVE Ws-New-Id TO SQLIO-INT-EDIT
     STRING "INSERT INTO bom.BomComponent (BomHeaderId, LineNum, "
            "PathRef, ComponentItemCode, ComponentRevision, "
            "UsageQty, UsageBasis, UomCode, ConversionFactor, "
            "ScrapPct, SupplyBehaviour, IsOptional, AlternateGroup,"
            " IsSelected, OptionChoiceRef, NeedOffsetDays, "
            "OperationRef, CostOnlyRate, CostElementCode, "
            "CreatedBy) VALUES ("
            FUNCTION TRIM(SQLIO-INT-EDIT)
            ",?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 19 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-K FROM 1 BY 1 UNTIL Ws-K > 19
         MOVE SPACES TO SQLIO-PARM-VAL(Ws-K)
         MOVE "N" TO SQLIO-PARM-NULL(Ws-K)
     END-PERFORM
     MOVE BC-LINE-NUM(Ws-I) TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
     MOVE SPACES TO SQLIO-PARM-VAL(2)
     STRING FUNCTION TRIM(BJ-TARGET-REF) "/"
            FUNCTION TRIM(SQLIO-INT-EDIT)
         DELIMITED BY SIZE INTO SQLIO-PARM-VAL(2)
     MOVE BC-ITEM(Ws-I) TO SQLIO-PARM-VAL(3)
     MOVE BC-REV(Ws-I) TO SQLIO-PARM-VAL(4)
     IF BC-REV(Ws-I) = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(4)
     END-IF
     MOVE BC-USAGE-QTY(Ws-I) TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
     MOVE BC-USAGE-BASIS(Ws-I) TO SQLIO-PARM-VAL(6)
     MOVE BC-UOM(Ws-I) TO SQLIO-PARM-VAL(7)
     MOVE BC-CONV-FACT(Ws-I) TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
     MOVE BC-SCRAP-PCT(Ws-I) TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
     MOVE BC-SUPPLY(Ws-I) TO SQLIO-PARM-VAL(10)
     IF BC-OPTIONAL(Ws-I) = "Y"
         MOVE "1" TO SQLIO-PARM-VAL(11)
     ELSE
         MOVE "0" TO SQLIO-PARM-VAL(11)
     END-IF
     MOVE BC-ALT-GROUP(Ws-I) TO SQLIO-PARM-VAL(12)
     IF BC-ALT-GROUP(Ws-I) = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(12)
     END-IF
     IF BC-SELECTED(Ws-I) = "Y"
         MOVE "1" TO SQLIO-PARM-VAL(13)
     ELSE
         MOVE "0" TO SQLIO-PARM-VAL(13)
     END-IF
     MOVE BC-OPTION-REF(Ws-I) TO SQLIO-PARM-VAL(14)
     IF BC-OPTION-REF(Ws-I) = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(14)
     END-IF
     MOVE BC-NEED-OFFSET(Ws-I) TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(15)
     MOVE BC-OPERATION(Ws-I) TO SQLIO-PARM-VAL(16)
     IF BC-OPERATION(Ws-I) = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(16)
     END-IF
     MOVE BC-COST-RATE(Ws-I) TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(17)
     IF BC-COST-RATE(Ws-I) = 0
         MOVE "Y" TO SQLIO-PARM-NULL(17)
     END-IF
     MOVE BC-COST-ELEMENT(Ws-I) TO SQLIO-PARM-VAL(18)
     IF BC-COST-ELEMENT(Ws-I) = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(18)
     END-IF
     MOVE CA-USER TO SQLIO-PARM-VAL(19)
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 2 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
     ELSE
         ADD 1 TO Ws-Copied
     END-IF
     MOVE 2 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row.

*>----------------------------------------------------------------
 Write-The-Lineage.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT INTO bom.BomLineage (CompanyCode, "
            "TargetBomHeaderId, SourceBomHeaderId, SourceBomRef, "
            "SourceBomRevision, DerivationType, DemandType, "
            "DemandRef, LinesCopied, LinesExcluded, ExclusionNote, "
            "DerivedBy, CorrelationId) "
            "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 13 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > 13
         MOVE SPACES TO SQLIO-PARM-VAL(Ws-I)
         MOVE "N" TO SQLIO-PARM-NULL(Ws-I)
     END-PERFORM
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE Ws-New-Id TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
     MOVE BJ-SOURCE-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
     MOVE BJ-SOURCE-REF TO SQLIO-PARM-VAL(4)
     MOVE BJ-SOURCE-REV TO SQLIO-PARM-VAL(5)
     MOVE "COPY" TO SQLIO-PARM-VAL(6)
     MOVE BJ-DEMAND-TYPE TO SQLIO-PARM-VAL(7)
     IF BJ-DEMAND-TYPE = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(7)
     END-IF
     MOVE BJ-DEMAND-REF TO SQLIO-PARM-VAL(8)
     IF BJ-DEMAND-REF = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(8)
     END-IF
     MOVE Ws-Copied TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(9)
     MOVE Ws-Excluded TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(10)
     MOVE BJ-EXCLUSION-NOTE TO SQLIO-PARM-VAL(11)
     MOVE CA-USER TO SQLIO-PARM-VAL(12)
     MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(13)
     IF CA-CORRELATION-ID = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(13)
     END-IF
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 2 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     MOVE 2 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row.

 Write-The-Audit.
     INITIALIZE Ws-Audit-Ca
     MOVE "WRITE" TO UA-FUNCTION
     MOVE CA-COMPANY TO UA-COMPANY
     MOVE CA-SITE TO UA-SITE
     MOVE CA-USER TO UA-USER
     MOVE CA-BUSINESS-DATE TO UA-BUSINESS-DATE
     MOVE "BOMHEADER" TO AU-ENTITY-TYPE
     MOVE BJ-TARGET-REF TO AU-ENTITY-KEY
     MOVE "DERIVE" TO AU-ACTION
     MOVE "BomRef" TO AU-FIELD-NAME
     MOVE BJ-SOURCE-REF TO AU-OLD-VALUE
     MOVE BJ-TARGET-REF TO AU-NEW-VALUE
     MOVE BJ-EXCLUSION-NOTE TO AU-REASON
     MOVE "BOMJOB" TO AU-SOURCE
     CALL "AUDITLOG" USING Ws-Audit-Ca.

 Publish-Result.
     MOVE "DRAFT" TO BJ-STATUS
     MOVE 0 TO CA-RETURN-CODE
     MOVE Ws-Copied TO Ws-Edit
     MOVE SPACES TO CA-MESSAGE
     STRING "Bill " FUNCTION TRIM(BJ-TARGET-REF)
            " created as a draft with "
            FUNCTION TRIM(Ws-Edit) " line(s)"
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
*> A new revision of the same bill is the controlled way to change
*> a released definition: the released one stays intact (BOM-004).
 Raise-A-New-Revision.
     MOVE BJ-SOURCE-REF TO BJ-TARGET-REF
     IF BJ-TARGET-REV = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "BOM1206" TO CA-REASON-CODE
         MOVE "A revision letter is required for the new revision"
             TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE "STANDARD" TO BJ-PURPOSE
     MOVE SPACES TO BJ-JOB-REF
     PERFORM Derive-A-Job-Bill.

 Read-The-Lineage.
     MOVE 0 TO BJ-LINEAGE-CNT
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT ISNULL(l.SourceBomRef,''), "
            "ISNULL(l.SourceBomRevision,''), l.DerivationType, "
            "l.DerivedBy, CONVERT(char(19), l.DerivedAt, 120), "
            "ISNULL(l.DemandRef,'') FROM bom.BomLineage l "
            "WHERE l.CompanyCode = ? AND l.TargetBomHeaderId = ? "
            "ORDER BY l.LineageId"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE BJ-TARGET-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE 6 TO SQLIO-COL-CNT
     MOVE 30 TO SQLIO-COL-WIDTH(1)
     MOVE 4 TO SQLIO-COL-WIDTH(2)
     MOVE 12 TO SQLIO-COL-WIDTH(3)
     MOVE 20 TO SQLIO-COL-WIDTH(4)
     MOVE 20 TO SQLIO-COL-WIDTH(5)
     MOVE 40 TO SQLIO-COL-WIDTH(6)
     MOVE 1 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA BJ-LINEAGE(1)
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     PERFORM UNTIL BJ-LINEAGE-CNT >= 10
         MOVE 1 TO SQLIO-CURSOR
         MOVE "FTCH" TO SQLIO-FUNC
         ADD 1 TO BJ-LINEAGE-CNT
         CALL "SQLIO" USING SQLIO-CA BJ-LINEAGE(BJ-LINEAGE-CNT)
         IF NOT SQLIO-OK
             SUBTRACT 1 FROM BJ-LINEAGE-CNT
             EXIT PERFORM
         END-IF
     END-PERFORM
     MOVE 1 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     IF BJ-LINEAGE-CNT = 0
         MOVE 16 TO CA-RETURN-CODE
         MOVE "BOM1207" TO CA-REASON-CODE
         MOVE "No lineage is recorded for that bill" TO CA-MESSAGE
     END-IF.

 Report-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE SQLIO-ERRMSG TO CA-MESSAGE.
