>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : TBOMGAP
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL - UNIT TESTS
*> PURPOSE    : The three governed-definition rules that had no
*>              test of their own.
*>
*>              BOM-001  A bill header carries a governed parent
*>                       identity.  A parent the item master says
*>                       is bought in, not manufactured, is an
*>                       incompatible classification and blocks
*>                       the release and the calculation approval
*>                       (BOMVALID finding BOM0317).
*>              BOM-022  A job bill is DERIVEd from a released
*>                       standard as a DRAFT with selected source
*>                       content, no inherited reviewer, release
*>                       or validation evidence, a written
*>                       exclusion note and complete lineage; a
*>                       repeated request returns the first copy
*>                       rather than breeding duplicates.
*>              BOM-025  A comparison separates added, removed and
*>                       changed occurrences from occurrences that
*>                       only moved in the sequence - a renumbered
*>                       bill is NOT a material change.
*>
*>              Every fixture is prefixed ZZBOMGAP / ZZ-BOM-GAP-
*>              and is removed before and after the run, so the
*>              suite is repeatable and leaves the seeded demo
*>              bills exactly as it found them.  With no database
*>              the suite skips rather than fails.
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2026-10-07 STERP ORIGINAL - COVERAGE FOR BOM-001, BOM-022,
*>                  BOM-025
*>================================================================
*> REQ: BOM-001, BOM-022, BOM-025
*> AC: AC-085, AC-094
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TBOMGAP.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY TESTKITW.
 COPY BOMVALIDCA REPLACING ==DFHCOMMAREA== BY ==Ws-Val-Ca==
      LEADING ==CA-== BY ==VA-==.
 COPY BOMJOBCA REPLACING ==DFHCOMMAREA== BY ==Ws-Job-Ca==
      LEADING ==CA-== BY ==JA-==.
 COPY BOMCOMPCA REPLACING ==DFHCOMMAREA== BY ==Ws-Cmp-Ca==
      LEADING ==CA-== BY ==PA-==.
 01  Ws-Flags.
     05  Ws-Db-Up                PIC X VALUE "Y".
     05  Ws-I                    PIC S9(4) VALUE 0.
     05  Ws-Hits                 PIC S9(4) VALUE 0.
     05  Ws-Want-Ref             PIC X(30) VALUE SPACES.
     05  Ws-Want-Rev             PIC X(4) VALUE SPACES.
     05  Ws-Found-Id             PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Src-Id               PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Bad-Id               PIC S9(9) COMP-5 VALUE 0.
     05  Ws-First-Tgt            PIC S9(9) COMP-5 VALUE 0.
 01  Ws-Id-Row.
     05  Gi-Id                   PIC X(12).
 01  Ws-Hdr-Row.
     05  Gh-Status               PIC X(12).
     05  Gh-Reviewer             PIC X(20).
     05  Gh-Released             PIC X(20).
     05  Gh-Validation           PIC X(12).
     05  Gh-Lines                PIC X(12).
     05  Gh-Headers              PIC X(12).
 PROCEDURE DIVISION.
 Main-Entry.
     MOVE "TBOMGAP header, copy and compare" TO TK-SUITE
     PERFORM TK-BEGIN
     PERFORM Check-Copy-Request-Rules
     PERFORM Probe-The-Database
     IF Ws-Db-Up = "N"
         PERFORM Skip-The-Database-Work
     ELSE
         PERFORM Remove-The-Fixtures
         PERFORM Build-The-Fixtures
         IF Ws-Db-Up = "Y"
             PERFORM Check-Parent-Classification
             PERFORM Check-Job-Bill-Derivation
             PERFORM Check-Revision-Comparison
         END-IF
         PERFORM Remove-The-Fixtures
     END-IF
     PERFORM TK-END
     STOP RUN.

*>----------------------------------------------------------------
*> BOM-022 - the request itself is governed before anything is
*> copied.  None of these paths reaches the database.
*>----------------------------------------------------------------
 Check-Copy-Request-Rules.
     PERFORM Set-Up-Derive-Request
     MOVE 0 TO BJ-SOURCE-BOM-ID
     CALL "BOMJOB" USING Ws-Job-Ca
     MOVE "A copy with no named source bill is refused"
         TO TK-NAME
     MOVE "BOM1202" TO TK-EXPECTED-X
     MOVE JA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The refused copy is an error, not a warning" TO TK-NAME
     MOVE 8 TO TK-EXPECTED-N
     MOVE JA-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM

     PERFORM Set-Up-Derive-Request
     MOVE 4321 TO BJ-SOURCE-BOM-ID
     MOVE SPACES TO BJ-TARGET-REF
     CALL "BOMJOB" USING Ws-Job-Ca
     MOVE "A copy with no reference for the new bill is refused"
         TO TK-NAME
     MOVE "BOM1203" TO TK-EXPECTED-X
     MOVE JA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR

     PERFORM Set-Up-Derive-Request
     MOVE 4321 TO BJ-SOURCE-BOM-ID
     MOVE SPACES TO BJ-JOB-REF
     CALL "BOMJOB" USING Ws-Job-Ca
     MOVE "A job bill must name the job or order it is for"
         TO TK-NAME
     MOVE "BOM1204" TO TK-EXPECTED-X
     MOVE JA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "Nothing was copied by a refused request" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE BJ-LINES-COPIED TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

 Set-Up-Derive-Request.
     INITIALIZE Ws-Job-Ca
     MOVE "DERIVE" TO JA-FUNCTION
     MOVE "STC" TO JA-COMPANY
     MOVE "RDG" TO JA-SITE
     MOVE "ENGINEER" TO JA-USER
     MOVE 20261001 TO JA-BUSINESS-DATE
     MOVE "ZZBOMGAP-JOB" TO BJ-TARGET-REF
     MOVE "A" TO BJ-TARGET-REV
     MOVE "JOB" TO BJ-PURPOSE
     MOVE "ZZJOB-0001" TO BJ-JOB-REF
     MOVE "WORKORDER" TO BJ-DEMAND-TYPE
     MOVE "ZZJOB-0001" TO BJ-DEMAND-REF
     MOVE 20261001 TO BJ-EFF-DATE
     MOVE "N" TO BJ-COPY-OPTIONAL
     MOVE "N" TO BJ-COPY-NOTES
     MOVE "N" TO BJ-COPY-BYPRODUCT.

*>----------------------------------------------------------------
 Probe-The-Database.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT TOP (1) CompanyCode FROM core.Company "
            "WHERE CompanyCode = 'STC'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 12 TO SQLIO-COL-WIDTH(1)
     MOVE 7 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     IF SQLIO-ERROR OR SQLIO-NOTFOUND
         MOVE "N" TO Ws-Db-Up
     END-IF.

 Skip-The-Database-Work.
     MOVE "Parent classification blocks release" TO TK-NAME
     MOVE "the STERP database is not available" TO TK-DETAIL
     PERFORM TK-SKIP
     MOVE "Job bill derivation and lineage" TO TK-NAME
     MOVE "the STERP database is not available" TO TK-DETAIL
     PERFORM TK-SKIP
     MOVE "Revision comparison" TO TK-NAME
     MOVE "the STERP database is not available" TO TK-DETAIL
     PERFORM TK-SKIP.

*>----------------------------------------------------------------
*> Fixtures.  Everything written here carries a ZZ prefix.
*>----------------------------------------------------------------
 Remove-The-Fixtures.
     MOVE SPACES TO SQLIO-STMT
     STRING "SET NOCOUNT ON; "
            "UPDATE bom.BomHeader SET Status = 'DRAFT' WHERE "
            "CompanyCode = 'STC' AND BomRef LIKE 'ZZBOMGAP%' AND "
            "Status <> 'DRAFT'; "
            "UPDATE bom.BomHeader SET ValidationId = NULL, "
            "SupersededByBomHeaderId = NULL, "
            "SourceBomHeaderId = NULL WHERE CompanyCode = 'STC' "
            "AND BomRef LIKE 'ZZBOMGAP%'; "
            "DELETE FROM bom.BomLineage WHERE CompanyCode = 'STC' "
            "AND (CorrelationId LIKE 'ZZBOMGAP%' OR "
            "TargetBomHeaderId IN (SELECT BomHeaderId FROM "
            "bom.BomHeader WHERE CompanyCode = 'STC' AND BomRef "
            "LIKE 'ZZBOMGAP%')); "
            "DELETE f FROM bom.BomValidationFinding f JOIN "
            "bom.BomValidation v ON v.ValidationId = f.ValidationId "
            "JOIN bom.BomHeader h ON h.BomHeaderId = v.BomHeaderId "
            "WHERE h.CompanyCode = 'STC' AND h.BomRef LIKE "
            "'ZZBOMGAP%'; "
            "DELETE v FROM bom.BomValidation v JOIN bom.BomHeader h "
            "ON h.BomHeaderId = v.BomHeaderId WHERE "
            "h.CompanyCode = 'STC' AND h.BomRef LIKE 'ZZBOMGAP%'; "
            "DELETE c FROM bom.BomComponent c JOIN bom.BomHeader h "
            "ON h.BomHeaderId = c.BomHeaderId WHERE "
            "h.CompanyCode = 'STC' AND h.BomRef LIKE 'ZZBOMGAP%'; "
            "DELETE FROM bom.BomHeader WHERE CompanyCode = 'STC' "
            "AND BomRef LIKE 'ZZBOMGAP%'; "
            "DELETE FROM core.AuditLog WHERE EntityKey LIKE "
            "'ZZBOMGAP%'; "
            "DELETE FROM core.IdempotencyKey WHERE CorrelationId "
            "LIKE 'ZZBOMGAP%'; "
            "DELETE FROM inv.Item WHERE CompanyCode = 'STC' AND "
            "ItemCode LIKE 'ZZ-BOM-GAP-%'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-The-Statement.

 Build-The-Fixtures.
     MOVE SPACES TO SQLIO-STMT
     STRING "SET NOCOUNT ON; INSERT inv.Item (CompanyCode, "
            "ItemCode, Description, ItemClass, StockUomCode, "
            "QuantumQty, IsIndivisible, TraceabilityType, MakeBuy, "
            "ValuationGroup, CostMethod, LifecycleStatus, "
            "IsManufacturable, CreatedBy) SELECT 'STC', v.C, v.D, "
            "v.K, 'EA', 1, 1, 'NONE', v.M, v.K, 'MWA', 'RELEASED', "
            "v.F, 'TEST' FROM (VALUES "
            "('ZZ-BOM-GAP-TOP','Gap test made parent',"
            "'MADE','MAKE',1), "
            "('ZZ-BOM-GAP-BUY','Gap test bought parent',"
            "'BOUGHT','BUY',0), "
            "('ZZ-BOM-GAP-C1','Gap test component one',"
            "'BOUGHT','BUY',0), "
            "('ZZ-BOM-GAP-C2','Gap test component two',"
            "'BOUGHT','BUY',0), "
            "('ZZ-BOM-GAP-C3','Gap test component three',"
            "'BOUGHT','BUY',0), "
            "('ZZ-BOM-GAP-C4','Gap test component four',"
            "'BOUGHT','BUY',0)) v(C,D,K,M,F)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-The-Statement
     IF Ws-Db-Up = "N"
         EXIT PARAGRAPH
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "SET NOCOUNT ON; INSERT bom.BomHeader (CompanyCode, "
            "SiteCode, BomRef, BomRevision, ParentItemCode, "
            "ParentRevision, Purpose, Description, NominalBasis, "
            "YieldFactor, ProductionQuantum, EffectiveFrom, "
            "Status, AuthorUser, CreatedBy) SELECT 'STC', 'RDG', "
            "v.R, v.V, v.P, 'A', 'STANDARD', v.D, 1, 1, 1, "
            "'2026-01-01', 'DRAFT', 'ENGINEER', 'TEST' FROM "
            "(VALUES ('ZZBOMGAP-SRC','A','ZZ-BOM-GAP-TOP',"
            "'Gap test released source bill'), "
            "('ZZBOMGAP-BAD','A','ZZ-BOM-GAP-BUY',"
            "'Gap test bill on a bought-in parent'), "
            "('ZZBOMGAP-CMP','A','ZZ-BOM-GAP-TOP',"
            "'Gap test comparison revision A'), "
            "('ZZBOMGAP-CMP','B','ZZ-BOM-GAP-TOP',"
            "'Gap test comparison revision B'), "
            "('ZZBOMGAP-CMP','C','ZZ-BOM-GAP-TOP',"
            "'Gap test comparison revision C')) v(R,V,P,D)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-The-Statement
     IF Ws-Db-Up = "N"
         EXIT PARAGRAPH
     END-IF
     PERFORM Build-The-Component-Lines
     IF Ws-Db-Up = "N"
         EXIT PARAGRAPH
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "SET NOCOUNT ON; UPDATE bom.BomHeader SET Status = "
            "'RELEASED', ReviewerUser = 'ENGMGR', ReleasedBy = "
            "'ENGMGR', ReleasedAt = SYSDATETIME() WHERE "
            "CompanyCode = 'STC' AND BomRef = 'ZZBOMGAP-SRC'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-The-Statement.

*>   Revision A and C hold the same three occurrences renumbered;
*>   revision B changes one quantity, removes one line, adds one
*>   and renumbers a fourth (BOM-025).
 Build-The-Component-Lines.
     MOVE SPACES TO SQLIO-STMT
     STRING "SET NOCOUNT ON; INSERT bom.BomComponent (BomHeaderId, "
            "LineNum, PathRef, ComponentItemCode, "
            "ComponentRevision, UsageQty, UsageBasis, UomCode, "
            "ConversionFactor, ScrapPct, SupplyBehaviour, "
            "IsOptional, IsSelected, CreatedBy) SELECT "
            "h.BomHeaderId, v.L, h.BomRef + '/' + "
            "CONVERT(varchar(10), v.L), v.I, 'A', v.Q, "
            "'PROPORTION', 'EA', 1, 0, v.S, v.O, 1, 'TEST' FROM "
            "bom.BomHeader h JOIN (VALUES "
            "('ZZBOMGAP-SRC','A',10,'ZZ-BOM-GAP-C1',2,"
            "'PURCHASED',0), "
            "('ZZBOMGAP-SRC','A',20,'ZZ-BOM-GAP-C2',1,"
            "'PURCHASED',1), "
            "('ZZBOMGAP-SRC','A',30,'ZZ-BOM-GAP-C3',0,'NOTE',0), "
            "('ZZBOMGAP-SRC','A',40,'ZZ-BOM-GAP-C4',3,"
            "'PURCHASED',0), "
            "('ZZBOMGAP-BAD','A',10,'ZZ-BOM-GAP-C1',1,"
            "'PURCHASED',0), "
            "('ZZBOMGAP-CMP','A',10,'ZZ-BOM-GAP-C1',2,"
            "'PURCHASED',0), "
            "('ZZBOMGAP-CMP','A',20,'ZZ-BOM-GAP-C2',5,"
            "'PURCHASED',0), "
            "('ZZBOMGAP-CMP','A',30,'ZZ-BOM-GAP-C3',1,"
            "'PURCHASED',0), "
            "('ZZBOMGAP-CMP','B',20,'ZZ-BOM-GAP-C2',7,"
            "'PURCHASED',0), "
            "('ZZBOMGAP-CMP','B',40,'ZZ-BOM-GAP-C1',2,"
            "'PURCHASED',0), "
            "('ZZBOMGAP-CMP','B',50,'ZZ-BOM-GAP-C4',3,"
            "'PURCHASED',0), "
            "('ZZBOMGAP-CMP','C',70,'ZZ-BOM-GAP-C1',2,"
            "'PURCHASED',0), "
            "('ZZBOMGAP-CMP','C',80,'ZZ-BOM-GAP-C2',5,"
            "'PURCHASED',0), "
            "('ZZBOMGAP-CMP','C',90,'ZZ-BOM-GAP-C3',1,"
            "'PURCHASED',0)) v(R,V,L,I,Q,S,O) ON v.R = h.BomRef "
            "AND v.V = h.BomRevision WHERE h.CompanyCode = 'STC'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-The-Statement.

 Run-The-Statement.
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 7 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     IF SQLIO-ERROR
         MOVE "N" TO Ws-Db-Up
     END-IF
     MOVE 7 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row.

 Find-The-Bom-Id.
     MOVE 0 TO Ws-Found-Id
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT h.BomHeaderId FROM bom.BomHeader h WHERE "
            "h.CompanyCode = 'STC' AND h.BomRef = ? AND "
            "h.BomRevision = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE Ws-Want-Ref TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE Ws-Want-Rev TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 12 TO SQLIO-COL-WIDTH(1)
     MOVE 7 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     IF SQLIO-OK
         COMPUTE Ws-Found-Id = FUNCTION NUMVAL(Gi-Id)
     END-IF.

*>----------------------------------------------------------------
*> BOM-001.  The parent identity is governed: a bill may not be
*> released or approved for calculation when the item master says
*> the parent is not something this company manufactures.
*> AC: AC-085
*>----------------------------------------------------------------
 Check-Parent-Classification.
     MOVE "ZZBOMGAP-SRC" TO Ws-Want-Ref
     MOVE "A" TO Ws-Want-Rev
     PERFORM Find-The-Bom-Id
     MOVE Ws-Found-Id TO Ws-Src-Id
     MOVE "ZZBOMGAP-BAD" TO Ws-Want-Ref
     MOVE "A" TO Ws-Want-Rev
     PERFORM Find-The-Bom-Id
     MOVE Ws-Found-Id TO Ws-Bad-Id

     PERFORM Validate-The-Good-Parent
     PERFORM Validate-The-Bought-Parent.

 Validate-The-Good-Parent.
     INITIALIZE Ws-Val-Ca
     MOVE "CHECK" TO VA-FUNCTION
     MOVE "STC" TO VA-COMPANY
     MOVE "RDG" TO VA-SITE
     MOVE "ENGINEER" TO VA-USER
     MOVE 20261001 TO VA-BUSINESS-DATE
     MOVE Ws-Src-Id TO BV-BOM-ID
     MOVE 20261001 TO BV-EFF-DATE
     CALL "BOMVALID" USING Ws-Val-Ca
     PERFORM Count-Parent-Findings
     MOVE "A manufactured parent raises no classification finding"
         TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE Ws-Hits TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A governed header may be released" TO TK-NAME
     MOVE "Y" TO TK-EXPECTED-X
     MOVE BV-RELEASE-OK TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The validation names the parent it governed" TO TK-NAME
     MOVE "ZZ-BOM-GAP-TOP" TO TK-EXPECTED-X
     MOVE BV-PARENT-ITEM TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

 Validate-The-Bought-Parent.
     INITIALIZE Ws-Val-Ca
     MOVE "CHECK" TO VA-FUNCTION
     MOVE "STC" TO VA-COMPANY
     MOVE "RDG" TO VA-SITE
     MOVE "ENGINEER" TO VA-USER
     MOVE 20261001 TO VA-BUSINESS-DATE
     MOVE Ws-Bad-Id TO BV-BOM-ID
     MOVE 20261001 TO BV-EFF-DATE
     CALL "BOMVALID" USING Ws-Val-Ca
     PERFORM Count-Parent-Findings
     MOVE "A bought-in parent is an incompatible classification"
         TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE Ws-Hits TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The classification finding is an error, not a note"
         TO TK-NAME
     MOVE "ERROR" TO TK-EXPECTED-X
     MOVE SPACES TO TK-ACTUAL-X
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > BV-FIND-CNT
         IF BV-F-REASON(Ws-I) = "BOM0317"
             MOVE BV-F-SEVERITY(Ws-I) TO TK-ACTUAL-X
         END-IF
     END-PERFORM
     PERFORM TK-ASSERT-STR
     MOVE "An incompatible parent blocks the release" TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE BV-RELEASE-OK TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The validation outcome is a failure" TO TK-NAME
     MOVE "FAIL" TO TK-EXPECTED-X
     MOVE BV-OUTCOME TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The calculation approval is refused with reason BOM0340"
         TO TK-NAME
     MOVE "BOM0340" TO TK-EXPECTED-X
     MOVE VA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

 Count-Parent-Findings.
     MOVE 0 TO Ws-Hits
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > BV-FIND-CNT
         IF BV-F-REASON(Ws-I) = "BOM0317"
             ADD 1 TO Ws-Hits
         END-IF
     END-PERFORM.

*>----------------------------------------------------------------
*> BOM-022.  A derived job bill is a draft with selected source
*> content, its own approval state and complete lineage.
*> AC: AC-094
*>----------------------------------------------------------------
 Check-Job-Bill-Derivation.
     PERFORM Set-Up-Derive-Request
     MOVE "ZZBOMGAP-DERIVE-1" TO JA-CORRELATION-ID
     MOVE Ws-Src-Id TO BJ-SOURCE-BOM-ID
     CALL "BOMJOB" USING Ws-Job-Ca
     MOVE BJ-TARGET-BOM-ID TO Ws-First-Tgt
     MOVE "The copy of a released bill is accepted" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE JA-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Copying a released bill does not copy its release"
         TO TK-NAME
     MOVE "DRAFT" TO TK-EXPECTED-X
     MOVE BJ-STATUS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "Only the selected source content is copied" TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     MOVE BJ-LINES-COPIED TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The optional and note lines are counted as excluded"
         TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     MOVE BJ-LINES-EXCLUDED TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The exclusion is written down, not assumed" TO TK-NAME
     MOVE SPACES TO TK-EXPECTED-X
     STRING "Excluded 2 line(s); execution history, stock "
            "allocations and prior approvals are never copied"
         DELIMITED BY SIZE INTO TK-EXPECTED-X
     MOVE BJ-EXCLUSION-NOTE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Read-The-Target-Header
     PERFORM Check-Lineage-Is-Complete
     PERFORM Check-Repeat-Is-Not-A-Duplicate.

*>   The copy carries no reviewer, no release evidence and no
*>   validation of its own - the approval starts again.
 Read-The-Target-Header.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT h.Status, ISNULL(h.ReviewerUser,'(none)'), "
            "ISNULL(h.ReleasedBy,'(none)'), "
            "ISNULL(CONVERT(varchar(12),h.ValidationId),'(none)'), "
            "CONVERT(varchar(12),(SELECT COUNT(*) FROM "
            "bom.BomComponent c WHERE c.BomHeaderId = "
            "h.BomHeaderId)), CONVERT(varchar(12),(SELECT COUNT(*) "
            "FROM bom.BomHeader g WHERE g.CompanyCode = 'STC' AND "
            "g.BomRef = 'ZZBOMGAP-JOB')) FROM bom.BomHeader h "
            "WHERE h.BomHeaderId = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 1 TO SQLIO-PARM-CNT
     MOVE BJ-TARGET-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE 6 TO SQLIO-COL-CNT
     MOVE 12 TO SQLIO-COL-WIDTH(1)
     MOVE 20 TO SQLIO-COL-WIDTH(2)
     MOVE 20 TO SQLIO-COL-WIDTH(3)
     MOVE 12 TO SQLIO-COL-WIDTH(4)
     MOVE 12 TO SQLIO-COL-WIDTH(5)
     MOVE 12 TO SQLIO-COL-WIDTH(6)
     MOVE 7 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Hdr-Row
     MOVE "The stored copy is held as a draft" TO TK-NAME
     MOVE "DRAFT" TO TK-EXPECTED-X
     MOVE Gh-Status TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The copy inherits no engineering reviewer" TO TK-NAME
     MOVE "(none)" TO TK-EXPECTED-X
     MOVE Gh-Reviewer TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The copy inherits no release signature" TO TK-NAME
     MOVE "(none)" TO TK-EXPECTED-X
     MOVE Gh-Released TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The copy inherits no prior validation evidence"
         TO TK-NAME
     MOVE "(none)" TO TK-EXPECTED-X
     MOVE Gh-Validation TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The excluded lines are absent from the stored copy"
         TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(Gh-Lines)
     PERFORM TK-ASSERT-NUM.

 Check-Lineage-Is-Complete.
     MOVE "LINEAGE" TO JA-FUNCTION
     CALL "BOMJOB" USING Ws-Job-Ca
     MOVE "The copy records exactly one derivation" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE BJ-LINEAGE-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The lineage names the bill it was copied from"
         TO TK-NAME
     MOVE "ZZBOMGAP-SRC" TO TK-EXPECTED-X
     MOVE BJ-G-SOURCE-REF(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The lineage names the revision it was copied from"
         TO TK-NAME
     MOVE "A" TO TK-EXPECTED-X
     MOVE BJ-G-SOURCE-REV(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The lineage records the derivation as a copy"
         TO TK-NAME
     MOVE "COPY" TO TK-EXPECTED-X
     MOVE BJ-G-TYPE(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The lineage names the demand the copy was made for"
         TO TK-NAME
     MOVE "ZZJOB-0001" TO TK-EXPECTED-X
     MOVE BJ-G-DEMAND(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

 Check-Repeat-Is-Not-A-Duplicate.
     PERFORM Set-Up-Derive-Request
     MOVE "ZZBOMGAP-DERIVE-2" TO JA-CORRELATION-ID
     MOVE Ws-Src-Id TO BJ-SOURCE-BOM-ID
     CALL "BOMJOB" USING Ws-Job-Ca
     MOVE "A repeated copy is reported as the original"
         TO TK-NAME
     MOVE "Y" TO TK-EXPECTED-X
     MOVE BJ-DUPLICATE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A repeated copy is a warning, not a new bill"
         TO TK-NAME
     MOVE 4 TO TK-EXPECTED-N
     MOVE JA-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The repeated request returns the first bill" TO TK-NAME
     MOVE Ws-First-Tgt TO TK-EXPECTED-N
     MOVE BJ-TARGET-BOM-ID TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Count-The-Job-Bills
     MOVE "Only one job bill exists after the repeat" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(Gi-Id)
     PERFORM TK-ASSERT-NUM.

 Count-The-Job-Bills.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(12), COUNT(*)) FROM "
            "bom.BomHeader WHERE CompanyCode = 'STC' AND BomRef = "
            "'ZZBOMGAP-JOB'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 12 TO SQLIO-COL-WIDTH(1)
     MOVE 7 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row.

*>----------------------------------------------------------------
*> BOM-025.  Added, removed and changed occurrences are reported
*> separately from occurrences that only moved in the sequence.
*> AC: AC-094
*>----------------------------------------------------------------
 Check-Revision-Comparison.
     PERFORM Set-Up-Comparison
     MOVE "B" TO BP-TO-REV
     CALL "BOMCOMP" USING Ws-Cmp-Ca
     MOVE "A to B reports one added occurrence" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE BP-ADDED-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A to B reports one removed occurrence" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE BP-REMOVED-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A to B reports one changed quantity" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE BP-CHANGED-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A to B keeps the resequenced line out of the changes"
         TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE BP-SEQONLY-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A to B is a material change" TO TK-NAME
     MOVE "Y" TO TK-EXPECTED-X
     MOVE BP-MATERIAL-CHANGE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Find-The-Quantity-Difference
     PERFORM Compare-A-Pure-Resequence.

*>   The changed line must carry the quantity on both sides so a
*>   reviewer sees the size of the change, not just that there was
*>   one: five becomes seven, a difference of two.
 Find-The-Quantity-Difference.
     MOVE 0 TO TK-ACTUAL-N
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > BP-WIN-CNT
         IF BP-L-ITEM(Ws-I) = "ZZ-BOM-GAP-C2"
             MOVE BP-L-QTY-DELTA(Ws-I) TO TK-ACTUAL-N
         END-IF
     END-PERFORM
     MOVE "The changed occurrence reports a difference of two"
         TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     MOVE SPACES TO TK-ACTUAL-X
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > BP-WIN-CNT
         IF BP-L-ITEM(Ws-I) = "ZZ-BOM-GAP-C4"
             MOVE BP-L-ACTION(Ws-I) TO TK-ACTUAL-X
         END-IF
     END-PERFORM
     MOVE "The new occurrence is reported as added" TO TK-NAME
     MOVE "ADDED" TO TK-EXPECTED-X
     PERFORM TK-ASSERT-STR
     MOVE SPACES TO TK-ACTUAL-X
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > BP-WIN-CNT
         IF BP-L-ITEM(Ws-I) = "ZZ-BOM-GAP-C3"
             MOVE BP-L-ACTION(Ws-I) TO TK-ACTUAL-X
         END-IF
     END-PERFORM
     MOVE "The dropped occurrence is reported as removed"
         TO TK-NAME
     MOVE "REMOVED" TO TK-EXPECTED-X
     PERFORM TK-ASSERT-STR.

*>   Revision C is revision A renumbered and nothing else.  An
*>   engineer who renumbers a bill must not be sent to a change
*>   review for it.
 Compare-A-Pure-Resequence.
     PERFORM Set-Up-Comparison
     MOVE "C" TO BP-TO-REV
     CALL "BOMCOMP" USING Ws-Cmp-Ca
     MOVE "A renumbered bill adds nothing" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE BP-ADDED-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A renumbered bill removes nothing" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE BP-REMOVED-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A renumbered bill changes nothing" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE BP-CHANGED-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "All three occurrences are reported as resequenced"
         TO TK-NAME
     MOVE 3 TO TK-EXPECTED-N
     MOVE BP-SEQONLY-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A pure sequence change is not a material change"
         TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE BP-MATERIAL-CHANGE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The header quantities did not move either" TO TK-NAME
     MOVE SPACES TO TK-EXPECTED-X
     MOVE BP-HEADER-DIFF TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE 0 TO Ws-Hits
     MOVE SPACES TO TK-ACTUAL-X
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > BP-WIN-CNT
         IF BP-L-ACTION(Ws-I) = "SEQUENCE"
             ADD 1 TO Ws-Hits
             MOVE BP-L-DETAIL(Ws-I) TO TK-ACTUAL-X
         END-IF
     END-PERFORM
     MOVE "Every reported line is marked as a sequence change"
         TO TK-NAME
     MOVE 3 TO TK-EXPECTED-N
     MOVE Ws-Hits TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The reason is spelled out on the line" TO TK-NAME
     MOVE "Sequence only - no material change" TO TK-EXPECTED-X
     PERFORM TK-ASSERT-STR.

 Set-Up-Comparison.
     INITIALIZE Ws-Cmp-Ca
     MOVE "BYREF" TO PA-FUNCTION
     MOVE "STC" TO PA-COMPANY
     MOVE "RDG" TO PA-SITE
     MOVE "ENGINEER" TO PA-USER
     MOVE 20261001 TO PA-BUSINESS-DATE
     MOVE "ZZBOMGAP-CMP" TO BP-BOM-REF
     MOVE "A" TO BP-FROM-REV
     MOVE 20261001 TO BP-EFF-DATE
     MOVE 25 TO BP-WIN-MAX
     MOVE 1 TO BP-FROM-LINE.

 COPY TESTKITP.
