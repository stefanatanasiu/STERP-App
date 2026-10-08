>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : TKITGAP1
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION (TESTS)
*> PURPOSE    : KITCREAT and KITCOMPL - a kit is only created from
*>              an approved, complete bill or an approved job
*>              requirement; release fixes the calculation so a
*>              later standard bill change cannot reach back into
*>              it; and parent completion records actual good
*>              output, keeps it inside the plan and transfers the
*>              right work in progress value once.
*>
*>              The seeded kit KIT0001001 is used read only: ten
*>              planned, four completed, 1,842.50 of work in
*>              progress, so six units remain.  Nothing here
*>              commits - every assertion is a dry run (COMPCHK)
*>              or an enquiry, and any write is wrapped in
*>              BEGN ... ROLB.
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2026-10-07 STERP ORIGINAL - CLOSING THE KIT TEST GAPS
*>================================================================
*> REQ: KIT-002, KIT-004, KIT-015
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TKITGAP1.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY TESTKITW.
 COPY KITCREATCA REPLACING ==DFHCOMMAREA== BY ==Ws-Creat-Ca==
      LEADING ==CA-== BY ==CC-==.
 COPY KITCOMPLCA REPLACING ==DFHCOMMAREA== BY ==Ws-Compl-Ca==
      LEADING ==CA-== BY ==CP-==.
 01  Ws-Flags.
     05  Ws-Db-Up                PIC X VALUE "N".
     05  Ws-Have-Kit             PIC X VALUE "N".
 01  Ws-Row.
     05  Ws-R1                   PIC X(30).
     05  Ws-R2                   PIC X(30).
     05  Ws-R3                   PIC X(30).
     05  Ws-R4                   PIC X(30).
     05  Ws-R5                   PIC X(30).
 01  Ws-Kit.
     05  Ws-Kit-Ref              PIC X(30) VALUE SPACES.
     05  Ws-Planned              PIC S9(13)V9(6) VALUE 0.
     05  Ws-Done                 PIC S9(13)V9(6) VALUE 0.
     05  Ws-Rejected             PIC S9(13)V9(6) VALUE 0.
     05  Ws-Wip                  PIC S9(11)V99 VALUE 0.
     05  Ws-Remaining            PIC S9(13)V9(6) VALUE 0.
     05  Ws-Half                 PIC S9(13)V9(6) VALUE 0.
     05  Ws-Expect               PIC S9(11)V99 VALUE 0.
     05  Ws-Rel-Kit              PIC X(30) VALUE SPACES.
     05  Ws-Bought               PIC X(40) VALUE SPACES.
     05  Ws-Gross-1              PIC S9(13)V9(6) VALUE 0.
     05  Ws-Yield-1              PIC S9(5)V9(6) VALUE 0.
     05  Ws-Req-1                PIC S9(13)V9(6) VALUE 0.
 PROCEDURE DIVISION.
 Main-Entry.
     MOVE "TKITGAP1 kit creation, release and completion"
         TO TK-SUITE
     PERFORM TK-BEGIN
     PERFORM Probe-The-Database
     IF Ws-Db-Up NOT = "Y"
         PERFORM Skip-Everything
     ELSE
         PERFORM Test-Creation-Edits
         PERFORM Test-Incomplete-Bill-Is-Not-Released
         PERFORM Test-Release-Fixes-The-Calculation
         PERFORM Test-Completion-Edits
         PERFORM Test-Output-Stays-Within-Plan
         PERFORM Test-Wip-Transfer
     END-IF
     PERFORM TK-END
     STOP RUN.

 Probe-The-Database.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT TOP (1) KitRef, CAST(PlannedGoodQty AS "
            "varchar(30)), CAST(CompletedGoodQty AS varchar(30)), "
            "CAST(RejectedQty AS varchar(30)), CAST(WipValueGBP "
            "AS varchar(30)) FROM kit.ExecutionKit WHERE "
            "CompanyCode = 'STC' AND Status = 'PARTCOMPLETE' AND "
            "WipValueGBP > 0 ORDER BY ExecutionKitId"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 5 TO SQLIO-COL-CNT
     MOVE 30 TO SQLIO-COL-WIDTH(1)
     MOVE 30 TO SQLIO-COL-WIDTH(2)
     MOVE 30 TO SQLIO-COL-WIDTH(3)
     MOVE 30 TO SQLIO-COL-WIDTH(4)
     MOVE 30 TO SQLIO-COL-WIDTH(5)
     MOVE 1 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-SQLCODE = 0 AND Ws-R1 NOT = SPACES
         MOVE "Y" TO Ws-Db-Up
         MOVE "Y" TO Ws-Have-Kit
         MOVE Ws-R1 TO Ws-Kit-Ref
         COMPUTE Ws-Planned = FUNCTION NUMVAL(Ws-R2)
         COMPUTE Ws-Done = FUNCTION NUMVAL(Ws-R3)
         COMPUTE Ws-Rejected = FUNCTION NUMVAL(Ws-R4)
         COMPUTE Ws-Wip = FUNCTION NUMVAL(Ws-R5)
         COMPUTE Ws-Remaining = Ws-Planned - Ws-Done - Ws-Rejected
     END-IF
     IF Ws-Db-Up = "Y"
         MOVE SPACES TO SQLIO-STMT
         STRING "SELECT TOP (1) KitRef, CAST(GrossStarts AS "
                "varchar(30)), CAST(YieldFactor AS varchar(30)), "
                "'', '' FROM kit.ExecutionKit WHERE CompanyCode = "
                "'STC' AND Status = 'RELEASED' ORDER BY "
                "ExecutionKitId"
             DELIMITED BY SIZE INTO SQLIO-STMT
         MOVE 0 TO SQLIO-PARM-CNT
         MOVE 5 TO SQLIO-COL-CNT
         MOVE 1 TO SQLIO-CURSOR
         MOVE "SNGL" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Row
         MOVE Ws-R1 TO Ws-Rel-Kit
         MOVE SPACES TO SQLIO-STMT
         STRING "SELECT TOP (1) ItemCode, '', '', '', '' FROM "
                "inv.Item i WHERE i.CompanyCode = 'STC' AND "
                "i.ItemClass = 'BOUGHT' AND NOT EXISTS (SELECT 1 "
                "FROM bom.BomHeader b WHERE b.CompanyCode = "
                "i.CompanyCode AND b.ParentItemCode = i.ItemCode) "
                "ORDER BY ItemCode"
             DELIMITED BY SIZE INTO SQLIO-STMT
         MOVE 0 TO SQLIO-PARM-CNT
         MOVE 5 TO SQLIO-COL-CNT
         MOVE 1 TO SQLIO-CURSOR
         MOVE "SNGL" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Row
         MOVE Ws-R1 TO Ws-Bought
     END-IF.

 Skip-Everything.
     MOVE "no database or no seeded execution kit" TO TK-DETAIL
     MOVE "KIT-002 kits come from approved bills" TO TK-NAME
     PERFORM TK-SKIP
     MOVE "no database or no seeded execution kit" TO TK-DETAIL
     MOVE "KIT-004 release fixes the calculation" TO TK-NAME
     PERFORM TK-SKIP
     MOVE "no database or no seeded execution kit" TO TK-DETAIL
     MOVE "KIT-015 parent completion and work in progress"
         TO TK-NAME
     PERFORM TK-SKIP.

 Begin-Transaction.
     MOVE "BEGN" TO SQLIO-FUNC
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 8 TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA Ws-Row.

 Rollback-Transaction.
     MOVE "ROLB" TO SQLIO-FUNC
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 8 TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA Ws-Row.

 Prime-Creation.
     INITIALIZE Ws-Creat-Ca
     MOVE "STC" TO CC-COMPANY
     MOVE "RDG" TO CC-SITE
     MOVE "KITPLAN" TO CC-USER
     MOVE 20261007 TO CC-BUSINESS-DATE
     MOVE "ZZKITGAP1-001" TO CC-CORRELATION-ID
     MOVE "EXECUTION" TO KC-KIT-KIND
     MOVE "RDG" TO KC-SITE
     MOVE "STOCK" TO KC-SOURCE-TYPE
     MOVE 10 TO KC-PLANNED-QTY
     MOVE 20261120 TO KC-NEED-DATE.

 Prime-Completion.
     INITIALIZE Ws-Compl-Ca
     MOVE "STC" TO CP-COMPANY
     MOVE "RDG" TO CP-SITE
     MOVE "KITOP" TO CP-USER
     MOVE 20261007 TO CP-BUSINESS-DATE
     MOVE "ZZKITGAP1-002" TO CP-CORRELATION-ID
     MOVE Ws-Kit-Ref TO KM-KIT-REF
     MOVE "RDG" TO KM-SITE
     MOVE 20261007 TO KM-COMPLETION-DATE
     MOVE 20261007 TO KM-POSTING-DATE
     MOVE "Y" TO KM-CHECK-ONLY.

*>   KIT-002.  A kit is a material requirement for a real parent,
*>   in a real quantity, traceable back to the demand that asked
*>   for it.  None of those may be left out.
 Test-Creation-Edits.
     PERFORM Prime-Creation
     MOVE "WIBBLE" TO CC-FUNCTION
     CALL "KITCREAT" USING Ws-Creat-Ca
     MOVE "An unknown kit instruction is refused with KIT0002"
         TO TK-NAME
     MOVE "KIT0002" TO TK-EXPECTED-X
     MOVE CC-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Prime-Creation
     MOVE "CREATE" TO CC-FUNCTION
     MOVE SPACES TO KC-PARENT-ITEM
     CALL "KITCREAT" USING Ws-Creat-Ca
     MOVE "A kit without a parent item is refused" TO TK-NAME
     MOVE "KIT0003" TO TK-EXPECTED-X
     MOVE CC-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Prime-Creation
     MOVE "CREATE" TO CC-FUNCTION
     MOVE Ws-Bought TO KC-PARENT-ITEM
     MOVE 0 TO KC-PLANNED-QTY
     CALL "KITCREAT" USING Ws-Creat-Ca
     MOVE "A kit for no output at all is refused" TO TK-NAME
     MOVE "KIT0003" TO TK-EXPECTED-X
     MOVE CC-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Prime-Creation
     MOVE "CREATE" TO CC-FUNCTION
     MOVE Ws-Bought TO KC-PARENT-ITEM
     MOVE "JOB" TO KC-SOURCE-TYPE
     MOVE SPACES TO KC-JOB-REF
     CALL "KITCREAT" USING Ws-Creat-Ca
     MOVE "A job kit must name the job it serves" TO TK-NAME
     MOVE "KIT0024" TO TK-EXPECTED-X
     MOVE CC-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A refused creation leaves no kit behind" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KC-EXEC-KIT-ID TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>   KIT-002.  An item with no approved bill cannot produce a
*>   material requirement.  The attempt has to fail as an error
*>   and the kit must not come back sitting in a released state.
 Test-Incomplete-Bill-Is-Not-Released.
     IF Ws-Bought = SPACES
         MOVE "no bought item without a bill is seeded"
             TO TK-DETAIL
         MOVE "An incomplete bill cannot create a released" TO TK-NAME
         PERFORM TK-SKIP
     ELSE
         PERFORM Begin-Transaction
         PERFORM Prime-Creation
         MOVE "CREATE" TO CC-FUNCTION
         MOVE Ws-Bought TO KC-PARENT-ITEM
         CALL "KITCREAT" USING Ws-Creat-Ca
         MOVE "An item with no approved bill cannot be kitted"
             TO TK-NAME
         IF CC-RETURN-CODE NOT < 4
             PERFORM TK-PASS
         ELSE
             MOVE KC-STATUS TO TK-DETAIL
             PERFORM TK-FAIL
         END-IF
         MOVE "An incomplete bill never reaches a released status"
             TO TK-NAME
         IF KC-STATUS NOT = "RELEASED"
             PERFORM TK-PASS
         ELSE
             MOVE KC-STATUS TO TK-DETAIL
             PERFORM TK-FAIL
         END-IF
         MOVE "An incomplete bill is reported as incomplete, not" TO TK-NAME
         IF KC-COMPLETENESS NOT = "COMPLETE"
             PERFORM TK-PASS
         ELSE
             MOVE KC-COMPLETENESS TO TK-DETAIL
             PERFORM TK-FAIL
         END-IF
         PERFORM Rollback-Transaction
     END-IF.

*>   KIT-004.  Release fixes the bill revision, the units and the
*>   calculation assumptions for that kit.  Reading a released kit
*>   twice gives the same answer, and release cannot be run again
*>   to re-derive it from whatever the standard bill says today.
 Test-Release-Fixes-The-Calculation.
     IF Ws-Rel-Kit = SPACES
         MOVE "no released kit is seeded" TO TK-DETAIL
         MOVE "A released kit holds its fixed calculation"
             TO TK-NAME
         PERFORM TK-SKIP
     ELSE
         PERFORM Prime-Creation
         MOVE "ENQUIRE" TO CC-FUNCTION
         MOVE Ws-Rel-Kit TO KC-KIT-REF
         CALL "KITCREAT" USING Ws-Creat-Ca
         MOVE KC-GROSS-STARTS TO Ws-Gross-1
         MOVE KC-YIELD TO Ws-Yield-1
         MOVE "A released kit reports the status it was left in"
             TO TK-NAME
         MOVE "RELEASED" TO TK-EXPECTED-X
         MOVE KC-STATUS TO TK-ACTUAL-X
         PERFORM TK-ASSERT-STR
         MOVE "A released kit carries fixed gross starts"
             TO TK-NAME
         IF KC-GROSS-STARTS > 0
             PERFORM TK-PASS
         ELSE
             MOVE "gross starts were not fixed at release"
                 TO TK-DETAIL
             PERFORM TK-FAIL
         END-IF
         PERFORM Prime-Creation
         MOVE "GETLINE" TO CC-FUNCTION
         MOVE Ws-Rel-Kit TO KC-KIT-REF
         MOVE 0 TO KC-FROM-LINE
         MOVE 20 TO KC-WIN-MAX
         CALL "KITCREAT" USING Ws-Creat-Ca
         IF KC-WIN-CNT > 0
             MOVE KC-L-CURR-REQ (1) TO Ws-Req-1
         END-IF
         PERFORM Begin-Transaction
         MOVE SPACES TO SQLIO-STMT
         STRING "UPDATE bom.BomComponent SET UsageQty = UsageQty "
                "* 2 WHERE BomHeaderId IN (SELECT b.BomHeaderId "
                "FROM bom.BomHeader b JOIN kit.ExecutionKit k ON "
                "k.CompanyCode = b.CompanyCode AND "
                "k.ParentItemCode = b.ParentItemCode WHERE "
                "k.CompanyCode = 'STC' AND k.KitRef = ?)"
             DELIMITED BY SIZE INTO SQLIO-STMT
         MOVE 1 TO SQLIO-PARM-CNT
         MOVE Ws-Rel-Kit TO SQLIO-PARM-VAL(1)
         MOVE "N" TO SQLIO-PARM-NULL(1)
         MOVE 0 TO SQLIO-COL-CNT
         MOVE 4 TO SQLIO-CURSOR
         MOVE "EXEC" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Row
         MOVE "CLOS" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Row
         PERFORM Prime-Creation
         MOVE "GETLINE" TO CC-FUNCTION
         MOVE Ws-Rel-Kit TO KC-KIT-REF
         MOVE 0 TO KC-FROM-LINE
         MOVE 20 TO KC-WIN-MAX
         CALL "KITCREAT" USING Ws-Creat-Ca
         MOVE "A later standard bill change does not alter a" TO TK-NAME
         IF KC-WIN-CNT > 0
             MOVE Ws-Req-1 TO TK-EXPECTED-N
             MOVE KC-L-CURR-REQ (1) TO TK-ACTUAL-N
             PERFORM TK-ASSERT-NUM
         ELSE
             MOVE "the released kit has no requirement lines"
                 TO TK-DETAIL
             PERFORM TK-SKIP
         END-IF
         PERFORM Prime-Creation
         MOVE "ENQUIRE" TO CC-FUNCTION
         MOVE Ws-Rel-Kit TO KC-KIT-REF
         CALL "KITCREAT" USING Ws-Creat-Ca
         MOVE "The fixed gross starts survive the bill change"
             TO TK-NAME
         MOVE Ws-Gross-1 TO TK-EXPECTED-N
         MOVE KC-GROSS-STARTS TO TK-ACTUAL-N
         PERFORM TK-ASSERT-NUM
         MOVE "The fixed yield assumption survives it too"
             TO TK-NAME
         MOVE Ws-Yield-1 TO TK-EXPECTED-N
         MOVE KC-YIELD TO TK-ACTUAL-N
         PERFORM TK-ASSERT-NUM
         PERFORM Rollback-Transaction
         PERFORM Prime-Creation
         MOVE "RELEASE" TO CC-FUNCTION
         MOVE Ws-Rel-Kit TO KC-KIT-REF
         CALL "KITCREAT" USING Ws-Creat-Ca
         MOVE "An already released kit cannot be released again"
             TO TK-NAME
         IF CC-RETURN-CODE NOT < 4
             PERFORM TK-PASS
         ELSE
             MOVE "release was accepted a second time" TO TK-DETAIL
             PERFORM TK-FAIL
         END-IF
     END-IF.

*>   KIT-015.  Completion is an actual event: it names the kit, it
*>   records real output, and it refuses anything else.
 Test-Completion-Edits.
     PERFORM Prime-Completion
     MOVE "WIBBLE" TO CP-FUNCTION
     CALL "KITCOMPL" USING Ws-Compl-Ca
     MOVE "An unknown completion instruction is refused"
         TO TK-NAME
     MOVE "KIT0002" TO TK-EXPECTED-X
     MOVE CP-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A refused completion records nothing" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KM-COMPLETION-ID TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Prime-Completion
     MOVE "COMPCHK" TO CP-FUNCTION
     MOVE "ZZ-NO-SUCH-KIT" TO KM-KIT-REF
     MOVE 1 TO KM-GOOD-QTY
     CALL "KITCOMPL" USING Ws-Compl-Ca
     MOVE "Completing a kit that does not exist is not found"
         TO TK-NAME
     MOVE 16 TO TK-EXPECTED-N
     MOVE CP-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Prime-Completion
     MOVE "COMPCHK" TO CP-FUNCTION
     MOVE 0 TO KM-GOOD-QTY
     MOVE 0 TO KM-REJECTED-QTY
     CALL "KITCOMPL" USING Ws-Compl-Ca
     MOVE "A completion of nothing at all is refused" TO TK-NAME
     MOVE "KIT0003" TO TK-EXPECTED-X
     MOVE CP-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>   KIT-015.  Actual good output is checked against the plan: a
*>   run of ten with four already completed has six left, and a
*>   seventh unit on top of that is not output, it is a keying
*>   error.
 Test-Output-Stays-Within-Plan.
     PERFORM Prime-Completion
     MOVE "COMPCHK" TO CP-FUNCTION
     COMPUTE KM-GOOD-QTY = Ws-Remaining + 1
     CALL "KITCOMPL" USING Ws-Compl-Ca
     MOVE "Completing more than was planned is refused" TO TK-NAME
     MOVE "KIT0018" TO TK-EXPECTED-X
     MOVE CP-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "An over completion is an error, not a warning"
         TO TK-NAME
     MOVE 8 TO TK-EXPECTED-N
     MOVE CP-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Prime-Completion
     MOVE "COMPCHK" TO CP-FUNCTION
     MOVE Ws-Remaining TO KM-GOOD-QTY
     CALL "KITCOMPL" USING Ws-Compl-Ca
     MOVE "The remaining planned output is reported" TO TK-NAME
     MOVE Ws-Remaining TO TK-EXPECTED-N
     MOVE KM-REMAINING-OUT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Completing exactly what is left is accepted" TO TK-NAME
     IF CP-RETURN-CODE < 8
         PERFORM TK-PASS
     ELSE
         MOVE CP-MESSAGE TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF.

*>   KIT-015.  The last receipt of a run takes the whole residual
*>   work in progress out of the job and leaves nothing behind; a
*>   partial receipt takes its share and leaves the rest.
 Test-Wip-Transfer.
     PERFORM Prime-Completion
     MOVE "COMPCHK" TO CP-FUNCTION
     MOVE Ws-Remaining TO KM-GOOD-QTY
     CALL "KITCOMPL" USING Ws-Compl-Ca
     MOVE "The final receipt transfers the whole residual" TO TK-NAME
     MOVE Ws-Wip TO TK-EXPECTED-N
     MOVE KM-WIP-TRANSFER TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The final receipt leaves no work in progress on the job"
         TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KM-WIP-RESIDUAL TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     COMPUTE Ws-Half ROUNDED = Ws-Remaining / 2
     IF Ws-Half > 0 AND Ws-Half < Ws-Remaining
         PERFORM Prime-Completion
         MOVE "COMPCHK" TO CP-FUNCTION
         MOVE Ws-Half TO KM-GOOD-QTY
         CALL "KITCOMPL" USING Ws-Compl-Ca
         COMPUTE Ws-Expect ROUNDED =
             Ws-Wip * Ws-Half / Ws-Remaining
         MOVE "A partial receipt takes its share of the work in" TO TK-NAME
         MOVE Ws-Expect TO TK-EXPECTED-N
         MOVE KM-WIP-TRANSFER TO TK-ACTUAL-N
         PERFORM TK-ASSERT-NUM
         MOVE "The rest of the work in progress stays on the job"
             TO TK-NAME
         COMPUTE Ws-Expect = Ws-Wip - KM-WIP-TRANSFER
         MOVE Ws-Expect TO TK-EXPECTED-N
         MOVE KM-WIP-RESIDUAL TO TK-ACTUAL-N
         PERFORM TK-ASSERT-NUM
     ELSE
         MOVE "the seeded kit has no room for a partial receipt"
             TO TK-DETAIL
         MOVE "A partial receipt takes its share of the work in" TO TK-NAME
         PERFORM TK-SKIP
     END-IF
     MOVE "A dry run completion writes nothing" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KM-COMPLETION-ID TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

 COPY TESTKITP.
