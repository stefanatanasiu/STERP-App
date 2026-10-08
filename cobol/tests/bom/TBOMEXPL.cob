>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : TBOMEXPL
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL - UNIT TESTS
*> PURPOSE    : Reproduce the published numbers of the bill of
*>              materials specification with the real engine and
*>              the seeded demonstration bills.  Every expected
*>              value below is quoted from the specification, not
*>              invented here - if the engine and the document
*>              disagree, the engine is wrong.
*>              Needs the database and the BOM demonstration seed;
*>              without them the suite reports TAP # SKIP.
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2026-10-05 STERP B1, B3, B4, B5, B6 AND AC-110
*>================================================================
*> REQ: BOM-006, BOM-007, BOM-008, BOM-009, BOM-010, BOM-011,
*> REQ: BOM-012, BOM-013, BOM-014, BOM-015, BOM-017, BOM-018,
*> REQ: BOM-019, BOM-027, BOM-029, BOM-032
*> AC: AC-085, AC-087, AC-088, AC-089, AC-090, AC-092, AC-110
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TBOMEXPL.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY TESTKITW.
 COPY BOMEXPLCA REPLACING ==DFHCOMMAREA== BY ==Ws-Expl==
      LEADING ==CA-== BY ==EA-==
      LEADING ==BX-== BY ==EX-==.
 COPY BOMLEAFCA REPLACING ==DFHCOMMAREA== BY ==Ws-Leaf==
      LEADING ==CA-== BY ==LA-==
      LEADING ==BL-== BY ==LL-==.
 COPY SQLIOCA.
 01  Ws-Probe                    PIC X(60) VALUE SPACES.
 01  Ws-Flags.
     05  Ws-Db-Ok                PIC X VALUE "Y".
     05  Ws-I                    PIC S9(4) VALUE 0.
     05  Ws-Skip-Note            PIC X(60) VALUE SPACES.
 PROCEDURE DIVISION.
 Main-Entry.
     MOVE "TBOMEXPL" TO TK-SUITE
     PERFORM TK-BEGIN
     PERFORM Check-Database
     IF Ws-Db-Ok = "N"
         PERFORM Skip-Everything
         PERFORM TK-END
         STOP RUN
     END-IF
     PERFORM B1-Ten-Good-Frames
     PERFORM B1-Leaf-Requirement
     PERFORM B1-Zero-Output
     PERFORM B3-Basis-Unit-Scrap
     PERFORM B4-Independent-Runs
     PERFORM B4-Consolidated-Run
     PERFORM B4-Split-Into-Two-Runs
     PERFORM B5-Rounding-And-Packs
     PERFORM B6-Shared-Descendant
     PERFORM B6-Invalid-Cycle
     PERFORM Deep-Chain-Ac110
     PERFORM Depth-Limit-Is-Labelled
     PERFORM Missing-Child-Bill
     PERFORM Unconfigured-Option-Group
     PERFORM Configured-Option-Group
     PERFORM TK-END
     STOP RUN.

*>----------------------------------------------------------------
 Check-Database.
     PERFORM Set-Up-Frame-Request
     MOVE 10 TO EX-REQ-QTY
     CALL "BOMEXPL" USING Ws-Expl
     IF EA-RETURN-CODE = 12 OR EX-OCC-COUNT = 0
         MOVE "N" TO Ws-Db-Ok
         MOVE EA-MESSAGE TO Ws-Skip-Note
     END-IF.

 Skip-Everything.
     MOVE "BOM demonstration data or database not available"
         TO TK-DETAIL
     MOVE "B1 to B6 worked examples" TO TK-NAME
     PERFORM TK-SKIP.

 Set-Up-Frame-Request.
     INITIALIZE Ws-Expl
     MOVE "EXPLODE" TO EA-FUNCTION
     MOVE "STC" TO EA-COMPANY
     MOVE "RDG" TO EA-SITE
     MOVE "ENGINEER" TO EA-USER
     MOVE 20261001 TO EA-BUSINESS-DATE
     MOVE "GROSS" TO EX-CALC-TYPE
     MOVE "DEMO-FRAME" TO EX-PARENT-ITEM
     MOVE "STANDARD" TO EX-PURPOSE
     MOVE 20261001 TO EX-EFF-DATE
     MOVE 25 TO EX-MAX-DEPTH
     MOVE "Y" TO EX-EXPAND-PHANTOM
     MOVE "Y" TO EX-INCLUDE-NOTES.

 Ask-Total.
     MOVE "LEAFQTY" TO EA-FUNCTION
     CALL "BOMEXPL" USING Ws-Expl
     MOVE EX-KEY-TOTAL TO TK-ACTUAL-N.

*>----------------------------------------------------------------
*> B1 - ten good frames.  Frame gross starts 10; good braces 20;
*> brace gross starts 25; brace batches 3.
 B1-Ten-Good-Frames.
     PERFORM Set-Up-Frame-Request
     MOVE 10 TO EX-REQ-QTY
     CALL "BOMEXPL" USING Ws-Expl
     MOVE "B1 explosion of ten frames is complete" TO TK-NAME
     MOVE "COMPLETE" TO TK-EXPECTED-X
     MOVE EX-COMPLETENESS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "B1 frame gross starts are ten" TO TK-NAME
     MOVE 10 TO TK-EXPECTED-N
     MOVE EX-GROSS-STARTS TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "B1 frame runs are one" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE EX-RUN-COUNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "B1 good braces required are twenty" TO TK-NAME
     MOVE "DEMO-BRACE" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 20 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     PERFORM Find-Brace-Node.

*> The brace node carries its own gross starts and batch count:
*> 20 / 0.80 = 25 starts, ceil(25 / 10) = 3 batches.
 Find-Brace-Node.
     MOVE "GETLINE" TO EA-FUNCTION
     MOVE "ALL" TO EX-VIEW
     MOVE 1 TO EX-FROM-LINE
     MOVE 25 TO EX-WIN-MAX
     CALL "BOMEXPL" USING Ws-Expl
     MOVE 0 TO TK-ACTUAL-N
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > EX-WIN-CNT
         IF EX-L-ITEM(Ws-I) = "DEMO-BRACE"
             MOVE EX-L-GROSS(Ws-I) TO TK-ACTUAL-N
             MOVE EX-L-BATCHES(Ws-I) TO TK-EXPECTED-N
         END-IF
     END-PERFORM
     MOVE "B1 brace gross starts are twenty five" TO TK-NAME
     MOVE 25 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     MOVE "B1 brace batches are three" TO TK-NAME
     MOVE 3 TO TK-EXPECTED-N
     MOVE 0 TO TK-ACTUAL-N
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > EX-WIN-CNT
         IF EX-L-ITEM(Ws-I) = "DEMO-BRACE"
             MOVE EX-L-BATCHES(Ws-I) TO TK-ACTUAL-N
         END-IF
     END-PERFORM
     PERFORM TK-ASSERT-NUM.

*> The flattened purchased material requirement of B1.
 B1-Leaf-Requirement.
     MOVE "B1 plate is eleven kilograms" TO TK-NAME
     MOVE "DEMO-PLATE" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 11 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     MOVE "B1 pins are seventy on all valid paths" TO TK-NAME
     MOVE "DEMO-PIN" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 70 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     MOVE "B1 resin is 0.6 litres from three batches" TO TK-NAME
     MOVE "DEMO-RESIN" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 0.6 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     MOVE "B1 cleaner is 0.5 litres from one run" TO TK-NAME
     MOVE "DEMO-CLEAN" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 0.5 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     MOVE "B1 labels are ten, the phantom needs no stock"
         TO TK-NAME
     MOVE "DEMO-LABEL" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 10 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     MOVE "B1 phantom packs are a grouping of ten, not stock"
         TO TK-NAME
     MOVE "DEMO-PACK" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 10 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM.

*> A zero output trial has no runs, no batches and no material -
*> including the fixed per run and fixed per batch lines.
 B1-Zero-Output.
     PERFORM Set-Up-Frame-Request
     MOVE 0 TO EX-REQ-QTY
     CALL "BOMEXPL" USING Ws-Expl
     MOVE "Zero output needs no cleaner" TO TK-NAME
     MOVE "DEMO-CLEAN" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 0 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     MOVE "Zero output needs no resin" TO TK-NAME
     MOVE "DEMO-RESIN" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 0 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     MOVE "Zero output has no execution run" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE EX-RUN-COUNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> B3 - 30 / 10 x 250 x 1.04 = 780 grams = 0.780 kilograms.
 B3-Basis-Unit-Scrap.
     PERFORM Set-Up-Frame-Request
     MOVE "DEMO-B3-PARENT" TO EX-PARENT-ITEM
     MOVE 30 TO EX-REQ-QTY
     CALL "BOMEXPL" USING Ws-Expl
     MOVE "B3 requirement is 0.780 kilograms" TO TK-NAME
     MOVE "DEMO-B3-GRAN" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 0.78 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> B4 - two independent six unit runs: the fixed per run aid totals
*> 0.6 litres and the fixed per batch aid 0.4 litres.
 B4-Independent-Runs.
     PERFORM Set-Up-Frame-Request
     MOVE "DEMO-B4-TOP" TO EX-PARENT-ITEM
     MOVE 1 TO EX-REQ-QTY
     MOVE "N" TO EX-CONSOLIDATE
     CALL "BOMEXPL" USING Ws-Expl
     MOVE "B4 independent runs need 0.6 litres of run aid"
         TO TK-NAME
     MOVE "DEMO-B4-RUNAID" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 0.6 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     MOVE "B4 independent runs need 0.4 litres of batch aid"
         TO TK-NAME
     MOVE "DEMO-B4-BATAID" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 0.4 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM.

*> B4 - approved consolidation into one twelve unit run: the run
*> aid falls to 0.3 litres while the batch aid stays 0.4.
 B4-Consolidated-Run.
     PERFORM Set-Up-Frame-Request
     MOVE "DEMO-B4-TOP" TO EX-PARENT-ITEM
     MOVE 1 TO EX-REQ-QTY
     MOVE "Y" TO EX-CONSOLIDATE
     CALL "BOMEXPL" USING Ws-Expl
     MOVE "B4 consolidated run needs 0.3 litres of run aid"
         TO TK-NAME
     MOVE "DEMO-B4-RUNAID" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 0.3 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     MOVE "B4 consolidated run still needs two batches" TO TK-NAME
     MOVE "DEMO-B4-BATAID" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 0.4 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM.

*> Eight units split into two four unit runs require two batches,
*> not the one batch implied by pooling eight units.
 B4-Split-Into-Two-Runs.
     PERFORM Set-Up-Frame-Request
     MOVE "DEMO-B4-SUB" TO EX-PARENT-ITEM
     MOVE 8 TO EX-REQ-QTY
     MOVE 2 TO EX-SPLIT-CNT
     MOVE 4 TO EX-SPLIT-QTY(1)
     MOVE 4 TO EX-SPLIT-QTY(2)
     CALL "BOMEXPL" USING Ws-Expl
     MOVE "Two four unit runs require two batches" TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     MOVE EX-BATCH-COUNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Two four unit runs require two run aids" TO TK-NAME
     MOVE "DEMO-B4-RUNAID" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 0.6 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     PERFORM Set-Up-Frame-Request
     MOVE "DEMO-B4-SUB" TO EX-PARENT-ITEM
     MOVE 8 TO EX-REQ-QTY
     MOVE 0 TO EX-SPLIT-CNT
     CALL "BOMEXPL" USING Ws-Expl
     MOVE "Pooled eight units require only one batch" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE EX-BATCH-COUNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> B5 - two occurrences of 1.2 pieces are rounded separately to
*> two each, so the issue requirement is four, not three.
 B5-Rounding-And-Packs.
     PERFORM B5-Own-The-Pack-Fixture
     PERFORM Set-Up-Frame-Request
     MOVE "DEMO-B5-TOP" TO EX-PARENT-ITEM
     MOVE 1 TO EX-REQ-QTY
     CALL "BOMEXPL" USING Ws-Expl
     MOVE "B5 issue requirement is four pieces" TO TK-NAME
     MOVE "DEMO-B5-PART" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 4 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     PERFORM B5-Check-Pack.

*> Packs of five: buy five and identify one as excess supply.
*> The purchase multiple is planning data that the demonstration
*> scripts of other modules re-profile, so this test owns the one
*> figure the published B5 example depends on: it writes the
*> seeded pack of five back before it reads.  The statement is the
*> seeded value, so it is idempotent and leaves the demonstration
*> dataset as db\seed\30-bom-items.sql describes it.
 B5-Own-The-Pack-Fixture.
     MOVE SPACES TO SQLIO-STMT
     STRING "UPDATE inv.ItemSite SET PurchaseMultipleQty = 5, "
            "ModifiedBy = 'TBOMEXPL', "
            "ModifiedAt = SYSDATETIME() "
            "WHERE CompanyCode = 'STC' AND SiteCode = 'RDG' "
            "AND ItemCode = 'DEMO-B5-PART'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 7 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Probe
     IF SQLIO-SQLCODE < 0
         MOVE "B5 pack fixture could not be written" TO TK-NAME
         MOVE SQLIO-ERRMSG(1:100) TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Probe.

 B5-Check-Pack.
     INITIALIZE Ws-Leaf
     MOVE "FLATTEN" TO LA-FUNCTION
     MOVE "STC" TO LA-COMPANY
     MOVE "RDG" TO LA-SITE
     MOVE "ENGINEER" TO LA-USER
     MOVE 20261001 TO LA-BUSINESS-DATE
     MOVE "DEMO-B5-TOP" TO LL-PARENT-ITEM
     MOVE 1 TO LL-REQ-QTY
     MOVE 20261001 TO LL-EFF-DATE
     MOVE "Y" TO LL-REEXPLODE
     MOVE 1 TO LL-FROM-LINE
     MOVE 25 TO LL-WIN-MAX
     CALL "BOMLEAF" USING Ws-Leaf
     MOVE "B5 purchase proposal is one pack of five" TO TK-NAME
     MOVE 5 TO TK-EXPECTED-N
     MOVE 0 TO TK-ACTUAL-N
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > LL-WIN-CNT
         IF LL-L-ITEM(Ws-I) = "DEMO-B5-PART"
             MOVE LL-L-PACK-QTY(Ws-I) TO TK-ACTUAL-N
         END-IF
     END-PERFORM
     PERFORM TK-ASSERT-NUM
     MOVE "B5 pack of five identifies one excess" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE 0 TO TK-ACTUAL-N
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > LL-WIN-CNT
         IF LL-L-ITEM(Ws-I) = "DEMO-B5-PART"
             MOVE LL-L-EXCESS(Ws-I) TO TK-ACTUAL-N
         END-IF
     END-PERFORM
     PERFORM TK-ASSERT-NUM
     MOVE "B5 two occurrences contribute to one leaf" TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     MOVE 0 TO TK-ACTUAL-N
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > LL-WIN-CNT
         IF LL-L-ITEM(Ws-I) = "DEMO-B5-PART"
             MOVE LL-L-CONTRIB(Ws-I) TO TK-ACTUAL-N
         END-IF
     END-PERFORM
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> B6 - a screw reached by two independent paths is valid and both
*> quantities contribute: three plus four is seven.
 B6-Shared-Descendant.
     PERFORM Set-Up-Frame-Request
     MOVE "DEMO-B6-ROOT" TO EX-PARENT-ITEM
     MOVE 1 TO EX-REQ-QTY
     CALL "BOMEXPL" USING Ws-Expl
     MOVE "B6 shared descendant is valid" TO TK-NAME
     MOVE "COMPLETE" TO TK-EXPECTED-X
     MOVE EX-COMPLETENESS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "B6 screws counted on both paths are seven" TO TK-NAME
     MOVE "DEMO-B6-SCREW" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 7 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM.

*> B6 - a genuine ancestry loop is blocked and names its path.
 B6-Invalid-Cycle.
     PERFORM Set-Up-Frame-Request
     MOVE "DEMO-B6-CYC" TO EX-PARENT-ITEM
     MOVE 1 TO EX-REQ-QTY
     CALL "BOMEXPL" USING Ws-Expl
     MOVE "B6 cycle is refused" TO TK-NAME
     MOVE "ERROR" TO TK-EXPECTED-X
     MOVE EX-COMPLETENESS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "B6 cycle names the offending path" TO TK-NAME
     IF EX-CYCLE-PATH NOT = SPACES
         PERFORM TK-PASS
     ELSE
         PERFORM TK-FAIL
     END-IF
     MOVE "B6 cycle reports reason BOM0513" TO TK-NAME
     MOVE "BOM0513" TO TK-EXPECTED-X
     MOVE EA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> AC-110 - twenty edges, usage two at every edge, root demand
*> one: the complete leaf requirement is 1,048,576.
 Deep-Chain-Ac110.
     PERFORM Set-Up-Frame-Request
     MOVE "DEMO-DEEP-00" TO EX-PARENT-ITEM
     MOVE 1 TO EX-REQ-QTY
     CALL "BOMEXPL" USING Ws-Expl
     MOVE "AC-110 deep chain is complete" TO TK-NAME
     MOVE "COMPLETE" TO TK-EXPECTED-X
     MOVE EX-COMPLETENESS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "AC-110 leaf requirement is 1048576" TO TK-NAME
     MOVE "DEMO-DEEP-20" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 1048576 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     MOVE "AC-110 reaches twenty levels" TO TK-NAME
     MOVE 20 TO TK-EXPECTED-N
     MOVE EX-DEPTH-REACHED TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*> A display depth limit is labelled and is never presented as a
*> complete leaf requirement.
 Depth-Limit-Is-Labelled.
     PERFORM Set-Up-Frame-Request
     MOVE "DEMO-DEEP-00" TO EX-PARENT-ITEM
     MOVE 1 TO EX-REQ-QTY
     MOVE 3 TO EX-MAX-DEPTH
     CALL "BOMEXPL" USING Ws-Expl
     MOVE "A depth limit yields an incomplete calculation"
         TO TK-NAME
     MOVE "INCOMPLETE" TO TK-EXPECTED-X
     MOVE EX-COMPLETENESS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A depth limit is reported with its own reason code"
         TO TK-NAME
     MOVE "BOM0520" TO TK-EXPECTED-X
     MOVE EA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*> A missing manufactured child bill reports its path and an
*> incomplete status - never a skipped branch valued at zero.
 Missing-Child-Bill.
     PERFORM Set-Up-Frame-Request
     MOVE "DEMO-ERR-TOP" TO EX-PARENT-ITEM
     MOVE 1 TO EX-REQ-QTY
     CALL "BOMEXPL" USING Ws-Expl
     MOVE "A missing child bill is incomplete, not zero"
         TO TK-NAME
     MOVE "INCOMPLETE" TO TK-EXPECTED-X
     MOVE EX-COMPLETENESS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A missing child bill reports the affected path"
         TO TK-NAME
     IF EX-ERROR-PATH NOT = SPACES
         PERFORM TK-PASS
     ELSE
         PERFORM TK-FAIL
     END-IF.

*>----------------------------------------------------------------
*> An alternate group with no selected choice cannot produce an
*> approved complete result, and the alternatives are not summed.
 Unconfigured-Option-Group.
     PERFORM Set-Up-Frame-Request
     MOVE "DEMO-OPT-TOP" TO EX-PARENT-ITEM
     MOVE 1 TO EX-REQ-QTY
     CALL "BOMEXPL" USING Ws-Expl
     MOVE "An unconfigured option group is explicit" TO TK-NAME
     MOVE "INCOMPLETE" TO TK-EXPECTED-X
     MOVE EX-COMPLETENESS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "Unselected alternatives are not all summed" TO TK-NAME
     MOVE "DEMO-OPT-RED" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 0 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM.

*> With the choice stated, exactly one alternative contributes.
 Configured-Option-Group.
     PERFORM Set-Up-Frame-Request
     MOVE "DEMO-OPT-TOP" TO EX-PARENT-ITEM
     MOVE 1 TO EX-REQ-QTY
     MOVE 1 TO EX-OPT-CNT
     MOVE "FINISH" TO EX-O-GROUP(1)
     MOVE "DEMO-OPT-RED" TO EX-O-CHOICE(1)
     CALL "BOMEXPL" USING Ws-Expl
     MOVE "The selected alternative contributes one" TO TK-NAME
     MOVE "DEMO-OPT-RED" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 1 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     MOVE "The rejected alternative contributes nothing"
         TO TK-NAME
     MOVE "DEMO-OPT-BLUE" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 0 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM
     MOVE "A note line reserves no stock" TO TK-NAME
     MOVE "DEMO-NOTE" TO EX-KEY-ITEM
     PERFORM Ask-Total
     MOVE 0 TO TK-EXPECTED-N
     PERFORM TK-ASSERT-NUM.

 COPY TESTKITP.
