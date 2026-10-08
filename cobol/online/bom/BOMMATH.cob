>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : BOMMATH
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : The required quantity mathematics of the bill of
*>              materials specification, section 5, in one place.
*>              Everything else in the module asks this program
*>              rather than repeating the formulae, so that the
*>              explosion, the netted plan, the cost roll-up, the
*>              kit derivation and the reports cannot drift apart.
*>
*>              Gross starts come from yield and the production
*>              quantum.  Scrap is an ADDITIONAL INPUT fraction -
*>              a ten per cent allowance multiplies the input by
*>              1.10 and is never a division by 0.90.  Fixed per
*>              run and fixed per batch are distinct bases and use
*>              the recorded run and batch counts.  Zero required
*>              output gives zero runs, zero batches and zero
*>              material, including the fixed lines.
*> CALLED BY  : BOMEXPL, BOMNET, BOMCOST, BOMJOB, the batch
*>              reports, the .NET client and the unit tests
*> INTERFACE  : CALL "BOMMATH" USING WS-BOMMATH-CA
*>              COPYBOOK BOMMATHCA
*> CALLS      : nothing (no database, no side effects)
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2014-03-17 AJS  CR2201 ENGINEERING REWRITE.  REPLACES THE
*>                 QUANTITY ROUTINES THAT WERE COPIED INTO BOMX01,
*>                 BOMX02 AND THE OLD COSTING SUITE.
*> 2014-09-30 AJS  CR2240 SCRAP RE-STATED AS AN ADDITIONAL INPUT
*>                 FRACTION AFTER THE AUDIT OF JOB 118446
*> 2015-07-02 AJS  CR2318 PACK AND MINIMUM ORDER PROPOSAL, WITH
*>                 THE EXCESS SHOWN SEPARATELY
*> 2017-01-23 KMP  CR2571 CEILING TOLERANCE - 2.9999999997 WAS
*>                 BEING ROUNDED UP TO 3 BATCHES
*> 2026-10-05 STERP D-10 CONVENTIONS CONFIRMED AGAINST B1 TO B6
*>================================================================
*> REQ: BOM-006, BOM-007, BOM-008, BOM-009, BOM-010, BOM-011
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BOMMATH.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 17/03/2014.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  Ws-Constants.
     05  Ws-Eps                  PIC S9(1)V9(9) VALUE 0.000000001.
     05  Ws-One                  PIC S9(1)V9(6) VALUE 1.
 01  Ws-Work.
     05  Ws-Quotient             PIC S9(15)V9(9) VALUE 0.
     05  Ws-Whole                PIC S9(15)V9(9) VALUE 0.
     05  Ws-Fraction             PIC S9(15)V9(9) VALUE 0.
     05  Ws-Steps                PIC S9(15)V9(6) VALUE 0.
     05  Ws-Scrap-Factor         PIC S9(5)V9(6) VALUE 0.
     05  Ws-Per-Basis            PIC S9(15)V9(9) VALUE 0.
     05  Ws-Base-Input           PIC S9(15)V9(9) VALUE 0.
     05  Ws-Yield-Starts         PIC S9(15)V9(9) VALUE 0.
     05  Ws-To-Buy               PIC S9(15)V9(6) VALUE 0.
     05  Ws-Edit                 PIC -(14)9.9(6).
     05  Ws-Rejected             PIC X VALUE "N".
         88  Ws-Is-Rejected                VALUE "Y".
 LINKAGE SECTION.
 COPY BOMMATHCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     PERFORM Initialise-Call
     IF Ws-Is-Rejected
         GOBACK
     END-IF
     EVALUATE CA-FUNCTION
         WHEN "PARENT"   PERFORM Parent-Quantities
         WHEN "COMPON"   PERFORM Component-Requirement
         WHEN "ROUNDUP"  PERFORM Round-To-Quantum
         WHEN "PACK"     PERFORM Pack-Proposal
         WHEN "CEIL"     PERFORM Ceiling-Only
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

*>----------------------------------------------------------------
*> Working storage inside STERPCOB.dll survives between calls, so
*> every entry starts by clearing what this call may set.
 Initialise-Call.
     INITIALIZE Ws-Work
     MOVE "N" TO Ws-Rejected
     MOVE "BOMMATH" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE 0 TO CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
     MOVE "BOM-006" TO CA-REQ-REF
     MOVE SPACES TO BM-EXPLAIN
     IF BM-RUN-COUNT < 1
         MOVE 1 TO BM-RUN-COUNT
     END-IF.

 Reject-Function.
     MOVE "BOM0101" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "BOMMATH does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE
     MOVE 8 TO CA-RETURN-CODE
     MOVE "Y" TO Ws-Rejected.

 Reject-Input.
     MOVE 8 TO CA-RETURN-CODE
     ADD 1 TO CA-MSG-COUNT
     MOVE "Y" TO Ws-Rejected.

*>----------------------------------------------------------------
*> PARENT - gross starts, batches and runs for one manufactured
*> parent occurrence.  G_raw = Q / Y, G = ceil(G_raw / g) x g,
*> N = ceil(G / L).  Batch count does NOT lift planned good output
*> to a multiple of L; that would be a production lot rounding
*> decision, which is a different rule (specification section 5).
 Parent-Quantities.
     PERFORM Validate-Parent
     IF Ws-Is-Rejected
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO BM-GROSS-RAW BM-GROSS-STARTS
     MOVE 0 TO BM-BATCH-COUNT
     IF BM-GOOD-QTY = 0
*>       a zero output trial has no runs, no batches and no
*>       material - setup stock needs an authorised job instead
         MOVE 0 TO BM-RUN-COUNT
         MOVE "Zero required output: no runs, batches or material"
             TO BM-EXPLAIN
         EXIT PARAGRAPH
     END-IF
     COMPUTE BM-GROSS-RAW = BM-GOOD-QTY / BM-YIELD
     MOVE BM-GROSS-RAW TO BM-VALUE
     MOVE BM-QUANTUM TO Ws-Steps
     PERFORM Ceiling-To-Step
     MOVE BM-RESULT TO BM-GROSS-STARTS
     IF BM-BATCH-SIZE > 0
         MOVE BM-GROSS-STARTS TO BM-VALUE
         MOVE BM-BATCH-SIZE TO Ws-Steps
         PERFORM Ceiling-To-Step
         COMPUTE BM-BATCH-COUNT ROUNDED =
             BM-RESULT / BM-BATCH-SIZE
     END-IF
     MOVE SPACES TO BM-EXPLAIN
     MOVE BM-GROSS-STARTS TO Ws-Edit
     STRING "Gross starts " FUNCTION TRIM(Ws-Edit)
            " from good output "
         DELIMITED BY SIZE INTO BM-EXPLAIN
     MOVE BM-GOOD-QTY TO Ws-Edit
     STRING FUNCTION TRIM(BM-EXPLAIN) FUNCTION TRIM(Ws-Edit)
         DELIMITED BY SIZE INTO BM-EXPLAIN.

 Validate-Parent.
     IF BM-YIELD <= 0 OR BM-YIELD > 1
         MOVE "BOM0102" TO CA-REASON-CODE
         MOVE "Yield must be greater than zero and not more than one"
             TO CA-MESSAGE
         PERFORM Reject-Input
         EXIT PARAGRAPH
     END-IF
     IF BM-QUANTUM <= 0
         MOVE "BOM0103" TO CA-REASON-CODE
         MOVE "The production quantum must be positive"
             TO CA-MESSAGE
         PERFORM Reject-Input
         EXIT PARAGRAPH
     END-IF
     IF BM-GOOD-QTY < 0
         MOVE "BOM0104" TO CA-REASON-CODE
         MOVE "Required good output cannot be negative"
             TO CA-MESSAGE
         PERFORM Reject-Input
         EXIT PARAGRAPH
     END-IF
     IF BM-BATCH-SIZE < 0
         MOVE "BOM0105" TO CA-REASON-CODE
         MOVE "The planned batch size cannot be negative"
             TO CA-MESSAGE
         PERFORM Reject-Input
     END-IF.

*>----------------------------------------------------------------
*> COMPON - the unrounded stock unit input requirement of one
*> component occurrence.  The three usage bases are deliberately
*> separate: a fixed per run line is not multiplied by good output
*> and is not repeated for partial receipts (BOM-007).
 Component-Requirement.
     PERFORM Validate-Component
     IF Ws-Is-Rejected
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO BM-UNROUNDED BM-YIELD-CONTRIB BM-SCRAP-CONTRIB
     COMPUTE Ws-Scrap-Factor = Ws-One + BM-SCRAP-PCT
     EVALUATE BM-USAGE-BASIS
         WHEN "PROPORTION"
             IF BM-GROSS-STARTS = 0
                 MOVE "No gross starts: no proportional requirement"
                     TO BM-EXPLAIN
                 EXIT PARAGRAPH
             END-IF
             COMPUTE Ws-Per-Basis =
                 BM-GROSS-STARTS / BM-NOMINAL-BASIS
             COMPUTE Ws-Base-Input =
                 Ws-Per-Basis * BM-USAGE-QTY * BM-CONV-FACT
             COMPUTE BM-UNROUNDED = Ws-Base-Input * Ws-Scrap-Factor
             COMPUTE BM-SCRAP-CONTRIB = Ws-Base-Input * BM-SCRAP-PCT
             IF BM-GOOD-QTY > 0 AND BM-GROSS-STARTS > BM-GOOD-QTY
                 COMPUTE Ws-Yield-Starts =
                     BM-GROSS-STARTS - BM-GOOD-QTY
                 COMPUTE BM-YIELD-CONTRIB =
                     Ws-Yield-Starts / BM-NOMINAL-BASIS
                     * BM-USAGE-QTY * BM-CONV-FACT * Ws-Scrap-Factor
             END-IF
             MOVE "Proportional to gross starts over nominal basis"
                 TO BM-EXPLAIN
         WHEN "FIXEDRUN"
             IF BM-RUN-COUNT < 1 OR BM-GOOD-QTY = 0
                 MOVE "No execution run: no fixed per run requirement"
                     TO BM-EXPLAIN
                 EXIT PARAGRAPH
             END-IF
             COMPUTE Ws-Base-Input =
                 BM-RUN-COUNT * BM-USAGE-QTY * BM-CONV-FACT
             COMPUTE BM-UNROUNDED = Ws-Base-Input * Ws-Scrap-Factor
             COMPUTE BM-SCRAP-CONTRIB = Ws-Base-Input * BM-SCRAP-PCT
             MOVE "Fixed per recorded execution run" TO BM-EXPLAIN
         WHEN "FIXEDBATCH"
             IF BM-BATCH-COUNT < 1 OR BM-GOOD-QTY = 0
                 MOVE "No batch: no fixed per batch requirement"
                     TO BM-EXPLAIN
                 EXIT PARAGRAPH
             END-IF
             COMPUTE Ws-Base-Input =
                 BM-BATCH-COUNT * BM-USAGE-QTY * BM-CONV-FACT
             COMPUTE BM-UNROUNDED = Ws-Base-Input * Ws-Scrap-Factor
             COMPUTE BM-SCRAP-CONTRIB = Ws-Base-Input * BM-SCRAP-PCT
             MOVE "Fixed per planned parent batch" TO BM-EXPLAIN
         WHEN OTHER
             MOVE "BOM0106" TO CA-REASON-CODE
             MOVE SPACES TO CA-MESSAGE
             STRING "Usage basis " FUNCTION TRIM(BM-USAGE-BASIS)
                    " is not a recognised basis"
                 DELIMITED BY SIZE INTO CA-MESSAGE
             PERFORM Reject-Input
     END-EVALUATE.

 Validate-Component.
     IF BM-NOMINAL-BASIS <= 0
         MOVE "BOM0107" TO CA-REASON-CODE
         MOVE "The nominal output basis must be positive"
             TO CA-MESSAGE
         PERFORM Reject-Input
         EXIT PARAGRAPH
     END-IF
     IF BM-CONV-FACT <= 0
         MOVE "BOM0108" TO CA-REASON-CODE
         MOVE "A positive unit conversion factor is required"
             TO CA-MESSAGE
         PERFORM Reject-Input
         EXIT PARAGRAPH
     END-IF
     IF BM-SCRAP-PCT < 0
         MOVE "BOM0109" TO CA-REASON-CODE
         MOVE "A scrap allowance cannot be negative" TO CA-MESSAGE
         PERFORM Reject-Input
         EXIT PARAGRAPH
     END-IF
     IF BM-USAGE-QTY < 0
         MOVE "BOM0110" TO CA-REASON-CODE
         MOVE "Negative usage is not a by-product mechanism"
             TO CA-MESSAGE
         PERFORM Reject-Input
     END-IF.

*>----------------------------------------------------------------
*> ROUNDUP - round upward once, at this independently supplied or
*> issued material occurrence, to the item's allowed stock quantum.
*> The unrounded amount and the increment are both kept, so that a
*> reviewer can see 2.2 pieces became 3 with 0.8 of rounding.
 Round-To-Quantum.
     IF BM-QUANTUM <= 0
         MOVE "BOM0103" TO CA-REASON-CODE
         MOVE "The allowed stock quantum must be positive"
             TO CA-MESSAGE
         PERFORM Reject-Input
         EXIT PARAGRAPH
     END-IF
     MOVE BM-UNROUNDED TO BM-VALUE
     MOVE BM-QUANTUM TO Ws-Steps
     PERFORM Ceiling-To-Step
     MOVE BM-RESULT TO BM-REQ-QTY
     COMPUTE BM-ROUND-INC = BM-REQ-QTY - BM-UNROUNDED
     IF BM-ROUND-INC < 0
         MOVE 0 TO BM-ROUND-INC
     END-IF
     MOVE SPACES TO BM-EXPLAIN
     MOVE BM-ROUND-INC TO Ws-Edit
     STRING "Rounded up by " FUNCTION TRIM(Ws-Edit)
            " to the allowed stock quantum"
         DELIMITED BY SIZE INTO BM-EXPLAIN.

*>----------------------------------------------------------------
*> PACK - procurement minimum and pack multiple.  This happens on
*> the supply proposal only; it never alters the engineering issue
*> requirement, and the excess it creates is named (B5).
 Pack-Proposal.
     COMPUTE Ws-To-Buy = BM-REQ-QTY - BM-FREE-STOCK
     IF Ws-To-Buy < 0
         MOVE 0 TO Ws-To-Buy
     END-IF
     MOVE Ws-To-Buy TO BM-PACK-QTY
     MOVE 0 TO BM-EXCESS
     IF Ws-To-Buy = 0
         MOVE "Free stock covers the requirement: nothing to buy"
             TO BM-EXPLAIN
         EXIT PARAGRAPH
     END-IF
     IF BM-PACK-MULT > 0
         MOVE Ws-To-Buy TO BM-VALUE
         MOVE BM-PACK-MULT TO Ws-Steps
         PERFORM Ceiling-To-Step
         MOVE BM-RESULT TO BM-PACK-QTY
     END-IF
     IF BM-MIN-ORDER > BM-PACK-QTY
         MOVE BM-MIN-ORDER TO BM-PACK-QTY
         IF BM-PACK-MULT > 0
             MOVE BM-PACK-QTY TO BM-VALUE
             MOVE BM-PACK-MULT TO Ws-Steps
             PERFORM Ceiling-To-Step
             MOVE BM-RESULT TO BM-PACK-QTY
         END-IF
     END-IF
     COMPUTE BM-EXCESS = BM-PACK-QTY - Ws-To-Buy
     IF BM-EXCESS < 0
         MOVE 0 TO BM-EXCESS
     END-IF
     MOVE SPACES TO BM-EXPLAIN
     MOVE BM-PACK-QTY TO Ws-Edit
     STRING "Purchase proposal " FUNCTION TRIM(Ws-Edit)
            " with excess "
         DELIMITED BY SIZE INTO BM-EXPLAIN
     MOVE BM-EXCESS TO Ws-Edit
     STRING FUNCTION TRIM(BM-EXPLAIN) FUNCTION TRIM(Ws-Edit)
         DELIMITED BY SIZE INTO BM-EXPLAIN.

 Ceiling-Only.
     IF BM-QUANTUM <= 0
         MOVE "BOM0103" TO CA-REASON-CODE
         MOVE "The step must be positive" TO CA-MESSAGE
         PERFORM Reject-Input
         EXIT PARAGRAPH
     END-IF
     MOVE BM-QUANTUM TO Ws-Steps
     PERFORM Ceiling-To-Step.

*>----------------------------------------------------------------
*> Ceiling of BM-VALUE to the step in Ws-Steps.  CR2571: a quotient
*> that only misses the whole number by a rounding artefact must
*> not buy another batch, hence the tolerance.
 Ceiling-To-Step.
     MOVE 0 TO BM-RESULT
     IF Ws-Steps <= 0
         MOVE BM-VALUE TO BM-RESULT
         EXIT PARAGRAPH
     END-IF
     IF BM-VALUE <= 0
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ws-Quotient = BM-VALUE / Ws-Steps
     COMPUTE Ws-Whole = FUNCTION INTEGER(Ws-Quotient)
     COMPUTE Ws-Fraction = Ws-Quotient - Ws-Whole
     IF Ws-Fraction > Ws-Eps
         ADD 1 TO Ws-Whole
     END-IF
     COMPUTE BM-RESULT = Ws-Whole * Ws-Steps.
