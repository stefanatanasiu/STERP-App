>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : KITQTY
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : The quantity arithmetic of a kit material line.
*>              Specification 09 section 5 states four rules and
*>              this program is the only place they are coded:
*>
*>                net issued = gross issued - confirmed returns
*>                remaining  = max(0, current requirement
*>                                      - net issued)
*>                excess     = max(0, net issued
*>                                      - current requirement)
*>                gross issued = returned + scrapped
*>                             + consumed + still on the job
*>
*>              Scrap is a DISPOSITION of issued material.  It
*>              does not reduce net issued and it does not
*>              increase the requirement; replacement material
*>              needs an approved amendment (KIT-014, D-12).
*>
*>              Reducing an approved requirement below what has
*>              already been issued produces EXCESS.  The 1998
*>              version quietly reduced the issue instead and the
*>              stores could never reconcile - CR2488.
*> CALLED BY  : KITCREAT, KITAMEND, KITISSUE, KITRET, KITSCRAP,
*>              KITCOMPL, KITCLOSE, KITCOST, the kit forms and
*>              the unit tests
*> INTERFACE  : CALL "KITQTY" USING WS-KITQTY-CA
*>              COPYBOOK KITQTYCA
*> CALLS      : nothing.  No database, no stock, no side effect -
*>              which is why a trial may call it freely.
*>----------------------------------------------------------------
*> CHANGE LOG
*> 1994-03-07 DRN  CR0112 KIT OUTSTANDING QUANTITY
*> 1999-11-30 KMP  Y2K - NO DATES HERE, CHECKED AND SIGNED OFF
*> 2014-09-02 PMK  CR2240 ASSEMBLY EXECUTION REWRITE - FREE FORMAT
*> 2016-05-30 PMK  CR2488 EXCESS SEPARATED FROM OUTSTANDING
*> 2026-10-06 STERP KIT-003, KIT-005, KIT-011, KIT-016
*>================================================================
*> REQ: KIT-003, KIT-005, KIT-011, KIT-016
 IDENTIFICATION DIVISION.
 PROGRAM-ID. KITQTY.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 02/09/2014.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  Ws-Constants.
     05  Ws-Tolerance            PIC S9(5)V9(6) VALUE 0.000001.
 LOCAL-STORAGE SECTION.
 01  Ls-Work.
     05  Ls-Net                  PIC S9(13)V9(6) VALUE 0.
     05  Ls-Remaining            PIC S9(13)V9(6) VALUE 0.
     05  Ls-Excess               PIC S9(13)V9(6) VALUE 0.
     05  Ls-Explained            PIC S9(13)V9(6) VALUE 0.
     05  Ls-Starts               PIC S9(13)V9(6) VALUE 0.
     05  Ls-Required             PIC S9(13)V9(6) VALUE 0.
     05  Ls-Batches              PIC S9(9) COMP-5 VALUE 0.
     05  Ls-Runs                 PIC S9(9) COMP-5 VALUE 0.
     05  Ls-Quantum-Units        PIC S9(13)V9(6) VALUE 0.
     05  Ls-Edit                 PIC -(13)9.9(6).
     05  Ls-Money-Edit           PIC -(9)9.99.
 LINKAGE SECTION.
 COPY KITQTYCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "KITQTY" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE 0 TO CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     MOVE "KIT-003" TO CA-REQ-REF
     MOVE SPACES TO KQ-IMPACT-TEXT
     MOVE "N" TO KQ-NEEDS-AUTH
     EVALUATE CA-FUNCTION
         WHEN "LINEQTY"  PERFORM Derive-Line-Quantities
         WHEN "REQCHG"   PERFORM Requirement-Change-Impact
         WHEN "SCALE"    PERFORM Scale-For-New-Output
         WHEN "ACCOUNT"  PERFORM Material-Accountability
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "KIT0002" TO CA-REASON-CODE
     STRING "KITQTY does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
*> A negative movement total is a corrupted caller, not a business
*> case.  Refuse rather than produce a plausible wrong number.
 Validate-Movement-Totals.
     IF KQ-GROSS-ISSUED < 0 OR KQ-RETURNED < 0
             OR KQ-SCRAPPED < 0 OR KQ-CURRENT-REQ < 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "Issued, returned, scrapped and requirement quantities must not be negative."
              TO CA-MESSAGE
     END-IF.

*>----------------------------------------------------------------
*> LINEQTY - the three derived quantities of specification 5.
*> Worked example from the specification: requirement 10, gross
*> issue 7, accepted return 2 gives net issued 5 and remaining 5.
 Derive-Line-Quantities.
     PERFORM Validate-Movement-Totals
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     IF KQ-RETURNED > KQ-GROSS-ISSUED + Ws-Tolerance
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0011" TO CA-REASON-CODE
         MOVE "Returned quantity exceeds the quantity issued to this kit line."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ls-Net = KQ-GROSS-ISSUED - KQ-RETURNED
     IF Ls-Net < 0
         MOVE 0 TO Ls-Net
     END-IF
     COMPUTE Ls-Remaining = KQ-CURRENT-REQ - Ls-Net
     IF Ls-Remaining < 0
         MOVE 0 TO Ls-Remaining
     END-IF
     COMPUTE Ls-Excess = Ls-Net - KQ-CURRENT-REQ
     IF Ls-Excess < 0
         MOVE 0 TO Ls-Excess
     END-IF
     MOVE Ls-Net TO KQ-NET-ISSUED
     MOVE Ls-Remaining TO KQ-REMAINING
     MOVE Ls-Excess TO KQ-EXCESS
     IF Ls-Excess > Ws-Tolerance
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0022" TO CA-REASON-CODE
         MOVE Ls-Excess TO Ls-Edit
         STRING "Excess material of " FUNCTION TRIM(Ls-Edit)
                " is on this line and needs a disposition."
             DELIMITED BY SIZE INTO CA-MESSAGE
         STRING "EXCESS " FUNCTION TRIM(Ls-Edit)
                " - return or obtain an approved disposition"
             DELIMITED BY SIZE INTO KQ-IMPACT-TEXT
     END-IF.

*>----------------------------------------------------------------
*> REQCHG - KIT-005.  Show the impact; do not apply it.  Authority
*> is needed whenever the change increases the requirement or
*> creates excess against material already issued.
 Requirement-Change-Impact.
     PERFORM Validate-Movement-Totals
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     IF KQ-PROPOSED-REQ < 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "A proposed requirement may not be negative." TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ls-Net = KQ-GROSS-ISSUED - KQ-RETURNED
     IF Ls-Net < 0
         MOVE 0 TO Ls-Net
     END-IF
     MOVE Ls-Net TO KQ-NET-ISSUED
     COMPUTE KQ-REQ-DELTA = KQ-PROPOSED-REQ - KQ-CURRENT-REQ
     MOVE KQ-PROPOSED-REQ TO KQ-NEW-REQ
     COMPUTE Ls-Remaining = KQ-PROPOSED-REQ - Ls-Net
     IF Ls-Remaining < 0
         MOVE 0 TO Ls-Remaining
     END-IF
     COMPUTE Ls-Excess = Ls-Net - KQ-PROPOSED-REQ
     IF Ls-Excess < 0
         MOVE 0 TO Ls-Excess
     END-IF
     MOVE Ls-Remaining TO KQ-REMAINING
     MOVE Ls-Excess TO KQ-EXCESS
     COMPUTE KQ-COST-IMPACT ROUNDED
         = KQ-REQ-DELTA * KQ-UNIT-COST
*>   A reduction frees the reservation that covered it; an
*>   increase asks for more.  Reservations never go below zero.
     COMPUTE KQ-RESV-IMPACT = KQ-REQ-DELTA
     IF Ls-Net > Ws-Tolerance OR KQ-GROSS-ISSUED > Ws-Tolerance
         MOVE "Y" TO KQ-ISSUED-HISTORY
     ELSE
         MOVE "N" TO KQ-ISSUED-HISTORY
     END-IF
     PERFORM Decide-Change-Authority.

 Decide-Change-Authority.
     EVALUATE TRUE
         WHEN KQ-PROPOSED-REQ = 0 AND KQ-HAS-HISTORY
             MOVE "Y" TO KQ-NEEDS-AUTH
             MOVE 6 TO CA-RETURN-CODE
             MOVE "KIT0015" TO CA-REASON-CODE
             MOVE "This line has issued material; its history and cost remain even at a zero requirement."
                  TO CA-MESSAGE
         WHEN Ls-Excess > Ws-Tolerance
             MOVE "Y" TO KQ-NEEDS-AUTH
             MOVE 6 TO CA-RETURN-CODE
             MOVE "KIT0009" TO CA-REASON-CODE
             MOVE Ls-Excess TO Ls-Edit
             STRING "Reducing the requirement leaves excess issued material of "
                    FUNCTION TRIM(Ls-Edit)
                    " which needs authority and a disposition."
                 DELIMITED BY SIZE INTO CA-MESSAGE
         WHEN KQ-REQ-DELTA > Ws-Tolerance
             MOVE "Y" TO KQ-NEEDS-AUTH
             MOVE 6 TO CA-RETURN-CODE
             MOVE "KIT0009" TO CA-REASON-CODE
             MOVE "An increased material requirement needs approval for the changed kit version."
                  TO CA-MESSAGE
         WHEN OTHER
             MOVE "N" TO KQ-NEEDS-AUTH
             MOVE 0 TO CA-RETURN-CODE
     END-EVALUATE
     PERFORM Describe-Impact.

 Describe-Impact.
     MOVE SPACES TO KQ-IMPACT-TEXT
     MOVE KQ-REQ-DELTA TO Ls-Edit
     MOVE KQ-COST-IMPACT TO Ls-Money-Edit
     STRING "Requirement " FUNCTION TRIM(Ls-Edit)
            ", cost impact GBP " FUNCTION TRIM(Ls-Money-Edit)
            ", net issued unchanged"
         DELIMITED BY SIZE INTO KQ-IMPACT-TEXT.

*>----------------------------------------------------------------
*> SCALE - KIT-016 and D-10.  Changing the planned output does not
*> change everything in the same way:
*>   PROPORTION  scales with the gross starts
*>   FIXEDRUN    occurs once per execution run, however many
*>               partial completions the run has
*>   FIXEDBATCH  follows the batch plan, so it steps
*> The loss conventions are applied once: yield first (gross
*> starts), then the additional-input scrap fraction.
 Scale-For-New-Output.
     IF KQ-NEW-OUTPUT < 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "The planned good output may not be negative." TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KQ-YIELD <= 0
         MOVE 1 TO KQ-YIELD
     END-IF
     PERFORM Compute-Gross-Starts
     IF KQ-NEW-OUTPUT = 0
*>       AC-087: zero output requires no runs, no batches and no
*>       fixed material at all.
         MOVE 0 TO KQ-NEW-REQ
         MOVE 0 TO KQ-GROSS-STARTS
         MOVE 0 TO KQ-RUN-COUNT
         MOVE 0 TO KQ-BATCH-COUNT
         MOVE "Zero planned output - no runs, batches or fixed usage."
              TO KQ-IMPACT-TEXT
         EXIT PARAGRAPH
     END-IF
     EVALUATE KQ-USAGE-BASIS
         WHEN "FIXEDRUN"
             PERFORM Scale-Fixed-Per-Run
         WHEN "FIXEDBATCH"
             PERFORM Scale-Fixed-Per-Batch
         WHEN OTHER
             PERFORM Scale-Proportional
     END-EVALUATE
     COMPUTE KQ-REQ-DELTA = KQ-NEW-REQ - KQ-CURRENT-REQ
     COMPUTE KQ-COST-IMPACT ROUNDED = KQ-REQ-DELTA * KQ-UNIT-COST
     PERFORM Describe-Impact.

 Compute-Gross-Starts.
     COMPUTE Ls-Starts ROUNDED MODE NEAREST-AWAY-FROM-ZERO
         = KQ-NEW-OUTPUT / KQ-YIELD
     IF KQ-QUANTUM > 0
*>       A quantum is a whole-unit constraint: round the starts up
*>       to the next whole quantum, never down (BOM-011).
         COMPUTE Ls-Quantum-Units = Ls-Starts / KQ-QUANTUM
         IF Ls-Quantum-Units > FUNCTION INTEGER (Ls-Quantum-Units)
             COMPUTE Ls-Starts =
                 (FUNCTION INTEGER (Ls-Quantum-Units) + 1) * KQ-QUANTUM
         ELSE
             COMPUTE Ls-Starts =
                 FUNCTION INTEGER (Ls-Quantum-Units) * KQ-QUANTUM
         END-IF
     END-IF
     MOVE Ls-Starts TO KQ-GROSS-STARTS
     IF KQ-RUN-COUNT < 1
         MOVE 1 TO KQ-RUN-COUNT
     END-IF
     IF KQ-BATCH-SIZE > 0
         COMPUTE Ls-Batches =
             FUNCTION INTEGER (Ls-Starts / KQ-BATCH-SIZE)
         IF Ls-Batches * KQ-BATCH-SIZE < Ls-Starts
             ADD 1 TO Ls-Batches
         END-IF
         MOVE Ls-Batches TO KQ-BATCH-COUNT
     ELSE
         MOVE 0 TO KQ-BATCH-COUNT
     END-IF.

 Scale-Proportional.
     COMPUTE Ls-Required ROUNDED
         = KQ-USAGE-PER * Ls-Starts * (1 + KQ-SCRAP-PCT / 100)
     MOVE Ls-Required TO KQ-NEW-REQ
     MOVE "Proportional usage rescaled with the gross starts."
          TO KQ-IMPACT-TEXT.

*>   Fixed per run material happens once for the run.  A second
*>   partial completion of the SAME run must not repeat it, which
*>   is why the run count - not the output - is the multiplier.
 Scale-Fixed-Per-Run.
     MOVE KQ-RUN-COUNT TO Ls-Runs
     COMPUTE Ls-Required ROUNDED = KQ-USAGE-PER * Ls-Runs
     MOVE Ls-Required TO KQ-NEW-REQ
     MOVE "Fixed per run usage is unchanged by the output quantity."
          TO KQ-IMPACT-TEXT.

 Scale-Fixed-Per-Batch.
     IF KQ-BATCH-COUNT < 1
         MOVE 1 TO KQ-BATCH-COUNT
     END-IF
     COMPUTE Ls-Required ROUNDED = KQ-USAGE-PER * KQ-BATCH-COUNT
     MOVE Ls-Required TO KQ-NEW-REQ
     MOVE "Fixed per batch usage follows the approved batch plan."
          TO KQ-IMPACT-TEXT.

*>----------------------------------------------------------------
*> ACCOUNT - specification 5, material accountability.  At
*> operational close every unit issued must be explained.  A zero
*> "remaining to issue" proves nothing about the material already
*> on the job (KIT-017).
 Material-Accountability.
     PERFORM Validate-Movement-Totals
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ls-Explained = KQ-RETURNED + KQ-SCRAPPED
                          + KQ-CONSUMED + KQ-ON-JOB
     COMPUTE KQ-ACCOUNT-DIFF = KQ-GROSS-ISSUED - Ls-Explained
     COMPUTE Ls-Net = KQ-GROSS-ISSUED - KQ-RETURNED
     MOVE Ls-Net TO KQ-NET-ISSUED
     IF FUNCTION ABS (KQ-ACCOUNT-DIFF) <= Ws-Tolerance
         MOVE 0 TO KQ-ACCOUNT-DIFF
         MOVE "Y" TO KQ-ACCOUNTED
         MOVE "All issued material is accounted for." TO KQ-IMPACT-TEXT
     ELSE
         MOVE "N" TO KQ-ACCOUNTED
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0017" TO CA-REASON-CODE
         MOVE KQ-ACCOUNT-DIFF TO Ls-Edit
         STRING "Unexplained material of " FUNCTION TRIM(Ls-Edit)
                " - return it or record an approved disposition."
             DELIMITED BY SIZE INTO CA-MESSAGE
         MOVE CA-MESSAGE (1:200) TO KQ-IMPACT-TEXT
     END-IF
     IF KQ-ON-JOB > Ws-Tolerance
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0017" TO CA-REASON-CODE
         MOVE KQ-ON-JOB TO Ls-Edit
         STRING "Unconsumed material of " FUNCTION TRIM(Ls-Edit)
                " is still held on the job."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.
