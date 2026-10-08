>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : BOMSELCT
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : Choose THE bill that applies to a company, site,
*>              purpose, variant and stated effective date.
*>              Selection is deterministic.  Effectivity is half
*>              open [start, end): a revision is active on its
*>              start date and inactive on its end date, so two
*>              adjacent revisions may change at a boundary
*>              without both being effective (D-10).
*>
*>              Nothing eligible is reported as not found.  More
*>              than one eligible release for the same context is
*>              an error with every candidate listed - it is NEVER
*>              resolved by taking the latest record, because that
*>              is exactly how an unapproved or superseded bill
*>              gets used in production.
*>
*>              A job or order bill is reached only by naming it.
*>              A standard selection never wanders into job bills
*>              looking for a convenient match.
*> CALLED BY  : BOMEXPL, BOMVALID, BOMNET, BOMCOST, BOMJOB, the
*>              batch reports, the .NET client
*> INTERFACE  : CALL "BOMSELCT" USING WS-BOMSELCT-CA
*>              COPYBOOK BOMSELCTCA
*> CALLS      : SQLIO
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2014-04-02 AJS  CR2203 ENGINEERING REWRITE
*> 2015-01-19 AJS  CR2287 HALF OPEN EFFECTIVITY.  THE OLD RULE
*>                 (end INCLUSIVE) MADE BOTH REVISIONS EFFECTIVE
*>                 ON THE CHANGEOVER DAY - SEE INCIDENT INC-8841.
*> 2017-08-30 KMP  CR2612 CANDIDATE LIST RETURNED FOR THE ENQUIRY
*> 2026-10-05 STERP JOB BILLS BY EXPLICIT REFERENCE ONLY
*>================================================================
*> REQ: BOM-003, BOM-004, BOM-005, BOM-016, BOM-029
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BOMSELCT.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 02/04/2014.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 01  Ws-Flags.
     05  Ws-Rejected             PIC X VALUE "N".
     05  Ws-More                 PIC X VALUE "Y".
     05  Ws-Eligible-Cnt         PIC S9(4) VALUE 0.
     05  Ws-Chosen               PIC S9(4) VALUE 0.
     05  Ws-Want-Hit             PIC S9(4) VALUE 0.
     05  Ws-I                    PIC S9(4) VALUE 0.
 01  Ws-Sel-Row.
     05  Sr-Bom-Id               PIC X(12).
     05  Sr-Bom-Ref              PIC X(30).
     05  Sr-Bom-Rev              PIC X(4).
     05  Sr-Status               PIC X(12).
     05  Sr-Site                 PIC X(10).
     05  Sr-Variant              PIC X(20).
     05  Sr-Purpose              PIC X(10).
     05  Sr-Parent-Rev           PIC X(4).
     05  Sr-Job-Ref              PIC X(20).
     05  Sr-Eff-From             PIC X(8).
     05  Sr-Eff-To               PIC X(8).
     05  Sr-Basis                PIC X(24).
     05  Sr-Yield                PIC X(24).
     05  Sr-Quantum              PIC X(24).
     05  Sr-Batch                PIC X(24).
     05  Sr-Phantom              PIC X(1).
     05  Sr-Consolidate          PIC X(1).
     05  Sr-Comp-Cnt             PIC X(8).
 01  Ws-Cand.
     05  Ws-C-From               PIC 9(8) VALUE 0.
     05  Ws-C-To                 PIC 9(8) VALUE 0.
     05  Ws-C-Reason             PIC X(40) VALUE SPACES.
     05  Ws-C-Ok                 PIC X VALUE "N".
 LINKAGE SECTION.
 COPY BOMSELCTCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     PERFORM Initialise-Call
     IF Ws-Rejected = "Y"
         GOBACK
     END-IF
     EVALUATE CA-FUNCTION
         WHEN "SELECT"  PERFORM Load-Candidates
                        IF Ws-Rejected = "N"
                            PERFORM Choose-One
                        END-IF
         WHEN "PIN"     PERFORM Load-Candidates
                        IF Ws-Rejected = "N"
                            PERFORM Choose-Pinned
                        END-IF
         WHEN "LIST"    PERFORM Load-Candidates
         WHEN OTHER     PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Initialise-Call.
     MOVE "N" TO Ws-Rejected
     MOVE "BOMSELCT" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
     MOVE "BOM-005" TO CA-REQ-REF
     MOVE 0 TO BS-BOM-ID BS-CAND-CNT
     MOVE 0 TO BS-EFF-FROM BS-EFF-TO BS-COMPONENT-CNT
     MOVE 0 TO BS-NOMINAL-BASIS BS-YIELD BS-QUANTUM BS-BATCH-SIZE
     MOVE SPACES TO BS-BOM-REF BS-BOM-REV BS-BOM-STATUS BS-BOM-SITE
     MOVE "N" TO BS-IS-PHANTOM
     MOVE "N" TO BS-CONSOLIDATE
     MOVE 20 TO BS-CAND-MAX
     MOVE 0 TO Ws-Eligible-Cnt Ws-Chosen Ws-Want-Hit
     MOVE FUNCTION UPPER-CASE(BS-PARENT-ITEM) TO BS-PARENT-ITEM
     IF BS-PURPOSE = SPACES
         MOVE "STANDARD" TO BS-PURPOSE
     END-IF
     IF BS-EFF-DATE = 0
         MOVE CA-BUSINESS-DATE TO BS-EFF-DATE
     END-IF
     IF BS-PARENT-ITEM = SPACES
         MOVE "BOM0201" TO CA-REASON-CODE
         MOVE "A parent item is required to select a bill"
             TO CA-MESSAGE
         PERFORM Reject-Input
         EXIT PARAGRAPH
     END-IF
     IF FUNCTION TEST-DATE-YYYYMMDD(BS-EFF-DATE) NOT = 0
         MOVE "BOM0202" TO CA-REASON-CODE
         MOVE "The effective date is not a valid calendar date"
             TO CA-MESSAGE
         PERFORM Reject-Input
         EXIT PARAGRAPH
     END-IF
     IF BS-PURPOSE NOT = "STANDARD" AND BS-JOB-REF = SPACES
         MOVE "BOM0208" TO CA-REASON-CODE
         MOVE SPACES TO CA-MESSAGE
         STRING "A " FUNCTION TRIM(BS-PURPOSE)
                " bill must be named explicitly by its job or "
                "order reference"
             DELIMITED BY SIZE INTO CA-MESSAGE
         PERFORM Reject-Input
     END-IF.

 Reject-Input.
     MOVE 8 TO CA-RETURN-CODE
     ADD 1 TO CA-MSG-COUNT
     MOVE "Y" TO Ws-Rejected.

 Reject-Function.
     MOVE "BOM0210" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "BOMSELCT does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE
     MOVE 8 TO CA-RETURN-CODE
     MOVE "Y" TO Ws-Rejected.

*>----------------------------------------------------------------
*> Every bill recorded for this parent and purpose, in revision
*> order.  Eligibility is decided here rather than in the WHERE
*> clause so that the enquiry can explain each rejection.
 Load-Candidates.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT h.BomHeaderId, h.BomRef, h.BomRevision, "
            "h.Status, ISNULL(h.SiteCode,''), "
            "ISNULL(h.VariantCode,''), h.Purpose, "
            "ISNULL(h.ParentRevision,''), ISNULL(h.JobRef,''), "
            "CONVERT(char(8),h.EffectiveFrom,112), "
            "ISNULL(CONVERT(char(8),h.EffectiveTo,112),'00000000'),"
            " h.NominalBasis, h.YieldFactor, h.ProductionQuantum, "
            "ISNULL(h.BatchSize,0), "
            "CASE WHEN h.IsPhantom=1 THEN 'Y' ELSE 'N' END, "
            "CASE WHEN h.ConsolidateRuns=1 THEN 'Y' ELSE 'N' END, "
            "(SELECT COUNT(*) FROM bom.BomComponent c "
            "WHERE c.BomHeaderId = h.BomHeaderId) "
            "FROM bom.BomHeader h "
            "WHERE h.CompanyCode = ? AND h.ParentItemCode = ? "
            "AND h.Purpose = ? "
            "ORDER BY h.BomRevision, h.BomHeaderId"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 3 TO SQLIO-PARM-CNT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE BS-PARENT-ITEM TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE BS-PURPOSE TO SQLIO-PARM-VAL(3)
     MOVE "N" TO SQLIO-PARM-NULL(3)
     PERFORM Set-Widths
     MOVE 5 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Sel-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE 5 TO SQLIO-CURSOR
         MOVE "FTCH" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Sel-Row
         EVALUATE TRUE
             WHEN SQLIO-ERROR
                 PERFORM Report-Sql-Failure
                 MOVE "N" TO Ws-More
             WHEN SQLIO-NOTFOUND
                 MOVE "N" TO Ws-More
             WHEN OTHER
                 PERFORM Examine-Candidate
         END-EVALUATE
     END-PERFORM
     MOVE 5 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Sel-Row.

 Set-Widths.
     MOVE 18 TO SQLIO-COL-CNT
     MOVE 12 TO SQLIO-COL-WIDTH(1)
     MOVE 30 TO SQLIO-COL-WIDTH(2)
     MOVE 4 TO SQLIO-COL-WIDTH(3)
     MOVE 12 TO SQLIO-COL-WIDTH(4)
     MOVE 10 TO SQLIO-COL-WIDTH(5)
     MOVE 20 TO SQLIO-COL-WIDTH(6)
     MOVE 10 TO SQLIO-COL-WIDTH(7)
     MOVE 4 TO SQLIO-COL-WIDTH(8)
     MOVE 20 TO SQLIO-COL-WIDTH(9)
     MOVE 8 TO SQLIO-COL-WIDTH(10)
     MOVE 8 TO SQLIO-COL-WIDTH(11)
     MOVE 24 TO SQLIO-COL-WIDTH(12)
     MOVE 24 TO SQLIO-COL-WIDTH(13)
     MOVE 24 TO SQLIO-COL-WIDTH(14)
     MOVE 24 TO SQLIO-COL-WIDTH(15)
     MOVE 1 TO SQLIO-COL-WIDTH(16)
     MOVE 1 TO SQLIO-COL-WIDTH(17)
     MOVE 8 TO SQLIO-COL-WIDTH(18).

*>----------------------------------------------------------------
*> One candidate.  The first failing test wins the reason, so the
*> enquiry shows the most specific thing wrong with the row.
 Examine-Candidate.
     IF BS-CAND-CNT >= BS-CAND-MAX
         MOVE 4 TO CA-RETURN-CODE
         MOVE "BOM0211" TO CA-REASON-CODE
         MOVE "More candidate bills exist than can be listed"
             TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-C-Ok
     MOVE SPACES TO Ws-C-Reason
     IF Sr-Eff-From IS NUMERIC
         MOVE Sr-Eff-From TO Ws-C-From
     ELSE
         MOVE 0 TO Ws-C-From
     END-IF
     IF Sr-Eff-To IS NUMERIC
         MOVE Sr-Eff-To TO Ws-C-To
     ELSE
         MOVE 0 TO Ws-C-To
     END-IF
     EVALUATE TRUE
         WHEN Sr-Status = "RETIRED"
             MOVE "N" TO Ws-C-Ok
             MOVE "Retired" TO Ws-C-Reason
         WHEN Sr-Status = "SUPERSEDED"
             MOVE "N" TO Ws-C-Ok
             MOVE "Superseded by a later revision" TO Ws-C-Reason
         WHEN Sr-Status NOT = "RELEASED" AND BS-ALLOW-DRAFT NOT = "Y"
             MOVE "N" TO Ws-C-Ok
             MOVE "Not released - review not complete"
                 TO Ws-C-Reason
         WHEN Ws-C-From > BS-EFF-DATE
             MOVE "N" TO Ws-C-Ok
             MOVE "Not yet effective on the stated date"
                 TO Ws-C-Reason
         WHEN Ws-C-To NOT = 0 AND Ws-C-To <= BS-EFF-DATE
             MOVE "N" TO Ws-C-Ok
             MOVE "Effectivity ended before the stated date"
                 TO Ws-C-Reason
         WHEN Sr-Site NOT = SPACES AND CA-SITE NOT = SPACES
              AND Sr-Site NOT = CA-SITE
             MOVE "N" TO Ws-C-Ok
             MOVE "Restricted to another site" TO Ws-C-Reason
         WHEN Sr-Variant NOT = SPACES AND Sr-Variant NOT = BS-VARIANT
             MOVE "N" TO Ws-C-Ok
             MOVE "Belongs to another product variant"
                 TO Ws-C-Reason
         WHEN Sr-Parent-Rev NOT = SPACES AND BS-PARENT-REV NOT = SPACES
              AND Sr-Parent-Rev NOT = BS-PARENT-REV
             MOVE "N" TO Ws-C-Ok
             MOVE "Written for another parent item revision"
                 TO Ws-C-Reason
         WHEN BS-JOB-REF NOT = SPACES AND Sr-Job-Ref NOT = BS-JOB-REF
             MOVE "N" TO Ws-C-Ok
             MOVE "Belongs to another job or order" TO Ws-C-Reason
         WHEN OTHER
             MOVE "Applicable" TO Ws-C-Reason
     END-EVALUATE
     ADD 1 TO BS-CAND-CNT
     MOVE BS-CAND-CNT TO Ws-I
     COMPUTE BS-C-BOM-ID(Ws-I) = FUNCTION NUMVAL(Sr-Bom-Id)
     MOVE Sr-Bom-Ref TO BS-C-BOM-REF(Ws-I)
     MOVE Sr-Bom-Rev TO BS-C-BOM-REV(Ws-I)
     MOVE Sr-Status TO BS-C-STATUS(Ws-I)
     MOVE Sr-Site TO BS-C-SITE(Ws-I)
     MOVE Sr-Variant TO BS-C-VARIANT(Ws-I)
     MOVE Sr-Purpose TO BS-C-PURPOSE(Ws-I)
     MOVE Ws-C-From TO BS-C-EFF-FROM(Ws-I)
     MOVE Ws-C-To TO BS-C-EFF-TO(Ws-I)
     MOVE Ws-C-Ok TO BS-C-ELIGIBLE(Ws-I)
     MOVE Ws-C-Reason TO BS-C-REASON(Ws-I)
     IF Ws-C-Ok = "Y"
         ADD 1 TO Ws-Eligible-Cnt
         MOVE Ws-I TO Ws-Chosen
     END-IF
     IF BS-WANT-REV NOT = SPACES AND Sr-Bom-Rev = BS-WANT-REV
         MOVE Ws-I TO Ws-Want-Hit
         PERFORM Keep-Selected-Values
     END-IF
     IF BS-WANT-REV = SPACES AND Ws-C-Ok = "Y"
         PERFORM Keep-Selected-Values
     END-IF.

*> The header values travel with the chosen row so that the caller
*> does not have to read the bill again just to learn its basis.
 Keep-Selected-Values.
     COMPUTE BS-NOMINAL-BASIS = FUNCTION NUMVAL(Sr-Basis)
     COMPUTE BS-YIELD = FUNCTION NUMVAL(Sr-Yield)
     COMPUTE BS-QUANTUM = FUNCTION NUMVAL(Sr-Quantum)
     COMPUTE BS-BATCH-SIZE = FUNCTION NUMVAL(Sr-Batch)
     MOVE Sr-Phantom TO BS-IS-PHANTOM
     MOVE Sr-Consolidate TO BS-CONSOLIDATE
     COMPUTE BS-COMPONENT-CNT = FUNCTION NUMVAL(Sr-Comp-Cnt).

*>----------------------------------------------------------------
 Choose-One.
     EVALUATE TRUE
         WHEN Ws-Eligible-Cnt = 1
             PERFORM Publish-Chosen
         WHEN Ws-Eligible-Cnt = 0
             MOVE 16 TO CA-RETURN-CODE
             MOVE "BOM0203" TO CA-REASON-CODE
             MOVE SPACES TO CA-MESSAGE
             STRING "No applicable released bill for "
                    FUNCTION TRIM(BS-PARENT-ITEM)
                    " on the stated effective date"
                 DELIMITED BY SIZE INTO CA-MESSAGE
         WHEN OTHER
             MOVE 8 TO CA-RETURN-CODE
             MOVE "BOM0204" TO CA-REASON-CODE
             MOVE SPACES TO CA-MESSAGE
             STRING "Overlapping released bills for "
                    FUNCTION TRIM(BS-PARENT-ITEM)
                    " - the context must identify exactly one"
                 DELIMITED BY SIZE INTO CA-MESSAGE
             MOVE 0 TO BS-BOM-ID
     END-EVALUATE.

 Choose-Pinned.
     IF Ws-Want-Hit = 0
         MOVE 16 TO CA-RETURN-CODE
         MOVE "BOM0205" TO CA-REASON-CODE
         MOVE SPACES TO CA-MESSAGE
         STRING "Revision " FUNCTION TRIM(BS-WANT-REV)
                " does not exist for " FUNCTION TRIM(BS-PARENT-ITEM)
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE Ws-Want-Hit TO Ws-Chosen
     IF BS-C-ELIGIBLE(Ws-Want-Hit) = "Y"
         PERFORM Publish-Chosen
         EXIT PARAGRAPH
     END-IF
     MOVE 8 TO CA-RETURN-CODE
     MOVE "BOM0206" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "Revision " FUNCTION TRIM(BS-WANT-REV) " cannot be used: "
            FUNCTION TRIM(BS-C-REASON(Ws-Want-Hit))
         DELIMITED BY SIZE INTO CA-MESSAGE
     MOVE BS-C-BOM-ID(Ws-Want-Hit) TO BS-BOM-ID
     MOVE BS-C-BOM-REV(Ws-Want-Hit) TO BS-BOM-REV
     MOVE BS-C-STATUS(Ws-Want-Hit) TO BS-BOM-STATUS.

 Publish-Chosen.
     MOVE BS-C-BOM-ID(Ws-Chosen) TO BS-BOM-ID
     MOVE BS-C-BOM-REF(Ws-Chosen) TO BS-BOM-REF
     MOVE BS-C-BOM-REV(Ws-Chosen) TO BS-BOM-REV
     MOVE BS-C-STATUS(Ws-Chosen) TO BS-BOM-STATUS
     MOVE BS-C-SITE(Ws-Chosen) TO BS-BOM-SITE
     MOVE BS-C-EFF-FROM(Ws-Chosen) TO BS-EFF-FROM
     MOVE BS-C-EFF-TO(Ws-Chosen) TO BS-EFF-TO
     MOVE 0 TO CA-RETURN-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "Bill " FUNCTION TRIM(BS-BOM-REF) " revision "
            FUNCTION TRIM(BS-BOM-REV) " selected"
         DELIMITED BY SIZE INTO CA-MESSAGE
     IF BS-BOM-STATUS NOT = "RELEASED"
         MOVE 4 TO CA-RETURN-CODE
         MOVE "BOM0209" TO CA-REASON-CODE
         MOVE SPACES TO CA-MESSAGE
         STRING "Bill " FUNCTION TRIM(BS-BOM-REF) " revision "
                FUNCTION TRIM(BS-BOM-REV)
                " is a draft and cannot produce an approved result"
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.

 Report-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE SQLIO-ERRMSG TO CA-MESSAGE
     MOVE "Y" TO Ws-Rejected.
