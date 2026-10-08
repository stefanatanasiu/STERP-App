      *>================================================================
      *> BOMJOBCA - COMMAREA of BOMJOB, job/order bill derivation.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : BILLS OF MATERIAL
      *> PURPOSE  : BOM-003/BOM-022.  Derive a job or order specific
      *>            bill from a released standard with complete
      *>            lineage.  The copy is always a DRAFT, excludes
      *>            execution history, stock allocations and prior
      *>            approvals, and editing it never mutates the
      *>            standard or another job's bill.  A repeated
      *>            confirmed copy under the same correlation id
      *>            returns the first bill rather than a duplicate.
      *> FUNCTIONS :
      *>   DERIVE   copy a source bill to a new job/order draft
      *>   LINEAGE  read the lineage of BJ-TARGET-BOM-ID
      *>   NEWREV   create the next draft revision of the same bill
      *> Reason codes BOM1201..BOM1212.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2015-11-17 AJS  CR2384 JOB SPECIFIC BILLS ADDED
      *> 2026-10-05 STERP LINEAGE TABLE AND EXCLUSION NOTE
      *>================================================================
      *> REQ: BOM-003, BOM-022, BOM-030
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BJ-BODY.
               10  BJ-SOURCE-BOM-ID    PIC S9(9) COMP-5.
               10  BJ-SOURCE-REF       PIC X(30).
               10  BJ-SOURCE-REV       PIC X(4).
               10  BJ-PARENT-ITEM      PIC X(40).
               10  BJ-EFF-DATE         PIC 9(8).
               10  BJ-TARGET-REF       PIC X(30).
               10  BJ-TARGET-REV       PIC X(4).
               10  BJ-TARGET-BOM-ID    PIC S9(9) COMP-5.
               10  BJ-PURPOSE          PIC X(10).
               10  BJ-JOB-REF          PIC X(20).
               10  BJ-DEMAND-TYPE      PIC X(12).
               10  BJ-DEMAND-REF       PIC X(40).
               10  BJ-SITE             PIC X(10).
               10  BJ-COPY-OPTIONAL    PIC X.
               10  BJ-COPY-BYPRODUCT   PIC X.
               10  BJ-COPY-NOTES       PIC X.
               10  BJ-LINES-COPIED     PIC S9(4).
               10  BJ-LINES-EXCLUDED   PIC S9(4).
               10  BJ-EXCLUSION-NOTE   PIC X(200).
               10  BJ-LINEAGE-ID       PIC S9(9) COMP-5.
               10  BJ-DUPLICATE        PIC X.
               10  BJ-STATUS           PIC X(12).
               10  BJ-LINEAGE-CNT      PIC S9(4).
               10  BJ-LINEAGE OCCURS 10 TIMES.
                   15  BJ-G-SOURCE-REF PIC X(30).
                   15  BJ-G-SOURCE-REV PIC X(4).
                   15  BJ-G-TYPE       PIC X(12).
                   15  BJ-G-BY         PIC X(20).
                   15  BJ-G-AT         PIC X(20).
                   15  BJ-G-DEMAND     PIC X(40).
