      *>================================================================
      *> BUDGCHKCA - COMMAREA for BUDGCHK, the purchasing budget and
      *>             authority check.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> D-08: the check is commitment plus actual against the annual
      *> cost centre budget.  The system WARNS at 90% and BLOCKS at
      *> 100% unless the financial controller approves.  Recoverable
      *> VAT is excluded from the committed cost; non recoverable VAT
      *> and known charges are included (PO-005, section 7).
      *>
      *> Related requests by the same requester, cost centre and item
      *> inside the splitting window are aggregated so that splitting
      *> a purchase cannot walk under an approval threshold.
      *>
      *> CA-FUNCTION values
      *>   CHECK     test a proposed value, no side effect
      *>   COMMIT    record a COMMITTED commitment movement
      *>   RELEASE   record a RELEASED movement (receipt/cancellation)
      *>   OVERRIDE  record an authorised override with its reason
      *>
      *> CA-RETURN-CODE 00 within budget, 04 warning (>= warn point),
      *>                06 hold - needs financial controller approval,
      *>                08 rejected, 16 no budget line found
      *>
      *> REASON CODES
      *>   BUD0001 budget code or cost centre missing
      *>   BUD0002 no approved budget for the year
      *>   BUD0003 budget warning point reached
      *>   BUD0004 budget would be exhausted - approval required
      *>   BUD0005 override needs an approver and a reason
      *>   BUD0006 unsupported function
      *>   BUD0007 related requests aggregate above the threshold
      *>   BUD0008 nothing to measure the commitment against - it is
      *>           recorded, but no approved budget covers it
      *>----------------------------------------------------------------
      *> 1998-05-06 RJH  CR0802 COMMITMENT ACCOUNTING
      *> 2007-04-02 DJM  CR1422 SPLIT PURCHASE DETECTION
      *> 2026-10-06 STERP D-08 WARN 90 BLOCK 100
      *>================================================================
      *> REQ: PO-005, PO-017, D-08, D-07
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BC-BODY.
               10  BC-BUDGET-CODE      PIC X(20).
               10  BC-FISCAL-YEAR      PIC 9(4).
               10  BC-ACCOUNT-CODE     PIC X(12).
               10  BC-COST-CENTRE      PIC X(20).
               10  BC-PROJECT          PIC X(20).
               10  BC-DOC-TYPE         PIC X(20).
               10  BC-DOC-REF          PIC X(40).
               10  BC-DOC-LINE         PIC S9(9) COMP-5.
               10  BC-SCHEDULE-NO      PIC S9(9) COMP-5.
               10  BC-REQUESTER        PIC X(20).
               10  BC-ITEM-CODE        PIC X(40).
               10  BC-MOVEMENT-DATE    PIC 9(8).
               10  BC-NET-GBP          PIC S9(13)V99.
               10  BC-NONREC-VAT-GBP   PIC S9(13)V99.
               10  BC-CHARGE-GBP       PIC S9(13)V99.
               10  BC-QUANTITY         PIC S9(13)V9(6).
               10  BC-OVERRIDE-BY      PIC X(20).
               10  BC-OVERRIDE-REASON  PIC X(200).
      *>     ---- answers ----
               10  BC-PROPOSED-GBP     PIC S9(13)V99.
               10  BC-BUDGET-GBP       PIC S9(13)V99.
               10  BC-ACTUAL-GBP       PIC S9(13)V99.
               10  BC-COMMITTED-GBP    PIC S9(13)V99.
               10  BC-AVAILABLE-GBP    PIC S9(13)V99.
               10  BC-USED-PCT         PIC S9(7)V9(4).
               10  BC-USED-AFTER-PCT   PIC S9(7)V9(4).
               10  BC-WARN-PCT         PIC S9(5)V9(4).
               10  BC-BLOCK-PCT        PIC S9(5)V9(4).
               10  BC-RELATED-CNT      PIC 9(4).
               10  BC-RELATED-GBP      PIC S9(13)V99.
               10  BC-COMMITMENT-ID    PIC S9(17).
               10  BC-OUTCOME          PIC X(10).
                   88  BC-WITHIN                 VALUE "WITHIN".
                   88  BC-WARNED                 VALUE "WARN".
                   88  BC-BLOCKED                VALUE "BLOCK".
                   88  BC-OVERRIDDEN             VALUE "OVERRIDE".
                   88  BC-UNBUDGETED             VALUE "NOBUDGET".
