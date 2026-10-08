      *>================================================================
      *> POREQCA  - COMMAREA for POREQ, the purchase requisition
      *>            service.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> A requisition is a justified NEED, never a supplier
      *> commitment (PO-001).  Nothing in this program writes to
      *> po.Commitment or po.PurchaseOrder; only POORDER does that,
      *> and only after POAPPRV.
      *>
      *> Demand ownership is carried on every requisition: who asked
      *> (RQ-REQUESTER), why (RQ-PURPOSE) and what demand it answers
      *> (RQ-DEMAND-TYPE / RQ-DEMAND-REF).  A replenishment or
      *> shortage requisition therefore still names the sales order,
      *> kit or min/max rule that created it, so the chain from
      *> customer promise to supplier order is never broken.
      *>
      *> CA-FUNCTION values
      *>   CREATE    open a requisition header (draft)
      *>   ADDLINE   add or replace a line on a draft requisition
      *>   SUBMIT    budget check (BUDGCHK) then submit for approval
      *>   APPROVE   record the approval decision
      *>   CANCEL    cancel the requisition or one line with a reason
      *>   CONVERT   mark a line converted to an order line
      *>   ENQUIRE   read the header, totals and open value
      *>
      *> CA-RETURN-CODE 00 ok, 04 warning, 06 hold (budget or
      *>   approval authority), 08 rejected, 12 severe, 16 not found
      *>
      *> REASON CODES
      *>   REQ0001 company, site or requester missing
      *>   REQ0002 requisition not found
      *>   REQ0003 requisition is not in a state that allows this
      *>   REQ0004 line description, quantity or value invalid
      *>   REQ0005 a non stock line needs an account code
      *>   REQ0006 needed-by date is before the requisition date
      *>   REQ0007 budget check blocked the submission
      *>   REQ0008 unsupported function
      *>   REQ0009 demand type is not recognised
      *>   REQ0010 number series failed
      *>   REQ0011 conversion exceeds the requisitioned quantity
      *>   REQ0012 cancellation needs a reason
      *>   REQ0013 the requester may not approve their own requisition
      *>           (COM-003 separation of duties)
      *>----------------------------------------------------------------
      *> 1998-04-21 RJH  CR0791 REQUISITION CONTROL
      *> 2005-11-30 DJM  CR1288 DEMAND OWNERSHIP ON SHORTAGE REQS
      *> 2026-10-06 STERP BUDGET CALL, CONVERSION TRACKING
      *>================================================================
      *> REQ: PO-001, PO-005
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  RQ-BODY.
               10  RQ-REQ-NUMBER       PIC X(30).
               10  RQ-REQUESTER        PIC X(20).
               10  RQ-PURPOSE          PIC X(200).
               10  RQ-REQ-DATE         PIC 9(8).
               10  RQ-NEEDED-BY        PIC 9(8).
               10  RQ-COST-CENTRE      PIC X(20).
               10  RQ-PROJECT          PIC X(20).
               10  RQ-JOB-REF          PIC X(20).
               10  RQ-BUDGET-CODE      PIC X(20).
               10  RQ-DEMAND-TYPE      PIC X(12).
                   88  RQ-DEM-MANUAL             VALUE "MANUAL".
                   88  RQ-DEM-SHORTAGE           VALUE "SHORTAGE".
                   88  RQ-DEM-REPLEN             VALUE "REPLEN".
                   88  RQ-DEM-MRP                VALUE "MRP".
                   88  RQ-DEM-SALESORD           VALUE "SALESORD".
                   88  RQ-DEM-KIT                VALUE "KIT".
                   88  RQ-DEM-KNOWN              VALUE "MANUAL",
                       "SHORTAGE", "REPLEN", "MRP", "SALESORD", "KIT".
               10  RQ-DEMAND-REF       PIC X(40).
      *>     ---- line fields ----
               10  RQ-LINE-NUM         PIC S9(9) COMP-5.
               10  RQ-ITEM-CODE        PIC X(40).
               10  RQ-DESCRIPTION      PIC X(200).
               10  RQ-QUANTITY         PIC S9(13)V9(6).
               10  RQ-UOM-CODE         PIC X(10).
               10  RQ-EST-UNIT-PRICE   PIC S9(11)V9(6).
               10  RQ-SUGGEST-SUPP     PIC X(20).
               10  RQ-ACCOUNT-CODE     PIC X(12).
               10  RQ-LINE-NEEDED-BY   PIC 9(8).
               10  RQ-CONVERT-QTY      PIC S9(13)V9(6).
               10  RQ-ORDER-NUMBER     PIC X(30).
               10  RQ-ORDER-LINE       PIC S9(9) COMP-5.
      *>     ---- decisions and overrides ----
               10  RQ-DECISION         PIC X(10).
               10  RQ-APPROVER         PIC X(20).
               10  RQ-REASON           PIC X(200).
               10  RQ-OVERRIDE-BY      PIC X(20).
               10  RQ-OVERRIDE-REASON  PIC X(200).
      *>     ---- answers ----
               10  RQ-REQ-ID           PIC S9(17).
               10  RQ-LINE-ID          PIC S9(17).
               10  RQ-LINE-VALUE-GBP   PIC S9(13)V99.
               10  RQ-TOTAL-GBP        PIC S9(13)V99.
               10  RQ-OPEN-GBP         PIC S9(13)V99.
               10  RQ-LINE-CNT         PIC 9(4).
               10  RQ-STATUS           PIC X(12).
               10  RQ-BUDGET-OUTCOME   PIC X(10).
               10  RQ-BUDGET-USED-PCT  PIC S9(7)V9(4).
               10  RQ-BUDGET-AVAIL-GBP PIC S9(13)V99.
               10  RQ-APPROVAL-ID      PIC S9(17).
               10  RQ-OUTCOME          PIC X(12).
                   88  RQ-DRAFTED                VALUE "DRAFT".
                   88  RQ-SUBMITTED              VALUE "SUBMITTED".
                   88  RQ-APPROVED               VALUE "APPROVED".
                   88  RQ-REJECTED               VALUE "REJECTED".
                   88  RQ-CANCELLED              VALUE "CANCELLED".
                   88  RQ-CONVERTED              VALUE "CONVERTED".
