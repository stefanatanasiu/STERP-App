      *>================================================================
      *> POAPPRVCA - COMMAREA for POAPPRV, purchase order approval.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> PO-006.  Approval is for the EXACT value, supplier, terms and
      *> demand.  The approved content is reduced to a hash by APHASH
      *> over supplier, currency, net, VAT, terms, Incoterms, the
      *> linked demand and the line fingerprint; core.usp_RequestApproval
      *> invalidates any earlier approval whose hash differs, so a
      *> material change cannot inherit the old authority.
      *>
      *> CHECK is what POORDER ISSUE calls: it recomputes the hash from
      *> the live order and only answers APPROVED when an APPROVED
      *> core.Approval row exists for THAT hash.
      *>
      *> CA-FUNCTION values
      *>   HASH      compute and store the content hash only
      *>   REQUEST   request approval for the current content
      *>   DECIDE    record an approve/reject decision
      *>   CHECK     is the current content approved
      *>   INVALID   invalidate approvals after a material change
      *>
      *> CA-RETURN-CODE 00 approved, 04 pending, 06 approval required,
      *>   08 rejected, 12 severe, 16 order not found
      *>
      *> REASON CODES
      *>   APV0001 company or order number missing
      *>   APV0002 order not found
      *>   APV0003 the order has no lines to approve
      *>   APV0004 the order status does not allow approval
      *>   APV0005 approval is pending a decision
      *>   APV0006 the approved content has changed - reapproval needed
      *>   APV0007 the approver may not approve their own request
      *>   APV0008 unsupported function
      *>   APV0009 no approval rule covers this value
      *>   APV0010 a decision needs an approver and a decision code
      *>   APV0011 the order has not been approved
      *>----------------------------------------------------------------
      *> 1998-07-02 RJH  CR0820 ORDER AUTHORISATION
      *> 2026-10-06 STERP CONTENT HASH AND REAPPROVAL
      *>================================================================
      *> REQ: PO-006, PO-007, D-07
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  AV-BODY.
               10  AV-ORDER-NUMBER     PIC X(30).
               10  AV-APPROVAL-TYPE    PIC X(30).
               10  AV-APPROVER         PIC X(20).
               10  AV-DECISION         PIC X(10).
                   88  AV-DEC-APPROVE            VALUE "APPROVE".
                   88  AV-DEC-REJECT             VALUE "REJECT".
               10  AV-COMMENT          PIC X(400).
               10  AV-CHANGE-REASON    PIC X(300).
      *>     ---- answers ----
               10  AV-ORDER-ID         PIC S9(17).
               10  AV-APPROVAL-ID      PIC S9(17).
               10  AV-CONTENT-HASH     PIC X(64).
               10  AV-STORED-HASH      PIC X(64).
               10  AV-SUPPLIER         PIC X(20).
               10  AV-CURRENCY         PIC X(3).
               10  AV-TERMS-CODE       PIC X(12).
               10  AV-NET-AMOUNT       PIC S9(13)V99.
               10  AV-GROSS-AMOUNT     PIC S9(13)V99.
               10  AV-AMOUNT-GBP       PIC S9(13)V99.
               10  AV-LINE-CNT         PIC 9(4).
               10  AV-ORDER-STATUS     PIC X(14).
               10  AV-APPROVAL-STATUS  PIC X(14).
               10  AV-REQUIRED-PERM    PIC X(40).
               10  AV-REQUIRED-ROLE    PIC X(20).
               10  AV-REQUESTED-BY     PIC X(20).
               10  AV-WAS-INVALIDATED  PIC X.
                   88  AV-INVALIDATED            VALUE "Y".
               10  AV-OUTCOME          PIC X(12).
                   88  AV-APPROVED               VALUE "APPROVED".
                   88  AV-PENDING                VALUE "PENDING".
                   88  AV-REJECTED               VALUE "REJECTED".
                   88  AV-NOT-APPROVED           VALUE "NOTAPPROVED".
                   88  AV-NEEDS-REAPPROVAL       VALUE "REAPPROVE".
