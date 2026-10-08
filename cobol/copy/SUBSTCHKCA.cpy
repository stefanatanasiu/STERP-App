      *>================================================================
      *> SUBSTCHKCA - COMMAREA for SUBSTCHK, substitution and
      *>              equivalent revision rules.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE
      *>
      *> A substitute is a proposal, never a silent replacement.  The
      *> rule must state the context it applies to, the approved
      *> quantity ratio, its validity dates and whether the customer or
      *> the engineering authority has to agree (ITM-019).
      *>
      *> CA-FUNCTION values
      *>   LIST      the approved substitutes for SB-ITEM-CODE in
      *>             context SB-CONTEXT at SB-AS-AT
      *>   CHECK     may SB-SUBSTITUTE replace SB-ITEM-CODE in that
      *>             context?  Returns the ratio and the substitute
      *>             quantity for SB-QTY-REQUIRED.
      *>   REVCHECK  is SB-SUBSTITUTE-REV an interchangeable revision
      *>             of the same item?
      *>   APPLY     record an accepted substitution as audit evidence
      *>             against the document in SB-DOCUMENT-REF.  The
      *>             standard bill is not rewritten.
      *>
      *> SB-CONTEXT : PURCHASE SALES KIT BOM
      *>   substitute quantity = required quantity x conversion ratio
      *>
      *> REASON CODES
      *>   SUB0001 no substitute rule for that pair
      *>   SUB0002 rule does not cover this context
      *>   SUB0003 rule is not valid at that date
      *>   SUB0004 substitute item is not usable (state or class)
      *>   SUB0005 customer agreement is required first
      *>   SUB0006 rule is not approved
      *>   SUB0007 revision is not interchangeable
      *>   SUB0008 unsupported function
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-07-21 DJM  CR0721 APPROVED ALTERNATIVES
      *> 2005-11-30 KMP  CR1320 CUSTOMER AGREEMENT FLAG
      *> 2026-10-05 STERP ITM-019 CONTEXT AND RATIO
      *>================================================================
      *> REQ: ITM-019
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SB-BODY.
               10  SB-ITEM-CODE        PIC X(40).
               10  SB-REVISION         PIC X(4).
               10  SB-SUBSTITUTE       PIC X(40).
               10  SB-SUBSTITUTE-REV   PIC X(4).
               10  SB-CONTEXT          PIC X(10).
               10  SB-AS-AT            PIC 9(8).
               10  SB-QTY-REQUIRED     PIC S9(13)V9(6).
               10  SB-QTY-SUBSTITUTE   PIC S9(13)V9(6).
               10  SB-RATIO            PIC S9(9)V9(9).
               10  SB-NEEDS-AGREEMENT  PIC X.
               10  SB-AGREEMENT-REF    PIC X(40).
               10  SB-APPROVED-BY      PIC X(20).
               10  SB-VALID-FROM       PIC 9(8).
               10  SB-VALID-TO         PIC 9(8).
               10  SB-RESTRICTIONS     PIC X(200).
               10  SB-DOCUMENT-TYPE    PIC X(20).
               10  SB-DOCUMENT-REF     PIC X(40).
               10  SB-PERMITTED        PIC X.
                   88  SB-IS-PERMITTED           VALUE "Y".
               10  SB-INTERCHANGEABLE  PIC X.
               10  SB-LINE-CNT         PIC 9(3).
               10  SB-LINES OCCURS 15 TIMES.
                   15  SB-L-ITEM       PIC X(40).
                   15  SB-L-DESC       PIC X(60).
                   15  SB-L-RATIO      PIC S9(9)V9(9).
                   15  SB-L-CONTEXTS   PIC X(20).
                   15  SB-L-AGREEMENT  PIC X.
                   15  SB-L-FROM       PIC 9(8).
                   15  SB-L-TO         PIC 9(8).
                   15  SB-L-APPROVER   PIC X(20).
                   15  SB-L-FREE-QTY   PIC S9(13)V9(6).
