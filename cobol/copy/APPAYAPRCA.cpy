      *>================================================================
      *> APPAYAPRCA - COMMAREA for APPAYAPR (payment approval of the
      *>              exact proposal content, and the invalidation of
      *>              that approval when any of it changes).
      *> FUNCTIONS  : HASH      compute the content hash of a proposal
      *>              REQUEST   request approval of the exact content
      *>              DECIDE    record an approver's decision
      *>              VERIFY    is the approval still valid?
      *>              INVALID   invalidate approvals after a change
      *> 1995-02-27 original DUAL AUTHORISATION OF PAYMENT RUNS
      *> 2026-10-05 STERP content hash, D-07 two authorisers
      *>================================================================
      *> REQ: AP-012, AP-002, COM-003, D-07
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  PA-BODY.
               10  PA-PROPOSAL-REF     PIC X(30).
               10  PA-PROPOSAL-ID      PIC S9(9) COMP-5.
               10  PA-VERSION          PIC 9(4).
               10  PA-DOCUMENT-TYPE    PIC X(20).
               10  PA-DOCUMENT-REF     PIC X(40).
               10  PA-APPROVAL-TYPE    PIC X(30).
               10  PA-CONTENT-HASH     PIC X(64).
               10  PA-STORED-HASH      PIC X(64).
               10  PA-AMOUNT-GBP       PIC S9(13)V99 COMP-3.
               10  PA-SINGLE-MAX-GBP   PIC S9(13)V99 COMP-3.
               10  PA-CURRENCY         PIC X(3).
               10  PA-APPROVER         PIC X(20).
               10  PA-DECISION         PIC X(10).
                   88  PA-APPROVE                VALUE "APPROVED".
                   88  PA-REJECT                 VALUE "REJECTED".
               10  PA-COMMENT          PIC X(200).
               10  PA-REQUESTED-BY     PIC X(20).
               10  PA-APPROVAL-ID      PIC S9(9) COMP-5.
               10  PA-APPROVER-1       PIC X(20).
               10  PA-APPROVER-2       PIC X(20).
               10  PA-REQUIRED-CNT     PIC 9(2).
               10  PA-HELD-CNT         PIC 9(2).
               10  PA-STATUS           PIC X(14).
               10  PA-VALID            PIC X.
               10  PA-NEEDS-FC         PIC X.
               10  PA-NEEDS-DIRECTOR   PIC X.
               10  PA-INVALIDATED-CNT  PIC 9(5).
               10  PA-HASH-SOURCE      PIC X(10).
               10  PA-HASH-INPUT-CNT   PIC 9(4).
               10  PA-NOTE             PIC X(200).
