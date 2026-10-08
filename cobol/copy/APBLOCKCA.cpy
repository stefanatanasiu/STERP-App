      *>================================================================
      *> APBLOCKCA - COMMAREA for APBLOCK (match disputes, payment
      *>             blocks, contractual due and discount dates and the
      *>             controller's pending-match recognition exception).
      *> FUNCTIONS : BLOCK     block an open item / invoice for payment
      *>             RELEASE   release a block with evidence
      *>             DISPUTE   raise or resolve a match dispute
      *>             DUEDATE   derive due and discount dates from terms
      *>             PENDING   approve pending-match recognition
      *> 1992-01-30 original PAYMENT SUSPENSE AND QUERY CONTROL
      *> 2026-10-05 STERP exception ownership, pending-match clearing
      *>================================================================
      *> REQ: AP-008, AP-009, D-22, D-25
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BK-BODY.
               10  BK-SUPPLIER         PIC X(20).
               10  BK-REGISTER-NUMBER  PIC X(30).
               10  BK-INVOICE-ID       PIC S9(9) COMP-5.
               10  BK-OPEN-ITEM-ID     PIC S9(9) COMP-5.
               10  BK-DOCUMENT-NUMBER  PIC X(30).
               10  BK-ACTION           PIC X(10).
                   88  BK-ACT-SET                VALUE "SET".
                   88  BK-ACT-CLEAR              VALUE "CLEAR".
               10  BK-REASON           PIC X(200).
               10  BK-RESOLUTION       PIC X(200).
               10  BK-OWNER-USER       PIC X(20).
               10  BK-OWNER-ROLE       PIC X(20).
               10  BK-CATEGORY         PIC X(30).
               10  BK-SEVERITY         PIC X(8).
               10  BK-AMOUNT-GBP       PIC S9(13)V99 COMP-3.
               10  BK-EXCEPTION-ID     PIC S9(9) COMP-5.
               10  BK-TERMS-CODE       PIC X(12).
               10  BK-DOCUMENT-DATE    PIC 9(8).
               10  BK-DATE-RECEIVED    PIC 9(8).
               10  BK-SUPPLY-DATE      PIC 9(8).
               10  BK-DUE-DATE         PIC 9(8).
               10  BK-DUE-DATE-RAW     PIC 9(8).
               10  BK-DISCOUNT-DATE    PIC 9(8).
               10  BK-DISCOUNT-AMT     PIC S9(11)V99 COMP-3.
               10  BK-TERM-DAYS        PIC S9(5) SIGN LEADING SEPARATE.
               10  BK-TERMS-SOURCE     PIC X(20).
               10  BK-OVERRIDE-REASON  PIC X(200).
               10  BK-CLEARING-ACCOUNT PIC X(12).
               10  BK-PENDING-GBP      PIC S9(13)V99 COMP-3.
               10  BK-APPROVAL-ID      PIC S9(9) COMP-5.
               10  BK-APPROVER         PIC X(20).
               10  BK-IS-BLOCKED       PIC X.
               10  BK-IS-DISPUTED      PIC X.
               10  BK-ACCT-STATE       PIC X(14).
               10  BK-PAY-ELIGIBILITY  PIC X(14).
               10  BK-ITEMS-AFFECTED   PIC 9(6).
               10  BK-NOTE             PIC X(200).
