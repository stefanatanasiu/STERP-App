      *>================================================================
      *> ARADJUSTCA - COMMAREA FOR ARADJUST, CUSTOMER BALANCE
      *>              ADJUSTMENTS AND WRITE OFFS
      *>   CA-FUNCTION: ASSESS    authority needed for this value
      *>                POST      post the adjustment
      *>   Balances are derived.  There is no direct balance edit:
      *>   every movement is a posted, reasoned document (AR-020).
      *>   D-22: up to GBP 5.00 per receipt may be written off by the
      *>   AR clerk with a reason; larger write offs follow D-07.
      *>   REQ: AR-012, AR-020, AR-021, D-22
      *> 1992-11-30 DJM CR0233 ADJUSTMENT TYPES AND REASON CODES
      *> 2026-10-05 STERP APPROVAL MATRIX AND TRANSFER LEG
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  AJ-BODY.
               10  AJ-CUSTOMER         PIC X(20).
               10  AJ-ADJ-TYPE         PIC X(12).
                   88  AJ-IS-WRITEOFF            VALUE "WRITEOFF".
                   88  AJ-IS-CREDIT              VALUE "CREDIT".
                   88  AJ-IS-TRANSFER            VALUE "TRANSFER".
                   88  AJ-IS-CORRECTION          VALUE "CORRECTION".
                   88  AJ-IS-PROVISION           VALUE "PROVISION".
               10  AJ-DOCUMENT-NO      PIC X(30).
               10  AJ-TARGET-DOC       PIC X(30).
               10  AJ-TARGET-ITEM-ID   PIC S9(9) COMP-5.
               10  AJ-TO-CUSTOMER      PIC X(20).
               10  AJ-AMOUNT-GBP       PIC S9(11)V99 COMP-3.
               10  AJ-RESIDUAL-BEFORE  PIC S9(11)V99 COMP-3.
               10  AJ-RESIDUAL-AFTER   PIC S9(11)V99 COMP-3.
               10  AJ-REASON-CODE      PIC X(20).
               10  AJ-REASON           PIC X(300).
               10  AJ-DOC-DATE         PIC 9(8).
               10  AJ-POSTING-DATE     PIC 9(8).
               10  AJ-ACCOUNT-CODE     PIC X(12).
               10  AJ-COST-CENTRE      PIC X(20).
               10  AJ-APPROVAL-ID      PIC S9(9) COMP-5.
               10  AJ-APPROVED-BY      PIC X(20).
               10  AJ-REQUIRED-PERM    PIC X(40).
               10  AJ-CLERK-LIMIT      PIC S9(11)V99 COMP-3.
               10  AJ-WITHIN-LIMIT     PIC X.
               10  AJ-OPEN-ITEM-ID     PIC S9(9) COMP-5.
               10  AJ-JOURNAL-NO       PIC X(30).
               10  AJ-EVENT-CODE       PIC X(30).
               10  FILLER              PIC X(40).
