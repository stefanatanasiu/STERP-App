      *>================================================================
      *> OPENBALCA - COMMAREA for OPENBAL, opening ledger balances at
      *>             cut over (FIN-024, COM-018).
      *> Opening subledger detail establishes the control balance once:
      *> AC-018 opening trade debtors are 500.00, not 1,000.00, because
      *> the detail is not added to a separately keyed control line.
      *> Functions: LOAD     load one opening detail row
      *>            VALIDATE check the batch and report the differences
      *>            POST     post the opening journal once
      *>            STATUS   totals, equity treatment and rejections
      *> 2000-03-27 R J HOLBROOK   2026-10-05 STERP NO DOUBLE COUNTING
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  OB-BODY.
               10  OB-BATCH-REF        PIC X(30).
               10  OB-AS-AT-DATE       PIC 9(8).
               10  OB-ACCOUNT          PIC X(12).
               10  OB-SOURCE-TYPE      PIC X(12).
               10  OB-SUBLEDGER-TYPE   PIC X(6).
               10  OB-SUBLEDGER-REF    PIC X(40).
               10  OB-DOCUMENT-REF     PIC X(40).
               10  OB-DOCUMENT-DATE    PIC 9(8).
               10  OB-DEBIT            PIC S9(11)V99 COMP-3.
               10  OB-CREDIT           PIC S9(11)V99 COMP-3.
               10  OB-DIM-CC           PIC X(20).
               10  OB-DIM-PROJ         PIC X(20).
               10  OB-NARRATIVE        PIC X(200).
               10  OB-EQUITY-ACCOUNT   PIC X(12).
      *>     ---- answers ----
               10  OB-TOTAL-DEBIT      PIC S9(13)V99 COMP-3.
               10  OB-TOTAL-CREDIT     PIC S9(13)V99 COMP-3.
               10  OB-CONTROL-GBP      PIC S9(13)V99 COMP-3.
               10  OB-SUBLEDGER-GBP    PIC S9(13)V99 COMP-3.
               10  OB-EQUITY-GBP       PIC S9(13)V99 COMP-3.
               10  OB-DIFFERENCE       PIC S9(13)V99 COMP-3.
               10  OB-ROW-CNT          PIC 9(5).
               10  OB-LOADED-CNT       PIC 9(5).
               10  OB-REJECT-CNT       PIC 9(5).
               10  OB-DUPLICATE        PIC X.
               10  OB-BALANCED         PIC X.
               10  OB-STATUS           PIC X(12).
               10  OB-JOURNAL-NUMBER   PIC X(30).
               10  OB-REJECT-REASON    PIC X(200).
