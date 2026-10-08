      *>================================================================
      *> BANKRECCA - COMMAREA for BANKREC, the cash book, bank charges,
      *>             transfers and many-to-many reconciliation matching
      *>             (FIN-013, FIN-014, FIN-015).
      *> Functions: BRIDGE   book balance, outstanding items, statement
      *>            MATCH    tie n statement lines to m book lines
      *>            UNMATCH  reverse a match group with a reason
      *>            CHARGE   record a bank charge or interest
      *>            TRANSFER move value between two bank accounts
      *>            COMPLETE complete a reconciliation when it bridges
      *> 1996-03-19 J M CLEARY   2026-10-05 STERP GROUPED MATCHING
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BR-BODY.
               10  BR-BANK-ACCOUNT     PIC X(20).
               10  BR-TO-BANK-ACCOUNT  PIC X(20).
               10  BR-STATEMENT-REF    PIC X(40).
               10  BR-RECON-ID         PIC S9(9) COMP-5.
               10  BR-MATCH-REF        PIC X(30).
               10  BR-MATCH-ID         PIC S9(9) COMP-5.
               10  BR-AS-AT-DATE       PIC 9(8).
               10  BR-FROM-DATE        PIC 9(8).
               10  BR-TO-DATE          PIC 9(8).
               10  BR-AMOUNT           PIC S9(11)V99 COMP-3.
               10  BR-CURRENCY         PIC X(3).
               10  BR-NARRATIVE        PIC X(200).
               10  BR-EXPENSE-ACCOUNT  PIC X(12).
               10  BR-DIM-CC           PIC X(20).
               10  BR-REASON           PIC X(200).
               10  BR-ENTRY-TYPE       PIC X(10).
               10  BR-STMT-CNT         PIC 9(3).
               10  BR-STMT OCCURS 20 TIMES.
                   15  BR-ST-LINE-ID   PIC S9(9) COMP-5.
                   15  BR-ST-AMOUNT    PIC S9(11)V99 COMP-3.
               10  BR-BOOK-CNT         PIC 9(3).
               10  BR-BOOK OCCURS 20 TIMES.
                   15  BR-BK-LINE-ID   PIC S9(9) COMP-5.
                   15  BR-BK-AMOUNT    PIC S9(11)V99 COMP-3.
      *>     ---- answers ----
               10  BR-STMT-TOTAL       PIC S9(13)V99 COMP-3.
               10  BR-BOOK-TOTAL       PIC S9(13)V99 COMP-3.
               10  BR-DIFFERENCE       PIC S9(13)V99 COMP-3.
               10  BR-MATCH-TYPE       PIC X(12).
               10  BR-BOOK-BALANCE     PIC S9(13)V99 COMP-3.
               10  BR-STMT-BALANCE     PIC S9(13)V99 COMP-3.
               10  BR-OUT-RECEIPTS     PIC S9(13)V99 COMP-3.
               10  BR-OUT-PAYMENTS     PIC S9(13)V99 COMP-3.
               10  BR-BRIDGED-BALANCE  PIC S9(13)V99 COMP-3.
               10  BR-UNEXPLAINED      PIC S9(13)V99 COMP-3.
               10  BR-UNMATCHED-CNT    PIC 9(5).
               10  BR-JOURNAL-NUMBER   PIC X(30).
               10  BR-RECON-STATUS     PIC X(12).
               10  BR-MATCHED-CNT      PIC 9(3).
