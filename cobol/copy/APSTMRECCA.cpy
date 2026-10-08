      *>================================================================
      *> APSTMRECCA - COMMAREA for APSTMREC (supplier statement
      *>              reconciliation: missing invoices and credits,
      *>              timing differences and unmatched payments).
      *> FUNCTIONS  : LOAD      record a statement header
      *>              LINE      add and classify one statement line
      *>              RECONCIL  compare the statement with the ledger
      *>                        (CA-FUNCTION holds eight characters)
      *>              RESOLVE   record the reconciliation outcome
      *> 1994-10-04 original SUPPLIER STATEMENT RECONCILIATION
      *> 2026-10-05 STERP exception ownership of every difference
      *>================================================================
      *> REQ: AP-019, AP-022
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SR-BODY.
               10  SR-SUPPLIER         PIC X(20).
               10  SR-STATEMENT-REF    PIC X(40).
               10  SR-STATEMENT-ID     PIC S9(9) COMP-5.
               10  SR-STATEMENT-DATE   PIC 9(8).
               10  SR-CURRENCY         PIC X(3).
               10  SR-STATEMENT-BAL    PIC S9(13)V99 COMP-3.
               10  SR-LEDGER-BAL       PIC S9(13)V99 COMP-3.
               10  SR-DIFFERENCE       PIC S9(13)V99 COMP-3.
               10  SR-EXPLAINED        PIC S9(13)V99 COMP-3.
               10  SR-UNEXPLAINED      PIC S9(13)V99 COMP-3.
               10  SR-TIMING-DIFF      PIC S9(13)V99 COMP-3.
               10  SR-MISSING-OURS     PIC S9(13)V99 COMP-3.
               10  SR-MISSING-THEIRS   PIC S9(13)V99 COMP-3.
               10  SR-DISPUTED-AMT     PIC S9(13)V99 COMP-3.
               10  SR-STATUS           PIC X(12).
               10  SR-OWNER-USER       PIC X(20).
               10  SR-NOTE             PIC X(300).
               10  SR-EXCEPTION-CNT    PIC 9(4).
               10  SR-MATCHED-CNT      PIC 9(4).
               10  SR-LINE-CNT         PIC 9(4).
               10  SR-LINE-TABLE.
                   15  SR-LINE OCCURS 40 TIMES.
                       20  SR-L-NUM        PIC 9(5).
                       20  SR-L-REF        PIC X(40).
                       20  SR-L-NORMALISED PIC X(40).
                       20  SR-L-DATE       PIC 9(8).
                       20  SR-L-DUE        PIC 9(8).
                       20  SR-L-AMOUNT     PIC S9(13)V99 COMP-3.
                       20  SR-L-OURS       PIC S9(13)V99 COMP-3.
                       20  SR-L-DIFF       PIC S9(13)V99 COMP-3.
                       20  SR-L-ITEM-ID    PIC S9(9) COMP-5.
                       20  SR-L-STATUS     PIC X(14).
                       20  SR-L-NOTE       PIC X(60).
