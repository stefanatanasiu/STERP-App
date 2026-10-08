      *>================================================================
      *> JNLENTRYCA - COMMAREA for JNLENTRY, manual journal preparation,
      *>              validation, submission and approval (FIN-004/005).
      *> Functions: VALIDAT  check header and every line, no ledger
      *>            STAGE    write the lines to fin.JournalStage
      *>            SUBMIT   draft -> submitted (balanced, evidenced)
      *>            APPROVE  submitted -> approved (independent user)
      *>            REJECT   submitted -> rejected with a reason
      *>            VOID     draft closed, reference retained
      *>            LOAD     read a stored journal back into the area
      *> 1994-06-14 A P WHITTAKER   2026-10-05 STERP EVIDENCE/APPROVAL
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  JE-BODY.
               10  JE-JOURNAL-REF      PIC X(30).
               10  JE-JOURNAL-TYPE     PIC X(12).
               10  JE-DESCRIPTION      PIC X(200).
               10  JE-SOURCE-MODULE    PIC X(6).
               10  JE-SOURCE-REF       PIC X(40).
               10  JE-EVENT-CODE       PIC X(30).
               10  JE-DOCUMENT-DATE    PIC 9(8).
               10  JE-POSTING-DATE     PIC 9(8).
               10  JE-CURRENCY         PIC X(3).
               10  JE-FX-RATE          PIC S9(10)V9(8) COMP-3.
               10  JE-EVIDENCE-REF     PIC X(120).
               10  JE-PREPARED-BY      PIC X(20).
               10  JE-APPROVED-BY      PIC X(20).
               10  JE-REASON           PIC X(200).
               10  JE-REQUESTED-STATUS PIC X(12).
               10  JE-LINE-CNT         PIC 9(3).
               10  JE-LINES OCCURS 50 TIMES.
                   15  JE-L-NUM        PIC 9(4).
                   15  JE-L-ACCOUNT    PIC X(12).
                   15  JE-L-DEBIT      PIC S9(11)V99 COMP-3.
                   15  JE-L-CREDIT     PIC S9(11)V99 COMP-3.
                   15  JE-L-DIM-CC     PIC X(20).
                   15  JE-L-DIM-PROJ   PIC X(20).
                   15  JE-L-NARRATIVE  PIC X(80).
                   15  JE-L-VAT-CODE   PIC X(4).
                   15  JE-L-SUBLDG-TYP PIC X(6).
                   15  JE-L-SUBLDG-REF PIC X(40).
                   15  JE-L-BANK-ACCT  PIC X(20).
                   15  JE-L-STATUS     PIC X(2).
                   15  JE-L-MESSAGE    PIC X(80).
      *>     ---- answers ----
               10  JE-TOTAL-DEBIT      PIC S9(13)V99 COMP-3.
               10  JE-TOTAL-CREDIT     PIC S9(13)V99 COMP-3.
               10  JE-TOTAL-DR-GBP     PIC S9(13)V99 COMP-3.
               10  JE-TOTAL-CR-GBP     PIC S9(13)V99 COMP-3.
               10  JE-OUT-OF-BALANCE   PIC S9(13)V99 COMP-3.
               10  JE-STATUS           PIC X(12).
               10  JE-FISCAL-YEAR      PIC 9(4).
               10  JE-PERIOD-NO        PIC 9(2).
               10  JE-PERIOD-STATUS    PIC X(8).
               10  JE-ERROR-CNT        PIC 9(3).
               10  JE-FIRST-BAD-LINE   PIC 9(4).
               10  JE-STAGED-CNT       PIC 9(3).
               10  JE-APPROVAL-REQD    PIC X.
               10  JE-SELF-APPROVAL    PIC X.
