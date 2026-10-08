      *>================================================================
      *> JNLPOSTCA - COMMAREA for JNLPOST, posting and reversal of
      *>             journals through fin.usp_PostJournal and
      *>             fin.usp_ReverseJournal (FIN-004, FIN-005, FIN-006).
      *> Functions: POST     post the staged lines of CA-CORRELATION-ID
      *>            REVERSE  linked full or partial reversal
      *>            ENQUIRE  header, totals and reversible balance
      *> 1994-07-02 A P WHITTAKER   2026-10-05 STERP CORRELATION IDS
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  JP-BODY.
               10  JP-JOURNAL-NUMBER   PIC X(30).
               10  JP-JOURNAL-TYPE     PIC X(12).
               10  JP-DESCRIPTION      PIC X(200).
               10  JP-SOURCE-MODULE    PIC X(6).
               10  JP-SOURCE-REF       PIC X(40).
               10  JP-EVENT-CODE       PIC X(30).
               10  JP-SERIES-CODE      PIC X(20).
               10  JP-DOCUMENT-DATE    PIC 9(8).
               10  JP-POSTING-DATE     PIC 9(8).
               10  JP-CURRENCY         PIC X(3).
               10  JP-FX-RATE          PIC S9(10)V9(8) COMP-3.
               10  JP-EVIDENCE-REF     PIC X(120).
               10  JP-REVERSE-PCT      PIC S9(5)V9(4) COMP-3.
               10  JP-REVERSE-AMOUNT   PIC S9(13)V99 COMP-3.
               10  JP-REASON           PIC X(200).
      *>     ---- answers ----
               10  JP-JOURNAL-ID       PIC S9(9) COMP-5.
               10  JP-NEW-JOURNAL-NO   PIC X(30).
               10  JP-FISCAL-YEAR      PIC 9(4).
               10  JP-PERIOD-NO        PIC 9(2).
               10  JP-TOTAL-DR-GBP     PIC S9(13)V99 COMP-3.
               10  JP-TOTAL-CR-GBP     PIC S9(13)V99 COMP-3.
               10  JP-REVERSED-GBP     PIC S9(13)V99 COMP-3.
               10  JP-REVERSIBLE-GBP   PIC S9(13)V99 COMP-3.
               10  JP-STATUS           PIC X(12).
               10  JP-WAS-DUPLICATE    PIC X.
               10  JP-IS-REVERSAL      PIC X.
               10  JP-LINE-COUNT       PIC 9(3).
