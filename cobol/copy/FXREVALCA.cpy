      *>================================================================
      *> FXREVALCA - COMMAREA for FXREVAL, foreign currency carrying
      *>             values, period end revaluation and realised
      *>             settlement differences (FIN-019, FIN-020).
      *> The revaluation of a period is reversed at the start of the
      *> next one, so a gain is never counted twice: AC-016 gives a
      *> lifetime gain of 62.50 and not 85.00.
      *> Functions: CARRY    carrying amount at an approved rate
      *>            REVALUE  unrealised adjustment at a date
      *>            REALISE  realised gain or loss on settlement
      *>            POST     post the revaluation and record the run
      *>            REVERSE  reverse a posted revaluation
      *> 1997-01-23 J M CLEARY   2026-10-05 STERP UNITS PER GBP (D-14)
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  FR-BODY.
               10  FR-CURRENCY         PIC X(3).
               10  FR-ACCOUNT          PIC X(12).
               10  FR-AS-AT-DATE       PIC 9(8).
               10  FR-FISCAL-YEAR      PIC 9(4).
               10  FR-PERIOD-NO        PIC 9(2).
               10  FR-RATE-TYPE        PIC X(6).
               10  FR-FOREIGN-AMOUNT   PIC S9(11)V99 COMP-3.
               10  FR-RATE-ORIGINAL    PIC S9(10)V9(8) COMP-3.
               10  FR-RATE-AT-DATE     PIC S9(10)V9(8) COMP-3.
               10  FR-RATE-SETTLE      PIC S9(10)V9(8) COMP-3.
               10  FR-CARRYING-GBP     PIC S9(13)V99 COMP-3.
               10  FR-SUBLEDGER-TYPE   PIC X(6).
               10  FR-SUBLEDGER-REF    PIC X(40).
               10  FR-POSITION         PIC X(10).
               10  FR-MODE             PIC X(8).
               10  FR-REVAL-ID         PIC S9(9) COMP-5.
      *>     ---- answers ----
               10  FR-ORIGINAL-GBP     PIC S9(13)V99 COMP-3.
               10  FR-REVALUED-GBP     PIC S9(13)V99 COMP-3.
               10  FR-ADJUSTMENT-GBP   PIC S9(13)V99 COMP-3.
               10  FR-SETTLED-GBP      PIC S9(13)V99 COMP-3.
               10  FR-REALISED-GBP     PIC S9(13)V99 COMP-3.
               10  FR-UNREALISED-GBP   PIC S9(13)V99 COMP-3.
               10  FR-LIFETIME-GBP     PIC S9(13)V99 COMP-3.
               10  FR-ADJUST-TYPE      PIC X(12).
               10  FR-GAIN-OR-LOSS     PIC X(4).
               10  FR-TOTAL-GAIN       PIC S9(13)V99 COMP-3.
               10  FR-TOTAL-LOSS       PIC S9(13)V99 COMP-3.
               10  FR-NET-ADJUSTMENT   PIC S9(13)V99 COMP-3.
               10  FR-JOURNAL-NUMBER   PIC X(30).
               10  FR-REVERSAL-JNL     PIC X(30).
               10  FR-REVAL-STATUS     PIC X(12).
               10  FR-LINE-CNT         PIC 9(3).
               10  FR-LINES OCCURS 20 TIMES.
                   15  FR-L-ACCOUNT    PIC X(12).
                   15  FR-L-CURRENCY   PIC X(3).
                   15  FR-L-FOREIGN    PIC S9(11)V99 COMP-3.
                   15  FR-L-CARRYING   PIC S9(13)V99 COMP-3.
                   15  FR-L-REVALUED   PIC S9(13)V99 COMP-3.
                   15  FR-L-ADJUST     PIC S9(13)V99 COMP-3.
