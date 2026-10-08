      *>================================================================
      *> BUDGCOMPCA - COMMAREA for BUDGCOMP, budget versions and the
      *>              budget to actual comparison (FIN-009, FIN-023).
      *> Functions: COMPARE  actuals against one version, with variance
      *>            VERSION  create a revised version from an existing
      *>            APPROVE  approve a version as the baseline
      *>            LIST     versions held for a year
      *> 1995-04-11 D R OKONJO   2026-10-05 STERP VERSION BASELINES
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BC-BODY.
               10  BC-BUDGET-CODE      PIC X(20).
               10  BC-FISCAL-YEAR      PIC 9(4).
               10  BC-VERSION          PIC 9(4).
               10  BC-BUDGET-TYPE      PIC X(12).
               10  BC-NAME             PIC X(80).
               10  BC-COMPARE-CODE     PIC X(20).
               10  BC-COMPARE-VERSION  PIC 9(4).
               10  BC-PERIOD-FROM      PIC 9(2).
               10  BC-PERIOD-TO        PIC 9(2).
               10  BC-ACCOUNT          PIC X(12).
               10  BC-DIM-CC           PIC X(20).
               10  BC-UPLIFT-PCT       PIC S9(5)V9(4) COMP-3.
      *>     ---- answers ----
               10  BC-BUDGET-GBP       PIC S9(13)V99 COMP-3.
               10  BC-ACTUAL-GBP       PIC S9(13)V99 COMP-3.
               10  BC-VARIANCE-GBP     PIC S9(13)V99 COMP-3.
               10  BC-VARIANCE-SENSE   PIC X(12).
               10  BC-COMPARE-GBP      PIC S9(13)V99 COMP-3.
               10  BC-COMPARE-VAR      PIC S9(13)V99 COMP-3.
               10  BC-NEW-VERSION      PIC 9(4).
               10  BC-STATUS           PIC X(10).
               10  BC-IS-BASELINE      PIC X.
               10  BC-ROW-CNT          PIC 9(3).
               10  BC-ROWS OCCURS 40 TIMES.
                   15  BC-R-ACCOUNT    PIC X(12).
                   15  BC-R-NAME       PIC X(60).
                   15  BC-R-DIM-CC     PIC X(20).
                   15  BC-R-BUDGET     PIC S9(13)V99 COMP-3.
                   15  BC-R-ACTUAL     PIC S9(13)V99 COMP-3.
                   15  BC-R-VARIANCE   PIC S9(13)V99 COMP-3.
                   15  BC-R-SENSE      PIC X(12).
                   15  BC-R-COMPARE    PIC S9(13)V99 COMP-3.
