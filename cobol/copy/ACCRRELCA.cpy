      *>================================================================
      *> ACCRRELCA - COMMAREA for ACCRREL, recurring journal, accrual
      *>             and prepayment release schedules (FIN-007/FIN-008).
      *> Functions: SCHEDULE build the occurrence schedule (no writing)
      *>            NEXTOCC  the next unposted occurrence and its period
      *>            RELEASE  post one occurrence once and once only
      *>            STATUS   released, remaining and occurrence history
      *> 1995-02-20 D R OKONJO   2026-10-05 STERP NO DOUBLE RELEASE
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  AK-BODY.
               10  AK-RECUR-CODE       PIC X(20).
               10  AK-NAME             PIC X(80).
               10  AK-KIND             PIC X(10).
               10  AK-FREQUENCY        PIC X(10).
               10  AK-START-DATE       PIC 9(8).
               10  AK-END-DATE         PIC 9(8).
               10  AK-SOURCE-AMOUNT    PIC S9(11)V99 COMP-3.
               10  AK-OCC-PLANNED      PIC 9(4).
               10  AK-OCCURRENCE-NO    PIC 9(4).
               10  AK-POSTING-DATE     PIC 9(8).
               10  AK-EVENT-CODE       PIC X(30).
               10  AK-MODE             PIC X(8).
      *>     ---- answers ----
               10  AK-RELEASE-AMOUNT   PIC S9(11)V99 COMP-3.
               10  AK-RELEASED-TOTAL   PIC S9(13)V99 COMP-3.
               10  AK-REMAINING        PIC S9(13)V99 COMP-3.
               10  AK-OCC-POSTED       PIC 9(4).
               10  AK-NEXT-DUE-DATE    PIC 9(8).
               10  AK-JOURNAL-NUMBER   PIC X(30).
               10  AK-PERIOD-STATUS    PIC X(8).
               10  AK-FISCAL-YEAR      PIC 9(4).
               10  AK-PERIOD-NO        PIC 9(2).
               10  AK-ALREADY-POSTED   PIC X.
               10  AK-BLOCKED          PIC X.
               10  AK-BLOCK-REASON     PIC X(200).
               10  AK-EXCEPTION-ID     PIC S9(9) COMP-5.
               10  AK-RECUR-STATUS     PIC X(10).
               10  AK-SCHED-CNT        PIC 9(3).
               10  AK-SCHED OCCURS 36 TIMES.
                   15  AK-S-OCC-NO     PIC 9(4).
                   15  AK-S-DATE       PIC 9(8).
                   15  AK-S-AMOUNT     PIC S9(11)V99 COMP-3.
                   15  AK-S-STATUS     PIC X(12).
                   15  AK-S-JOURNAL    PIC X(30).
