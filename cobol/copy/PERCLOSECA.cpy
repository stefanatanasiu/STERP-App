      *>================================================================
      *> PERCLOSECA - COMMAREA for PERCLOSE, period controls, the close
      *>              checklist and sign-off (FIN-010, FIN-011).
      *> Functions: STATUS   period state and checklist readiness
      *>            BUILD    create the standard checklist for a period
      *>            STEP     complete or accept one checklist step
      *>            SOFT     soft close (authorised adjustments only)
      *>            HARD     hard close (blocked unless ready)
      *>            REOPEN   reopen with reason, approver and reissues
      *> 1994-11-08 A P WHITTAKER   2026-10-05 STERP SIGN-OFF EVIDENCE
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  CL-BODY.
               10  CL-FISCAL-YEAR      PIC 9(4).
               10  CL-PERIOD-NO        PIC 9(2).
               10  CL-STEP-CODE        PIC X(20).
               10  CL-STEP-ACTION      PIC X(10).
               10  CL-EVIDENCE-REF     PIC X(200).
               10  CL-DIFFERENCE       PIC S9(11)V99 COMP-3.
               10  CL-REASON           PIC X(200).
               10  CL-REVIEWER         PIC X(20).
               10  CL-FORCE            PIC X.
      *>     ---- answers ----
               10  CL-PERIOD-NAME      PIC X(20).
               10  CL-PERIOD-STATUS    PIC X(8).
               10  CL-START-DATE       PIC 9(8).
               10  CL-END-DATE         PIC 9(8).
               10  CL-STEP-CNT         PIC 9(3).
               10  CL-DONE-CNT         PIC 9(3).
               10  CL-BLOCKING-OPEN    PIC 9(3).
               10  CL-UNRESOLVED-DIFF  PIC S9(13)V99 COMP-3.
               10  CL-MATERIALITY      PIC S9(11)V99 COMP-3.
               10  CL-READINESS        PIC X(10).
               10  CL-CLOSED-BY        PIC X(20).
               10  CL-SIGNED-OFF       PIC X.
               10  CL-STEPS-LISTED     PIC 9(3).
               10  CL-STEPS OCCURS 20 TIMES.
                   15  CL-S-CODE       PIC X(20).
                   15  CL-S-NAME       PIC X(60).
                   15  CL-S-STATUS     PIC X(12).
                   15  CL-S-BLOCKING   PIC X.
                   15  CL-S-DIFF       PIC S9(11)V99 COMP-3.
                   15  CL-S-OWNER      PIC X(20).
                   15  CL-S-EVIDENCE   PIC X(60).
