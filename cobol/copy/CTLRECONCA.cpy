      *>================================================================
      *> CTLRECONCA - COMMAREA for CTLRECON, control account to
      *>              supporting detail reconciliation (FIN-021).
      *> A difference is reported with its contributing references and
      *> an owner; it is never squared off automatically.
      *> Functions: RUN     reconcile one or every control account
      *>            RECORD  write the run to rpt.ReconciliationRun
      *>            ACCEPT  record an explained or corrected difference
      *> 1998-05-14 J M CLEARY   2026-10-05 STERP OWNERSHIP (COM-022)
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  CR-BODY.
               10  CR-CONTROL-TYPE     PIC X(10).
               10  CR-ACCOUNT          PIC X(12).
               10  CR-AS-AT-DATE       PIC 9(8).
               10  CR-FISCAL-YEAR      PIC 9(4).
               10  CR-PERIOD-NO        PIC 9(2).
               10  CR-OWNER-USER       PIC X(20).
               10  CR-NOTE             PIC X(300).
               10  CR-MATERIALITY      PIC S9(11)V99 COMP-3.
               10  CR-RUN-ID           PIC S9(9) COMP-5.
      *>     ---- answers ----
               10  CR-CONTROL-GBP      PIC S9(13)V99 COMP-3.
               10  CR-DETAIL-GBP       PIC S9(13)V99 COMP-3.
               10  CR-DIFFERENCE-GBP   PIC S9(13)V99 COMP-3.
               10  CR-STATE            PIC X(12).
               10  CR-MATERIAL         PIC X.
               10  CR-EXCEPTION-ID     PIC S9(9) COMP-5.
               10  CR-ROW-CNT          PIC 9(3).
               10  CR-DIFF-COUNT       PIC 9(3).
               10  CR-ROWS OCCURS 16 TIMES.
                   15  CR-R-ACCOUNT    PIC X(12).
                   15  CR-R-NAME       PIC X(60).
                   15  CR-R-TYPE       PIC X(10).
                   15  CR-R-CONTROL    PIC S9(13)V99 COMP-3.
                   15  CR-R-DETAIL     PIC S9(13)V99 COMP-3.
                   15  CR-R-DIFF       PIC S9(13)V99 COMP-3.
                   15  CR-R-STATE      PIC X(12).
