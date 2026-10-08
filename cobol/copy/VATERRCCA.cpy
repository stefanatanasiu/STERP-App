      *>================================================================
      *> VATERRCCA - COMMAREA for VATERRC, VAT error correction and the
      *>             disclosure threshold rules (FIN-027).
      *> Baseline threshold: a net error up to GBP 10,000, or up to one
      *> per cent of box 6 turnover capped at GBP 50,000, may be
      *> corrected on the next return.  Anything larger, and any
      *> deliberate error, is routed for separate disclosure to HMRC.
      *> Functions: ASSESS  decide the method without writing anything
      *>            RECORD  record the correction and its method
      *>            APPROVE approve a recorded correction
      *> 2009-07-01 R J HOLBROOK   2026-10-05 STERP THRESHOLD TEST
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  VE-BODY.
               10  VE-ORIG-PERIOD-KEY  PIC X(10).
               10  VE-CORR-PERIOD-KEY  PIC X(10).
               10  VE-DISCOVERED-DATE  PIC 9(8).
               10  VE-NET-ERROR        PIC S9(11)V99 COMP-3.
               10  VE-BOX6-TURNOVER    PIC S9(13)V99 COMP-3.
               10  VE-IS-DELIBERATE    PIC X.
               10  VE-REASON           PIC X(300).
               10  VE-SOURCE-REFS      PIC X(300).
               10  VE-DISCLOSURE-REF   PIC X(60).
               10  VE-ERROR-ID         PIC S9(9) COMP-5.
      *>     ---- answers ----
               10  VE-METHOD           PIC X(14).
               10  VE-THRESHOLD-USED   PIC S9(13)V99 COMP-3.
               10  VE-ONE-PCT-LIMIT    PIC S9(13)V99 COMP-3.
               10  VE-FLAT-LIMIT       PIC S9(11)V99 COMP-3.
               10  VE-CAP-LIMIT        PIC S9(11)V99 COMP-3.
               10  VE-WITHIN-THRESHOLD PIC X.
               10  VE-STATUS           PIC X(12).
               10  VE-EXPLANATION      PIC X(200).
