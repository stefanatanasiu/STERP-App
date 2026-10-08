      *>================================================================
      *> VATRET9CA - COMMAREA for VATRET9, the nine box VAT return built
      *>             from posted VAT records with box drill-down and
      *>             finalisation (FIN-018, FIN-025, FIN-026, FIN-029).
      *> Functions: PREPARE  compute boxes 1 to 9 for a VAT period
      *>            DRILL    the source records behind one box
      *>            FINALISE freeze the figures and the detail
      *>            SUBMIT   record the submission and HMRC receipt
      *>            CHECK    VAT control account reconciliation
      *> 1996-09-02 J M CLEARY   2019-04-01 MAKING TAX DIGITAL
      *> 2026-10-05 STERP NINE BOX DRILL-DOWN
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  VR-BODY.
               10  VR-VAT-PERIOD-KEY   PIC X(10).
               10  VR-BASIS            PIC X(10).
               10  VR-VERSION          PIC 9(4).
               10  VR-BOX-NO           PIC 9(2).
               10  VR-SUBMISSION-REF   PIC X(60).
               10  VR-HMRC-RESPONSE    PIC X(300).
               10  VR-REASON           PIC X(200).
      *>     ---- answers ----
               10  VR-RETURN-ID        PIC S9(9) COMP-5.
               10  VR-VAT-REG-NO       PIC X(15).
               10  VR-START-DATE       PIC 9(8).
               10  VR-END-DATE         PIC 9(8).
               10  VR-FILING-DUE       PIC 9(8).
               10  VR-PAYMENT-DUE      PIC 9(8).
               10  VR-BOX1             PIC S9(13)V99 COMP-3.
               10  VR-BOX2             PIC S9(13)V99 COMP-3.
               10  VR-BOX3             PIC S9(13)V99 COMP-3.
               10  VR-BOX4             PIC S9(13)V99 COMP-3.
               10  VR-BOX5             PIC S9(13)V99 COMP-3.
               10  VR-BOX6             PIC S9(13) COMP-3.
               10  VR-BOX7             PIC S9(13) COMP-3.
               10  VR-BOX8             PIC S9(13) COMP-3.
               10  VR-BOX9             PIC S9(13) COMP-3.
               10  VR-PERIOD-STATUS    PIC X(14).
               10  VR-RETURN-STATUS    PIC X(14).
               10  VR-RECORD-CNT       PIC 9(6).
               10  VR-LATE-CNT         PIC 9(6).
               10  VR-CONTROL-BALANCE  PIC S9(13)V99 COMP-3.
               10  VR-CONTROL-DIFF     PIC S9(13)V99 COMP-3.
               10  VR-IS-REPAYMENT     PIC X.
               10  VR-DRILL-CNT        PIC 9(3).
               10  VR-DRILL OCCURS 20 TIMES.
                   15  VR-D-DOC-TYPE   PIC X(20).
                   15  VR-D-DOC-REF    PIC X(40).
                   15  VR-D-TAX-POINT  PIC 9(8).
                   15  VR-D-VAT-CODE   PIC X(4).
                   15  VR-D-NET        PIC S9(11)V99 COMP-3.
                   15  VR-D-VAT        PIC S9(11)V99 COMP-3.
                   15  VR-D-AMOUNT     PIC S9(11)V99 COMP-3.
                   15  VR-D-LATE       PIC X.
