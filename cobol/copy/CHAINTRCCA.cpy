      *>================================================================
      *> CHAINTRCCA - COMMAREA for CHAINTRC (business chain trace)
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : CROSS MODULE CONTROLS AND REPORTING (XMD)
      *> PURPOSE  : One document reference in, the ordered steps of
      *>            its order to cash, procure to pay or assembly
      *>            chain out, with the independent return, credit
      *>            and reversal consequences marked separately.
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-06-30 RJH  CR0655 ORIGINAL DOCUMENT TRAIL ENQUIRY
      *> 2026-10-06 STERP XMD-001..004 ONE TRACEABLE CHAIN
      *>================================================================
      *> REQ: XMD-001, XMD-002, XMD-003, XMD-004
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  CT-BODY.
               10  CT-DOC-TYPE         PIC X(12).
               10  CT-DOC-REF          PIC X(40).
               10  CT-CHAIN-TYPE       PIC X(12).
               10  CT-CHAIN-NAME       PIC X(40).
               10  CT-PARTY            PIC X(20).
               10  CT-STEP-CNT         PIC S9(9) COMP-5.
               10  CT-MAX-STEPS        PIC S9(9) COMP-5.
               10  CT-TRUNCATED        PIC X.
               10  CT-COMPLETE         PIC X.
               10  CT-BROKEN           PIC X.
               10  CT-REVERSALS        PIC S9(9) COMP-5.
               10  CT-TOTAL-GBP        PIC S9(13)V99.
               10  CT-STEP OCCURS 24 TIMES.
                   15  CT-SEQ          PIC 9(2).
                   15  CT-STAGE        PIC X(24).
                   15  CT-DOC-T        PIC X(12).
                   15  CT-REF          PIC X(40).
                   15  CT-DATE         PIC 9(8).
                   15  CT-STATUS       PIC X(12).
                   15  CT-QTY          PIC S9(11)V9(3).
                   15  CT-VALUE        PIC S9(13)V99.
                   15  CT-KIND         PIC X(10).
                   15  CT-NOTE         PIC X(60).
