      *>================================================================
      *> WIPCALCCA - COMMAREA FOR WIPCALC, WORK IN PROGRESS ROLL-FORWARD
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : COSTING AND PROFITABILITY
      *>
      *>   closing WIP = opening WIP + material issues
      *>               + approved conversion and other additions
      *>               + value adjustments - material returns
      *>               - finished output transfers - approved write-offs
      *>
      *> A FINISHED GOODS RECEIPT TRANSFERS VALUE OUT OF WIP.  IT DOES
      *> NOT LEAVE THE SAME AMOUNT IN BOTH BALANCES (CST-018).
      *>
      *> WORKED EXAMPLE C6 OF THE SPECIFICATION
      *>   OPENING 100.00 + ISSUES 500.00 + LABOUR 200.00
      *>   + OVERHEAD 50.00 - RETURNS 20.00 - TRANSFER 700.00
      *>   = CLOSING 130.00
      *>
      *> CA-FUNCTION VALUES
      *>   CALC      ROLL FORWARD ONE JOB OR KIT FOR ONE PERIOD FROM
      *>             THE COST TRANSACTIONS
      *>   POST      CALC AND WRITE THE cst.WipBalance ROW
      *>   ENQUIRE   READ A STORED WIP BALANCE
      *>   RESIDUAL  TEST WHETHER A CLOSE WOULD STRAND VALUE (CST-020)
      *>   WRITEOFF  BOOK AN APPROVED WIP WRITE-OFF
      *>
      *> REASON CODES
      *>   CST0901 COMPANY MISSING
      *>   CST0902 A JOB OR A KIT REFERENCE IS REQUIRED
      *>   CST0903 FISCAL YEAR AND PERIOD ARE REQUIRED
      *>   CST0904 PERIOD IS HARD CLOSED
      *>   CST0905 WIP BALANCE NOT FOUND
      *>   CST0906 RESIDUAL WIP CANNOT BE DISCARDED SILENTLY
      *>   CST0907 WRITE-OFF NEEDS AN APPROVER AND A REASON
      *>   CST0908 UNSUPPORTED FUNCTION
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-04-30 DJM  CR0651 ORIGINAL WIP ROLL-FORWARD
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT PERIOD DATES
      *> 2026-10-06 STERP CST-018 CST-020 RESIDUAL REVIEW
      *>================================================================
      *> REQ: CST-018, CST-020   AC: AC-C6
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  WC-BODY.
               10  WC-JOB-REF          PIC X(20).
               10  WC-KIT-REF          PIC X(30).
               10  WC-FISCAL-YEAR      PIC 9(4).
               10  WC-PERIOD-NO        PIC 9(2).
               10  WC-INCLUDE-SUBJOBS  PIC X.
               10  WC-COMMIT           PIC X.
               10  WC-REASON           PIC X(200).
      *>       ---- the roll-forward ----
               10  WC-OPENING-GBP      PIC S9(13)V9(2).
               10  WC-MAT-ISSUES-GBP   PIC S9(13)V9(2).
               10  WC-MAT-RETURNS-GBP  PIC S9(13)V9(2).
               10  WC-LABOUR-GBP       PIC S9(13)V9(2).
               10  WC-MACHINE-GBP      PIC S9(13)V9(2).
               10  WC-OVERHEAD-GBP     PIC S9(13)V9(2).
               10  WC-SUBCON-GBP       PIC S9(13)V9(2).
               10  WC-ADJUSTMENT-GBP   PIC S9(13)V9(2).
               10  WC-TRANSFERS-GBP    PIC S9(13)V9(2).
               10  WC-SCRAP-GBP        PIC S9(13)V9(2).
               10  WC-WRITEOFF-GBP     PIC S9(13)V9(2).
               10  WC-CLOSING-GBP      PIC S9(13)V9(2).
               10  WC-ADDITIONS-GBP    PIC S9(13)V9(2).
               10  WC-DEDUCTIONS-GBP   PIC S9(13)V9(2).
               10  WC-RESIDUAL-GBP     PIC S9(13)V9(2).
               10  WC-HAS-RESIDUAL     PIC X.
                   88  WC-RESIDUAL-HELD          VALUE "Y".
               10  WC-TXN-COUNT        PIC S9(9).
               10  WC-BALANCED         PIC X.
               10  WC-WIP-ID           PIC S9(9).
               10  WC-PERIOD-STATUS    PIC X(8).
               10  WC-WARNING          PIC X(80).
