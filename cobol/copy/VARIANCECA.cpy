      *>================================================================
      *> VARIANCECA - COMMAREA FOR VARIANCE, VARIANCE ANALYSIS
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *>
      *> VARIANCE COMPONENTS MUST RECONCILE TO THE TOTAL DIFFERENCE
      *> BETWEEN ACTUAL AND BASELINE, AND THE BASELINE QUANTITY MUST BE
      *> DISCLOSED (CST-019).  THE STANDARD SPLIT USED HERE IS
      *>
      *>   usage/efficiency = (actual qty - allowed qty) x standard rate
      *>   price/rate       = (actual rate - standard rate) x actual qty
      *>
      *> WHICH ADDS EXACTLY TO actual qty x actual rate
      *>                      - allowed qty x standard rate.
      *>
      *> WORKED EXAMPLE C5 OF THE SPECIFICATION
      *>   ALLOWED 20 AT 4.00, ACTUAL 22 AT 4.50
      *>   USAGE (22-20) x 4 = 8.00, PRICE (4.50-4) x 22 = 11.00
      *>   TOTAL ADVERSE 19.00 = ACTUAL 99.00 LESS ALLOWED 80.00
      *>
      *> CA-FUNCTION VALUES
      *>   SPLIT     SPLIT ONE VARIANCE INTO QUANTITY AND RATE PARTS
      *>   MATERIAL  MATERIAL USAGE AND PRICE VARIANCE
      *>   LABOUR    LABOUR EFFICIENCY AND RATE VARIANCE
      *>   OVERHEAD  OVERHEAD ABSORPTION VARIANCE
      *>   RECORD    WRITE THE VARIANCE TO cst.Variance
      *>   SUMMARY   TOTALS BY VARIANCE TYPE FOR A PERIOD
      *>
      *> REASON CODES
      *>   CST1001 COMPANY MISSING
      *>   CST1002 VARIANCE TYPE NOT RECOGNISED
      *>   CST1003 BASELINE QUANTITY MUST BE DISCLOSED
      *>   CST1004 COMPONENTS DO NOT RECONCILE TO THE TOTAL
      *>   CST1005 PERIOD REQUIRED FOR A RECORDED VARIANCE
      *>   CST1006 UNSUPPORTED FUNCTION
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-06-22 RJH  CR0668 ORIGINAL VARIANCE ANALYSIS
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT PERIOD DATES
      *> 2026-10-06 STERP CST-019 COMPONENTS RECONCILE TO THE TOTAL
      *>================================================================
      *> REQ: CST-019   AC: AC-C5
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  VA-BODY.
               10  VA-VARIANCE-TYPE    PIC X(16).
               10  VA-ITEM-CODE        PIC X(40).
               10  VA-JOB-REF          PIC X(20).
               10  VA-KIT-REF          PIC X(30).
               10  VA-WORK-CENTRE      PIC X(20).
               10  VA-DOC-TYPE         PIC X(20).
               10  VA-DOC-REF          PIC X(40).
               10  VA-FISCAL-YEAR      PIC 9(4).
               10  VA-PERIOD-NO        PIC 9(2).
               10  VA-COMMIT           PIC X.
      *>       ---- inputs ----
               10  VA-BASELINE-QTY     PIC S9(13)V9(6).
               10  VA-ACTUAL-QTY       PIC S9(13)V9(6).
               10  VA-BASELINE-RATE    PIC S9(11)V9(6).
               10  VA-ACTUAL-RATE      PIC S9(11)V9(6).
               10  VA-OUTPUT-QTY       PIC S9(13)V9(6).
               10  VA-QTY-PER-OUTPUT   PIC S9(13)V9(6).
      *>       ---- results ----
               10  VA-ALLOWED-QTY      PIC S9(13)V9(6).
               10  VA-BASELINE-GBP     PIC S9(13)V9(2).
               10  VA-ACTUAL-GBP       PIC S9(13)V9(2).
               10  VA-QTY-VAR-GBP      PIC S9(13)V9(2).
               10  VA-RATE-VAR-GBP     PIC S9(13)V9(2).
               10  VA-TOTAL-VAR-GBP    PIC S9(13)V9(2).
               10  VA-ADVERSE          PIC X.
                   88  VA-IS-ADVERSE             VALUE "Y".
               10  VA-RECONCILED       PIC X.
               10  VA-VARIANCE-ID      PIC S9(15).
               10  VA-REASON-CODE      PIC X(20).
               10  VA-REASON           PIC X(200).
               10  VA-OWNER-USER       PIC X(20).
               10  VA-DISPOSAL         PIC X(16).
               10  VA-WARNING          PIC X(80).
      *>       ---- summary window ----
               10  VA-FROM-LINE        PIC S9(9).
               10  VA-LINE-CNT         PIC S9(4).
               10  VA-MORE-LINES       PIC X.
               10  VA-LINES OCCURS 15 TIMES.
                   15  VA-L-TYPE       PIC X(16).
                   15  VA-L-DESC       PIC X(40).
                   15  VA-L-COUNT      PIC S9(9).
                   15  VA-L-QTY-VAR    PIC S9(13)V9(2).
                   15  VA-L-RATE-VAR   PIC S9(13)V9(2).
                   15  VA-L-TOTAL      PIC S9(13)V9(2).
                   15  VA-L-ADVERSE    PIC X.
