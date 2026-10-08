      *>================================================================
      *> NRVTESTCA - COMMAREA FOR NRVTEST, NET REALISABLE VALUE TESTING
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : COSTING AND PROFITABILITY
      *>
      *> FRS 102 SECTIONS 13 AND 27.  AT EACH REPORTING DATE INVENTORY
      *> IS MEASURED AT THE LOWER OF COST AND ESTIMATED SELLING PRICE
      *> LESS COSTS TO COMPLETE AND SELL, ITEM BY ITEM OR BY GROUPS OF
      *> SIMILAR ITEMS.  QUANTITY NEVER CHANGES - ONLY VALUE (CST-021).
      *>
      *>   nrv unit = estimated selling price - costs to complete/sell
      *>   write-down = (carrying unit cost - nrv unit) x quantity
      *>
      *> A REVERSAL IS PERMITTED WHEN CIRCUMSTANCES CHANGE BUT CAN
      *> NEVER TAKE CARRYING VALUE ABOVE ORIGINAL COST (CST-025).
      *>
      *> CA-FUNCTION VALUES
      *>   TEST      TEST ONE ITEM AND PROPOSE A WRITE-DOWN
      *>   SCAN      TEST EVERY VALUED POOL AND LIST THE EXPOSURES
      *>   APPROVE   APPROVE A PROPOSED IMPAIRMENT
      *>   POST      POST AN APPROVED IMPAIRMENT TO THE ALLOWANCE
      *>   REVERSE   REVERSE AN EARLIER WRITE-DOWN, CAPPED AT COST
      *>
      *> REASON CODES
      *>   CST1101 COMPANY OR ITEM MISSING
      *>   CST1102 ASSESSMENT DATE REQUIRED
      *>   CST1103 NO CARRYING VALUE FOR THAT POOL
      *>   CST1104 SELLING PRICE EVIDENCE REQUIRED
      *>   CST1105 A REVERSAL MAY NOT EXCEED THE EARLIER WRITE-DOWN
      *>   CST1106 CARRYING VALUE MAY NOT EXCEED ORIGINAL COST
      *>   CST1107 IMPAIRMENT NOT FOUND
      *>   CST1108 APPROVER MAY NOT BE THE PREPARER
      *>   CST1109 UNSUPPORTED FUNCTION
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-09-08 RJH  CR0702 ORIGINAL STOCK PROVISION
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT ASSESSMENT DATES
      *> 2015-04-06 DJM  CR2301 FRS 102 REPLACES SSAP 9
      *> 2026-10-06 STERP CST-021 CST-025 REVERSAL CAPPED AT COST
      *>================================================================
      *> REQ: CST-021, CST-025
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  NR-BODY.
               10  NR-ITEM-CODE        PIC X(40).
               10  NR-REVISION         PIC X(4).
               10  NR-ITEM-GROUP       PIC X(20).
               10  NR-ASSESS-DATE      PIC 9(8).
               10  NR-QUANTITY         PIC S9(13)V9(6).
               10  NR-CARRYING-UNIT    PIC S9(11)V9(6).
               10  NR-SELLING-PRICE    PIC S9(11)V9(6).
               10  NR-COST-TO-SELL     PIC S9(11)V9(6).
               10  NR-NRV-UNIT         PIC S9(11)V9(6).
               10  NR-ORIGINAL-UNIT    PIC S9(11)V9(6).
               10  NR-REASON-CODE      PIC X(20).
               10  NR-EVIDENCE         PIC X(300).
               10  NR-IMPAIRMENT-ID    PIC S9(9).
               10  NR-REVERSE-OF-ID    PIC S9(9).
               10  NR-COMMIT           PIC X.
      *>       ---- results ----
               10  NR-CARRYING-GBP     PIC S9(13)V9(2).
               10  NR-NRV-GBP          PIC S9(13)V9(2).
               10  NR-WRITEDOWN-GBP    PIC S9(13)V9(2).
               10  NR-REVERSAL-GBP     PIC S9(13)V9(2).
               10  NR-ALLOWANCE-GBP    PIC S9(13)V9(2).
               10  NR-NEW-CARRYING     PIC S9(13)V9(2).
               10  NR-IMPAIRED         PIC X.
                   88  NR-IS-IMPAIRED            VALUE "Y".
               10  NR-CAPPED           PIC X.
               10  NR-STATUS           PIC X(12).
               10  NR-ITEMS-TESTED     PIC S9(9).
               10  NR-ITEMS-IMPAIRED   PIC S9(9).
               10  NR-WARNING          PIC X(80).
      *>       ---- exposure window ----
               10  NR-FROM-LINE        PIC S9(9).
               10  NR-LINE-CNT         PIC S9(4).
               10  NR-MORE-LINES       PIC X.
               10  NR-LINES OCCURS 20 TIMES.
                   15  NR-L-ITEM       PIC X(40).
                   15  NR-L-DESC       PIC X(40).
                   15  NR-L-QTY        PIC S9(13)V9(6).
                   15  NR-L-UNIT-COST  PIC S9(11)V9(6).
                   15  NR-L-NRV-UNIT   PIC S9(11)V9(6).
                   15  NR-L-WRITEDOWN  PIC S9(13)V9(2).
                   15  NR-L-REASON     PIC X(20).
