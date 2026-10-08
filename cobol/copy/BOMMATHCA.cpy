      *>================================================================
      *> BOMMATHCA - COMMAREA of BOMMATH, the required quantity
      *>             mathematics of specification section 5.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : BILLS OF MATERIAL
      *> FUNCTIONS :
      *>   PARENT  G_raw = Q / Y,  G = ceil(G_raw / g) x g,
      *>           N = ceil(G / L) when Q > 0,  R defaults to 1.
      *>           Q = 0 gives zero starts, zero runs and zero batches.
      *>   COMPON  proportional    (G / B) x q x f x (1 + s)
      *>           fixed per run   R x q x f x (1 + s)
      *>           fixed per batch N x q x f x (1 + s)
      *>   ROUNDUP round up once to the item's allowed stock quantum,
      *>           keeping the unrounded amount and the increment.
      *>   PACK    procurement minimum / multiple proposal and the
      *>           excess it creates - a separate decision from the
      *>           engineering issue requirement.
      *>   CEIL    ceiling of BM-VALUE to BM-QUANTUM (shared helper).
      *> Reason codes BOM0101..BOM0112.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2014-03-17 AJS  CR2201 ENGINEERING REWRITE
      *> 2014-09-30 AJS  CR2240 SCRAP IS AN ADDITIONAL INPUT FRACTION,
      *>                 NOT A DIVISION BY (1 - s)          (BOM-009)
      *> 2026-10-05 STERP D-10 CONVENTIONS CONFIRMED
      *>================================================================
      *> REQ: BOM-006, BOM-007, BOM-008, BOM-009, BOM-010, BOM-011
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BM-BODY.
               10  BM-GOOD-QTY         PIC S9(15)V9(6).
               10  BM-YIELD            PIC S9(5)V9(6).
               10  BM-QUANTUM          PIC S9(13)V9(6).
               10  BM-BATCH-SIZE       PIC S9(13)V9(6).
               10  BM-NOMINAL-BASIS    PIC S9(13)V9(6).
               10  BM-USAGE-QTY        PIC S9(13)V9(6).
               10  BM-CONV-FACT        PIC S9(9)V9(9).
               10  BM-SCRAP-PCT        PIC S9(5)V9(6).
               10  BM-USAGE-BASIS      PIC X(12).
               10  BM-RUN-COUNT        PIC S9(9) COMP-5.
               10  BM-BATCH-COUNT      PIC S9(9) COMP-5.
               10  BM-GROSS-RAW        PIC S9(15)V9(9).
               10  BM-GROSS-STARTS     PIC S9(15)V9(6).
               10  BM-UNROUNDED        PIC S9(15)V9(9).
               10  BM-YIELD-CONTRIB    PIC S9(15)V9(9).
               10  BM-SCRAP-CONTRIB    PIC S9(15)V9(9).
               10  BM-REQ-QTY          PIC S9(15)V9(6).
               10  BM-ROUND-INC        PIC S9(13)V9(6).
               10  BM-PACK-MULT        PIC S9(13)V9(6).
               10  BM-MIN-ORDER        PIC S9(13)V9(6).
               10  BM-FREE-STOCK       PIC S9(15)V9(6).
               10  BM-PACK-QTY         PIC S9(15)V9(6).
               10  BM-EXCESS           PIC S9(15)V9(6).
               10  BM-VALUE            PIC S9(15)V9(9).
               10  BM-RESULT           PIC S9(15)V9(6).
               10  BM-EXPLAIN          PIC X(120).
