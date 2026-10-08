      *>================================================================
      *> BOMNETCA - COMMAREA of BOMNET, netted supply planning.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : BILLS OF MATERIAL
      *> PURPOSE  : BOM-020.  Eligible free stock is consumed once -
      *>            a supplied subassembly is not also exploded for new
      *>            material for the same units, and a stock unit
      *>            cannot satisfy two demands.  The residual leaf
      *>            demand, its shortages and the proposed procurement
      *>            excess are reported separately from the gross
      *>            structural explosion.
      *> FUNCTIONS :
      *>   NET       netted plan for BN-REQ-QTY of the parent
      *>   MAXBUILD  the largest whole-parent output that passes with
      *>             the same eligible supply (specification B2)
      *>   GETLINE   page the netted result
      *> BN-STOCK-SOURCE: LIVE reads inv.StockBalance / inv.Lot and
      *> deducts inv.StockReservation; TRIAL uses the supplied table so
      *> an engineer can run a what-if without touching live balances.
      *> Reason codes BOM0701..BOM0715.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2015-02-06 AJS  CR2295 NETTED PLANNING ADDED
      *> 2015-04-28 AJS  CR2312 MAXIMUM BUILDABLE OUTPUT
      *> 2026-10-05 STERP TRIAL SUPPLY TABLE FOR WHAT-IF PLANNING
      *>================================================================
      *> REQ: BOM-016, BOM-020, BOM-029, BOM-032
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BN-BODY.
               10  BN-PARENT-ITEM      PIC X(40).
               10  BN-PARENT-REV       PIC X(4).
               10  BN-PURPOSE          PIC X(10).
               10  BN-VARIANT          PIC X(20).
               10  BN-REQ-QTY          PIC S9(15)V9(6).
               10  BN-EFF-DATE         PIC 9(8).
               10  BN-MAX-DEPTH        PIC S9(4).
               10  BN-CALC-REF         PIC X(30).
               10  BN-STOCK-SOURCE     PIC X(8).
               10  BN-PRIORITY-NOTE    PIC X(60).
               10  BN-USE-RESERVED     PIC X.
               10  BN-USE-INBOUND      PIC X.
               10  BN-SUPPLY-CNT       PIC S9(4).
               10  BN-SUPPLY OCCURS 30 TIMES.
                   15  BN-S-ITEM       PIC X(40).
                   15  BN-S-REV        PIC X(4).
                   15  BN-S-FREE-QTY   PIC S9(15)V9(6).
                   15  BN-S-INBOUND    PIC S9(15)V9(6).
                   15  BN-S-USED       PIC S9(15)V9(6).
      *>       ---- result ----
               10  BN-GROSS-STARTS     PIC S9(15)V9(6).
               10  BN-RUN-COUNT        PIC S9(9) COMP-5.
               10  BN-BATCH-COUNT      PIC S9(9) COMP-5.
               10  BN-OCC-COUNT        PIC S9(9) COMP-5.
               10  BN-LEAF-COUNT       PIC S9(9) COMP-5.
               10  BN-SHORTAGE-CNT     PIC S9(4).
               10  BN-SHORTAGE-ITEM    PIC X(40).
               10  BN-SHORTAGE-QTY     PIC S9(15)V9(6).
               10  BN-MAX-BUILDABLE    PIC S9(15)V9(6).
               10  BN-TRIALS           PIC S9(4).
               10  BN-BUILDABLE-NOW    PIC X.
               10  BN-COMPLETENESS     PIC X(12).
               10  BN-ERROR-PATH       PIC X(200).
               10  BN-FROM-LINE        PIC S9(9) COMP-5.
               10  BN-WIN-MAX          PIC S9(4).
               10  BN-WIN-CNT          PIC S9(4).
               10  BN-MORE-LINES       PIC X.
               10  BN-LINES OCCURS 25 TIMES.
                   15  BN-L-SEQ        PIC S9(9) COMP-5.
                   15  BN-L-DEPTH      PIC S9(4).
                   15  BN-L-PATH       PIC X(80).
                   15  BN-L-ITEM       PIC X(40).
                   15  BN-L-REV        PIC X(4).
                   15  BN-L-DESC       PIC X(40).
                   15  BN-L-UOM        PIC X(10).
                   15  BN-L-NODE-TYPE  PIC X(8).
                   15  BN-L-SUPPLY     PIC X(10).
                   15  BN-L-STOP       PIC X(20).
                   15  BN-L-REQ-QTY    PIC S9(15)V9(6).
                   15  BN-L-GOOD-QTY   PIC S9(15)V9(6).
                   15  BN-L-GROSS      PIC S9(15)V9(6).
                   15  BN-L-BATCHES    PIC S9(9) COMP-5.
                   15  BN-L-STOCK-USED PIC S9(15)V9(6).
                   15  BN-L-INBOUND    PIC S9(15)V9(6).
                   15  BN-L-TO-MAKE    PIC S9(15)V9(6).
                   15  BN-L-SHORTAGE   PIC S9(15)V9(6).
                   15  BN-L-PACK-QTY   PIC S9(15)V9(6).
                   15  BN-L-EXCESS     PIC S9(15)V9(6).
                   15  BN-L-WARNING    PIC X(60).
