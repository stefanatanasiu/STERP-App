      *>================================================================
      *> BOMCOSTCA - COMMAREA of BOMCOST, costed explosion / roll-up.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : BILLS OF MATERIAL
      *> PURPOSE   : BOM-021.  One value boundary per branch: a branch
      *>            roll-up value is reported for presentation and is
      *>            NEVER added again on top of the expanded leaf
      *>            values it already contains.  Material comes from
      *>            the stated cost set (cst.StandardCost) or, when the
      *>            set holds nothing for the item, from the item's
      *>            standard cost column (cst.ItemCost.StandardUnitCost)
      *>            for the requesting site - the item level (blank
      *>            revision) ahead of a revision or a site of its own,
      *>            so a valuation pool written elsewhere can never
      *>            become the engineering rate.
      *>            Conversion comes from the parent bill's setup per
      *>            run and conversion per gross start.  A cost-only
      *>            resource with no rate prevents a complete cost
      *>            (BOM-027) and a by-product recovery is a separate
      *>            positive output, never a negative consumption
      *>            (BOM-026).
      *> FUNCTIONS :
      *>   COST     costed explosion of BC-REQ-QTY good parent output
      *>   GETLINE  page the costed result
      *>   BRANCH   roll-up value of the branch rooted at BC-BRANCH-SEQ
      *> Reason codes BOM0801..BOM0818.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2015-06-15 AJS  CR2324 COST ROLL-UP ADDED
      *> 2016-02-29 AJS  CR2431 BY-PRODUCT RECOVERY CREDIT (BOM-026)
      *> 2026-10-05 STERP MISSING RATE BLOCKS A COMPLETE COST
      *>================================================================
      *> REQ: BOM-021, BOM-026, BOM-027, BOM-032
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BC-BODY.
               10  BC-PARENT-ITEM      PIC X(40).
               10  BC-PARENT-REV       PIC X(4).
               10  BC-PURPOSE          PIC X(10).
               10  BC-VARIANT          PIC X(20).
               10  BC-REQ-QTY          PIC S9(15)V9(6).
               10  BC-EFF-DATE         PIC 9(8).
               10  BC-MAX-DEPTH        PIC S9(4).
               10  BC-CALC-REF         PIC X(30).
               10  BC-COST-SET         PIC X(20).
               10  BC-CURRENCY         PIC X(3).
               10  BC-COST-DATE        PIC 9(8).
               10  BC-NETTED           PIC X.
      *>       ---- result ----
               10  BC-GROSS-STARTS     PIC S9(15)V9(6).
               10  BC-RUN-COUNT        PIC S9(9) COMP-5.
               10  BC-BATCH-COUNT      PIC S9(9) COMP-5.
               10  BC-OCC-COUNT        PIC S9(9) COMP-5.
               10  BC-MATERIAL-COST    PIC S9(13)V9(2).
               10  BC-CONVERSION-COST  PIC S9(13)V9(2).
               10  BC-SETUP-COST       PIC S9(13)V9(2).
               10  BC-SERVICE-COST     PIC S9(13)V9(2).
               10  BC-RECOVERY-CREDIT  PIC S9(13)V9(2).
               10  BC-TOTAL-COST       PIC S9(13)V9(2).
               10  BC-UNIT-COST        PIC S9(11)V9(6).
               10  BC-UNIT-DENOM       PIC S9(15)V9(6).
               10  BC-MISSING-RATES    PIC S9(4).
               10  BC-COMPLETENESS     PIC X(12).
               10  BC-ERROR-PATH       PIC X(200).
               10  BC-BRANCH-SEQ       PIC S9(9) COMP-5.
               10  BC-BRANCH-ITEM      PIC X(40).
               10  BC-BRANCH-QTY       PIC S9(15)V9(6).
               10  BC-BRANCH-COST      PIC S9(13)V9(2).
               10  BC-BRANCH-UNIT      PIC S9(11)V9(6).
               10  BC-FROM-LINE        PIC S9(9) COMP-5.
               10  BC-WIN-MAX          PIC S9(4).
               10  BC-WIN-CNT          PIC S9(4).
               10  BC-MORE-LINES       PIC X.
               10  BC-LINES OCCURS 25 TIMES.
                   15  BC-L-SEQ        PIC S9(9) COMP-5.
                   15  BC-L-DEPTH      PIC S9(4).
                   15  BC-L-PATH       PIC X(80).
                   15  BC-L-ITEM       PIC X(40).
                   15  BC-L-REV        PIC X(4).
                   15  BC-L-DESC       PIC X(40).
                   15  BC-L-UOM        PIC X(10).
                   15  BC-L-NODE-TYPE  PIC X(8).
                   15  BC-L-ELEMENT    PIC X(10).
                   15  BC-L-SOURCE     PIC X(12).
                   15  BC-L-REQ-QTY    PIC S9(15)V9(6).
                   15  BC-L-UNIT-COST  PIC S9(11)V9(6).
                   15  BC-L-EXT-COST   PIC S9(13)V9(2).
                   15  BC-L-BRANCH     PIC S9(13)V9(2).
                   15  BC-L-FIXED      PIC X.
                   15  BC-L-WARNING    PIC X(60).
