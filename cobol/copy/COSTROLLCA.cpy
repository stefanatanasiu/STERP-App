      *>================================================================
      *> COSTROLLCA - COMMAREA FOR COSTROLL, BOM COST ROLL-UP
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *>
      *> COSTING DOES NOT EXPLODE BILLS OF MATERIAL.  THE ENGINEERING
      *> MODULE OWNS THE REQUIRED QUANTITIES, SO COSTROLL CALLS BOMCOST
      *> (WHICH ITSELF USES BOMEXPL AND BOMNET) AND CONSUMES THE RESULT.
      *> A SECOND, SUBTLY DIFFERENT EXPLOSION IS FORBIDDEN (CST-010).
      *>
      *>   estimated output cost = component cost + conversion cost
      *>                         + applicable overhead + other charges
      *>                         - approved by-product recovery
      *>   estimated unit cost   = output cost / planned good output
      *>
      *> ONE VALUATION BOUNDARY PER BRANCH: EITHER THE SUBASSEMBLY'S
      *> STATED UNIT COST IS USED AND EXPANSION STOPS THERE, OR THE
      *> SUBASSEMBLY IS EXPANDED AND ITS OWN FINISHED UNIT COST IS NOT
      *> ADDED AS WELL.  BOTH MAY BE DISPLAYED, ONLY ONE CONTRIBUTES.
      *>
      *> THE COSTING LOT SIZE IS STATED BECAUSE A FIXED SETUP CHARGE
      *> CANNOT BECOME A PERMANENT UNIT COST WITHOUT A DENOMINATOR.
      *>
      *> CA-FUNCTION VALUES
      *>   ROLLUP    ROLL UP CR-ITEM-CODE FOR CR-LOT-SIZE GOOD OUTPUT
      *>   GETLINE   PAGE THE COSTED TREE FROM CR-FROM-LINE
      *>   SAVESET   WRITE THE RESULT INTO A DRAFT COST SET
      *>   EXPLAIN   BREAKDOWN AND COMPLETENESS OF THE LAST ROLL-UP
      *>
      *> REASON CODES
      *>   CST0601 COMPANY OR ITEM MISSING
      *>   CST0602 LOT SIZE MUST BE POSITIVE
      *>   CST0603 BOM EXPLOSION FAILED - SEE CR-ERROR-PATH
      *>   CST0604 MISSING COMPONENT COST - ROLL-UP INCOMPLETE
      *>   CST0605 MISSING WORK CENTRE RATE - ROLL-UP INCOMPLETE
      *>   CST0606 BOUNDARY RULE NOT RECOGNISED
      *>   CST0607 COST SET IS NOT A DRAFT - CANNOT BE WRITTEN TO
      *>   CST0608 UNSUPPORTED FUNCTION
      *>   CST0609 RECOVERY CREDIT WOULD DRIVE THE COST BELOW ZERO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-09-19 DJM  CR0577 ORIGINAL ROLL-UP (SINGLE LEVEL)
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT EFFECTIVE DATES
      *> 2015-07-01 AJS  CR2330 USE THE ENGINEERING EXPLOSION, NOT OURS
      *> 2026-10-06 STERP CST-010 ONE VALUE BOUNDARY PER BRANCH
      *>================================================================
      *> REQ: CST-002, CST-010, CST-024
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  CR-BODY.
               10  CR-ITEM-CODE        PIC X(40).
               10  CR-REVISION         PIC X(4).
               10  CR-COST-SET         PIC X(20).
               10  CR-TARGET-SET       PIC X(20).
               10  CR-EFF-DATE         PIC 9(8).
               10  CR-COST-DATE        PIC 9(8).
               10  CR-LOT-SIZE         PIC S9(13)V9(6).
               10  CR-CURRENCY         PIC X(3).
               10  CR-BOUNDARY         PIC X(10).
                   88  CR-STOP-AT-SUB            VALUE "STOP".
                   88  CR-EXPAND-SUB             VALUE "EXPAND".
               10  CR-MAX-DEPTH        PIC S9(4).
               10  CR-NETTED           PIC X.
               10  CR-WORK-CENTRE      PIC X(20).
      *>       ---- results ----
               10  CR-GROSS-STARTS     PIC S9(13)V9(6).
               10  CR-GOOD-OUTPUT      PIC S9(13)V9(6).
               10  CR-MATERIAL-COST    PIC S9(13)V9(2).
               10  CR-LABOUR-COST      PIC S9(13)V9(2).
               10  CR-MACHINE-COST     PIC S9(13)V9(2).
               10  CR-SETUP-COST       PIC S9(13)V9(2).
               10  CR-SUBCON-COST      PIC S9(13)V9(2).
               10  CR-OVERHEAD-COST    PIC S9(13)V9(2).
               10  CR-OTHER-COST       PIC S9(13)V9(2).
               10  CR-RECOVERY-CREDIT  PIC S9(13)V9(2).
               10  CR-TOTAL-COST       PIC S9(13)V9(2).
               10  CR-UNIT-COST        PIC S9(11)V9(6).
               10  CR-BRANCH-VALUE     PIC S9(13)V9(2).
               10  CR-OCC-COUNT        PIC S9(9).
               10  CR-MISSING-COUNT    PIC S9(4).
               10  CR-COMPLETENESS     PIC X(12).
                   88  CR-COMPLETE               VALUE "COMPLETE".
               10  CR-ERROR-PATH       PIC X(200).
               10  CR-SET-WRITTEN      PIC S9(9).
               10  CR-WARNING          PIC X(80).
      *>       ---- costed tree window ----
               10  CR-FROM-LINE        PIC S9(9).
               10  CR-WIN-MAX          PIC S9(4).
               10  CR-LINE-CNT         PIC S9(4).
               10  CR-MORE-LINES       PIC X.
               10  CR-LINES OCCURS 20 TIMES.
                   15  CR-L-SEQ        PIC S9(9).
                   15  CR-L-DEPTH      PIC S9(4).
                   15  CR-L-ITEM       PIC X(40).
                   15  CR-L-REV        PIC X(4).
                   15  CR-L-DESC       PIC X(40).
                   15  CR-L-ELEMENT    PIC X(10).
                   15  CR-L-SOURCE     PIC X(12).
                   15  CR-L-BOUNDARY   PIC X(8).
                   15  CR-L-REQ-QTY    PIC S9(13)V9(6).
                   15  CR-L-UNIT-COST  PIC S9(11)V9(6).
                   15  CR-L-EXT-COST   PIC S9(13)V9(2).
                   15  CR-L-CONTRIB    PIC X.
                   15  CR-L-WARNING    PIC X(60).
