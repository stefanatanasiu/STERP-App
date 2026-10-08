      *>================================================================
      *> LABOVHCA  - COMMAREA FOR LABOVH, LABOUR, RESOURCE AND
      *>             OVERHEAD ABSORPTION
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : COSTING AND PROFITABILITY
      *>
      *>   labour = setup hours x setup rate x execution runs
      *>          + run hours per gross unit x gross units x run rate
      *>
      *> SETUP IS CHARGED PER RUN, NEVER PER UNIT.  OVERHEAD (D-05) IS
      *> A RATE PER DIRECT LABOUR OR MACHINE HOUR DERIVED FROM BUDGETED
      *> PRODUCTION OVERHEAD DIVIDED BY HOURS AT NORMAL CAPACITY.  THE
      *> PERCENTAGE BASE EXCLUDES OVERHEAD ITSELF AND EXCLUDES ALREADY
      *> ABSORBED OVERHEAD, SO THE CALCULATION STAYS NON CIRCULAR
      *> (CST-011).  UNABSORBED FIXED OVERHEAD CAUSED BY PRODUCTION
      *> BELOW NORMAL CAPACITY IS A PERIOD EXPENSE, NOT STOCK VALUE
      *> (CST-026).
      *>
      *> CA-FUNCTION VALUES
      *>   RATES     EFFECTIVE WORK CENTRE RATES AT LO-EFF-DATE
      *>   LABOUR    SETUP AND RUN LABOUR FOR LO-GROSS-UNITS
      *>   OVERHEAD  ABSORB OVERHEAD ON THE STATED BASIS
      *>   ABSORB    PERIOD ABSORPTION AGAINST NORMAL CAPACITY AND THE
      *>             RESULTING OVER OR UNDER RECOVERY
      *>   OVHRATE   DERIVE A RATE FROM BUDGET / NORMAL CAPACITY HOURS
      *>
      *> REASON CODES
      *>   CST0501 COMPANY OR WORK CENTRE MISSING
      *>   CST0502 WORK CENTRE NOT EFFECTIVE AT THAT DATE
      *>   CST0503 HOURS MUST NOT BE NEGATIVE
      *>   CST0504 RUN COUNT MUST BE POSITIVE
      *>   CST0505 NORMAL CAPACITY HOURS MUST BE POSITIVE
      *>   CST0506 OVERHEAD BASIS NOT RECOGNISED
      *>   CST0507 PERCENTAGE BASE WOULD BE CIRCULAR
      *>   CST0508 UNSUPPORTED FUNCTION
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-08-04 DJM  CR0560 ORIGINAL LABOUR AND BURDEN
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT EFFECTIVE DATES
      *> 2010-03-22 RJH  CR1755 NORMAL CAPACITY BASIS FOR FIXED BURDEN
      *> 2026-10-06 STERP CST-011 CST-026 NON CIRCULAR BASE
      *>================================================================
      *> REQ: CST-011, CST-026, D-05
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  LO-BODY.
               10  LO-WORK-CENTRE      PIC X(20).
               10  LO-EFF-DATE         PIC 9(8).
               10  LO-ITEM-CODE        PIC X(40).
               10  LO-JOB-REF          PIC X(20).
      *>       ---- quantities and hours ----
               10  LO-GROSS-UNITS      PIC S9(13)V9(6).
               10  LO-RUN-COUNT        PIC S9(9).
               10  LO-BATCH-COUNT      PIC S9(9).
               10  LO-SETUP-HOURS      PIC S9(9)V9(6).
               10  LO-RUN-HOURS-UNIT   PIC S9(9)V9(6).
               10  LO-MACHINE-HRS-UNIT PIC S9(9)V9(6).
               10  LO-RATE-BASIS       PIC X(8).
                   88  LO-PER-UNIT               VALUE "UNIT".
                   88  LO-PER-BATCH              VALUE "BATCH".
                   88  LO-PER-RUN                VALUE "RUN".
      *>       ---- rates (zero means take the work centre rate) ----
               10  LO-SETUP-RATE       PIC S9(11)V9(6).
               10  LO-RUN-RATE         PIC S9(11)V9(6).
               10  LO-MACHINE-RATE     PIC S9(11)V9(6).
               10  LO-OVHD-RATE        PIC S9(11)V9(6).
               10  LO-OVHD-BASIS       PIC X(10).
                   88  LO-ON-LABOUR-HR           VALUE "LABOURHR".
                   88  LO-ON-MACHINE-HR          VALUE "MACHINEHR".
                   88  LO-ON-PCT-COST            VALUE "PCTCOST".
               10  LO-OVHD-PCT         PIC S9(5)V9(4).
               10  LO-BASE-COST        PIC S9(13)V9(2).
               10  LO-BASE-EXCL-OVHD   PIC X.
                   88  LO-EXCLUDE-OVHD           VALUE "Y".
               10  LO-EMBEDDED-OVHD    PIC S9(13)V9(2).
      *>       ---- normal capacity (CST-026) ----
               10  LO-NORMAL-CAP-HRS   PIC S9(11)V9(6).
               10  LO-ACTUAL-HOURS     PIC S9(11)V9(6).
               10  LO-BUDGET-OVHD-GBP  PIC S9(13)V9(2).
      *>       ---- results ----
               10  LO-SETUP-COST       PIC S9(13)V9(2).
               10  LO-RUN-COST         PIC S9(13)V9(2).
               10  LO-LABOUR-COST      PIC S9(13)V9(2).
               10  LO-MACHINE-COST     PIC S9(13)V9(2).
               10  LO-OVHD-COST        PIC S9(13)V9(2).
               10  LO-TOTAL-COST       PIC S9(13)V9(2).
               10  LO-UNIT-COST        PIC S9(11)V9(6).
               10  LO-TOTAL-HOURS      PIC S9(11)V9(6).
               10  LO-DERIVED-RATE     PIC S9(11)V9(6).
               10  LO-ABSORBED-GBP     PIC S9(13)V9(2).
               10  LO-INCURRED-GBP     PIC S9(13)V9(2).
               10  LO-RECOVERY-GBP     PIC S9(13)V9(2).
               10  LO-UNDER-RECOVERED  PIC X.
                   88  LO-IS-UNDER               VALUE "Y".
               10  LO-CAPACITY-PCT     PIC S9(5)V9(4).
               10  LO-BASIS-TEXT       PIC X(60).
               10  LO-WARNING          PIC X(80).
