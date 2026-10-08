      *>================================================================
      *> BOMWKCTL - control record of the explosion work pool BOMWORK.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : BILLS OF MATERIAL
      *> PURPOSE  : BOMWORK owns the occurrence pool and the aggregated
      *>            leaf pool for one calculation.  Every program of the
      *>            module reaches them through this record:
      *>              CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
      *>            Functions (BW-FUNC):
      *>              CLEAR    empty both pools, set the capacity
      *>              ADD      append BOM-OCC-ROW, BW-INDEX = its seq
      *>              GET      read occurrence BW-INDEX
      *>              PUT      overwrite occurrence BW-INDEX
      *>              COUNT    BW-COUNT = occurrences held
      *>              AGGREG   build the leaf pool from the occurrence
      *>                       pool, merging compatible identity /
      *>                       revision / unit / site / supply context
      *>              LEAFCNT  BW-COUNT = aggregated leaf lines
      *>              GETLEAF  read leaf line BW-INDEX (BO-SEQ holds the
      *>                       number of contributing occurrences)
      *>              FINDLEAF find the leaf line for BW-KEY-ITEM /
      *>                       BW-KEY-REV / BW-KEY-UOM
      *>              TOTAL    BW-TOTAL = sum of BO-REQ-QTY for
      *>                       BW-KEY-ITEM over the whole pool
      *>              COSTSUM  BW-TOTAL = sum of BO-EXT-COST, filtered
      *>                       by BW-FILTER (ALL / LEAF / INTERMED /
      *>                       BRANCH where BW-INDEX is the branch root)
      *>            BW-STATUS is OK, FULL, NOTFOUND or BADFUNC - a full
      *>            pool is never silently truncated (BOM-029).
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2014-03-17 AJS  CR2201 ENGINEERING REWRITE
      *> 2016-11-08 AJS  CR2502 COSTSUM FOR ONE VALUE BOUNDARY PER
      *>                 BRANCH (BOM-021)
      *>================================================================
      *> REQ: BOM-015, BOM-019, BOM-021, BOM-029
       01  BOM-WK-CTL.
           05  BW-FUNC                 PIC X(8).
           05  BW-INDEX                PIC S9(9) COMP-5.
           05  BW-COUNT                PIC S9(9) COMP-5.
           05  BW-CAPACITY             PIC S9(9) COMP-5.
           05  BW-STATUS               PIC X(8).
               88  BW-OK                         VALUE "OK".
               88  BW-FULL                       VALUE "FULL".
               88  BW-NOTFOUND                   VALUE "NOTFOUND".
               88  BW-BADFUNC                    VALUE "BADFUNC".
           05  BW-KEY-ITEM             PIC X(40).
           05  BW-KEY-REV              PIC X(4).
           05  BW-KEY-UOM              PIC X(10).
           05  BW-FILTER               PIC X(8).
           05  BW-TOTAL                PIC S9(15)V9(6).
           05  BW-MONEY                PIC S9(13)V9(2).
           05  BW-MAX-DEPTH            PIC S9(4).
