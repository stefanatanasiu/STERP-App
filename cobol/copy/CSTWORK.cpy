      *>================================================================
      *> CSTWORK  - STERP COSTING COMMON WORKING STORAGE
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : COSTING AND PROFITABILITY
      *> PURPOSE  : FIELDS EVERY COSTING PROGRAM NEEDS - SQL RESULT
      *>            STAGING, NUMERIC EDITING AND THE COST ELEMENT
      *>            BUCKETS THAT CST-002 REQUIRES TO RECONCILE TO THE
      *>            TOTAL COST.
      *> NOTE     : ROW AREAS ARE PLAIN X(100) COLUMNS BECAUSE SQLIO
      *>            HANDS EVERY COLUMN BACK AS LEFT JUSTIFIED TEXT.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-03-17 RJH  CR0502 ORIGINAL COSTING WORK AREA
      *> 1999-11-30 KMP  Y2K - WS-CENTURY WINDOW RETIRED, 4 DIGIT YEARS
      *> 2026-10-06 STERP CST-002 ELEMENT BUCKETS
      *>================================================================
      *> REQ: CST-002
       01  WS-CST-WORK.
           05  WS-SQL-FAILED           PIC X VALUE "N".
               88  WS-SQL-BAD                    VALUE "Y".
           05  WS-ROW-FOUND            PIC X VALUE "N".
               88  WS-GOT-ROW                    VALUE "Y".
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-IX2                  PIC S9(4) VALUE 0.
           05  WS-IX3                  PIC S9(4) VALUE 0.
           05  WS-COUNT                PIC S9(9) VALUE 0.
           05  WS-DATE-TEXT            PIC X(10) VALUE SPACES.
           05  WS-DATE-NUM             PIC 9(8) VALUE 0.
           05  WS-TODAY                PIC 9(8) VALUE 0.
           05  WS-WORK-TEXT            PIC X(200) VALUE SPACES.
           05  WS-NUM-TEXT             PIC X(100) VALUE SPACES.
           05  WS-MONEY-ED             PIC -(11)9.99.
           05  WS-QTY-ED               PIC -(11)9.999999.
           05  WS-RATE-ED              PIC -(7)9.999999.
           05  WS-PCT-ED               PIC -(4)9.99.
           05  WS-INT-ED               PIC -(8)9.
      *>   working amounts - twelve decimals before any rounding
           05  WS-CALC-1               PIC S9(13)V9(12) VALUE 0.
           05  WS-CALC-2               PIC S9(13)V9(12) VALUE 0.
           05  WS-CALC-3               PIC S9(13)V9(12) VALUE 0.
           05  WS-AMT1                 PIC S9(13)V9(6) VALUE 0.
           05  WS-AMT2                 PIC S9(13)V9(6) VALUE 0.
           05  WS-AMT3                 PIC S9(13)V9(6) VALUE 0.
           05  WS-QTY1                 PIC S9(13)V9(6) VALUE 0.
           05  WS-QTY2                 PIC S9(13)V9(6) VALUE 0.
           05  WS-UNIT1                PIC S9(11)V9(6) VALUE 0.
      *>   generic one row landing area for SNGL lookups
       01  WS-CST-ROW.
           05  WS-COL                  PIC X(100) OCCURS 24 TIMES.
      *>   element buckets - CST-002 total must equal the sum of these
       01  WS-ELEMENT-TOTALS.
           05  WS-EL-MATERIAL          PIC S9(13)V9(2) VALUE 0.
           05  WS-EL-LABOUR            PIC S9(13)V9(2) VALUE 0.
           05  WS-EL-MACHINE           PIC S9(13)V9(2) VALUE 0.
           05  WS-EL-SUBCON            PIC S9(13)V9(2) VALUE 0.
           05  WS-EL-OVHDVAR           PIC S9(13)V9(2) VALUE 0.
           05  WS-EL-OVHDFIX           PIC S9(13)V9(2) VALUE 0.
           05  WS-EL-LANDED            PIC S9(13)V9(2) VALUE 0.
           05  WS-EL-RECOVERY          PIC S9(13)V9(2) VALUE 0.
           05  WS-EL-VARIANCE          PIC S9(13)V9(2) VALUE 0.
           05  WS-EL-TOTAL             PIC S9(13)V9(2) VALUE 0.
           05  WS-EL-EXCLUDED          PIC S9(13)V9(2) VALUE 0.
           05  WS-EL-EXCL-NOTE         PIC X(60) VALUE SPACES.
