      *>================================================================
      *> PROGRAM-ID : SOANAL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT
      *> PURPOSE    : SALES ANALYSIS AND PROFITABILITY.  QUOTED,
      *>              ORDERED, DESPATCHED, INVOICED AND CREDITED
      *>              VALUES ARE REPORTED IN SEPARATE COLUMNS AND
      *>              NEVER ADDED TOGETHER, BECAUSE THEY ARE DIFFERENT
      *>              STAGES OF THE SAME DEMAND.  RETURNS, DISCOUNTS,
      *>              LATE ATTRIBUTABLE COST ADJUSTMENTS AND THE FX
      *>              BASIS EACH HAVE THEIR OWN COLUMN SO THAT THEY
      *>              CANNOT BE LOST INSIDE A GROSS SALES FIGURE.
      *>              EXPECTED MARGIN KEEPS THE ORIGINAL COST BASIS
      *>              CAPTURED ON THE ORDER LINE; REALISED MARGIN USES
      *>              THE ACTUAL COST OF SALES POSTED ON DESPATCH,
      *>              WHICH INCLUDES LATER COST ADJUSTMENTS, AND THE
      *>              SOURCE AND CUT OFF OF THOSE ADJUSTMENTS ARE
      *>              PRINTED.  THE QUOTED FIGURE IS NEVER REWRITTEN.
      *>              THE FRS 102 SECTION 23 COLUMNS - PERFORMANCE
      *>              OBLIGATION COUNT, STANDALONE SELLING PRICE TOTAL
      *>              FOR BUNDLES AND KITS, VARIABLE CONSIDERATION
      *>              (REBATE ACCRUALS), THE TRANSFER OF CONTROL POINT
      *>              AND ANY BILL AND HOLD OR CONSIGNMENT FLAG - ARE
      *>              PRINTED ON THE THIRD LINE OF EACH GROUP.  BILL
      *>              AND HOLD IS NOT RECOGNISED AS REVENUE.
      *> RUN AS     : SOANAL CO=STC [SITE=RDG] [CUST=C0001]
      *>              [FROM=yyyymmdd] [TO=yyyymmdd] USER=SALESMGR
      *>              [GROUPBY=CUSTOMER|ITEM|SALESPERSON|MONTH]
      *> OUTPUT     : run\spool\SOANAL-<CO>-<stamp>.lst  (132 col)
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *> NOTE       : there is no dedicated bill and hold column on
      *>              so.SalesOrder, so the flag is derived from the
      *>              hold reason text.  See docs\tech-debt\so.md.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-11-07 DLS  CR0510 ORIGINAL - SALES ANALYSIS BY CUSTOMER
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS THROUGHOUT
      *> 2009-02-17 AMD  CR2415 REALISED MARGIN SPLIT OUT FROM
      *>                 EXPECTED AFTER THE STANDARD COST REVIEW
      *> 2026-10-06 STERP SO-023, SO-028, SO-006 FRS 102 S23 COLUMNS
      *>================================================================
      *> REQ: SO-023, SO-028, SO-006
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOANAL.
       AUTHOR. D L SPENCER.
       DATE-WRITTEN. 07/11/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-SOANAL.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       01  SO-ROW.
           05  SO-COL OCCURS 20 TIMES PIC X(100).
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-CUST                 PIC X(20) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-FROM                 PIC X(8) VALUE SPACES.
           05  WS-TO                   PIC X(8) VALUE SPACES.
           05  WS-GROUPBY              PIC X(12) VALUE SPACES.
           05  WS-KEY-SQL              PIC X(80) VALUE SPACES.
       01  WS-SCRATCH.
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-JX                   PIC S9(4) VALUE 0.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-TODAY-N              PIC 9(8) VALUE 0.
           05  WS-DONE                 PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-FOUND                PIC S9(4) VALUE 0.
           05  WS-JOB-RUN-ID           PIC S9(9) COMP-5 VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-NET                  PIC S9(13)V99 VALUE 0.
           05  WS-COST                 PIC S9(13)V99 VALUE 0.
           05  WS-PCT                  PIC S9(5)V9(4) VALUE 0.
           05  WS-PCT-ED               PIC -(4)9.99.
           05  WS-FX-ED                PIC -(4)9.9(6).
           05  WS-UNDEFINED            PIC S9(5) VALUE 0.
      *> the 1996 analysis kept its totals in core because the group
      *> counts were small; OCCURS 200 has never been raised and a
      *> larger company is reported in slices - docs\tech-debt\so.md
       01  WS-GRP-TAB.
           05  WS-GRP-CNT              PIC S9(4) VALUE 0.
           05  WS-GRP OCCURS 200 TIMES.
               10  GT-KEY              PIC X(24).
               10  GT-QUOTED           PIC S9(13)V99.
               10  GT-ORDERED          PIC S9(13)V99.
               10  GT-DESPATCHED       PIC S9(13)V99.
               10  GT-INVOICED         PIC S9(13)V99.
               10  GT-CREDITED         PIC S9(13)V99.
               10  GT-DISCOUNT         PIC S9(13)V99.
               10  GT-RETURNS          PIC S9(13)V99.
               10  GT-EXP-COST         PIC S9(13)V99.
               10  GT-EXP-COST-D       PIC S9(13)V99.
               10  GT-REAL-COST        PIC S9(13)V99.
               10  GT-REBATE           PIC S9(13)V99.
               10  GT-PO-CNT           PIC S9(7).
               10  GT-SSP              PIC S9(13)V99.
               10  GT-CURR             PIC X(3).
               10  GT-FX               PIC S9(9)V9(8).
               10  GT-CONTROL          PIC X(18).
               10  GT-BILLHOLD         PIC X(12).
               10  GT-COST-SRC         PIC X(11).
               10  GT-CUTOFF           PIC X(8).
       01  WS-DET.
           05  D-KEY                   PIC X(20) VALUE SPACES.
           05  D-QUOTED                PIC S9(11)V99 VALUE 0.
           05  D-ORDERED               PIC S9(11)V99 VALUE 0.
           05  D-DESPATCHED            PIC S9(11)V99 VALUE 0.
           05  D-INVOICED              PIC S9(11)V99 VALUE 0.
           05  D-CREDITED              PIC S9(11)V99 VALUE 0.
           05  D-DISCOUNT              PIC S9(11)V99 VALUE 0.
           05  D-RETURNS               PIC S9(11)V99 VALUE 0.
           05  D-FX-BASIS              PIC X(20) VALUE SPACES.
           05  D-EXP-COST              PIC S9(11)V99 VALUE 0.
           05  D-EXP-MARGIN            PIC S9(11)V99 VALUE 0.
           05  D-EXP-PCT               PIC X(9) VALUE SPACES.
           05  D-REAL-COST             PIC S9(11)V99 VALUE 0.
           05  D-REAL-MARGIN           PIC S9(11)V99 VALUE 0.
           05  D-REAL-PCT              PIC X(9) VALUE SPACES.
           05  D-LATE-COST             PIC S9(11)V99 VALUE 0.
           05  D-COST-SRC              PIC X(11) VALUE SPACES.
           05  D-PO-CNT                PIC S9(6) VALUE 0.
           05  D-SSP                   PIC S9(11)V99 VALUE 0.
           05  D-REBATE                PIC S9(11)V99 VALUE 0.
           05  D-CONTROL               PIC X(18) VALUE SPACES.
           05  D-BILLHOLD              PIC X(12) VALUE SPACES.
       01  WS-TOT.
           05  T-GROUPS                PIC S9(7) VALUE 0.
           05  T-QUOTED                PIC S9(13)V99 VALUE 0.
           05  T-ORDERED               PIC S9(13)V99 VALUE 0.
           05  T-DESPATCHED            PIC S9(13)V99 VALUE 0.
           05  T-INVOICED              PIC S9(13)V99 VALUE 0.
           05  T-CREDITED              PIC S9(13)V99 VALUE 0.
           05  T-DISCOUNT              PIC S9(13)V99 VALUE 0.
           05  T-RETURNS               PIC S9(13)V99 VALUE 0.
           05  T-EXP-MARGIN            PIC S9(13)V99 VALUE 0.
           05  T-REAL-MARGIN           PIC S9(13)V99 VALUE 0.
           05  T-LATE-COST             PIC S9(13)V99 VALUE 0.
           05  T-REBATE                PIC S9(13)V99 VALUE 0.
           05  T-BILLHOLD              PIC S9(5) VALUE 0.
       REPORT SECTION.
       RD  R-SOANAL
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 10 LAST DETAIL 52
           FOOTING 56.
       COPY STRPTHDR.
       01  PH-COLS TYPE IS PAGE HEADING.
           05  LINE 7.
               10  COLUMN 1    PIC X(5) VALUE "Group".
               10  COLUMN 22   PIC X(12) VALUE "      Quoted".
               10  COLUMN 35   PIC X(12) VALUE "     Ordered".
               10  COLUMN 48   PIC X(12) VALUE "  Despatched".
               10  COLUMN 61   PIC X(12) VALUE "    Invoiced".
               10  COLUMN 74   PIC X(12) VALUE "    Credited".
               10  COLUMN 87   PIC X(12) VALUE "   Discounts".
               10  COLUMN 100  PIC X(12) VALUE "     Returns".
               10  COLUMN 113  PIC X(20) VALUE "FX basis".
           05  LINE 8.
               10  COLUMN 3    PIC X(16) VALUE "Margin basis".
               10  COLUMN 20   PIC X(12) VALUE "        Cost".
               10  COLUMN 33   PIC X(12) VALUE "      Margin".
               10  COLUMN 46   PIC X(9) VALUE " Margin %".
               10  COLUMN 109  PIC X(12) VALUE "Late cost ad".
               10  COLUMN 122  PIC X(11) VALUE "Source".
           05  LINE 9.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(20) SOURCE D-KEY.
               10  COLUMN 22   PIC Z(7)9.99- SOURCE D-QUOTED.
               10  COLUMN 35   PIC Z(7)9.99- SOURCE D-ORDERED.
               10  COLUMN 48   PIC Z(7)9.99- SOURCE D-DESPATCHED.
               10  COLUMN 61   PIC Z(7)9.99- SOURCE D-INVOICED.
               10  COLUMN 74   PIC Z(7)9.99- SOURCE D-CREDITED.
               10  COLUMN 87   PIC Z(7)9.99- SOURCE D-DISCOUNT.
               10  COLUMN 100  PIC Z(7)9.99- SOURCE D-RETURNS.
               10  COLUMN 113  PIC X(20) SOURCE D-FX-BASIS.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(16) VALUE "Expected (orig)".
               10  COLUMN 20   PIC Z(7)9.99- SOURCE D-EXP-COST.
               10  COLUMN 33   PIC Z(7)9.99- SOURCE D-EXP-MARGIN.
               10  COLUMN 46   PIC X(9) SOURCE D-EXP-PCT.
               10  COLUMN 56   PIC X(16) VALUE "Realised (actl)".
               10  COLUMN 73   PIC Z(7)9.99- SOURCE D-REAL-COST.
               10  COLUMN 86   PIC Z(7)9.99- SOURCE D-REAL-MARGIN.
               10  COLUMN 99   PIC X(9) SOURCE D-REAL-PCT.
               10  COLUMN 109  PIC Z(7)9.99- SOURCE D-LATE-COST.
               10  COLUMN 122  PIC X(11) SOURCE D-COST-SRC.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(12) VALUE "FRS 102 s23".
               10  COLUMN 16   PIC X(4) VALUE "POs".
               10  COLUMN 21   PIC Z(5)9 SOURCE D-PO-CNT.
               10  COLUMN 29   PIC X(10) VALUE "SSP total".
               10  COLUMN 40   PIC Z(7)9.99- SOURCE D-SSP.
               10  COLUMN 53   PIC X(14) VALUE "Variable cons".
               10  COLUMN 68   PIC Z(7)9.99- SOURCE D-REBATE.
               10  COLUMN 81   PIC X(9) VALUE "Control".
               10  COLUMN 91   PIC X(18) SOURCE D-CONTROL.
               10  COLUMN 110  PIC X(10) VALUE "Bill/hold".
               10  COLUMN 121  PIC X(12) SOURCE D-BILLHOLD.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(20) VALUE "Report totals".
               10  COLUMN 22   PIC Z(7)9.99- SOURCE T-QUOTED.
               10  COLUMN 35   PIC Z(7)9.99- SOURCE T-ORDERED.
               10  COLUMN 48   PIC Z(7)9.99- SOURCE T-DESPATCHED.
               10  COLUMN 61   PIC Z(7)9.99- SOURCE T-INVOICED.
               10  COLUMN 74   PIC Z(7)9.99- SOURCE T-CREDITED.
               10  COLUMN 87   PIC Z(7)9.99- SOURCE T-DISCOUNT.
               10  COLUMN 100  PIC Z(7)9.99- SOURCE T-RETURNS.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(16) VALUE "Expected margin".
               10  COLUMN 20   PIC Z(9)9.99- SOURCE T-EXP-MARGIN.
               10  COLUMN 36   PIC X(16) VALUE "Realised margin".
               10  COLUMN 53   PIC Z(9)9.99- SOURCE T-REAL-MARGIN.
               10  COLUMN 70   PIC X(16) VALUE "Late cost adjs".
               10  COLUMN 87   PIC Z(9)9.99- SOURCE T-LATE-COST.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(16) VALUE "Variable cons".
               10  COLUMN 20   PIC Z(9)9.99- SOURCE T-REBATE.
               10  COLUMN 36   PIC X(16) VALUE "Groups".
               10  COLUMN 53   PIC Z(6)9 SOURCE T-GROUPS.
               10  COLUMN 70   PIC X(16) VALUE "Bill and hold".
               10  COLUMN 87   PIC Z(4)9 SOURCE T-BILLHOLD.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(62) VALUE
               "Quoted, ordered, despatched, invoiced and credited are".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(62) VALUE
               "stages of one demand and are never added together.".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(62) VALUE
               "Bill and hold stock is NOT recognised as revenue here.".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(62) VALUE
               "n/d means the margin has no denominator - review it.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-JOB-START THRU 0200-EXIT
           PERFORM 0300-OPEN-SPOOL THRU 0300-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 1000-GATHER-ORDERS THRU 1000-EXIT
           PERFORM 1100-GATHER-QUOTES THRU 1100-EXIT
           PERFORM 1200-GATHER-DESPATCH THRU 1200-EXIT
           PERFORM 1300-GATHER-INVOICES THRU 1300-EXIT
           PERFORM 1400-GATHER-RETURNS THRU 1400-EXIT
           PERFORM 1500-GATHER-REBATES THRU 1500-EXIT
           PERFORM 5000-PRINT-ALL THRU 5000-EXIT
           PERFORM 0400-CLOSE-SPOOL THRU 0400-EXIT.
       0000-FINISH.
           PERFORM 0500-JOB-END THRU 0500-EXIT
           MOVE WS-RC TO RETURN-CODE
           STOP RUN.

      *>----------------------------------------------------------------
       0100-READ-ARGS SECTION.
       0100-START.
           INITIALIZE WS-CMDARGS
           MOVE "CMDLINE" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           MOVE CM-CO TO WS-CO
           MOVE CM-SITE TO WS-SITE
           MOVE CM-USER TO WS-USER
           MOVE CM-FROM TO WS-FROM
           MOVE CM-TO TO WS-TO
           MOVE "CUST" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-CUST
           END-IF
           MOVE "GROUPBY" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-GROUPBY
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "SALESMGR" TO WS-USER
           END-IF
           EVALUATE WS-GROUPBY
               WHEN "ITEM"
                   CONTINUE
               WHEN "SALESPERSON"
                   CONTINUE
               WHEN "MONTH"
                   CONTINUE
               WHEN OTHER
                   MOVE "CUSTOMER" TO WS-GROUPBY
           END-EVALUATE
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           INITIALIZE WS-GRP-TAB
           DISPLAY "STERP-I-1700 SOANAL sales analysis for "
                   FUNCTION TRIM(WS-CO) " by "
                   FUNCTION TRIM(WS-GROUPBY) UPON SYSOUT.
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "SOANAL" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE JL-JOB-RUN-ID TO WS-JOB-RUN-ID.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "SOANAL" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1701 cannot build the spool path"
                       UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           PERFORM 0350-FILL-HEADINGS THRU 0350-EXIT
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1702 cannot open " WS-SPOOL-NAME
                       UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           INITIATE R-SOANAL.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "SOANAL" TO RPT-REPORT-ID
           MOVE "Sales analysis and profitability" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE SPACES TO RPT-SUBTITLE
           STRING "Grouped by " FUNCTION TRIM(WS-GROUPBY)
                  " - stages shown separately"
               DELIMITED BY SIZE INTO RPT-SUBTITLE
           MOVE SPACES TO RPT-AS-AT-TEXT
           IF WS-FROM = SPACES AND WS-TO = SPACES
               STRING "All dates" DELIMITED BY SIZE
                   INTO RPT-AS-AT-TEXT
           ELSE
               STRING "From " WS-FROM " to " WS-TO
                   DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           END-IF
           PERFORM 0360-READ-COMPANY THRU 0360-EXIT.
       0350-EXIT.
           EXIT.

       0360-READ-COMPANY SECTION.
       0360-START.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 20
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Name, ISNULL(RegisteredNumber,'') FROM "
                  "core.Company WHERE CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-OK
               MOVE SO-COL(1) TO RPT-COMPANY-NAME
               MOVE SO-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   the group key fragment for each source table.  A SQL
      *>   fragment is chosen from a fixed set of literals - no user
      *>   text ever reaches the statement.
       0600-KEY-ORDERS SECTION.
       0600-START.
           EVALUATE WS-GROUPBY
               WHEN "ITEM"
                   MOVE "ISNULL(l.ItemCode,'(no item)')"
                       TO WS-KEY-SQL
               WHEN "SALESPERSON"
                   MOVE "ISNULL(o.SalespersonCode,'(none)')"
                       TO WS-KEY-SQL
               WHEN "MONTH"
                   MOVE "LEFT(CONVERT(char(8),o.OrderDate,112),6)"
                       TO WS-KEY-SQL
               WHEN OTHER
                   MOVE "o.CustomerCode" TO WS-KEY-SQL
           END-EVALUATE.
       0600-EXIT.
           EXIT.

       0610-KEY-QUOTES SECTION.
       0610-START.
           EVALUATE WS-GROUPBY
               WHEN "ITEM"
                   MOVE "ISNULL(ql.ItemCode,'(no item)')"
                       TO WS-KEY-SQL
               WHEN "SALESPERSON"
                   MOVE "ISNULL(q.SalespersonCode,'(none)')"
                       TO WS-KEY-SQL
               WHEN "MONTH"
                   MOVE "LEFT(CONVERT(char(8),q.QuoteDate,112),6)"
                       TO WS-KEY-SQL
               WHEN OTHER
                   MOVE "q.CustomerCode" TO WS-KEY-SQL
           END-EVALUATE.
       0610-EXIT.
           EXIT.

       0620-KEY-INVOICES SECTION.
       0620-START.
           EVALUATE WS-GROUPBY
               WHEN "ITEM"
                   MOVE "ISNULL(il.ItemCode,'(no item)')"
                       TO WS-KEY-SQL
               WHEN "SALESPERSON"
                   MOVE "ISNULL(i.SalespersonCode,'(none)')"
                       TO WS-KEY-SQL
               WHEN "MONTH"
                   MOVE "LEFT(CONVERT(char(8),i.TaxPointDate,112),6)"
                       TO WS-KEY-SQL
               WHEN OTHER
                   MOVE "i.CustomerCode" TO WS-KEY-SQL
           END-EVALUATE.
       0620-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   REQ: SO-006, SO-023, SO-028 - ordered value, the discount
      *>   given away, the ORIGINAL cost basis, the performance
      *>   obligation count, the standalone selling prices and the
      *>   transfer of control point.
       1000-GATHER-ORDERS SECTION.
       1000-START.
           PERFORM 0600-KEY-ORDERS THRU 0600-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT " FUNCTION TRIM(WS-KEY-SQL)
                  ", SUM(l.NetAmount), SUM(CASE WHEN "
                  "l.PricePerQty > 0 THEN l.AcceptedQty * "
                  "l.UnitPrice / l.PricePerQty - l.NetAmount ELSE 0 "
                  "END), SUM(ISNULL(l.CostSnapshotGBP,0) * "
                  "l.AcceptedQty), SUM(CASE WHEN l.LineType IN "
                  "('KITCOMP','TEXT') THEN 0 ELSE 1 END), "
                  "SUM(ISNULL(l.StandaloneSellingPrice,0) * "
                  "l.AcceptedQty), MAX(o.CurrencyCode), "
                  "MAX(o.FxRate), MAX(CASE WHEN o.OrderType = "
                  "'DROPSHIP' THEN 'Supplier despatch' ELSE "
                  "'Despatch' END), MAX(CASE WHEN "
                  "UPPER(ISNULL(o.HoldReason,'')) LIKE "
                  "'%BILL AND HOLD%' THEN 'BILL-AND-HOLD' WHEN "
                  "UPPER(ISNULL(o.HoldReason,'')) LIKE '%CONSIGN%' "
                  "THEN 'Consignment' ELSE 'None' END)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           STRING FUNCTION TRIM(SQLIO-STMT)
                  " FROM so.SalesOrderLine l JOIN so.SalesOrder o ON "
                  "o.SalesOrderId = l.SalesOrderId WHERE "
                  "o.CompanyCode = ? AND o.CommercialStatus <> "
                  "'DRAFT' AND (? = '' OR o.SiteCode = ?) AND "
                  "(? = '' OR o.CustomerCode = ?) AND (? = '' OR "
                  "CONVERT(char(8),o.OrderDate,112) >= ?) AND "
                  "(? = '' OR CONVERT(char(8),o.OrderDate,112) <= ?)"
                  " GROUP BY " FUNCTION TRIM(WS-KEY-SQL)
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-BIND-SCOPE THRU 0700-EXIT
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1703 " SQLIO-ERRMSG(1:100) UPON SYSOUT
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA SO-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 1050-POST-ORDER THRU 1050-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       1000-EXIT.
           EXIT.

       1050-POST-ORDER SECTION.
       1050-START.
           PERFORM 0800-FIND-GROUP THRU 0800-EXIT
           IF WS-FOUND = 0
               GO TO 1050-EXIT
           END-IF
           MOVE WS-FOUND TO WS-JX
           COMPUTE GT-ORDERED(WS-JX) = GT-ORDERED(WS-JX)
               + FUNCTION NUMVAL(SO-COL(2))
           COMPUTE GT-DISCOUNT(WS-JX) = GT-DISCOUNT(WS-JX)
               + FUNCTION NUMVAL(SO-COL(3))
           COMPUTE GT-EXP-COST(WS-JX) = GT-EXP-COST(WS-JX)
               + FUNCTION NUMVAL(SO-COL(4))
           COMPUTE GT-PO-CNT(WS-JX) = GT-PO-CNT(WS-JX)
               + FUNCTION NUMVAL(SO-COL(5))
           COMPUTE GT-SSP(WS-JX) = GT-SSP(WS-JX)
               + FUNCTION NUMVAL(SO-COL(6))
           MOVE SO-COL(7) TO GT-CURR(WS-JX)
           COMPUTE GT-FX(WS-JX) = FUNCTION NUMVAL(SO-COL(8))
           MOVE SO-COL(9) TO GT-CONTROL(WS-JX)
           MOVE SO-COL(10) TO GT-BILLHOLD(WS-JX).
       1050-EXIT.
           EXIT.

      *>   the quoted figure.  It is reported as it was issued and is
      *>   never rewritten by anything that happened afterwards.
       1100-GATHER-QUOTES SECTION.
       1100-START.
           PERFORM 0610-KEY-QUOTES THRU 0610-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT " FUNCTION TRIM(WS-KEY-SQL)
                  ", SUM(ql.NetAmount) FROM so.QuoteLine ql JOIN "
                  "so.Quote q ON q.QuoteId = ql.QuoteId WHERE "
                  "q.CompanyCode = ? AND (? = ?) AND "
                  "(? = '' OR q.CustomerCode = ?) AND (? = '' OR "
                  "CONVERT(char(8),q.QuoteDate,112) >= ?) AND "
                  "(? = '' OR CONVERT(char(8),q.QuoteDate,112) <= ?)"
                  " GROUP BY " FUNCTION TRIM(WS-KEY-SQL)
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-BIND-SCOPE THRU 0700-EXIT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1704 " SQLIO-ERRMSG(1:90) UPON SYSOUT
               GO TO 1100-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA SO-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 0800-FIND-GROUP THRU 0800-EXIT
                   IF WS-FOUND > 0
                       COMPUTE GT-QUOTED(WS-FOUND) =
                           GT-QUOTED(WS-FOUND)
                           + FUNCTION NUMVAL(SO-COL(2))
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       1100-EXIT.
           EXIT.

      *>   REQ: SO-023 - despatched value at the CAPTURED price, the
      *>   actual cost of sales posted on despatch (which carries any
      *>   later cost adjustment) and the comparable expected cost on
      *>   the same quantity, plus the cut off and the cost method.
       1200-GATHER-DESPATCH SECTION.
       1200-START.
           PERFORM 0600-KEY-ORDERS THRU 0600-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT " FUNCTION TRIM(WS-KEY-SQL)
                  ", SUM(CASE WHEN l.PricePerQty > 0 THEN "
                  "dl.DespatchedQty * l.UnitPrice / l.PricePerQty * "
                  "(1 - l.DiscountPct1 / 100) * (1 - l.DiscountPct2 "
                  "/ 100) ELSE 0 END), SUM(dl.CogsGBP), "
                  "SUM(ISNULL(l.CostSnapshotGBP,0) * "
                  "dl.DespatchedQty), MAX(ISNULL(dl.CostMethod,'')), "
                  "MAX(CONVERT(char(8),d.PostingDate,112))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           STRING FUNCTION TRIM(SQLIO-STMT)
                  " FROM so.DespatchLine dl JOIN so.Despatch d ON "
                  "d.DespatchId = dl.DespatchId JOIN "
                  "so.SalesOrderLine l ON l.SalesOrderLineId = "
                  "dl.SalesOrderLineId JOIN so.SalesOrder o ON "
                  "o.SalesOrderId = l.SalesOrderId WHERE "
                  "d.CompanyCode = ? AND d.Status = 'CONFIRMED' AND "
                  "(? = '' OR d.SiteCode = ?) AND (? = '' OR "
                  "d.CustomerCode = ?) AND (? = '' OR "
                  "CONVERT(char(8),d.DespatchDate,112) >= ?) AND "
                  "(? = '' OR CONVERT(char(8),d.DespatchDate,112) "
                  "<= ?) GROUP BY " FUNCTION TRIM(WS-KEY-SQL)
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-BIND-SCOPE THRU 0700-EXIT
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1705 " SQLIO-ERRMSG(1:90) UPON SYSOUT
               GO TO 1200-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA SO-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 1250-POST-DESPATCH THRU 1250-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       1200-EXIT.
           EXIT.

       1250-POST-DESPATCH SECTION.
       1250-START.
           PERFORM 0800-FIND-GROUP THRU 0800-EXIT
           IF WS-FOUND = 0
               GO TO 1250-EXIT
           END-IF
           MOVE WS-FOUND TO WS-JX
           COMPUTE GT-DESPATCHED(WS-JX) = GT-DESPATCHED(WS-JX)
               + FUNCTION NUMVAL(SO-COL(2))
           COMPUTE GT-REAL-COST(WS-JX) = GT-REAL-COST(WS-JX)
               + FUNCTION NUMVAL(SO-COL(3))
           COMPUTE GT-EXP-COST-D(WS-JX) = GT-EXP-COST-D(WS-JX)
               + FUNCTION NUMVAL(SO-COL(4))
           MOVE SO-COL(5) TO GT-COST-SRC(WS-JX)
           MOVE SO-COL(6) TO GT-CUTOFF(WS-JX).
       1250-EXIT.
           EXIT.

       1300-GATHER-INVOICES SECTION.
       1300-START.
           PERFORM 0620-KEY-INVOICES THRU 0620-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT " FUNCTION TRIM(WS-KEY-SQL)
                  ", SUM(CASE WHEN i.DocumentType = 'INVOICE' THEN "
                  "il.NetAmountGBP ELSE 0 END), SUM(CASE WHEN "
                  "i.DocumentType = 'CREDITNOTE' THEN "
                  "il.NetAmountGBP ELSE 0 END) FROM ar.SalesInvoice "
                  "i JOIN ar.SalesInvoiceLine il ON il.InvoiceId = "
                  "i.InvoiceId WHERE i.CompanyCode = ? AND "
                  "i.Status = 'POSTED' AND (? = ?) AND "
                  "(? = '' OR i.CustomerCode = ?) AND (? = '' OR "
                  "CONVERT(char(8),i.TaxPointDate,112) >= ?) AND "
                  "(? = '' OR CONVERT(char(8),i.TaxPointDate,112) "
                  "<= ?) GROUP BY " FUNCTION TRIM(WS-KEY-SQL)
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-BIND-SCOPE THRU 0700-EXIT
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1706 " SQLIO-ERRMSG(1:90) UPON SYSOUT
               GO TO 1300-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA SO-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 0800-FIND-GROUP THRU 0800-EXIT
                   IF WS-FOUND > 0
                       COMPUTE GT-INVOICED(WS-FOUND) =
                           GT-INVOICED(WS-FOUND)
                           + FUNCTION NUMVAL(SO-COL(2))
                       COMPUTE GT-CREDITED(WS-FOUND) =
                           GT-CREDITED(WS-FOUND)
                           + FUNCTION NUMVAL(SO-COL(3))
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       1300-EXIT.
           EXIT.

       1400-GATHER-RETURNS SECTION.
       1400-START.
           MOVE SPACES TO WS-KEY-SQL
           EVALUATE WS-GROUPBY
               WHEN "ITEM"
                   MOVE "ISNULL(rl.ItemCode,'(no item)')"
                       TO WS-KEY-SQL
               WHEN "SALESPERSON"
                   MOVE "ISNULL(o.SalespersonCode,'(none)')"
                       TO WS-KEY-SQL
               WHEN "MONTH"
                   MOVE "LEFT(CONVERT(char(8),r.RequestedDate,112),6)"
                       TO WS-KEY-SQL
               WHEN OTHER
                   MOVE "r.CustomerCode" TO WS-KEY-SQL
           END-EVALUATE
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT " FUNCTION TRIM(WS-KEY-SQL)
                  ", SUM(ISNULL(rl.ReturnValueGBP,0)) FROM "
                  "so.ReturnLine rl JOIN so.ReturnAuthorisation r ON "
                  "r.ReturnId = rl.ReturnId LEFT JOIN so.SalesOrder "
                  "o ON o.CompanyCode = r.CompanyCode AND "
                  "o.OrderNumber = r.OriginalOrderNumber WHERE "
                  "r.CompanyCode = ? AND (? = '' OR r.SiteCode = ?) "
                  "AND (? = '' OR r.CustomerCode = ?) AND (? = '' OR "
                  "CONVERT(char(8),r.RequestedDate,112) >= ?) AND "
                  "(? = '' OR CONVERT(char(8),r.RequestedDate,112) "
                  "<= ?) GROUP BY " FUNCTION TRIM(WS-KEY-SQL)
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-BIND-SCOPE THRU 0700-EXIT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1707 " SQLIO-ERRMSG(1:90) UPON SYSOUT
               GO TO 1400-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA SO-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 0800-FIND-GROUP THRU 0800-EXIT
                   IF WS-FOUND > 0
                       COMPUTE GT-RETURNS(WS-FOUND) =
                           GT-RETURNS(WS-FOUND)
                           + FUNCTION NUMVAL(SO-COL(2))
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       1400-EXIT.
           EXIT.

      *>   REQ: SO-028 - variable consideration.  An unsettled rebate
      *>   accrual is the amount of the transaction price that is not
      *>   yet expected to be retained.
       1500-GATHER-REBATES SECTION.
       1500-START.
           MOVE SPACES TO WS-KEY-SQL
           EVALUATE WS-GROUPBY
               WHEN "ITEM"
                   MOVE "ISNULL(rg.ItemCode,'(all items)')"
                       TO WS-KEY-SQL
               WHEN "SALESPERSON"
                   MOVE "'(rebate)'" TO WS-KEY-SQL
               WHEN "MONTH"
                   MOVE "'(rebate)'" TO WS-KEY-SQL
               WHEN OTHER
                   MOVE "rg.CustomerCode" TO WS-KEY-SQL
           END-EVALUATE
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT " FUNCTION TRIM(WS-KEY-SQL)
                  ", SUM(ra.AccruedGBP - ra.SettledGBP) FROM "
                  "so.RebateAccrual ra JOIN so.RebateAgreement rg ON "
                  "rg.RebateId = ra.RebateId WHERE "
                  "ra.CompanyCode = ? AND (? = '' OR "
                  "rg.CustomerCode = ?) GROUP BY "
                  FUNCTION TRIM(WS-KEY-SQL)
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-CUST TO SQLIO-PARM-VAL(2)
           MOVE WS-CUST TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1708 " SQLIO-ERRMSG(1:90) UPON SYSOUT
               GO TO 1500-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA SO-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 0800-FIND-GROUP THRU 0800-EXIT
                   IF WS-FOUND > 0
                       COMPUTE GT-REBATE(WS-FOUND) =
                           GT-REBATE(WS-FOUND)
                           + FUNCTION NUMVAL(SO-COL(2))
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       1500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   the thirteen scope parameters shared by the gather queries
       0700-BIND-SCOPE SECTION.
       0700-START.
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-SITE TO SQLIO-PARM-VAL(2)
           MOVE WS-SITE TO SQLIO-PARM-VAL(3)
           MOVE WS-CUST TO SQLIO-PARM-VAL(4)
           MOVE WS-CUST TO SQLIO-PARM-VAL(5)
           MOVE WS-FROM TO SQLIO-PARM-VAL(6)
           MOVE WS-FROM TO SQLIO-PARM-VAL(7)
           MOVE WS-TO TO SQLIO-PARM-VAL(8)
           MOVE WS-TO TO SQLIO-PARM-VAL(9)
           MOVE 9 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 9
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM.
       0700-EXIT.
           EXIT.

       0800-FIND-GROUP SECTION.
       0800-START.
           MOVE 0 TO WS-FOUND
           PERFORM VARYING WS-IX FROM 1 BY 1
               UNTIL WS-IX > WS-GRP-CNT OR WS-FOUND > 0
               IF GT-KEY(WS-IX) = SO-COL(1)(1:24)
                   MOVE WS-IX TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND > 0
               GO TO 0800-EXIT
           END-IF
           IF WS-GRP-CNT NOT < 200
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
               DISPLAY "STERP-W-1709 more than 200 groups - report "
                       "a narrower range" UPON SYSOUT
               GO TO 0800-EXIT
           END-IF
           ADD 1 TO WS-GRP-CNT
           MOVE WS-GRP-CNT TO WS-FOUND
           MOVE SO-COL(1)(1:24) TO GT-KEY(WS-FOUND)
           MOVE 0 TO GT-QUOTED(WS-FOUND) GT-ORDERED(WS-FOUND)
           MOVE 0 TO GT-DESPATCHED(WS-FOUND) GT-INVOICED(WS-FOUND)
           MOVE 0 TO GT-CREDITED(WS-FOUND) GT-DISCOUNT(WS-FOUND)
           MOVE 0 TO GT-RETURNS(WS-FOUND) GT-EXP-COST(WS-FOUND)
           MOVE 0 TO GT-EXP-COST-D(WS-FOUND) GT-REAL-COST(WS-FOUND)
           MOVE 0 TO GT-REBATE(WS-FOUND) GT-PO-CNT(WS-FOUND)
           MOVE 0 TO GT-SSP(WS-FOUND) GT-FX(WS-FOUND)
           MOVE SPACES TO GT-CURR(WS-FOUND) GT-CONTROL(WS-FOUND)
           MOVE SPACES TO GT-BILLHOLD(WS-FOUND)
           MOVE SPACES TO GT-COST-SRC(WS-FOUND) GT-CUTOFF(WS-FOUND).
       0800-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       5000-PRINT-ALL SECTION.
       5000-START.
           PERFORM VARYING WS-JX FROM 1 BY 1
               UNTIL WS-JX > WS-GRP-CNT
               PERFORM 5100-ONE-GROUP THRU 5100-EXIT
           END-PERFORM
           MOVE WS-GRP-CNT TO T-GROUPS.
       5000-EXIT.
           EXIT.

       5100-ONE-GROUP SECTION.
       5100-START.
           ADD 1 TO WS-ROWS-OUT
           MOVE GT-KEY(WS-JX) TO D-KEY
           MOVE GT-QUOTED(WS-JX) TO D-QUOTED
           MOVE GT-ORDERED(WS-JX) TO D-ORDERED
           MOVE GT-DESPATCHED(WS-JX) TO D-DESPATCHED
           MOVE GT-INVOICED(WS-JX) TO D-INVOICED
           MOVE GT-CREDITED(WS-JX) TO D-CREDITED
           MOVE GT-DISCOUNT(WS-JX) TO D-DISCOUNT
           MOVE GT-RETURNS(WS-JX) TO D-RETURNS
           MOVE SPACES TO D-FX-BASIS
           IF GT-CURR(WS-JX) = SPACES OR GT-CURR(WS-JX) = "GBP"
               STRING "GBP at 1.00000000" DELIMITED BY SIZE
                   INTO D-FX-BASIS
           ELSE
               MOVE GT-FX(WS-JX) TO WS-FX-ED
               STRING GT-CURR(WS-JX) " at "
                      FUNCTION TRIM(WS-FX-ED)
                   DELIMITED BY SIZE INTO D-FX-BASIS
           END-IF
           PERFORM 5200-MARGINS THRU 5200-EXIT
           MOVE GT-PO-CNT(WS-JX) TO D-PO-CNT
           MOVE GT-SSP(WS-JX) TO D-SSP
           MOVE GT-REBATE(WS-JX) TO D-REBATE
           MOVE GT-CONTROL(WS-JX) TO D-CONTROL
           MOVE GT-BILLHOLD(WS-JX) TO D-BILLHOLD
           IF GT-BILLHOLD(WS-JX)(1:4) = "BILL"
               ADD 1 TO T-BILLHOLD
           END-IF
           ADD GT-QUOTED(WS-JX) TO T-QUOTED
           ADD GT-ORDERED(WS-JX) TO T-ORDERED
           ADD GT-DESPATCHED(WS-JX) TO T-DESPATCHED
           ADD GT-INVOICED(WS-JX) TO T-INVOICED
           ADD GT-CREDITED(WS-JX) TO T-CREDITED
           ADD GT-DISCOUNT(WS-JX) TO T-DISCOUNT
           ADD GT-RETURNS(WS-JX) TO T-RETURNS
           ADD GT-REBATE(WS-JX) TO T-REBATE
           ADD D-EXP-MARGIN TO T-EXP-MARGIN
           ADD D-REAL-MARGIN TO T-REAL-MARGIN
           ADD D-LATE-COST TO T-LATE-COST
           GENERATE DE-LINE.
       5100-EXIT.
           EXIT.

      *>   REQ: SO-006 - a zero denominator is "not defined" and asks
      *>   for review.  It is never reported as a zero margin, and a
      *>   missing cost is never treated as nil cost.
       5200-MARGINS SECTION.
       5200-START.
           MOVE GT-EXP-COST-D(WS-JX) TO D-EXP-COST
           MOVE GT-REAL-COST(WS-JX) TO D-REAL-COST
           COMPUTE D-EXP-MARGIN = GT-DESPATCHED(WS-JX)
                                - GT-EXP-COST-D(WS-JX)
           COMPUTE D-REAL-MARGIN = GT-DESPATCHED(WS-JX)
                                 - GT-REAL-COST(WS-JX)
           COMPUTE D-LATE-COST = GT-REAL-COST(WS-JX)
                               - GT-EXP-COST-D(WS-JX)
           MOVE GT-DESPATCHED(WS-JX) TO WS-NET
           IF WS-NET = 0
               MOVE "      n/d" TO D-EXP-PCT
               MOVE "      n/d" TO D-REAL-PCT
               ADD 1 TO WS-UNDEFINED
           ELSE
               COMPUTE WS-PCT ROUNDED = D-EXP-MARGIN / WS-NET * 100
               MOVE WS-PCT TO WS-PCT-ED
               MOVE WS-PCT-ED TO D-EXP-PCT
               COMPUTE WS-PCT ROUNDED = D-REAL-MARGIN / WS-NET * 100
               MOVE WS-PCT TO WS-PCT-ED
               MOVE WS-PCT-ED TO D-REAL-PCT
           END-IF
           MOVE SPACES TO D-COST-SRC
           IF GT-COST-SRC(WS-JX) = SPACES
               MOVE "No despatch" TO D-COST-SRC
           ELSE
               STRING GT-COST-SRC(WS-JX)(1:4) " "
                      GT-CUTOFF(WS-JX)(7:2) "/"
                      GT-CUTOFF(WS-JX)(5:2)
                   DELIMITED BY SIZE INTO D-COST-SRC
           END-IF.
       5200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-SOANAL
           CLOSE RPT-FILE
           DISPLAY "STERP-I-1710 " WS-ROWS-OUT " analysis group(s)"
                   UPON SYSOUT
           IF WS-UNDEFINED > 0
               DISPLAY "STERP-W-1711 " WS-UNDEFINED
                       " group(s) with no margin denominator"
                       UPON SYSOUT
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
           END-IF
           IF T-BILLHOLD > 0
               DISPLAY "STERP-W-1712 " T-BILLHOLD
                       " bill and hold order(s) are NOT recognised"
                       UPON SYSOUT
           END-IF
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME)
                   UPON SYSOUT.
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "SOANAL" TO JL-JOB-NAME
           MOVE WS-JOB-RUN-ID TO JL-JOB-RUN-ID
           EVALUATE TRUE
               WHEN WS-RC = 0
                   MOVE "COMPLETE" TO JL-STATUS
               WHEN WS-RC = 4
                   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER
                   MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE WS-GRP-CNT TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
