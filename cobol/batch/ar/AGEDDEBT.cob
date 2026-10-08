      *>================================================================
      *> PROGRAM-ID : AGEDDEBT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : AGED RECEIVABLES ANALYSIS AS AT A SELECTED DATE.
      *>              132 COLUMN REPORT WRITER LISTING OF EVERY OPEN
      *>              ITEM WITH ITS DUE DATE BUCKET (NOT YET DUE,
      *>              1-30, 31-60, 61-90, OVER 90 DAYS OVERDUE).
      *>              UNAPPLIED CREDITS AND CUSTOMER DEPOSITS ARE
      *>              CARRIED IN THEIR OWN COLUMNS SO THAT A CREDIT
      *>              BALANCE CANNOT BE READ AS OVERDUE DEBT, AND
      *>              DISPUTED AMOUNTS ARE CLASSIFIED SEPARATELY.
      *>              THE REPORT IS RECONSTRUCTED AS AT THE SELECTED
      *>              DATE - RECEIPTS AND ALLOCATIONS EFFECTIVE AFTER
      *>              THAT DATE ARE IGNORED, SO AN EARLIER REPORT CAN
      *>              ALWAYS BE REPRODUCED.  THE FINAL PAGE COMPARES
      *>              THE LEDGER TO THE GENERAL LEDGER AR CONTROL
      *>              ACCOUNT BALANCE ON THE SAME DATE.
      *> RUN AS     : AGEDDEBT CO=STC ASAT=20261004 USER=ARCLERK1
      *>              [CUST=C0001]
      *> OUTPUT     : run\spool\AGEDDEBT-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-04-16 RJH  ORIGINAL AGED DEBT LISTING (MVS PORT)
      *> 1994-07-21 RJH  CR0318 FIVE BUCKETS - THE AUDITORS WANTED
      *>                 90 DAYS SPLIT OUT FROM 61-90
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS IN THE HEADINGS
      *> 2003-09-08 KMP  CR1402 UNAPPLIED CASH SHOWN SEPARATELY AFTER
      *>                 THE 2003 INTERIM AUDIT POINT
      *> 2026-10-05 STERP AR-017 AS-AT RECONSTRUCTION, AR-021 CONTROL
      *>                 ACCOUNT COMPARISON LINE
      *>================================================================
      *> REQ: AR-017, AR-021
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AGEDDEBT.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 16/04/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO DYNAMIC WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-AGEDDEBT.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ARWORK.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
      *>   the 1991 listing fetched 14 columns; the slot width has
      *>   been left at 120 since the SQL Server port (CR2601)
       01  BR-ROW.
           05  BR-COL OCCURS 30 TIMES  PIC X(120).
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-CUST                 PIC X(20) VALUE SPACES.
           05  WS-ASAT                 PIC 9(8) VALUE 0.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-JOBID                PIC S9(9) COMP-5 VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-SKIPPED              PIC S9(9) VALUE 0.
           05  WS-RESID                PIC S9(13)V99 VALUE 0.
           05  WS-DUE-N                PIC 9(8) VALUE 0.
           05  WS-DOC-N                PIC 9(8) VALUE 0.
           05  WS-CTL-ACCT             PIC X(12) VALUE SPACES.
           05  WS-GL-BAL               PIC S9(13)V99 VALUE 0.
           05  WS-DIFF                 PIC S9(13)V99 VALUE 0.
       01  WS-UK.
           05  WS-UK-DD                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-UK-MM                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-UK-YYYY              PIC 9(4) VALUE 0.
      *>   control break and detail fields - D-B0..D-B4 are zero on a
      *>   credit line and D-CRED/D-DEPOS are zero on a debit line, so
      *>   the Report Writer SUM counters stay honest either way
       01  WS-DET.
           05  D-CUST                  PIC X(20) VALUE SPACES.
           05  D-CNAME                 PIC X(40) VALUE SPACES.
           05  D-TYPE                  PIC X(10) VALUE SPACES.
           05  D-DOC                   PIC X(16) VALUE SPACES.
           05  D-DDATE                 PIC X(10) VALUE SPACES.
           05  D-DUE                   PIC X(10) VALUE SPACES.
           05  D-DAYS                  PIC S9(5) VALUE 0.
           05  D-CURR                  PIC X(3) VALUE SPACES.
           05  D-B0                    PIC S9(11)V99 VALUE 0.
           05  D-B1                    PIC S9(11)V99 VALUE 0.
           05  D-B2                    PIC S9(11)V99 VALUE 0.
           05  D-B3                    PIC S9(11)V99 VALUE 0.
           05  D-B4                    PIC S9(11)V99 VALUE 0.
           05  D-CRED                  PIC S9(11)V99 VALUE 0.
           05  D-DEPOS                 PIC S9(11)V99 VALUE 0.
           05  D-DISP                  PIC S9(11)V99 VALUE 0.
           05  D-NOTE                  PIC X(22) VALUE SPACES.
           05  D-FLAG                  PIC X(3) VALUE SPACES.
       01  WS-TOT.
           05  T-DEBT                  PIC S9(13)V99 VALUE 0.
           05  T-CRED                  PIC S9(13)V99 VALUE 0.
           05  T-DEPOS                 PIC S9(13)V99 VALUE 0.
           05  T-DISP                  PIC S9(13)V99 VALUE 0.
           05  T-ITEMS                 PIC S9(7) VALUE 0.
           05  T-CUSTS                 PIC S9(7) VALUE 0.
       REPORT SECTION.
       RD  R-AGEDDEBT
           CONTROLS ARE FINAL D-CUST
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 54
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-CUST TYPE IS CONTROL HEADING D-CUST.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(9) VALUE "Customer ".
               10  COLUMN 10   PIC X(20) SOURCE D-CUST.
               10  COLUMN 31   PIC X(40) SOURCE D-CNAME.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(8) VALUE "Type".
               10  COLUMN 10   PIC X(8) VALUE "Document".
               10  COLUMN 27   PIC X(8) VALUE "Doc date".
               10  COLUMN 38   PIC X(8) VALUE "Due date".
               10  COLUMN 49   PIC X(4) VALUE "Days".
               10  COLUMN 56   PIC X(11) VALUE "Not yet due".
               10  COLUMN 73   PIC X(4) VALUE "1-30".
               10  COLUMN 86   PIC X(5) VALUE "31-60".
               10  COLUMN 100  PIC X(5) VALUE "61-90".
               10  COLUMN 112  PIC X(10) VALUE "Over 90   ".
               10  COLUMN 124  PIC X(3) VALUE "Dis".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(60) VALUE ALL "-".
       01  DE-DEBT TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(8) SOURCE D-TYPE.
               10  COLUMN 10   PIC X(16) SOURCE D-DOC.
               10  COLUMN 27   PIC X(10) SOURCE D-DDATE.
               10  COLUMN 38   PIC X(10) SOURCE D-DUE.
               10  COLUMN 48   PIC ZZZZ9- SOURCE D-DAYS.
               10  COLUMN 55   PIC Z,ZZZ,ZZ9.99- SOURCE D-B0.
               10  COLUMN 69   PIC Z,ZZZ,ZZ9.99- SOURCE D-B1.
               10  COLUMN 83   PIC Z,ZZZ,ZZ9.99- SOURCE D-B2.
               10  COLUMN 97   PIC Z,ZZZ,ZZ9.99- SOURCE D-B3.
               10  COLUMN 111  PIC Z,ZZZ,ZZ9.99- SOURCE D-B4.
               10  COLUMN 125  PIC X(3) SOURCE D-FLAG.
       01  DE-CRED TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(8) SOURCE D-TYPE.
               10  COLUMN 10   PIC X(16) SOURCE D-DOC.
               10  COLUMN 27   PIC X(10) SOURCE D-DDATE.
               10  COLUMN 38   PIC X(10) SOURCE D-DUE.
               10  COLUMN 49   PIC X(3) SOURCE D-CURR.
               10  COLUMN 55   PIC X(22) SOURCE D-NOTE.
               10  COLUMN 83   PIC Z,ZZZ,ZZ9.99- SOURCE D-CRED.
               10  COLUMN 97   PIC Z,ZZZ,ZZ9.99- SOURCE D-DEPOS.
               10  COLUMN 125  PIC X(3) SOURCE D-FLAG.
       01  CF-CUST TYPE IS CONTROL FOOTING D-CUST.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(14) VALUE "Customer total".
               10  COLUMN 55   PIC Z,ZZZ,ZZ9.99- SUM D-B0.
               10  COLUMN 69   PIC Z,ZZZ,ZZ9.99- SUM D-B1.
               10  COLUMN 83   PIC Z,ZZZ,ZZ9.99- SUM D-B2.
               10  COLUMN 97   PIC Z,ZZZ,ZZ9.99- SUM D-B3.
               10  COLUMN 111  PIC Z,ZZZ,ZZ9.99- SUM D-B4.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(34) VALUE
                   "of which disputed (still owed)".
               10  COLUMN 55   PIC Z,ZZZ,ZZ9.99- SUM D-DISP.
               10  COLUMN 70   PIC X(13) VALUE "Unapplied cr".
               10  COLUMN 83   PIC Z,ZZZ,ZZ9.99- SUM D-CRED.
               10  COLUMN 97   PIC Z,ZZZ,ZZ9.99- SUM D-DEPOS.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL ".".
               10  COLUMN 67   PIC X(60) VALUE ALL ".".
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(16) VALUE "Company totals".
               10  COLUMN 55   PIC Z,ZZZ,ZZ9.99- SUM D-B0.
               10  COLUMN 69   PIC Z,ZZZ,ZZ9.99- SUM D-B1.
               10  COLUMN 83   PIC Z,ZZZ,ZZ9.99- SUM D-B2.
               10  COLUMN 97   PIC Z,ZZZ,ZZ9.99- SUM D-B3.
               10  COLUMN 111  PIC Z,ZZZ,ZZ9.99- SUM D-B4.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(24) VALUE "Gross overdue debt".
               10  COLUMN 29   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-DEBT.
               10  COLUMN 53   PIC X(21) VALUE "Disputed included".
               10  COLUMN 76   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-DISP.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(24) VALUE "Unapplied credits".
               10  COLUMN 29   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-CRED.
               10  COLUMN 53   PIC X(21) VALUE "Deposits held".
               10  COLUMN 76   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-DEPOS.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(28) VALUE
                   "GL control account".
               10  COLUMN 32   PIC X(12) SOURCE WS-CTL-ACCT.
               10  COLUMN 46   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE WS-GL-BAL.
               10  COLUMN 70   PIC X(16) VALUE "Difference".
               10  COLUMN 88   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE WS-DIFF.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(24) VALUE "Items listed".
               10  COLUMN 29   PIC ZZZ,ZZ9 SOURCE T-ITEMS.
               10  COLUMN 40   PIC X(12) VALUE "Customers".
               10  COLUMN 53   PIC ZZZ,ZZ9 SOURCE T-CUSTS.
               10  COLUMN 64   PIC X(22) VALUE "Settled before as at".
               10  COLUMN 88   PIC ZZZ,ZZ9 SOURCE WS-SKIPPED.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(48) VALUE
                   "Credit balances are not aged as overdue debt.".
               10  COLUMN 52   PIC X(44) VALUE
                   "Buckets are by due date; an item is not".
               10  COLUMN 92   PIC X(28) VALUE
                   "overdue on its due date.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-JOB-START
           PERFORM 0300-OPEN-SPOOL
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 1000-PRODUCE-REPORT
           PERFORM 1900-CONTROL-ACCOUNT
           PERFORM 0400-CLOSE-SPOOL.
       0000-FINISH.
           PERFORM 0500-JOB-END
           MOVE WS-RC TO RETURN-CODE
           STOP RUN.

      *>----------------------------------------------------------------
       0100-READ-ARGS SECTION.
       0100-START.
           INITIALIZE WS-CMDARGS
           MOVE "CMDLINE" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           MOVE CM-CO TO WS-CO
           MOVE CM-USER TO WS-USER
           MOVE "CUST" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-CUST
           END-IF
           IF CM-ASAT NOT = SPACES
               COMPUTE WS-ASAT = FUNCTION NUMVAL(CM-ASAT)
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "ARCLERK1" TO WS-USER
           END-IF
           IF WS-ASAT = 0
               MOVE FUNCTION CURRENT-DATE TO AR-NOW
               COMPUTE WS-ASAT = FUNCTION NUMVAL(AR-NOW(1:8))
           END-IF
           IF WS-ASAT < 19900101 OR WS-ASAT > 20991231
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0101 ASAT is not a sensible date"
               GO TO 0100-EXIT
           END-IF
           DISPLAY "STERP-I-0100 AGEDDEBT aged receivables for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE WS-ASAT TO CA-BUSINESS-DATE OF WS-JOBLOG
           MOVE "AGEDDEBT" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE JL-JOB-RUN-ID TO WS-JOBID.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "AGEDDEBT" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0301 cannot build the spool path"
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           PERFORM 0350-FILL-HEADINGS
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0302 cannot open " WS-SPOOL-NAME
               GO TO 0300-EXIT
           END-IF
           INITIATE R-AGEDDEBT.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "AGEDDEBT" TO RPT-REPORT-ID
           MOVE "Aged receivables analysis" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO AR-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(AR-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(AR-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(AR-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(AR-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(AR-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(AR-NOW(13:2))
           MOVE WS-ASAT TO AR-NUM-DATE
           PERFORM 8100-FORMAT-DATE
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "As at " WS-UK DELIMITED BY SIZE
               INTO RPT-AS-AT-TEXT
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-CUST = SPACES
               STRING "All customers - amounts in GBP"
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           ELSE
               STRING "Customer " FUNCTION TRIM(WS-CUST)
                      " - amounts in GBP"
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           END-IF
           PERFORM 0360-READ-COMPANY.
       0350-EXIT.
           EXIT.

       0360-READ-COMPANY SECTION.
       0360-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Name, ISNULL(RegisteredNumber,'') FROM "
                  "core.Company WHERE CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               MOVE BR-COL(1) TO RPT-COMPANY-NAME
               MOVE BR-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-017  reconstruct the ledger as at the selected date:
      *> an allocation dated after ASAT is deliberately left out of the
      *> residual, so a reprint of an old report gives the old answer
      *>----------------------------------------------------------------
       1000-PRODUCE-REPORT SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT o.CustomerCode, c.LegalName, o.ItemType, "
                  "o.DocumentNumber, CONVERT(char(8),o.DocumentDate,"
                  "112), CONVERT(char(8),o.DueDate,112), "
                  "o.CurrencyCode, CONVERT(varchar(24),"
                  "o.OriginalAmountGBP), CONVERT(varchar(24), "
                  "o.OriginalAmountGBP - ISNULL((SELECT SUM("
                  "a.AppliedAmountGBP + a.DiscountAmountGBP + "
                  "a.WriteOffAmountGBP) FROM ar.Allocation a WHERE "
                  "a.TargetOpenItemId = o.OpenItemId AND a.Status = "
                  "'ACTIVE' AND a.EffectiveDate <= ?),0) + ISNULL(("
                  "SELECT SUM(b.AppliedAmountGBP) FROM ar.Allocation "
                  "b WHERE b.SourceOpenItemId = o.OpenItemId AND "
                  "b.Status = 'ACTIVE' AND b.EffectiveDate <= ?),0)), "
                  "CASE WHEN o.IsDisputed = 1 THEN 'Y' ELSE 'N' END, "
                  "CONVERT(varchar(24),o.DisputedAmountGBP), "
                  "o.CollectionStatus, ISNULL(c.ArControlAccount,''), "
                  "c.Status FROM ar.OpenItem o JOIN ar.Customer c ON "
                  "c.CompanyCode = o.CompanyCode AND c.CustomerCode "
                  "= o.CustomerCode WHERE o.CompanyCode = ? AND "
                  "o.PostingDate <= ? AND o.SettlementStatus <> "
                  "'REVERSED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-ASAT TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(1) SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           IF WS-CUST = SPACES
               MOVE 4 TO SQLIO-PARM-CNT
           ELSE
               MOVE 5 TO SQLIO-PARM-CNT
               MOVE WS-CUST TO SQLIO-PARM-VAL(5)
               MOVE "N" TO SQLIO-PARM-NULL(5)
               MOVE " AND o.CustomerCode = ?" TO AR-TEXT
               PERFORM 8200-APPEND-SQL
           END-IF
           MOVE " ORDER BY o.CustomerCode, o.DueDate, " TO AR-TEXT
           PERFORM 8200-APPEND-SQL
           MOVE "o.DocumentNumber" TO AR-TEXT
           PERFORM 8200-APPEND-SQL
           MOVE 14 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1001 " SQLIO-ERRMSG(1:100)
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO AR-DONE
           PERFORM UNTIL AR-FINISHED
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO AR-DONE
               ELSE
                   PERFORM 1100-CLASSIFY-ITEM
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       1000-EXIT.
           EXIT.

       1100-CLASSIFY-ITEM SECTION.
       1100-START.
           COMPUTE WS-RESID = FUNCTION NUMVAL(BR-COL(9))
           IF WS-RESID > -0.005 AND WS-RESID < 0.005
               ADD 1 TO WS-SKIPPED
               GO TO 1100-EXIT
           END-IF
           IF D-CUST NOT = BR-COL(1)(1:20)
               ADD 1 TO T-CUSTS
           END-IF
           MOVE BR-COL(1) TO D-CUST
           MOVE BR-COL(2) TO D-CNAME
           MOVE BR-COL(3) TO D-TYPE
           MOVE BR-COL(4) TO D-DOC
           MOVE BR-COL(7) TO D-CURR
           MOVE 0 TO D-B0 D-B1 D-B2 D-B3 D-B4
           MOVE 0 TO D-CRED D-DEPOS D-DISP
           MOVE SPACES TO D-FLAG D-NOTE
           MOVE BR-COL(5) TO AR-COMPACT-DATE
           COMPUTE WS-DOC-N = FUNCTION NUMVAL(AR-COMPACT-DATE)
           MOVE WS-DOC-N TO AR-NUM-DATE
           PERFORM 8100-FORMAT-DATE
           MOVE WS-UK TO D-DDATE
           MOVE BR-COL(6) TO AR-COMPACT-DATE
           COMPUTE WS-DUE-N = FUNCTION NUMVAL(AR-COMPACT-DATE)
           MOVE WS-DUE-N TO AR-NUM-DATE
           PERFORM 8100-FORMAT-DATE
           MOVE WS-UK TO D-DUE
           COMPUTE D-DAYS = FUNCTION INTEGER-OF-DATE(WS-ASAT)
                          - FUNCTION INTEGER-OF-DATE(WS-DUE-N)
           IF BR-COL(10)(1:1) = "Y"
               MOVE "Dis" TO D-FLAG
           END-IF
           IF BR-COL(14)(1:7) = "RETIRED"
      *>       AR-021 a retired customer with debt stays collectable
               MOVE "Ret" TO D-FLAG
           END-IF
           ADD 1 TO T-ITEMS WS-ROWS-OUT
           IF WS-RESID < 0
               PERFORM 1200-CREDIT-LINE
           ELSE
               PERFORM 1300-DEBIT-LINE
           END-IF.
       1100-EXIT.
           EXIT.

       1200-CREDIT-LINE SECTION.
       1200-START.
           MOVE SPACES TO D-NOTE
           IF D-TYPE(1:7) = "DEPOSIT"
               COMPUTE D-DEPOS = 0 - WS-RESID
               ADD D-DEPOS TO T-DEPOS
               MOVE "Deposit held" TO D-NOTE
           ELSE
               COMPUTE D-CRED = 0 - WS-RESID
               ADD D-CRED TO T-CRED
               IF D-TYPE(1:7) = "RECEIPT"
                   MOVE "Unapplied receipt" TO D-NOTE
               ELSE
                   MOVE "Unapplied credit" TO D-NOTE
               END-IF
           END-IF
           MOVE SPACES TO D-DUE
           GENERATE DE-CRED.
       1200-EXIT.
           EXIT.

       1300-DEBIT-LINE SECTION.
       1300-START.
           EVALUATE TRUE
               WHEN D-DAYS <= 0
                   MOVE WS-RESID TO D-B0
               WHEN D-DAYS <= 30
                   MOVE WS-RESID TO D-B1
               WHEN D-DAYS <= 60
                   MOVE WS-RESID TO D-B2
               WHEN D-DAYS <= 90
                   MOVE WS-RESID TO D-B3
               WHEN OTHER
                   MOVE WS-RESID TO D-B4
           END-EVALUATE
           ADD WS-RESID TO T-DEBT
           IF D-FLAG = "Dis"
      *>       REQ: AR-017 a dispute classifies the debt, it does not
      *>       remove it from the age analysis
               COMPUTE D-DISP = FUNCTION NUMVAL(BR-COL(11))
               IF D-DISP > WS-RESID
                   MOVE WS-RESID TO D-DISP
               END-IF
               ADD D-DISP TO T-DISP
           END-IF
           GENERATE DE-DEBT.
       1300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-021  the same-date general ledger control balance
      *>----------------------------------------------------------------
       1900-CONTROL-ACCOUNT SECTION.
       1900-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(MIN(ac.AccountCode),'none'), "
                  "CONVERT(varchar(24),ISNULL(SUM("
                  "l.DebitAmountGBP - l.CreditAmountGBP),0)) FROM "
                  "fin.Account ac LEFT JOIN fin.JournalLine l ON "
                  "l.CompanyCode = ac.CompanyCode AND l.AccountCode "
                  "= ac.AccountCode LEFT JOIN fin.Journal j ON "
                  "j.JournalId = l.JournalId AND j.Status = 'POSTED' "
                  "AND j.PostingDate <= ? WHERE ac.CompanyCode = ? "
                  "AND ac.ControlType = 'AR'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-ASAT TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-CO TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               MOVE BR-COL(1) TO WS-CTL-ACCT
               COMPUTE WS-GL-BAL = FUNCTION NUMVAL(BR-COL(2))
           ELSE
               MOVE "n/a" TO WS-CTL-ACCT
               MOVE 0 TO WS-GL-BAL
           END-IF
           COMPUTE WS-DIFF = (T-DEBT - T-CRED - T-DEPOS) - WS-GL-BAL
           IF WS-DIFF > 0.005 OR WS-DIFF < -0.005
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1901 ledger and control account "
                       "differ - see ARRECON"
           END-IF.
       1900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-SET-WIDTHS SECTION.
       8000-START.
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 30
               MOVE 120 TO SQLIO-COL-WIDTH(AR-I)
           END-PERFORM.
       8000-EXIT.
           EXIT.

       8200-APPEND-SQL SECTION.
       8200-START.
      *>   the statement is well under 4000 bytes, so the tail in
      *>   AR-TEXT is simply laid after the last non-space character
           PERFORM VARYING AR-J FROM 3000 BY -1
               UNTIL AR-J < 2 OR SQLIO-STMT(AR-J:1) NOT = SPACE
               CONTINUE
           END-PERFORM
           ADD 1 TO AR-J
           PERFORM VARYING AR-K FROM 200 BY -1
               UNTIL AR-K < 2 OR AR-TEXT(AR-K:1) NOT = SPACE
               CONTINUE
           END-PERFORM
           MOVE AR-TEXT(1:AR-K) TO SQLIO-STMT(AR-J:AR-K)
           MOVE SPACES TO AR-TEXT.
       8200-EXIT.
           EXIT.

       8100-FORMAT-DATE SECTION.
       8100-START.
      *>   AR-NUM-DATE (yyyymmdd) -> WS-UK (dd/mm/yyyy)
           MOVE AR-NUM-DATE TO AR-DATE-ED
           MOVE AR-DATE-ED(1:4) TO AR-TEXT(1:4)
           COMPUTE WS-UK-YYYY = FUNCTION NUMVAL(AR-DATE-ED(1:4))
           COMPUTE WS-UK-MM = FUNCTION NUMVAL(AR-DATE-ED(5:2))
           COMPUTE WS-UK-DD = FUNCTION NUMVAL(AR-DATE-ED(7:2)).
       8100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-AGEDDEBT
           CLOSE RPT-FILE
           DISPLAY "STERP-I-0401 " WS-ROWS-OUT " open item(s) aged"
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "AGEDDEBT" TO JL-JOB-NAME
           MOVE WS-JOBID TO JL-JOB-RUN-ID
           EVALUATE TRUE
               WHEN WS-RC = 0   MOVE "COMPLETE" TO JL-STATUS
               WHEN WS-RC = 4   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER       MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE WS-ROWS-OUT TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
