      *>================================================================
      *> PROGRAM-ID : APAGED
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : AGED CREDITORS AND CASH REQUIREMENT.  EVERY OPEN
      *>              ITEM IS AGED INTO CURRENT / 30 / 60 / 90 / OVER
      *>              90 BUCKETS, BY DUE DATE OR BY DOCUMENT DATE, AND
      *>              THE CASH NEEDED IN THE NEXT 7, 14 AND 30 DAYS IS
      *>              SUMMARISED.  BLOCKED AND DISPUTED ITEMS ARE
      *>              FLAGGED BECAUSE THEY ARE NOT A CASH COMMITMENT
      *>              UNTIL THEY ARE RELEASED.
      *> RUN AS     : APAGED CO=STC ASAT=yyyymmdd USER=APMGR
      *>              [MODE=DUE|INVOICE]
      *> OUTPUT     : run\spool\APAGED-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-05-22 RJH  CR0122 ORIGINAL - AGED CREDITOR LISTING
      *> 1994-09-08 RJH  CR0540 CASH REQUIREMENT COLUMNS ADDED
      *> 1999-11-02 KMP  Y2K - 4 DIGIT YEARS
      *> 2026-10-06 STERP AP-026 DISPUTED ITEMS EXCLUDED FROM CASH
      *>================================================================
      *> REQ: AP-026, AP-011, AP-009
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APAGED.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 22/05/1991.
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
       FD  RPT-FILE REPORT IS R-APAGED.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APBATWK.
       COPY STRPTWS.
       COPY STBATLOG.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-ASAT                 PIC 9(8) VALUE 0.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
           05  WS-SUPP                 PIC X(20) VALUE SPACES.
           05  WS-SUBTITLE-HOLD        PIC X(60) VALUE SPACES.
       01  WS-RUN-STATE.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-AGE-DAYS             PIC S9(7) VALUE 0.
           05  WS-ASAT-INT             PIC S9(9) VALUE 0.
           05  WS-DUE-INT              PIC S9(9) VALUE 0.
       01  WS-DET.
           05  D-SUPPLIER              PIC X(20) VALUE SPACES.
           05  D-SUPP-NAME             PIC X(34) VALUE SPACES.
           05  D-DOC-REF               PIC X(22) VALUE SPACES.
           05  D-DOC-DATE              PIC X(10) VALUE SPACES.
           05  D-DUE-DATE              PIC X(10) VALUE SPACES.
           05  D-STATE                 PIC X(10) VALUE SPACES.
           05  D-CURRENT               PIC S9(13)V99 VALUE 0.
           05  D-B30                   PIC S9(13)V99 VALUE 0.
           05  D-B60                   PIC S9(13)V99 VALUE 0.
           05  D-B90                   PIC S9(13)V99 VALUE 0.
           05  D-BOVER                 PIC S9(13)V99 VALUE 0.
           05  D-BALANCE               PIC S9(13)V99 VALUE 0.
       01  WS-TOT.
           05  T-CASH-7                PIC S9(15)V99 VALUE 0.
           05  T-CASH-14               PIC S9(15)V99 VALUE 0.
           05  T-CASH-30               PIC S9(15)V99 VALUE 0.
           05  T-BLOCKED               PIC S9(15)V99 VALUE 0.
           05  T-ITEMS                 PIC S9(7) VALUE 0.
       REPORT SECTION.
       RD  R-APAGED
           CONTROLS ARE FINAL D-SUPPLIER
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 54
           FOOTING 56.
       COPY STRPTHDR.
       01  CH-SUPP TYPE IS CONTROL HEADING D-SUPPLIER.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(9) VALUE "Supplier ".
               10  COLUMN 11   PIC X(20) SOURCE D-SUPPLIER.
               10  COLUMN 33   PIC X(34) SOURCE D-SUPP-NAME.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(20) VALUE "Document            ".
               10  COLUMN 25   PIC X(10) VALUE "Doc date  ".
               10  COLUMN 37   PIC X(10) VALUE "Due date  ".
               10  COLUMN 49   PIC X(7) VALUE "State  ".
               10  COLUMN 60   PIC X(7) VALUE "Current".
               10  COLUMN 74   PIC X(6) VALUE "1-30  ".
               10  COLUMN 88   PIC X(6) VALUE "31-60 ".
               10  COLUMN 102  PIC X(6) VALUE "61-90 ".
               10  COLUMN 116  PIC X(7) VALUE "Over 90".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(66) VALUE ALL "-".
               10  COLUMN 69   PIC X(64) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(22) SOURCE D-DOC-REF.
               10  COLUMN 25   PIC X(10) SOURCE D-DOC-DATE.
               10  COLUMN 37   PIC X(10) SOURCE D-DUE-DATE.
               10  COLUMN 49   PIC X(10) SOURCE D-STATE.
               10  COLUMN 60   PIC Z(8)9.99- SOURCE D-CURRENT.
               10  COLUMN 74   PIC Z(8)9.99- SOURCE D-B30.
               10  COLUMN 88   PIC Z(8)9.99- SOURCE D-B60.
               10  COLUMN 102  PIC Z(8)9.99- SOURCE D-B90.
               10  COLUMN 116  PIC Z(8)9.99- SOURCE D-BOVER.
       01  CF-SUPP TYPE IS CONTROL FOOTING D-SUPPLIER.
           05  LINE PLUS 1.
               10  COLUMN 37   PIC X(20) VALUE "Supplier total      ".
               10  COLUMN 60   PIC Z(8)9.99- SUM D-CURRENT.
               10  COLUMN 74   PIC Z(8)9.99- SUM D-B30.
               10  COLUMN 88   PIC Z(8)9.99- SUM D-B60.
               10  COLUMN 102  PIC Z(8)9.99- SUM D-B90.
               10  COLUMN 116  PIC Z(8)9.99- SUM D-BOVER.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 37   PIC X(20) VALUE "Creditors total     ".
               10  COLUMN 60   PIC Z(8)9.99- SUM D-CURRENT.
               10  COLUMN 74   PIC Z(8)9.99- SUM D-B30.
               10  COLUMN 88   PIC Z(8)9.99- SUM D-B60.
               10  COLUMN 102  PIC Z(8)9.99- SUM D-B90.
               10  COLUMN 116  PIC Z(8)9.99- SUM D-BOVER.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(34) VALUE
                   "CASH REQUIREMENT (unblocked items)".
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(24) VALUE "Due within 7 days     ".
               10  COLUMN 30   PIC Z(12)9.99- SOURCE T-CASH-7.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(24) VALUE "Due within 14 days    ".
               10  COLUMN 30   PIC Z(12)9.99- SOURCE T-CASH-14.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(24) VALUE "Due within 30 days    ".
               10  COLUMN 30   PIC Z(12)9.99- SOURCE T-CASH-30.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(24) VALUE "Blocked or disputed   ".
               10  COLUMN 30   PIC Z(12)9.99- SOURCE T-BLOCKED.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(24) VALUE "Open items aged       ".
               10  COLUMN 30   PIC Z(6)9 SOURCE T-ITEMS.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(58) VALUE
                   "Blocked and disputed items are not a cash".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(58) VALUE
                   "commitment until they are released.".
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           PERFORM 0100-READ-ARGS
           PERFORM 0200-JOB-START
           IF WS-RC < 8
               PERFORM 0300-OPEN-SPOOL
           END-IF
           IF WS-RC < 8
               PERFORM 1000-PRODUCE-REPORT
               PERFORM 0400-CLOSE-SPOOL
           END-IF.
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
           MOVE CM-MODE TO WS-MODE
           IF CM-ASAT NOT = SPACES
               COMPUTE WS-ASAT = FUNCTION NUMVAL(CM-ASAT)
           END-IF
      *>   AP-012: the creditors clerk often wants one supplier only.
           MOVE "SUPP" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE FUNCTION UPPER-CASE(CM-LOOKUP-VALUE) TO WS-SUPP
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "APMGR" TO WS-USER
           END-IF
           IF WS-MODE = SPACES
               MOVE "DUE" TO WS-MODE
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           IF WS-ASAT = 0
               MOVE WS-TODAY-N TO WS-ASAT
           END-IF
           MOVE WS-MODE TO BL-MODE
           COMPUTE WS-ASAT-INT = FUNCTION INTEGER-OF-DATE(WS-ASAT)
           DISPLAY "STERP-I-0100 APAGED aged creditors for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "APAGED" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE "APAGED" TO BL-JOB-NAME.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "APAGED" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0301 cannot build the spool path"
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           MOVE RP-FULL-PATH TO BL-SPOOL-FILE
           PERFORM 0350-FILL-HEADINGS
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0302 cannot open the spool file"
               GO TO 0300-EXIT
           END-IF
           INITIATE R-APAGED.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "APAGED" TO RPT-REPORT-ID
           MOVE "Aged creditors and cash requirement" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE WS-ASAT TO WS-DATE-N
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "As at " WS-DATE-DD "/" WS-DATE-MM "/"
                  WS-DATE-YYYY
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-MODE = "INVOICE"
               MOVE "Aged by document date" TO RPT-SUBTITLE
           ELSE
               MOVE "Aged by due date" TO RPT-SUBTITLE
           END-IF
           IF WS-SUPP NOT = SPACES
               MOVE RPT-SUBTITLE TO WS-SUBTITLE-HOLD
               MOVE SPACES TO RPT-SUBTITLE
               STRING FUNCTION TRIM(WS-SUBTITLE-HOLD)
                      " - supplier " FUNCTION TRIM(WS-SUPP)
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
           PERFORM 0290-SET-WIDTHS
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-OK
               MOVE AP-COL(1) TO RPT-COMPANY-NAME
               MOVE AP-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.

       0290-SET-WIDTHS SECTION.
       0290-START.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0290-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-PRODUCE-REPORT SECTION.
       1000-START.
           PERFORM 1100-OPEN-ITEMS
           IF WS-SQL-FAILED = "Y"
               MOVE 12 TO WS-RC
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-DONE = "Y"
               PERFORM 1200-FETCH-ITEM
               IF WS-DONE NOT = "Y"
                   PERFORM 1300-AGE-ITEM
                   PERFORM 1400-CASH-REQUIREMENT
                   ADD 1 TO T-ITEMS
                   ADD 1 TO BL-WRITTEN-COUNT
                   GENERATE DE-LINE
               END-IF
           END-PERFORM
           PERFORM 1900-CLOSE-CURSOR.
       1000-EXIT.
           EXIT.

       1100-OPEN-ITEMS SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT SupplierCode, SupplierName, "
                  "DocumentNumber, "
                  "ISNULL(CONVERT(varchar(10),DocumentDate,103),''), "
                  "ISNULL(CONVERT(varchar(10),DueDate,103),''), "
                  "CONVERT(varchar(30), ResidualAmountGBP), "
                  "BlockState, "
                  "CONVERT(varchar(10), DueDate, 112), "
                  "CONVERT(varchar(10), DocumentDate, 112) "
                  "FROM ap.vw_OpenItemAge "
                  "WHERE CompanyCode = ? AND ResidualAmountGBP <> 0 "
                  "AND DocumentDate <= ? "
                  "AND (? = '' OR SupplierCode = ?) "
                  "ORDER BY SupplierCode, DocumentNumber"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-ASAT TO WS-DATE-N
           MOVE SPACES TO SQLIO-PARM-VAL(2)
           STRING WS-DATE-YYYY "-" WS-DATE-MM "-" WS-DATE-DD
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE WS-SUPP TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE WS-SUPP TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE 9 TO SQLIO-COL-CNT
           PERFORM 0290-SET-WIDTHS
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-ERROR
               MOVE "Y" TO WS-SQL-FAILED
               DISPLAY "STERP-E-1101 " SQLIO-ERRMSG
           END-IF.
       1100-EXIT.
           EXIT.

       1200-FETCH-ITEM SECTION.
       1200-START.
           MOVE 6 TO SQLIO-CURSOR
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "Y" TO WS-DONE
               GO TO 1200-EXIT
           END-IF
           ADD 1 TO BL-READ-COUNT
           MOVE AP-COL(1) TO D-SUPPLIER
           MOVE AP-COL(2) TO D-SUPP-NAME
           MOVE AP-COL(3) TO D-DOC-REF
           MOVE AP-COL(4) TO D-DOC-DATE
           MOVE AP-COL(5) TO D-DUE-DATE
           COMPUTE D-BALANCE = FUNCTION NUMVAL(AP-COL(6))
           MOVE AP-COL(7) TO D-STATE
           IF WS-MODE = "INVOICE"
               COMPUTE WS-DATE-N = FUNCTION NUMVAL(AP-COL(9))
           ELSE
               COMPUTE WS-DATE-N = FUNCTION NUMVAL(AP-COL(8))
           END-IF
           COMPUTE WS-DUE-INT = FUNCTION INTEGER-OF-DATE(WS-DATE-N).
       1200-EXIT.
           EXIT.

      *>   Positive age means overdue.  An item due in the future is
      *>   current whichever basis was asked for.
       1300-AGE-ITEM SECTION.
       1300-START.
           MOVE 0 TO D-CURRENT D-B30 D-B60 D-B90 D-BOVER
           COMPUTE WS-AGE-DAYS = WS-ASAT-INT - WS-DUE-INT
           EVALUATE TRUE
               WHEN WS-AGE-DAYS NOT > 0
                   MOVE D-BALANCE TO D-CURRENT
               WHEN WS-AGE-DAYS NOT > 30
                   MOVE D-BALANCE TO D-B30
               WHEN WS-AGE-DAYS NOT > 60
                   MOVE D-BALANCE TO D-B60
               WHEN WS-AGE-DAYS NOT > 90
                   MOVE D-BALANCE TO D-B90
               WHEN OTHER
                   MOVE D-BALANCE TO D-BOVER
           END-EVALUATE.
       1300-EXIT.
           EXIT.

      *>   REQ: AP-026 - a blocked or disputed item is reported but it
      *>   is not cash the business has to find.
       1400-CASH-REQUIREMENT SECTION.
       1400-START.
           IF D-STATE = "BLOCKED" OR D-STATE = "DISPUTED"
               ADD D-BALANCE TO T-BLOCKED
               GO TO 1400-EXIT
           END-IF
           IF D-BALANCE NOT > 0
               GO TO 1400-EXIT
           END-IF
           COMPUTE WS-AGE-DAYS = WS-DUE-INT - WS-ASAT-INT
           IF WS-AGE-DAYS NOT > 7
               ADD D-BALANCE TO T-CASH-7
           END-IF
           IF WS-AGE-DAYS NOT > 14
               ADD D-BALANCE TO T-CASH-14
           END-IF
           IF WS-AGE-DAYS NOT > 30
               ADD D-BALANCE TO T-CASH-30
           END-IF.
       1400-EXIT.
           EXIT.

       1900-CLOSE-CURSOR SECTION.
       1900-START.
           MOVE 6 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW.
       1900-EXIT.
           EXIT.

       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-APAGED
           CLOSE RPT-FILE.
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           MOVE WS-RC TO BL-RETURN-CODE
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "APAGED" TO JL-JOB-NAME
           MOVE BL-WRITTEN-COUNT TO JL-ROWS-PROCESSED
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE BL-SPOOL-FILE TO JL-SPOOL-FILE
           MOVE WS-RC TO CA-RETURN-CODE OF WS-JOBLOG
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE T-ITEMS TO BL-COUNT-ED
           DISPLAY "STERP-I-0500 APAGED aged " BL-COUNT-ED
                   " items to " FUNCTION TRIM(WS-SPOOL-NAME)
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0500-EXIT.
           EXIT.
