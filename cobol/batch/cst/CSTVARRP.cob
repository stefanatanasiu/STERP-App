      *>================================================================
      *> PROGRAM-ID : CSTVARRP
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : VARIANCE ANALYSIS (CST-019).  EVERY VARIANCE THE
      *>              COSTING ENGINE RAISED, GROUPED BY TYPE, SPLIT
      *>              INTO ITS QUANTITY AND RATE HALVES, WITH THE
      *>              OWNER WHO HAS TO EXPLAIN IT AND THE DISPOSAL
      *>              DECISION IF ONE HAS BEEN TAKEN.  A VARIANCE WITH
      *>              NO OWNER AND NO DISPOSAL IS FLAGGED - AN
      *>              UNEXPLAINED VARIANCE IS THE WHOLE POINT OF THE
      *>              REPORT.
      *> RUN AS     : CSTVARRP CO=STC [PERIOD=yyyynn] [TYPE=PPV]
      *>              [JOB=xxx] [ITEM=xxx] [MODE=ALL|UNDISPOSED]
      *>              [USER=xxx]
      *> OUTPUT     : run\spool\CSTVARRP-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-11-25 RJH  CR0548 ORIGINAL - VARIANCE LISTING
      *> 1999-11-30 KMP  Y2K - PERIOD AS YYYYNN ON THE PARAMETER
      *> 2002-04-08 DJM  CR0999 OWNER AND DISPOSAL COLUMNS
      *> 2026-10-06 STERP CST-019 QUANTITY AND RATE SPLIT
      *>================================================================
      *> REQ: CST-005, CST-006, CST-010, CST-011, CST-019, CST-024
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CSTVARRP.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 25/11/1997.
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
       FD  RPT-FILE REPORT IS R-CSTVARRP.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY CSTWORK.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-TYPE                 PIC X(16) VALUE SPACES.
           05  WS-JOB                  PIC X(20) VALUE SPACES.
           05  WS-ITEM                 PIC X(40) VALUE SPACES.
           05  WS-MODE                 PIC X(12) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-PERIOD               PIC X(8) VALUE SPACES.
           05  WS-FY                   PIC S9(4) VALUE 0.
           05  WS-PER-NO               PIC S9(4) VALUE 0.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-EOF                  PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-NEW-KEY              PIC X(16) VALUE SPACES.
      *>   type totals.  SUM cannot be used because the group has a
      *>   second detail line, so the figures are parked here as the
      *>   control breaks and the accumulators start again.
       01  WS-BAND.
           05  A-QTY-VAR               PIC S9(13)V99 VALUE 0.
           05  A-RATE-VAR              PIC S9(13)V99 VALUE 0.
           05  A-TOTAL-VAR             PIC S9(13)V99 VALUE 0.
           05  BT-QTY-VAR              PIC S9(13)V99 VALUE 0.
           05  BT-RATE-VAR             PIC S9(13)V99 VALUE 0.
           05  BT-TOTAL-VAR            PIC S9(13)V99 VALUE 0.
       01  WS-DET.
           05  D-TYPE                  PIC X(16) VALUE SPACES.
           05  D-TYPE-NAME             PIC X(34) VALUE SPACES.
           05  D-SUBJECT               PIC X(30) VALUE SPACES.
           05  D-DOC                   PIC X(26) VALUE SPACES.
           05  D-PERIOD                PIC X(8) VALUE SPACES.
           05  D-QTY-VAR               PIC S9(11)V99 VALUE 0.
           05  D-RATE-VAR              PIC S9(11)V99 VALUE 0.
           05  D-TOTAL-VAR             PIC S9(11)V99 VALUE 0.
           05  D-SENSE                 PIC X(4) VALUE SPACES.
           05  D-OWNER                 PIC X(12) VALUE SPACES.
           05  D-DISPOSAL              PIC X(16) VALUE SPACES.
           05  D-REASON                PIC X(34) VALUE SPACES.
       01  WS-TOT.
           05  T-ADVERSE               PIC S9(13)V99 VALUE 0.
           05  T-FAVOURABLE            PIC S9(13)V99 VALUE 0.
           05  T-NET                   PIC S9(13)V99 VALUE 0.
           05  T-ROWS                  PIC S9(7) VALUE 0.
           05  T-UNEXPLAINED           PIC S9(7) VALUE 0.
           05  T-UNEXPLAINED-GBP       PIC S9(13)V99 VALUE 0.
       REPORT SECTION.
       RD  R-CSTVARRP
           CONTROLS ARE FINAL D-TYPE
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 56
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-TYPE TYPE IS CONTROL HEADING D-TYPE.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(14) VALUE "Variance type ".
               10  COLUMN 15   PIC X(16) SOURCE D-TYPE.
               10  COLUMN 33   PIC X(34) SOURCE D-TYPE-NAME.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(14) VALUE "Item or job".
               10  COLUMN 34   PIC X(8) VALUE "Document".
               10  COLUMN 61   PIC X(6) VALUE "Period".
               10  COLUMN 72   PIC X(8) VALUE "Quantity".
               10  COLUMN 87   PIC X(4) VALUE "Rate".
               10  COLUMN 102  PIC X(5) VALUE "Total".
               10  COLUMN 117  PIC X(5) VALUE "Sense".
               10  COLUMN 124  PIC X(5) VALUE "Owner".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(64) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(30) SOURCE D-SUBJECT.
               10  COLUMN 34   PIC X(26) SOURCE D-DOC.
               10  COLUMN 61   PIC X(8) SOURCE D-PERIOD.
               10  COLUMN 69   PIC Z(8)9.99- SOURCE D-QTY-VAR.
               10  COLUMN 84   PIC Z(8)9.99- SOURCE D-RATE-VAR.
               10  COLUMN 99   PIC Z(8)9.99- SOURCE D-TOTAL-VAR.
               10  COLUMN 117  PIC X(4) SOURCE D-SENSE.
               10  COLUMN 123  PIC X(10) SOURCE D-OWNER.
       01  DR-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 8    PIC X(9) VALUE "disposal".
               10  COLUMN 18   PIC X(16) SOURCE D-DISPOSAL.
               10  COLUMN 36   PIC X(7) VALUE "reason".
               10  COLUMN 44   PIC X(34) SOURCE D-REASON.
       01  CF-TYPE TYPE IS CONTROL FOOTING D-TYPE.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(14) VALUE "Type total".
               10  COLUMN 18   PIC X(16) SOURCE D-TYPE.
               10  COLUMN 69   PIC Z(8)9.99- SOURCE BT-QTY-VAR.
               10  COLUMN 84   PIC Z(8)9.99- SOURCE BT-RATE-VAR.
               10  COLUMN 99   PIC Z(8)9.99- SOURCE BT-TOTAL-VAR.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(24) VALUE "Adverse variance".
               10  COLUMN 28   PIC Z(11)9.99 SOURCE T-ADVERSE.
               10  COLUMN 48   PIC X(22) VALUE "Favourable variance".
               10  COLUMN 70   PIC Z(11)9.99- SOURCE T-FAVOURABLE.
               10  COLUMN 90   PIC X(4) VALUE "Net".
               10  COLUMN 95   PIC Z(11)9.99- SOURCE T-NET.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(22) VALUE "Variances reported:".
               10  COLUMN 26   PIC Z(6)9 SOURCE T-ROWS.
               10  COLUMN 38   PIC X(30) VALUE
                   "without owner or disposal:".
               10  COLUMN 68   PIC Z(6)9 SOURCE T-UNEXPLAINED.
               10  COLUMN 80   PIC X(8) VALUE "value".
               10  COLUMN 89   PIC Z(11)9.99- SOURCE
                                              T-UNEXPLAINED-GBP.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(62) VALUE
                   "Adverse variances are positive.  A favourable one".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(62) VALUE
                   "is negative, so the column adds to the charge.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS
           PERFORM 0200-JOB-START
           PERFORM 0300-OPEN-SPOOL
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 1000-PRODUCE-REPORT
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
           MOVE CM-MODE TO WS-MODE
           MOVE CM-PERIOD TO WS-PERIOD
           MOVE "TYPE" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-TYPE
           END-IF
           MOVE "JOB" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-JOB
           END-IF
           MOVE "ITEM" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-ITEM
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "COST" TO WS-USER
           END-IF
           IF WS-MODE = SPACES
               MOVE "ALL" TO WS-MODE
           END-IF
      *>   PERIOD arrives as yyyynn - the fiscal year and the period
      *>   number were separate columns long before that convention
           MOVE 0 TO WS-FY WS-PER-NO
           IF WS-PERIOD NOT = SPACES
               COMPUTE WS-FY = FUNCTION NUMVAL(WS-PERIOD(1:4))
               COMPUTE WS-PER-NO = FUNCTION NUMVAL(WS-PERIOD(5:2))
           END-IF
           DISPLAY "STERP-I-0100 CSTVARRP variance analysis for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "CSTVARRP" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "CSTVARRP" TO RP-REPORT-ID
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
           INITIATE R-CSTVARRP.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "CSTVARRP" TO RPT-REPORT-ID
           MOVE "Variance analysis by type and owner" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE SPACES TO RPT-AS-AT-TEXT
           IF WS-PERIOD = SPACES
               MOVE "All periods" TO RPT-AS-AT-TEXT
           ELSE
               STRING "Period " FUNCTION TRIM(WS-PERIOD)
                   DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           END-IF
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-TYPE = SPACES
               STRING "All variance types" DELIMITED BY SIZE
                   INTO RPT-SUBTITLE
           ELSE
               STRING "Type " FUNCTION TRIM(WS-TYPE)
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           END-IF
           IF WS-MODE = "UNDISPOSED"
               MOVE "Undisposed only" TO RPT-STATUS-TEXT
           ELSE
               MOVE "All variances" TO RPT-STATUS-TEXT
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
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-OK
               MOVE WS-COL(1) TO RPT-COMPANY-NAME
               MOVE WS-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-PRODUCE-REPORT SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT v.VarianceType, "
                  "ISNULL(v.ItemCode, ISNULL(v.JobRef, "
                  "ISNULL(v.KitRef,'(general)'))), "
                  "LTRIM(RTRIM(ISNULL(v.SourceDocType,'') + ' ' + "
                  "ISNULL(v.SourceDocRef,''))), "
                  "ISNULL(CONVERT(varchar(4), v.FiscalYear),'') + "
                  "RIGHT('0' + ISNULL(CONVERT(varchar(2), "
                  "v.PeriodNo),''), 2), "
                  "v.QuantityVarianceGBP, v.RateVarianceGBP, "
                  "v.TotalVarianceGBP, v.IsAdverse, "
                  "ISNULL(v.OwnerUser,''), ISNULL(v.Disposal,''), "
                  "ISNULL(v.Reason, ISNULL(v.ReasonCode,'')) "
                  "FROM cst.Variance v WHERE v.CompanyCode = ? "
                  "AND (? = '' OR v.VarianceType = ?) "
                  "AND (? = '' OR ISNULL(v.JobRef,'') = ?) "
                  "AND (? = '' OR ISNULL(v.ItemCode,'') = ?) "
                  "AND (? = 0 OR ISNULL(v.FiscalYear,0) = ?) "
                  "AND (? = 0 OR ISNULL(v.PeriodNo,0) = ?) "
                  "ORDER BY v.VarianceType, v.FiscalYear, "
                  "v.PeriodNo, v.VarianceId"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 11 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-TYPE TO SQLIO-PARM-VAL(2)
           MOVE WS-TYPE TO SQLIO-PARM-VAL(3)
           MOVE WS-JOB TO SQLIO-PARM-VAL(4)
           MOVE WS-JOB TO SQLIO-PARM-VAL(5)
           MOVE WS-ITEM TO SQLIO-PARM-VAL(6)
           MOVE WS-ITEM TO SQLIO-PARM-VAL(7)
           MOVE WS-FY TO SQLIO-PARM-VAL(8)
           MOVE WS-FY TO SQLIO-PARM-VAL(9)
           MOVE WS-PER-NO TO SQLIO-PARM-VAL(10)
           MOVE WS-PER-NO TO SQLIO-PARM-VAL(11)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 11
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 11 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1001 " SQLIO-ERRMSG(1:100)
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-EOF
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-EOF
               ELSE
                   PERFORM 1100-EMIT-VARIANCE
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW.
       1000-EXIT.
           EXIT.

       1100-EMIT-VARIANCE SECTION.
       1100-START.
           MOVE WS-COL(1) TO WS-NEW-KEY
           PERFORM 1300-TYPE-NAME
           MOVE WS-COL(2) TO D-SUBJECT
           MOVE WS-COL(3) TO D-DOC
           MOVE WS-COL(4) TO D-PERIOD
           COMPUTE D-QTY-VAR = FUNCTION NUMVAL(WS-COL(5))
           COMPUTE D-RATE-VAR = FUNCTION NUMVAL(WS-COL(6))
           COMPUTE D-TOTAL-VAR = FUNCTION NUMVAL(WS-COL(7))
           MOVE WS-COL(9) TO D-OWNER
           MOVE WS-COL(10) TO D-DISPOSAL
           MOVE WS-COL(11) TO D-REASON
           IF WS-COL(8)(1:1) = "1"
               MOVE "ADV" TO D-SENSE
           ELSE
               MOVE "FAV" TO D-SENSE
           END-IF
           IF WS-MODE = "UNDISPOSED" AND D-DISPOSAL NOT = SPACES
               GO TO 1100-EXIT
           END-IF
           IF WS-NEW-KEY NOT = D-TYPE AND D-TYPE NOT = SPACES
               PERFORM 1400-CLOSE-TYPE
           END-IF
           MOVE WS-NEW-KEY TO D-TYPE
           IF D-TOTAL-VAR >= 0
               ADD D-TOTAL-VAR TO T-ADVERSE
           ELSE
               ADD D-TOTAL-VAR TO T-FAVOURABLE
           END-IF
           ADD D-QTY-VAR TO A-QTY-VAR
           ADD D-RATE-VAR TO A-RATE-VAR
           ADD D-TOTAL-VAR TO T-NET A-TOTAL-VAR
           ADD 1 TO T-ROWS WS-ROWS-OUT
           IF D-OWNER = SPACES AND D-DISPOSAL = SPACES
               ADD 1 TO T-UNEXPLAINED
               ADD D-TOTAL-VAR TO T-UNEXPLAINED-GBP
               MOVE "UNEXPLAINED" TO D-DISPOSAL
               IF D-REASON = SPACES
                   MOVE "*** no owner and no disposal" TO D-REASON
               END-IF
           END-IF
           GENERATE DE-LINE
           IF D-DISPOSAL NOT = SPACES OR D-REASON NOT = SPACES
               GENERATE DR-LINE
           END-IF.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1300 - the long names were a 2002 change request; they live in
      *> the program rather than a table because the check constraint
      *> on cst.Variance already fixes the list.
      *>----------------------------------------------------------------
       1300-TYPE-NAME SECTION.
       1300-START.
           EVALUATE WS-NEW-KEY
               WHEN "PPV"
                   MOVE "Purchase price at receipt" TO D-TYPE-NAME
               WHEN "INVOICEPRICE"
                   MOVE "Invoice price, incremental" TO D-TYPE-NAME
               WHEN "MATUSAGE"
                   MOVE "Material usage against standard"
                       TO D-TYPE-NAME
               WHEN "MATPRICE"
                   MOVE "Material price on issue" TO D-TYPE-NAME
               WHEN "LABEFF"
                   MOVE "Labour efficiency" TO D-TYPE-NAME
               WHEN "LABRATE"
                   MOVE "Labour rate" TO D-TYPE-NAME
               WHEN "OVHD"
                   MOVE "Overhead absorption" TO D-TYPE-NAME
               WHEN "RETURNVAL"
                   MOVE "Return valued at standard" TO D-TYPE-NAME
               WHEN "LANDED"
                   MOVE "Landed cost after settlement"
                       TO D-TYPE-NAME
               WHEN "REVALUE"
                   MOVE "Standard cost revaluation" TO D-TYPE-NAME
               WHEN "YIELD"
                   MOVE "Process yield" TO D-TYPE-NAME
               WHEN OTHER
                   MOVE "Other" TO D-TYPE-NAME
           END-EVALUATE.
       1300-EXIT.
           EXIT.

       1400-CLOSE-TYPE SECTION.
       1400-START.
           MOVE A-QTY-VAR TO BT-QTY-VAR
           MOVE A-RATE-VAR TO BT-RATE-VAR
           MOVE A-TOTAL-VAR TO BT-TOTAL-VAR
           MOVE 0 TO A-QTY-VAR A-RATE-VAR A-TOTAL-VAR.
       1400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           PERFORM 1400-CLOSE-TYPE
           TERMINATE R-CSTVARRP
           CLOSE RPT-FILE
           DISPLAY "STERP-I-0401 " WS-ROWS-OUT " variance(s)"
           IF T-UNEXPLAINED > 0
               DISPLAY "STERP-W-0402 " T-UNEXPLAINED
                       " variance(s) have no owner"
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
           END-IF
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "CSTVARRP" TO JL-JOB-NAME
           IF WS-RC > 4
               MOVE "FAILED" TO JL-STATUS
           ELSE
               MOVE "COMPLETE" TO JL-STATUS
           END-IF
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE WS-ROWS-OUT TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
