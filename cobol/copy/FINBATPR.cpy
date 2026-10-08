      *>================================================================
      *> FINBATPR - STERP FINANCE BATCH SUITE COMMON PARAGRAPHS
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : GENERAL LEDGER (FIN)
      *> PURPOSE  : THE HOUSEKEEPING PARAGRAPHS SHARED BY EVERY FIN
      *>            BATCH PROGRAM - JOB LOGGING, SPOOL PATH, PAGE
      *>            HEADING FILL, COLUMN WIDTHS AND DATE EDITING.
      *>            COPY AS THE LAST THING IN THE PROCEDURE DIVISION.
      *>            REQUIRES COPY FINBATWK, SQLIOCA, STRPTWS AND THE
      *>            CMDARGS / RUNPATH / JOBLOG COMMAREAS RENAMED TO
      *>            WS-CMDARGS / WS-RUNPATH / WS-JOBLOG.
      *>            Set RPT-REPORT-ID (= job name), RPT-TITLE and
      *>            RPT-SUBTITLE before PERFORM 9200-OPEN-SPOOL.
      *>            The program keeps INITIATE / TERMINATE because the
      *>            report description name differs per program.
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-03-07 PGW  CR0255 ORIGINAL - THE 1994 LEDGER BATCH SUITE
      *> 1997-05-19 RJH  CR0702 COMPANY NAME AND REGISTERED NUMBER
      *> 2026-10-06 STERP core.JobRun REGISTRATION THROUGH JOBLOG
      *>================================================================
      *> REQ: FIN-023, COM-020
       9100-JOB-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE WS-ASAT TO CA-BUSINESS-DATE OF WS-JOBLOG
           MOVE RPT-REPORT-ID TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE JL-JOB-RUN-ID TO WS-JOBID.
       9100-EXIT.
           EXIT.

       9150-JOB-END.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE RPT-REPORT-ID TO JL-JOB-NAME
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
       9150-EXIT.
           EXIT.

       9200-OPEN-SPOOL.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE RPT-REPORT-ID TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-9201 cannot build the spool path"
               GO TO 9200-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           PERFORM 9250-FILL-HEADINGS THRU 9250-EXIT
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-9202 cannot open "
                       FUNCTION TRIM(WS-SPOOL-NAME)
           END-IF.
       9200-EXIT.
           EXIT.

       9250-FILL-HEADINGS.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO FB-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(FB-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(FB-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(FB-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(FB-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(FB-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(FB-NOW(13:2))
           PERFORM 9300-READ-COMPANY THRU 9300-EXIT.
       9250-EXIT.
           EXIT.

       9300-READ-COMPANY.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Name, ISNULL(RegisteredNumber,'') FROM "
                  "core.Company WHERE CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               MOVE BR-COL(1) TO RPT-COMPANY-NAME
               MOVE BR-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       9300-EXIT.
           EXIT.

       9350-CLOSE-SPOOL.
           CLOSE RPT-FILE
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       9350-EXIT.
           EXIT.

       9400-SET-WIDTHS.
           PERFORM VARYING FB-I FROM 1 BY 1 UNTIL FB-I > 30
               MOVE 120 TO SQLIO-COL-WIDTH(FB-I)
           END-PERFORM.
       9400-EXIT.
           EXIT.

       9450-FORMAT-DATE.
      *>   FB-NUM-DATE (yyyymmdd) -> WS-UK (dd/mm/yyyy)
           MOVE FB-NUM-DATE TO FB-DATE-ED
           COMPUTE WS-UK-YYYY = FUNCTION NUMVAL(FB-DATE-ED(1:4))
           COMPUTE WS-UK-MM = FUNCTION NUMVAL(FB-DATE-ED(5:2))
           COMPUTE WS-UK-DD = FUNCTION NUMVAL(FB-DATE-ED(7:2)).
       9450-EXIT.
           EXIT.

       9500-STD-ARGS.
      *>   CO, USER, ASAT, MODE - every fin batch program takes these
           INITIALIZE WS-CMDARGS
           MOVE "CMDLINE" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           MOVE CM-CO TO WS-CO
           MOVE CM-USER TO WS-USER
           IF CM-ASAT NOT = SPACES
               COMPUTE WS-ASAT = FUNCTION NUMVAL(CM-ASAT)
           END-IF
           IF CM-MODE NOT = SPACES
               MOVE CM-MODE TO WS-MODE
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "GLCLERK" TO WS-USER
           END-IF
           IF WS-ASAT = 0
               MOVE FUNCTION CURRENT-DATE TO FB-NOW
               COMPUTE WS-ASAT = FUNCTION NUMVAL(FB-NOW(1:8))
           END-IF.
       9500-EXIT.
           EXIT.

       9550-GET-ARG.
      *>   FB-TEXT holds the key on entry, the value on exit
           MOVE FB-TEXT(1:12) TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           MOVE SPACES TO FB-TEXT
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO FB-TEXT
           END-IF.
       9550-EXIT.
           EXIT.

       9600-RESOLVE-PERIOD.
      *>   WS-ASAT -> WS-FY / WS-PER from the open calendar
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT FiscalYear, PeriodNo, Status FROM "
                  "fin.Period WHERE CompanyCode = ? AND ? BETWEEN "
                  "CONVERT(char(8),StartDate,112) AND "
                  "CONVERT(char(8),EndDate,112)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-ASAT TO FB-DATE-ED
           MOVE FB-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 3 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               COMPUTE WS-FY = FUNCTION NUMVAL(BR-COL(1))
               COMPUTE WS-PER = FUNCTION NUMVAL(BR-COL(2))
           ELSE
               MOVE 0 TO WS-FY
               MOVE 0 TO WS-PER
           END-IF.
       9600-EXIT.
           EXIT.
