      *>================================================================
      *> XMDBATPR - STERP CROSS MODULE BATCH SUITE COMMON PARAGRAPHS
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : CROSS MODULE CONTROLS AND REPORTING (XMD)
      *> PURPOSE  : HOUSEKEEPING SHARED BY EVERY XMD BATCH PROGRAM -
      *>            JOB LOGGING, SPOOL PATH, PAGE HEADING FILL, FETCH
      *>            COLUMN WIDTHS, DATE EDITING, PERIOD RESOLUTION,
      *>            PARAMETER READING AND EXCEPTION RAISING.
      *>            COPY AS THE LAST THING IN THE PROCEDURE DIVISION.
      *>            REQUIRES COPY XMDBATWK, SQLIOCA, STRPTWS AND THE
      *>            CMDARGS / RUNPATH / JOBLOG / PARMGET / EXCPRAIS
      *>            COMMAREAS RENAMED TO WS-CMDARGS / WS-RUNPATH /
      *>            WS-JOBLOG / WS-PARMGET / WS-EXCPRAIS.
      *>            Set RPT-REPORT-ID (= job name), RPT-TITLE and
      *>            RPT-SUBTITLE before PERFORM 9200-OPEN-SPOOL.
      *>            INITIATE / TERMINATE stay with the program because
      *>            the report description name differs per program.
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-04-19 DJM  CR0163 ORIGINAL - CONTROL ACCOUNT BATCH SUITE
      *> 1997-01-22 RJH  CR0611 EXCEPTION RAISING MOVED OUT OF EACH
      *>                 PROGRAM AND INTO THE SUITE COPYBOOK
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS THROUGHOUT
      *> 2026-10-06 STERP core.JobRun REGISTRATION THROUGH JOBLOG
      *>================================================================
      *> REQ: XMD-006, COM-018, COM-020, COM-022
       9100-JOB-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE XB-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE XB-USER TO CA-USER OF WS-JOBLOG
           MOVE XB-ASAT TO CA-BUSINESS-DATE OF WS-JOBLOG
           MOVE RPT-REPORT-ID TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE JL-JOB-RUN-ID TO XB-JOBID.
       9100-EXIT.
           EXIT.

       9150-JOB-END.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE XB-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE XB-USER TO CA-USER OF WS-JOBLOG
           MOVE RPT-REPORT-ID TO JL-JOB-NAME
           MOVE XB-JOBID TO JL-JOB-RUN-ID
           EVALUATE TRUE
               WHEN XB-RC = 0   MOVE "COMPLETE" TO JL-STATUS
               WHEN XB-RC = 4   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER       MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE XB-RC TO JL-RETURN-CODE
           MOVE XB-ROWS-OUT TO JL-ROWS-PROCESSED
           MOVE XB-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       9150-EXIT.
           EXIT.

       9200-OPEN-SPOOL.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE RPT-REPORT-ID TO RP-REPORT-ID
           MOVE XB-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO XB-RC
               DISPLAY "STERP-E-9201 cannot build the spool path"
               GO TO 9200-EXIT
           END-IF
           MOVE RP-FULL-PATH TO XB-SPOOL-NAME
           PERFORM 9250-FILL-HEADINGS THRU 9250-EXIT
           OPEN OUTPUT RPT-FILE
           IF XB-FILE-STATUS NOT = "00"
               MOVE 12 TO XB-RC
               DISPLAY "STERP-E-9202 cannot open "
                       FUNCTION TRIM(XB-SPOOL-NAME)
           END-IF.
       9200-EXIT.
           EXIT.

       9250-FILL-HEADINGS.
           MOVE XB-CO TO RPT-COMPANY-CODE
           MOVE XB-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO XB-NOW
           MOVE XB-NOW(1:14) TO XB-STAMP
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(XB-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(XB-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(XB-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(XB-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(XB-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(XB-NOW(13:2))
           PERFORM 9300-READ-COMPANY THRU 9300-EXIT.
       9250-EXIT.
           EXIT.

       9300-READ-COMPANY.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Name, ISNULL(RegisteredNumber,''), "
                  "ISNULL(Jurisdiction,'England and Wales'), "
                  "ISNULL(AccountingRefDate,'03-31'), "
                  "ISNULL(AccountingFramework,'FRS102'), "
                  "ISNULL(VatRegNo,''), ISNULL(VatStagger,''), "
                  "ISNULL(VatScheme,'INVOICE'), "
                  "CONVERT(char(1),ISNULL(PaymentPracticesInScope,0"
                  ")), CONVERT(char(1),ISNULL(PackagingInScope,0)), "
                  "ISNULL(LegalForm,'LTD') FROM core.Company "
                  "WHERE CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 11 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               MOVE BR-COL(1) TO RPT-COMPANY-NAME
               MOVE BR-COL(2) TO RPT-REG-NUMBER
               MOVE BR-COL(3) TO RPT-REG-PLACE
           ELSE
               MOVE XB-CO TO RPT-COMPANY-NAME
           END-IF.
       9300-EXIT.
           EXIT.

       9350-CLOSE-SPOOL.
           CLOSE RPT-FILE
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(XB-SPOOL-NAME).
       9350-EXIT.
           EXIT.

       9400-SET-WIDTHS.
           PERFORM VARYING XB-K FROM 1 BY 1 UNTIL XB-K > 30
               MOVE 120 TO SQLIO-COL-WIDTH(XB-K)
           END-PERFORM.
       9400-EXIT.
           EXIT.

       9450-FORMAT-DATE.
      *>   XB-NUM-DATE (yyyymmdd) -> XB-UK (dd/mm/yyyy)
           IF XB-NUM-DATE = 0
               MOVE 0 TO XB-UK-DD
               MOVE 0 TO XB-UK-MM
               MOVE 0 TO XB-UK-YYYY
               GO TO 9450-EXIT
           END-IF
           MOVE XB-NUM-DATE TO XB-DATE-ED
           COMPUTE XB-UK-YYYY = FUNCTION NUMVAL(XB-DATE-ED(1:4))
           COMPUTE XB-UK-MM = FUNCTION NUMVAL(XB-DATE-ED(5:2))
           COMPUTE XB-UK-DD = FUNCTION NUMVAL(XB-DATE-ED(7:2)).
       9450-EXIT.
           EXIT.

       9460-FORMAT-DATE2.
      *>   XB-NUM-DATE (yyyymmdd) -> XB-UK2 (dd/mm/yyyy)
           IF XB-NUM-DATE = 0
               MOVE 0 TO XB-UK2-DD
               MOVE 0 TO XB-UK2-MM
               MOVE 0 TO XB-UK2-YYYY
               GO TO 9460-EXIT
           END-IF
           MOVE XB-NUM-DATE TO XB-DATE-ED
           COMPUTE XB-UK2-YYYY = FUNCTION NUMVAL(XB-DATE-ED(1:4))
           COMPUTE XB-UK2-MM = FUNCTION NUMVAL(XB-DATE-ED(5:2))
           COMPUTE XB-UK2-DD = FUNCTION NUMVAL(XB-DATE-ED(7:2)).
       9460-EXIT.
           EXIT.

       9470-ISO-TO-NUM.
      *>   XB-TEXT holds yyyy-mm-dd or yyyymmdd -> XB-NUM-DATE
           MOVE 0 TO XB-NUM-DATE
           IF XB-TEXT(5:1) = "-"
               MOVE XB-TEXT(1:4) TO XB-DATE-ED(1:4)
               MOVE XB-TEXT(6:2) TO XB-DATE-ED(5:2)
               MOVE XB-TEXT(9:2) TO XB-DATE-ED(7:2)
               MOVE XB-DATE-ED TO XB-NUM-DATE
           ELSE
               IF XB-TEXT(1:8) NUMERIC
                   COMPUTE XB-NUM-DATE =
                       FUNCTION NUMVAL(XB-TEXT(1:8))
               END-IF
           END-IF.
       9470-EXIT.
           EXIT.

       9500-STD-ARGS.
      *>   CO, SITE, USER, ASAT, MODE - every xmd batch job takes these
           INITIALIZE WS-CMDARGS
           MOVE "CMDLINE" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           MOVE CM-CO TO XB-CO
           MOVE CM-SITE TO XB-SITE
           MOVE CM-USER TO XB-USER
           IF CM-ASAT NOT = SPACES
               COMPUTE XB-ASAT = FUNCTION NUMVAL(CM-ASAT)
           END-IF
           IF CM-FROM NOT = SPACES
               COMPUTE XB-FROM = FUNCTION NUMVAL(CM-FROM)
           END-IF
           IF CM-TO NOT = SPACES
               COMPUTE XB-TO = FUNCTION NUMVAL(CM-TO)
           END-IF
           IF CM-MODE NOT = SPACES
               MOVE FUNCTION UPPER-CASE(CM-MODE) TO XB-MODE
           END-IF
           IF XB-CO = SPACES
               MOVE "STC" TO XB-CO
           END-IF
           IF XB-USER = SPACES
               MOVE "FINCTRL" TO XB-USER
           END-IF
           IF XB-MODE = SPACES
               MOVE "PREVIEW" TO XB-MODE
           END-IF
           IF XB-ASAT = 0
               MOVE FUNCTION CURRENT-DATE TO XB-NOW
               COMPUTE XB-ASAT = FUNCTION NUMVAL(XB-NOW(1:8))
           END-IF.
       9500-EXIT.
           EXIT.

       9550-GET-ARG.
      *>   XB-TEXT holds the key on entry, the value on exit
           MOVE XB-TEXT(1:12) TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           MOVE SPACES TO XB-TEXT
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO XB-TEXT
           END-IF.
       9550-EXIT.
           EXIT.

       9600-RESOLVE-PERIOD.
      *>   XB-ASAT -> XB-FY / XB-PER / XB-PERIOD-STATUS
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT FiscalYear, PeriodNo, Status FROM "
                  "fin.Period WHERE CompanyCode = ? AND ? BETWEEN "
                  "CONVERT(char(8),StartDate,112) AND "
                  "CONVERT(char(8),EndDate,112)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE XB-ASAT TO XB-DATE-ED
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 3 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               COMPUTE XB-FY = FUNCTION NUMVAL(BR-COL(1))
               COMPUTE XB-PER = FUNCTION NUMVAL(BR-COL(2))
               MOVE BR-COL(3) TO XB-PERIOD-STATUS
           ELSE
               MOVE 0 TO XB-FY
               MOVE 0 TO XB-PER
               MOVE "NONE" TO XB-PERIOD-STATUS
           END-IF.
       9600-EXIT.
           EXIT.

       9650-GET-PARM-NUM.
      *>   XB-CODE holds the parameter code, XB-AMT the default on
      *>   entry and the effective value on exit (COM-025)
           INITIALIZE WS-PARMGET
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARMGET
           MOVE XB-CO TO CA-COMPANY OF WS-PARMGET
           MOVE XB-USER TO CA-USER OF WS-PARMGET
           MOVE XB-ASAT TO CA-BUSINESS-DATE OF WS-PARMGET
           MOVE XB-CODE TO PM-PARAM-CODE
           MOVE XB-ASAT TO PM-AS-OF
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE XB-AMT TO XB-MON-ED
           MOVE FUNCTION TRIM(XB-MON-ED) TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARMGET
           IF CA-RETURN-CODE OF WS-PARMGET < 8
               IF PM-IS-NUMERIC = "Y"
                   MOVE PM-VALUE-NUM TO XB-AMT
               END-IF
           END-IF.
       9650-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-006, COM-022  every exception carries an owner, a
      *> due date and the contributing document - an unowned exception
      *> is refused by EXCPRAIS and never silently dropped
      *>----------------------------------------------------------------
       9700-RAISE-EXCEPTION.
           IF NOT XB-COMMIT
               ADD 1 TO XB-RAISED
               GO TO 9700-EXIT
           END-IF
           MOVE XB-CO TO CA-COMPANY OF WS-EXCPRAIS
           MOVE XB-SITE TO CA-SITE OF WS-EXCPRAIS
           MOVE XB-USER TO CA-USER OF WS-EXCPRAIS
           MOVE XB-ASAT TO CA-BUSINESS-DATE OF WS-EXCPRAIS
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXCPRAIS
           MOVE "xmd" TO EX-MODULE
           CALL "EXCPRAIS" USING WS-EXCPRAIS
           IF CA-RETURN-CODE OF WS-EXCPRAIS > 6
               DISPLAY "STERP-W-9701 exception refused "
                       CA-REASON-CODE OF WS-EXCPRAIS " "
                       FUNCTION TRIM(EX-DOCUMENT-REF)
           ELSE
               ADD 1 TO XB-RAISED
           END-IF.
       9700-EXIT.
           EXIT.
