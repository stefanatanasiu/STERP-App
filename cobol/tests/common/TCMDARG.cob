      *>================================================================
      *> TCMDARG - unit tests for CMDARGS and RUNPATH, the two helpers
      *> every batch main program uses.  Arguments are KEY=VALUE pairs
      *> as used by the job streams; output files land under
      *> %STERP_RUN% in the folder for their class.
      *>================================================================
      *> REQ: COM-018, COM-020
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCMDARG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       01  WS-RUN-ENV                  PIC X(160) VALUE SPACES.
       01  WS-EXPECT                   PIC X(200) VALUE SPACES.
       01  WS-OK                       PIC X VALUE "Y".
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CM-CA==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RP-CA==
                      LEADING ==CA-== BY ==RPH-==.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TCMDARG" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-PARSE-TESTS
           PERFORM 2000-LOOKUP-TESTS
           PERFORM 3000-PARSE-ERRORS
           PERFORM 4000-RUNPATH-TESTS
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
       1000-PARSE-TESTS.
           MOVE "a typical job stream line is parsed" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "AGEDDEBT CO=STC ASAT=20261004 USER=ARCLERK1"
               TO CM-COMMAND-LINE OF WS-CM-CA
           PERFORM 8100-CALL-PARSE
           MOVE 3 TO TK-EXPECTED-N
           MOVE CM-PAIR-CNT OF WS-CM-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "the common keys are returned in their own fields"
               TO TK-NAME
           IF CM-CO OF WS-CM-CA = "STC"
              AND CM-ASAT OF WS-CM-CA = "20261004"
              AND CM-USER OF WS-CM-CA = "ARCLERK1"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "the leading program name is ignored" TO TK-NAME
           MOVE "CO" TO TK-EXPECTED-X
           MOVE CM-KEY OF WS-CM-CA(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "keys are folded to upper case" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "co=stc mode=commit" TO CM-COMMAND-LINE OF WS-CM-CA
           PERFORM 8100-CALL-PARSE
           IF CM-KEY OF WS-CM-CA(1) = "CO"
              AND CM-CO OF WS-CM-CA = "stc"
              AND CM-MODE OF WS-CM-CA = "COMMIT"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "a quoted value may contain spaces" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "CO=STC OUT=""monthly run.lst"" MODE=PREVIEW"
               TO CM-COMMAND-LINE OF WS-CM-CA
           PERFORM 8100-CALL-PARSE
           MOVE "monthly run.lst" TO TK-EXPECTED-X
           MOVE CM-OUT OF WS-CM-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "runs of spaces between arguments are tolerated"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "CO=STC    PERIOD=202607   MODE=COMMIT"
               TO CM-COMMAND-LINE OF WS-CM-CA
           PERFORM 8100-CALL-PARSE
           IF CM-PAIR-CNT OF WS-CM-CA = 3
              AND CM-PERIOD OF WS-CM-CA = "202607"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "an empty command line is not an error" TO TK-NAME
           PERFORM 8000-RESET
           MOVE SPACES TO CM-COMMAND-LINE OF WS-CM-CA
           PERFORM 8100-CALL-PARSE
           IF CA-RETURN-CODE OF WS-CM-CA = 0
              AND CM-PAIR-CNT OF WS-CM-CA = 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "a key with an empty value is accepted" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "CO=STC SITE=" TO CM-COMMAND-LINE OF WS-CM-CA
           PERFORM 8100-CALL-PARSE
           IF CM-PAIR-CNT OF WS-CM-CA = 2
              AND CM-SITE OF WS-CM-CA = SPACES
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "FROM and TO ranges are picked up" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "RPT FROM=20260401 TO=20270331 CO=CAL"
               TO CM-COMMAND-LINE OF WS-CM-CA
           PERFORM 8100-CALL-PARSE
           IF CM-FROM OF WS-CM-CA = "20260401"
              AND CM-TO OF WS-CM-CA = "20270331"
              AND CM-CO OF WS-CM-CA = "CAL"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
       2000-LOOKUP-TESTS.
           MOVE "GET finds a key that was supplied" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "AGEDDEBT CO=STC BUCKETS=30,60,90"
               TO CM-COMMAND-LINE OF WS-CM-CA
           PERFORM 8100-CALL-PARSE
           MOVE "BUCKETS" TO CM-LOOKUP-KEY OF WS-CM-CA
           MOVE "GET" TO CA-FUNCTION OF WS-CM-CA
           CALL "CMDARGS" USING WS-CM-CA
           MOVE "30,60,90" TO TK-EXPECTED-X
           MOVE CM-LOOKUP-VALUE OF WS-CM-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "GET answers 16 for a key that was not supplied"
               TO TK-NAME
           MOVE "SITE" TO CM-LOOKUP-KEY OF WS-CM-CA
           MOVE "GET" TO CA-FUNCTION OF WS-CM-CA
           CALL "CMDARGS" USING WS-CM-CA
           IF CA-RETURN-CODE OF WS-CM-CA = 16
              AND CM-FOUND OF WS-CM-CA = "N"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "GET is not case sensitive about the key" TO TK-NAME
           MOVE "buckets" TO CM-LOOKUP-KEY OF WS-CM-CA
           MOVE "GET" TO CA-FUNCTION OF WS-CM-CA
           CALL "CMDARGS" USING WS-CM-CA
           IF CM-FOUND OF WS-CM-CA = "Y"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> COM-018 - a rejected argument is not silently skipped
      *>----------------------------------------------------------------
       3000-PARSE-ERRORS.
           MOVE "an argument without an = is rejected (CMD0001)"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "AGEDDEBT CO=STC PREVIEW"
               TO CM-COMMAND-LINE OF WS-CM-CA
           PERFORM 8100-CALL-PARSE
           IF CA-RETURN-CODE OF WS-CM-CA = 8
              AND CA-REASON-CODE OF WS-CM-CA = "CMD0001"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-CM-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "an empty key is rejected (CMD0002)" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "AGEDDEBT =STC" TO CM-COMMAND-LINE OF WS-CM-CA
           PERFORM 8100-CALL-PARSE
           IF CA-RETURN-CODE OF WS-CM-CA = 8
              AND CA-REASON-CODE OF WS-CM-CA = "CMD0002"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "an unclosed quote is rejected (CMD0003)" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "AGEDDEBT OUT=""run.lst"
               TO CM-COMMAND-LINE OF WS-CM-CA
           PERFORM 8100-CALL-PARSE
           IF CA-RETURN-CODE OF WS-CM-CA = 8
              AND CA-REASON-CODE OF WS-CM-CA = "CMD0003"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "an unknown function is rejected (CMD0009)" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "SPLIT" TO CA-FUNCTION OF WS-CM-CA
           CALL "CMDARGS" USING WS-CM-CA
           IF CA-RETURN-CODE OF WS-CM-CA = 8
              AND CA-REASON-CODE OF WS-CM-CA = "CMD0009"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
       4000-RUNPATH-TESTS.
           ACCEPT WS-RUN-ENV FROM ENVIRONMENT "STERP_RUN"

           MOVE "the run directory is taken from STERP_RUN" TO TK-NAME
           PERFORM 8200-RESET-RP
           MOVE "BASE" TO RPH-FUNCTION
           CALL "RUNPATH" USING WS-RP-CA
           IF WS-RUN-ENV = SPACES
               IF RP-BASE-PATH = "run" AND RP-FROM-DEFAULT
                   PERFORM TK-PASS
               ELSE
                   PERFORM TK-FAIL
               END-IF
           ELSE
               IF RP-BASE-PATH NOT = SPACES AND RP-FROM-ENV
                   PERFORM TK-PASS
               ELSE
                   MOVE RP-BASE-PATH TO TK-DETAIL
                   PERFORM TK-FAIL
               END-IF
           END-IF

           MOVE "a spool file is named report-company-stamp.lst"
               TO TK-NAME
           PERFORM 8200-RESET-RP
           MOVE "BUILD" TO RPH-FUNCTION
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "AGEDDEBT" TO RP-REPORT-ID
           MOVE "STC" TO RP-COMPANY-REF
           MOVE "20261005093000" TO RP-TIMESTAMP
           CALL "RUNPATH" USING WS-RP-CA
           MOVE "AGEDDEBT-STC-20261005093000.lst" TO TK-EXPECTED-X
           MOVE RP-FILE-NAME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "the spool file sits in the spool folder" TO TK-NAME
           MOVE SPACES TO WS-EXPECT
           STRING FUNCTION TRIM(RP-BASE-PATH)
                  "\spool\AGEDDEBT-STC-20261005093000.lst"
               DELIMITED BY SIZE INTO WS-EXPECT
           MOVE WS-EXPECT TO TK-EXPECTED-X
           MOVE RP-FULL-PATH TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "a Bacs file defaults to the txt extension" TO TK-NAME
           PERFORM 8200-RESET-RP
           MOVE "BUILD" TO RPH-FUNCTION
           MOVE "BACS" TO RP-FOLDER
           MOVE "BACSRUN" TO RP-REPORT-ID
           MOVE "STC" TO RP-COMPANY-REF
           MOVE "20261005093000" TO RP-TIMESTAMP
           CALL "RUNPATH" USING WS-RP-CA
           MOVE "BACSRUN-STC-20261005093000.txt" TO TK-EXPECTED-X
           MOVE RP-FILE-NAME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "an MTD extract defaults to the json extension"
               TO TK-NAME
           PERFORM 8200-RESET-RP
           MOVE "BUILD" TO RPH-FUNCTION
           MOVE "MTD" TO RP-FOLDER
           MOVE "VATRET" TO RP-REPORT-ID
           MOVE "STC" TO RP-COMPANY-REF
           MOVE "20261005093000" TO RP-TIMESTAMP
           CALL "RUNPATH" USING WS-RP-CA
           MOVE "VATRET-STC-20261005093000.json" TO TK-EXPECTED-X
           MOVE RP-FILE-NAME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "the extension can be overridden" TO TK-NAME
           PERFORM 8200-RESET-RP
           MOVE "BUILD" TO RPH-FUNCTION
           MOVE "EXTRACTS" TO RP-FOLDER
           MOVE "TRIALBAL" TO RP-REPORT-ID
           MOVE "CAL" TO RP-COMPANY-REF
           MOVE "xml" TO RP-EXTENSION
           MOVE "20261005093000" TO RP-TIMESTAMP
           CALL "RUNPATH" USING WS-RP-CA
           MOVE "TRIALBAL-CAL-20261005093000.xml" TO TK-EXPECTED-X
           MOVE RP-FILE-NAME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "a fixed name can be built without a timestamp"
               TO TK-NAME
           PERFORM 8200-RESET-RP
           MOVE "BUILD" TO RPH-FUNCTION
           MOVE "WORK" TO RP-FOLDER
           MOVE "RESTART" TO RP-REPORT-ID
           MOVE "N" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RP-CA
           MOVE "RESTART.tmp" TO TK-EXPECTED-X
           MOVE RP-FILE-NAME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "the system clock fills a blank timestamp" TO TK-NAME
           PERFORM 8200-RESET-RP
           MOVE "BUILD" TO RPH-FUNCTION
           MOVE "LOGS" TO RP-FOLDER
           MOVE "NIGHTLY" TO RP-REPORT-ID
           CALL "RUNPATH" USING WS-RP-CA
           IF RP-TIMESTAMP IS NUMERIC AND RP-TIMESTAMP(1:2) = "20"
               PERFORM TK-PASS
           ELSE
               MOVE RP-TIMESTAMP TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "an unknown folder is rejected (RUN0001)" TO TK-NAME
           PERFORM 8200-RESET-RP
           MOVE "BUILD" TO RPH-FUNCTION
           MOVE "ARCHIVE" TO RP-FOLDER
           MOVE "NIGHTLY" TO RP-REPORT-ID
           CALL "RUNPATH" USING WS-RP-CA
           IF RPH-RETURN-CODE = 8
              AND RPH-REASON-CODE = "RUN0001"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "a missing report id is rejected (RUN0002)" TO TK-NAME
           PERFORM 8200-RESET-RP
           MOVE "BUILD" TO RPH-FUNCTION
           MOVE "SPOOL" TO RP-FOLDER
           CALL "RUNPATH" USING WS-RP-CA
           IF RPH-RETURN-CODE = 8
              AND RPH-REASON-CODE = "RUN0002"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "an unknown function is rejected (RUN0009)" TO TK-NAME
           PERFORM 8200-RESET-RP
           MOVE "DELETE" TO RPH-FUNCTION
           CALL "RUNPATH" USING WS-RP-CA
           IF RPH-RETURN-CODE = 8
              AND RPH-REASON-CODE = "RUN0009"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
       8000-RESET.
           INITIALIZE WS-CM-CA.

       8100-CALL-PARSE.
           MOVE "PARSE" TO CA-FUNCTION OF WS-CM-CA
           CALL "CMDARGS" USING WS-CM-CA.

       8200-RESET-RP.
           INITIALIZE WS-RP-CA.

       COPY TESTKITP.
