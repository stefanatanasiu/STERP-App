      *>================================================================
      *> PROGRAM  : TRUNPATH
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : COMMON SERVICES - UNIT TEST
      *> PURPOSE  : Unit test for RUNPATH, the service that decides
      *>            where a batch run writes its spool file, Bacs file,
      *>            MTD payload, e-invoice, extract or job log.
      *> CALLED BY: build\run-tests.ps1 -Layer cobol
      *> INTERFACE: TAP on stdout, RETURN-CODE = number of failures.
      *>
      *> The test sets STERP_RUN itself so that both answers - the run
      *> directory taken from the environment and the "run" fallback -
      *> are exercised without depending on how the developer's shell
      *> happens to be set up.  Timestamps are supplied by the caller
      *> so that every expected file name is fixed.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2026-10-05 STERP INITIAL - OUTPUT FILE NAMING AND FOLDERS
      *>================================================================
      *> REQ: COM-018, COM-020
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRUNPATH.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RP==.
       01  WS-BASE-SET                 PIC X(40)
                                       VALUE "D:\sterp\run".

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TRUNPATH" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-SET-RUN-DIRECTORY
           PERFORM 1000-SPOOL-FILE
           PERFORM 2000-FOLDER-TABLE
           PERFORM 3000-NAME-VARIANTS
           PERFORM 4000-BASE-RESOLUTION
           PERFORM 5000-REFUSALS
           PERFORM TK-END
           STOP RUN.

       0100-SET-RUN-DIRECTORY.
           DISPLAY "STERP_RUN" UPON ENVIRONMENT-NAME
           DISPLAY WS-BASE-SET UPON ENVIRONMENT-VALUE.

      *>----------------------------------------------------------------
      *> 1000 - the ordinary case from CONVENTIONS 3.6: a 132 column
      *> report lands in run\spool as <REPORT>-<CO>-<stamp>.lst
      *>----------------------------------------------------------------
      *> REQ: COM-018
       1000-SPOOL-FILE.
           INITIALIZE WS-RP
           MOVE "RUNPATH" TO CA-PROGRAM OF WS-RP
           MOVE "BUILD" TO CA-FUNCTION OF WS-RP
           MOVE "STC" TO CA-COMPANY OF WS-RP
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "AGEDDEBT" TO RP-REPORT-ID
           MOVE "STC" TO RP-COMPANY-REF
           MOVE "20261015120000" TO RP-TIMESTAMP
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH BUILD accepts a spool request" TO TK-NAME
           IF CA-RETURN-CODE OF WS-RP = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-RP TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "RUNPATH names the file <REPORT>-<CO>-<stamp>.lst"
               TO TK-NAME
           MOVE "AGEDDEBT-STC-20261015120000.lst" TO TK-EXPECTED-X
           MOVE RP-FILE-NAME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "RUNPATH puts a report under the spool directory"
               TO TK-NAME
           MOVE "D:\sterp\run\spool\AGEDDEBT-STC-20261015120000.lst"
               TO TK-EXPECTED-X
           MOVE RP-FULL-PATH TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "RUNPATH defaults a spool file to extension lst"
               TO TK-NAME
           MOVE "lst" TO TK-EXPECTED-X
           MOVE RP-EXTENSION TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "RUNPATH reports ENV as the source of the directory"
               TO TK-NAME
           MOVE "ENV" TO TK-EXPECTED-X
           MOVE RP-SOURCE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 2000 - every folder of CONVENTIONS 3.6 and its default
      *> extension.  A wrong extension here would quietly produce a
      *> Bacs file called .lst, which the bank would reject.
      *>----------------------------------------------------------------
      *> REQ: COM-018, COM-020
       2000-FOLDER-TABLE.
           PERFORM 2100-CHECK-BACS
           PERFORM 2200-CHECK-MTD
           PERFORM 2300-CHECK-EINVOICE
           PERFORM 2400-CHECK-EXTRACTS
           PERFORM 2500-CHECK-LOGS
           PERFORM 2600-CHECK-WORK.

       2100-CHECK-BACS.
           PERFORM 9000-BUILD-SAMPLE
           MOVE "BACS" TO RP-FOLDER
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH BACS payments go to run\bacs as .txt"
               TO TK-NAME
           MOVE "D:\sterp\run\bacs\PAYRUN-STC-20261015120000.txt"
               TO TK-EXPECTED-X
           MOVE RP-FULL-PATH TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       2200-CHECK-MTD.
           PERFORM 9000-BUILD-SAMPLE
           MOVE "MTD" TO RP-FOLDER
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH Making Tax Digital goes to run\mtd as .json"
               TO TK-NAME
           MOVE "D:\sterp\run\mtd\PAYRUN-STC-20261015120000.json"
               TO TK-EXPECTED-X
           MOVE RP-FULL-PATH TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       2300-CHECK-EINVOICE.
           PERFORM 9000-BUILD-SAMPLE
           MOVE "EINVOICE" TO RP-FOLDER
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH e-invoices go to run\einvoice as .xml"
               TO TK-NAME
           MOVE "D:\sterp\run\einvoice\PAYRUN-STC-20261015120000.xml"
               TO TK-EXPECTED-X
           MOVE RP-FULL-PATH TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       2400-CHECK-EXTRACTS.
           PERFORM 9000-BUILD-SAMPLE
           MOVE "EXTRACTS" TO RP-FOLDER
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH extracts go to run\extracts as .csv"
               TO TK-NAME
           MOVE "D:\sterp\run\extracts\PAYRUN-STC-20261015120000.csv"
               TO TK-EXPECTED-X
           MOVE RP-FULL-PATH TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       2500-CHECK-LOGS.
           PERFORM 9000-BUILD-SAMPLE
           MOVE "LOGS" TO RP-FOLDER
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH job logs go to run\logs as .log" TO TK-NAME
           MOVE "D:\sterp\run\logs\PAYRUN-STC-20261015120000.log"
               TO TK-EXPECTED-X
           MOVE RP-FULL-PATH TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       2600-CHECK-WORK.
           PERFORM 9000-BUILD-SAMPLE
           MOVE "WORK" TO RP-FOLDER
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH scratch files go to run\work as .tmp"
               TO TK-NAME
           MOVE "D:\sterp\run\work\PAYRUN-STC-20261015120000.tmp"
               TO TK-EXPECTED-X
           MOVE RP-FULL-PATH TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 3000 - name variants.  A job that writes several files wants
      *> them stamped alike, and some files (a reprint, a fixed
      *> interface file) must not be stamped at all.
      *>----------------------------------------------------------------
      *> REQ: COM-018
       3000-NAME-VARIANTS.
           PERFORM 9000-BUILD-SAMPLE
           MOVE "N" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH omits the stamp when it is not wanted"
               TO TK-NAME
           MOVE "PAYRUN-STC.txt" TO TK-EXPECTED-X
           MOVE RP-FILE-NAME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "RUNPATH clears the timestamp it did not use"
               TO TK-NAME
           IF RP-TIMESTAMP = SPACES
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           PERFORM 9000-BUILD-SAMPLE
           MOVE SPACES TO RP-COMPANY-REF
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH drops the company when there is none"
               TO TK-NAME
           MOVE "PAYRUN-20261015120000.txt" TO TK-EXPECTED-X
           MOVE RP-FILE-NAME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 9000-BUILD-SAMPLE
           MOVE "dat" TO RP-EXTENSION
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH honours an extension the caller insists on"
               TO TK-NAME
           MOVE "PAYRUN-STC-20261015120000.dat" TO TK-EXPECTED-X
           MOVE RP-FILE-NAME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 9000-BUILD-SAMPLE
           MOVE "spool" TO RP-FOLDER
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH accepts a folder given in lower case"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-RP = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-RP TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           PERFORM 9000-BUILD-SAMPLE
           MOVE SPACES TO RP-TIMESTAMP
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH stamps from the clock when none is given"
               TO TK-NAME
           IF RP-TIMESTAMP NOT = SPACES
              AND RP-TIMESTAMP(1:2) = "20"
               PERFORM TK-PASS
           ELSE
               MOVE RP-TIMESTAMP TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 4000 - where the run directory comes from.
      *>----------------------------------------------------------------
      *> REQ: COM-018
       4000-BASE-RESOLUTION.
           INITIALIZE WS-RP
           MOVE "BASE" TO CA-FUNCTION OF WS-RP
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH BASE reports the directory from STERP_RUN"
               TO TK-NAME
           MOVE "D:\sterp\run" TO TK-EXPECTED-X
           MOVE RP-BASE-PATH TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   a trailing separator must not double up in the built path
           DISPLAY "STERP_RUN" UPON ENVIRONMENT-NAME
           DISPLAY "D:\sterp\run\" UPON ENVIRONMENT-VALUE
           INITIALIZE WS-RP
           MOVE "BASE" TO CA-FUNCTION OF WS-RP
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH strips a trailing separator" TO TK-NAME
           MOVE "D:\sterp\run" TO TK-EXPECTED-X
           MOVE RP-BASE-PATH TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   with nothing in the environment the default is "run"
           DISPLAY "STERP_RUN" UPON ENVIRONMENT-NAME
           DISPLAY " " UPON ENVIRONMENT-VALUE
           INITIALIZE WS-RP
           MOVE "BASE" TO CA-FUNCTION OF WS-RP
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH falls back to run when STERP_RUN is unset"
               TO TK-NAME
           MOVE "run" TO TK-EXPECTED-X
           MOVE RP-BASE-PATH TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "RUNPATH reports DEFAULT when it used the fallback"
               TO TK-NAME
           MOVE "DEFAULT" TO TK-EXPECTED-X
           MOVE RP-SOURCE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0100-SET-RUN-DIRECTORY.

      *>----------------------------------------------------------------
      *> 5000 - an online program must never crash on bad input: it
      *> validates and returns 08 (CONVENTIONS 3.3).
      *>----------------------------------------------------------------
      *> REQ: COM-018
       5000-REFUSALS.
           PERFORM 9000-BUILD-SAMPLE
           MOVE "ARCHIVE" TO RP-FOLDER
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH rejects a folder it does not know" TO TK-NAME
           IF CA-RETURN-CODE OF WS-RP = 8
              AND CA-REASON-CODE OF WS-RP = "RUN0001"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-RP TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "RUNPATH lists the folders it does know" TO TK-NAME
           IF CA-MESSAGE OF WS-RP (1:100) NOT = SPACES
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           PERFORM 9000-BUILD-SAMPLE
           MOVE SPACES TO RP-REPORT-ID
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH rejects a file with no identifier" TO TK-NAME
           IF CA-RETURN-CODE OF WS-RP = 8
              AND CA-REASON-CODE OF WS-RP = "RUN0002"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           PERFORM 9000-BUILD-SAMPLE
           MOVE "DELETE" TO CA-FUNCTION OF WS-RP
           CALL "RUNPATH" USING WS-RP
           MOVE "RUNPATH rejects a function it does not support"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-RP = 8
              AND CA-REASON-CODE OF WS-RP = "RUN0009"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE "RUNPATH builds no path when it refused the request"
               TO TK-NAME
           IF RP-FULL-PATH = SPACES
               PERFORM TK-PASS
           ELSE
               MOVE RP-FULL-PATH TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> A known good Bacs request, reused by most of the cases above.
      *>----------------------------------------------------------------
       9000-BUILD-SAMPLE.
           INITIALIZE WS-RP
           MOVE "BUILD" TO CA-FUNCTION OF WS-RP
           MOVE "STC" TO CA-COMPANY OF WS-RP
           MOVE "BACS" TO RP-FOLDER
           MOVE "PAYRUN" TO RP-REPORT-ID
           MOVE "STC" TO RP-COMPANY-REF
           MOVE "20261015120000" TO RP-TIMESTAMP
           MOVE "Y" TO RP-USE-TIMESTAMP.

       COPY TESTKITP.
