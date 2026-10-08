      *>================================================================
      *> PROGRAM-ID : RUNPATH
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : BUILD THE FULL PATH OF A BATCH OUTPUT FILE UNDER
      *>              THE RUN DIRECTORY, SO THAT EVERY REPORT, BACS
      *>              FILE, MTD EXTRACT AND JOB LOG LANDS IN THE SAME
      *>              PLACE AND CARRIES THE SAME NAMING.
      *>                  <RUN>\<FOLDER>\<REPORT>-<CO>-<STAMP>.<EXT>
      *>              THE RUN DIRECTORY COMES FROM THE ENVIRONMENT
      *>              VARIABLE STERP_RUN; WHEN THAT IS NOT SET, "run"
      *>              RELATIVE TO THE CURRENT DIRECTORY IS USED.
      *>              THE CALLER MAY SUPPLY RP-TIMESTAMP SO THAT EVERY
      *>              FILE OF ONE JOB SHARES A STAMP; OTHERWISE THE
      *>              SYSTEM CLOCK IS READ.
      *> CALLED BY  : BATCH MAIN PROGRAMS IN EVERY MODULE
      *> INTERFACE  : CALL "RUNPATH" USING WS-RUNPATH-CA
      *>              COPYBOOK RUNPATHCA
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-03-02 DJM  CR0151 SPOOL FILE NAMING FOR THE BATCH SUITE
      *> 2005-04-19 KMP  CR1955 SEPARATE FOLDERS PER OUTPUT CLASS
      *> 2026-10-05 STERP STERP_RUN ENVIRONMENT VARIABLE, MTD AND
      *>                 E-INVOICE FOLDERS
      *>================================================================
      *> REQ: COM-018, COM-020
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUNPATH.
       AUTHOR. D J MATHERS.
       DATE-WRITTEN. 02/03/1993.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-LEN                  PIC S9(4) VALUE 0.
           05  WS-ENV                  PIC X(160) VALUE SPACES.
           05  WS-BASE                 PIC X(160) VALUE SPACES.
           05  WS-SUB                  PIC X(10) VALUE SPACES.
           05  WS-EXT                  PIC X(8) VALUE SPACES.
           05  WS-SEP                  PIC X VALUE "\".
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
       01  WS-NOW.
           05  WS-NOW-DATE             PIC 9(8).
           05  WS-NOW-TIME             PIC 9(8).
           05  WS-NOW-REST             PIC X(5).
       01  WS-STAMP.
           05  WS-STAMP-DATE           PIC 9(8).
           05  WS-STAMP-TIME           PIC 9(6).
       LINKAGE SECTION.
       COPY RUNPATHCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 1000-RESOLVE-BASE
           EVALUATE CA-FUNCTION
               WHEN "BASE"     CONTINUE
               WHEN "BUILD"    PERFORM 2000-BUILD
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE WS-WORK
           MOVE "\" TO WS-SEP
           MOVE "N" TO WS-ERROR
           MOVE "RUNPATH" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "COM-018" TO CA-REQ-REF
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO RP-FULL-PATH RP-FILE-NAME RP-BASE-PATH
           IF RP-USE-TIMESTAMP = SPACE OR RP-USE-TIMESTAMP = LOW-VALUE
               MOVE "Y" TO RP-USE-TIMESTAMP
           END-IF
           MOVE FUNCTION UPPER-CASE(RP-FOLDER) TO RP-FOLDER.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-RESOLVE-BASE SECTION.
       1000-START.
           MOVE SPACES TO WS-ENV
           ACCEPT WS-ENV FROM ENVIRONMENT "STERP_RUN"
           IF WS-ENV = SPACES OR WS-ENV = LOW-VALUES
               MOVE "run" TO WS-BASE
               MOVE "DEFAULT" TO RP-SOURCE
           ELSE
               MOVE WS-ENV TO WS-BASE
               MOVE "ENV" TO RP-SOURCE
           END-IF
      *>   a trailing separator would double up in the built path
           PERFORM 8000-BASE-LENGTH
           IF WS-LEN > 1
               IF WS-BASE(WS-LEN:1) = "\" OR WS-BASE(WS-LEN:1) = "/"
                   MOVE SPACE TO WS-BASE(WS-LEN:1)
                   SUBTRACT 1 FROM WS-LEN
               END-IF
           END-IF
           MOVE WS-BASE TO RP-BASE-PATH
           MOVE SPACES TO CA-MESSAGE
           STRING "Run directory " FUNCTION TRIM(RP-BASE-PATH)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       2000-BUILD SECTION.
       2000-START.
           PERFORM 2100-FOLDER
           IF WS-INPUT-BAD
               GO TO 2000-EXIT
           END-IF
           IF RP-REPORT-ID = SPACES OR RP-REPORT-ID = LOW-VALUES
               MOVE "RUN0002" TO CA-REASON-CODE
               MOVE "A report or file identifier is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           IF RP-EXTENSION NOT = SPACES
               MOVE RP-EXTENSION TO WS-EXT
           END-IF
           MOVE WS-EXT TO RP-EXTENSION
           PERFORM 2200-TIMESTAMP
           PERFORM 2300-FILE-NAME
           MOVE SPACES TO RP-FULL-PATH
           STRING FUNCTION TRIM(RP-BASE-PATH) WS-SEP
                  FUNCTION TRIM(WS-SUB) WS-SEP
                  FUNCTION TRIM(RP-FILE-NAME)
               DELIMITED BY SIZE INTO RP-FULL-PATH
           MOVE SPACES TO CA-MESSAGE
           STRING "Output file " FUNCTION TRIM(RP-FULL-PATH)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

       2100-FOLDER SECTION.
       2100-START.
           EVALUATE RP-FOLDER
               WHEN "SPOOL"    MOVE "spool" TO WS-SUB
                               MOVE "lst" TO WS-EXT
               WHEN "BACS"     MOVE "bacs" TO WS-SUB
                               MOVE "txt" TO WS-EXT
               WHEN "MTD"      MOVE "mtd" TO WS-SUB
                               MOVE "json" TO WS-EXT
               WHEN "EINVOICE" MOVE "einvoice" TO WS-SUB
                               MOVE "xml" TO WS-EXT
               WHEN "EXTRACTS" MOVE "extracts" TO WS-SUB
                               MOVE "csv" TO WS-EXT
               WHEN "LOGS"     MOVE "logs" TO WS-SUB
                               MOVE "log" TO WS-EXT
               WHEN "WORK"     MOVE "work" TO WS-SUB
                               MOVE "tmp" TO WS-EXT
               WHEN OTHER
                   MOVE "RUN0001" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Unknown output folder "
                          FUNCTION TRIM(RP-FOLDER)
                          " - use SPOOL BACS MTD EINVOICE EXTRACTS "
                          "LOGS or WORK"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   PERFORM 9000-REJECT
           END-EVALUATE.
       2100-EXIT.
           EXIT.

       2200-TIMESTAMP SECTION.
       2200-START.
           IF RP-USE-TIMESTAMP NOT = "Y"
               MOVE SPACES TO RP-TIMESTAMP
               GO TO 2200-EXIT
           END-IF
           IF RP-TIMESTAMP NOT = SPACES
              AND RP-TIMESTAMP NOT = LOW-VALUES
               GO TO 2200-EXIT
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           MOVE WS-NOW-DATE TO WS-STAMP-DATE
           COMPUTE WS-STAMP-TIME = WS-NOW-TIME / 100
           MOVE WS-STAMP TO RP-TIMESTAMP.
       2200-EXIT.
           EXIT.

       2300-FILE-NAME SECTION.
       2300-START.
           MOVE SPACES TO RP-FILE-NAME
           IF RP-COMPANY-REF = SPACES OR RP-COMPANY-REF = LOW-VALUES
               IF RP-USE-TIMESTAMP = "Y"
                   STRING FUNCTION TRIM(RP-REPORT-ID) "-"
                          RP-TIMESTAMP "."
                          FUNCTION TRIM(RP-EXTENSION)
                       DELIMITED BY SIZE INTO RP-FILE-NAME
               ELSE
                   STRING FUNCTION TRIM(RP-REPORT-ID) "."
                          FUNCTION TRIM(RP-EXTENSION)
                       DELIMITED BY SIZE INTO RP-FILE-NAME
               END-IF
           ELSE
               IF RP-USE-TIMESTAMP = "Y"
                   STRING FUNCTION TRIM(RP-REPORT-ID) "-"
                          FUNCTION TRIM(RP-COMPANY-REF) "-"
                          RP-TIMESTAMP "."
                          FUNCTION TRIM(RP-EXTENSION)
                       DELIMITED BY SIZE INTO RP-FILE-NAME
               ELSE
                   STRING FUNCTION TRIM(RP-REPORT-ID) "-"
                          FUNCTION TRIM(RP-COMPANY-REF) "."
                          FUNCTION TRIM(RP-EXTENSION)
                       DELIMITED BY SIZE INTO RP-FILE-NAME
               END-IF
           END-IF.
       2300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-BASE-LENGTH SECTION.
       8000-START.
           MOVE 0 TO WS-LEN
           PERFORM VARYING WS-I FROM 160 BY -1
               UNTIL WS-I < 1 OR WS-LEN > 0
               IF WS-BASE(WS-I:1) NOT = SPACE
                  AND WS-BASE(WS-I:1) NOT = LOW-VALUE
                   MOVE WS-I TO WS-LEN
               END-IF
           END-PERFORM.
       8000-EXIT.
           EXIT.

       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "RUN0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "RUNPATH does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
