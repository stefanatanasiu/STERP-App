      *>================================================================
      *> PROGRAM-ID : JOBLOG
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : REGISTER A BATCH RUN IN core.JobRun SO THAT THE
      *>              JOB MONITOR IN THE CLIENT CAN SHOW WHAT RAN, WHO
      *>              SUBMITTED IT, HOW IT ENDED AND WHICH SPOOL FILE
      *>              IT PRODUCED.  EVERY BATCH MAIN PROGRAM CALLS
      *>              START AS ITS FIRST ACT AND END AS ITS LAST, SO
      *>              THAT AN ABANDONED RUN IS VISIBLE AS ONE STILL
      *>              MARKED RUNNING RATHER THAN SILENTLY MISSING
      *>              (COM-014 - A FAILED ACTION MUST REPORT THAT IT
      *>              DID NOT COMPLETE).
      *> CALLED BY  : BATCH MAIN PROGRAMS IN EVERY MODULE
      *> INTERFACE  : CALL "JOBLOG" USING WS-JOBLOG-CA
      *>              COPYBOOK JOBLOGCA
      *> CALLS      : SQLIO -> core.usp_JobRunStart / usp_JobRunEnd
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2004-08-30 KMP  CR1860 NIGHT SHIFT JOB MONITOR
      *> 2026-10-05 STERP core.JobRun, SPOOL FILE AND ROW COUNTS
      *>================================================================
      *> REQ: COM-014, COM-018
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JOBLOG.
       AUTHOR. K M PATEL.
       DATE-WRITTEN. 30/08/2004.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       01  WS-FLAGS.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
      *>     core.JobRun constrains Status to RUNNING / OK / WARNING /
      *>     FAILED / ABENDED, while a batch program speaks the wording
      *>     of CONVENTIONS 3.6 (COMPLETE / CANCELLED).  The two are
      *>     reconciled here so that neither side has to change.
           05  WS-DB-STATUS            PIC X(12) VALUE SPACES.
       01  WS-START-ROW.
           05  WS-S-ID                 PIC X(20).
           05  WS-S-NAME               PIC X(40).
           05  WS-S-STATUS             PIC X(12).
           05  WS-S-STARTED            PIC X(23).
       01  WS-END-ROW.
           05  WS-E-ID                 PIC X(20).
           05  WS-E-NAME               PIC X(40).
           05  WS-E-COMPANY            PIC X(10).
           05  WS-E-STATUS             PIC X(12).
           05  WS-E-RC                 PIC X(8).
           05  WS-E-ROWS               PIC X(12).
           05  WS-E-STARTED            PIC X(23).
           05  WS-E-ENDED              PIC X(23).
           05  WS-E-SPOOL              PIC X(200).
           05  WS-E-MESSAGE            PIC X(200).
       LINKAGE SECTION.
       COPY JOBLOGCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "START"    PERFORM 1000-START-RUN
               WHEN "END"      PERFORM 2000-END-RUN
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR
           MOVE "JOBLOG" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "COM-014" TO CA-REQ-REF
           MOVE 0 TO CA-MSG-COUNT
           MOVE FUNCTION UPPER-CASE(JL-JOB-NAME) TO JL-JOB-NAME
           MOVE FUNCTION UPPER-CASE(JL-STATUS) TO JL-STATUS.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-FUNCTION = "START"
               IF JL-JOB-NAME = SPACES OR JL-JOB-NAME = LOW-VALUES
                   MOVE "JOB0001" TO CA-REASON-CODE
                   MOVE "A job name is required" TO CA-MESSAGE
                   PERFORM 9000-REJECT
               END-IF
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION = "END"
               IF JL-JOB-RUN-ID <= 0
                   MOVE "JOB0002" TO CA-REASON-CODE
                   MOVE "A job run id is required to close a run"
                       TO CA-MESSAGE
                   PERFORM 9000-REJECT
                   GO TO 0200-EXIT
               END-IF
               IF JL-STATUS NOT = "COMPLETE"
                  AND JL-STATUS NOT = "WARNING"
                  AND JL-STATUS NOT = "FAILED"
                  AND JL-STATUS NOT = "CANCELLED"
                   MOVE "JOB0003" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "A run ends COMPLETE, WARNING, FAILED or "
                          "CANCELLED, not "
                          FUNCTION TRIM(JL-STATUS)
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   PERFORM 9000-REJECT
               END-IF
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-START-RUN SECTION.
       1000-START.
           MOVE 0 TO JL-JOB-RUN-ID
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_JobRunStart ?, ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE JL-JOB-NAME TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE CA-SITE TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE JL-ARGUMENTS TO SQLIO-PARM-VAL(5)
           MOVE "N" TO SQLIO-PARM-NULL(5)
           IF CA-COMPANY = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(2)
           END-IF
           IF CA-USER = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(3)
           END-IF
           IF CA-SITE = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(4)
           END-IF
           IF JL-ARGUMENTS = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(5)
           END-IF
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 23 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-START-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1000-EXIT
           END-IF
           COMPUTE JL-JOB-RUN-ID = FUNCTION NUMVAL(WS-S-ID)
           MOVE WS-S-STATUS TO JL-STATUS
           MOVE WS-S-STARTED TO JL-STARTED-AT
           MOVE SPACES TO CA-MESSAGE
           MOVE JL-JOB-RUN-ID TO SQLIO-INT-EDIT
           STRING "Job " FUNCTION TRIM(JL-JOB-NAME)
                  " registered as run "
                  FUNCTION TRIM(SQLIO-INT-EDIT)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       2000-END-RUN SECTION.
       2000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_JobRunEnd ?, ?, ?, ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE JL-JOB-RUN-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           EVALUATE JL-STATUS
               WHEN "COMPLETE"
                   MOVE "OK" TO WS-DB-STATUS
               WHEN "CANCELLED"
                   MOVE "ABENDED" TO WS-DB-STATUS
               WHEN OTHER
                   MOVE JL-STATUS TO WS-DB-STATUS
           END-EVALUATE
           MOVE WS-DB-STATUS TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE JL-RETURN-CODE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE JL-ROWS-PROCESSED TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE JL-LOG-FILE TO SQLIO-PARM-VAL(5)
           MOVE "N" TO SQLIO-PARM-NULL(5)
           MOVE JL-SPOOL-FILE TO SQLIO-PARM-VAL(6)
           MOVE "N" TO SQLIO-PARM-NULL(6)
           MOVE JL-MESSAGE-TEXT TO SQLIO-PARM-VAL(7)
           MOVE "N" TO SQLIO-PARM-NULL(7)
           IF JL-LOG-FILE = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(5)
           END-IF
           IF JL-SPOOL-FILE = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(6)
           END-IF
           IF JL-MESSAGE-TEXT = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(7)
           END-IF
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 12 TO SQLIO-COL-WIDTH(6)
           MOVE 23 TO SQLIO-COL-WIDTH(7)
           MOVE 23 TO SQLIO-COL-WIDTH(8)
           MOVE 200 TO SQLIO-COL-WIDTH(9)
           MOVE 200 TO SQLIO-COL-WIDTH(10)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-END-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2000-EXIT
           END-IF
           EVALUATE WS-E-STATUS
               WHEN "OK"
                   MOVE "COMPLETE" TO JL-STATUS
               WHEN "ABENDED"
                   MOVE "CANCELLED" TO JL-STATUS
               WHEN OTHER
                   MOVE WS-E-STATUS TO JL-STATUS
           END-EVALUATE
           MOVE WS-E-STARTED TO JL-STARTED-AT
           MOVE WS-E-ENDED TO JL-ENDED-AT
           MOVE SPACES TO CA-MESSAGE
           STRING "Job " FUNCTION TRIM(JL-JOB-NAME) " ended "
                  FUNCTION TRIM(JL-STATUS)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "JOB0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "JOBLOG does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in JOBLOG" TO CA-MESSAGE
           END-IF
           IF CA-MESSAGE(1:8) = "STERP-E-"
              OR CA-MESSAGE(1:8) = "STERP-W-"
               MOVE "JOB0008" TO CA-REASON-CODE
               MOVE 8 TO CA-RETURN-CODE
           ELSE
               MOVE "SQL0001" TO CA-REASON-CODE
               MOVE 12 TO CA-RETURN-CODE
           END-IF
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
