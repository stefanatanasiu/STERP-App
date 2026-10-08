      *>================================================================
      *> PROGRAM-ID : IDEMPOT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : IDEMPOTENCY FOR REPEATED BUSINESS INSTRUCTIONS
      *>              (COM-015).  A CONFIRMED INSTRUCTION CARRIES A
      *>              CORRELATION ID; CLAIMING IT HERE BEFORE DOING THE
      *>              WORK MEANS THAT REPEATING THE INSTRUCTION CANNOT
      *>              CREATE A SECOND FINANCIAL, STOCK OR SETTLEMENT
      *>              EFFECT.
      *>              THE THREE ANSWERS ARE DELIBERATELY DISTINCT
      *>              (SPEC 01 SECTION 7 - AN UNCERTAIN OUTCOME MUST
      *>              NOT BE REPRESENTED AS EITHER AN UNQUALIFIED
      *>              SUCCESS OR A SAFE TO REPEAT FAILURE):
      *>                STARTED     this caller owns the instruction
      *>                INPROGRESS  another attempt is still running -
      *>                            establish the outcome first
      *>                COMPLETED   it already happened; the original
      *>                            result is in ID-RESULT-REF
      *>                FAILED      the earlier attempt failed
      *> CALLED BY  : EVERY MODULE THAT CONFIRMS A BUSINESS ACTION
      *> INTERFACE  : CALL "IDEMPOT" USING WS-IDEMPOT-CA
      *>              COPYBOOK IDEMPOTCA
      *> CALLS      : SQLIO -> core.usp_IdempotentBegin / End
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2014-09-08 ANW  CR2810 DOUBLE PAYMENT INCIDENT 2014/22 - THE
      *>                 CLIENT RESENT A PAYMENT RUN AFTER A TIMEOUT
      *> 2026-10-05 STERP core.IdempotencyKey
      *>================================================================
      *> REQ: COM-015, COM-014
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDEMPOT.
       AUTHOR. A N WRIGHT.
       DATE-WRITTEN. 08/09/2014.
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
       01  WS-BEGIN-ROW.
           05  WS-B-CORR               PIC X(64).
           05  WS-B-OPERATION          PIC X(60).
           05  WS-B-OUTCOME            PIC X(12).
           05  WS-B-REF                PIC X(60).
           05  WS-B-MESSAGE            PIC X(200).
       01  WS-END-ROW.
           05  WS-E-CORR               PIC X(64).
           05  WS-E-OPERATION          PIC X(60).
           05  WS-E-STATUS             PIC X(12).
           05  WS-E-REF                PIC X(60).
           05  WS-E-MESSAGE            PIC X(200).
           05  WS-E-STARTED            PIC X(23).
           05  WS-E-ENDED              PIC X(23).
       LINKAGE SECTION.
       COPY IDEMPOTCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "BEGIN"    PERFORM 1000-BEGIN
               WHEN "END"      PERFORM 2000-END
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR
           MOVE "IDEMPOT" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "COM-015" TO CA-REQ-REF
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO ID-OUTCOME
           IF CA-FUNCTION = "BEGIN"
               MOVE SPACES TO ID-RESULT-REF ID-RESULT-MESSAGE
               MOVE SPACES TO ID-STARTED-AT ID-ENDED-AT
           END-IF.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-CORRELATION-ID = SPACES
              OR CA-CORRELATION-ID = LOW-VALUES
               MOVE "IDM0001" TO CA-REASON-CODE
               MOVE "A correlation id is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION = "BEGIN"
              AND (ID-OPERATION = SPACES OR ID-OPERATION = LOW-VALUES)
               MOVE "IDM0002" TO CA-REASON-CODE
               MOVE "The instruction must name its operation"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION = "END"
               MOVE FUNCTION UPPER-CASE(ID-STATUS) TO ID-STATUS
               IF ID-STATUS NOT = "COMPLETED"
                  AND ID-STATUS NOT = "FAILED"
                   MOVE "IDM0003" TO CA-REASON-CODE
                   MOVE "An instruction ends COMPLETED or FAILED"
                       TO CA-MESSAGE
                   PERFORM 9000-REJECT
               END-IF
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-BEGIN SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_IdempotentBegin ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE ID-OPERATION TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           IF CA-COMPANY = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(3)
           END-IF
           IF CA-USER = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(4)
           END-IF
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 64 TO SQLIO-COL-WIDTH(1)
           MOVE 60 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 60 TO SQLIO-COL-WIDTH(4)
           MOVE 200 TO SQLIO-COL-WIDTH(5)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BEGIN-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "IDM0005" TO CA-REASON-CODE
               MOVE "The idempotency register gave no answer"
                   TO CA-MESSAGE
               MOVE 12 TO CA-RETURN-CODE
               GO TO 1000-EXIT
           END-IF
           MOVE FUNCTION UPPER-CASE(WS-B-OUTCOME) TO ID-OUTCOME
      *>     core.usp_IdempotentBegin speaks its own dialect: NEW for a
      *>     claim that may proceed and DUPLICATE for an instruction
      *>     that already completed.  Translate once, here, so that the
      *>     rest of the program and every caller only ever sees the
      *>     COMMAREA vocabulary STARTED / INPROGRESS / COMPLETED /
      *>     FAILED (COM-015).
           EVALUATE ID-OUTCOME
               WHEN "NEW"
                   MOVE "STARTED" TO ID-OUTCOME
               WHEN "DUPLICATE"
                   MOVE "COMPLETED" TO ID-OUTCOME
           END-EVALUATE
           MOVE WS-B-REF TO ID-RESULT-REF
           MOVE WS-B-MESSAGE TO ID-RESULT-MESSAGE
           PERFORM 1100-REPORT-OUTCOME.
       1000-EXIT.
           EXIT.

       1100-REPORT-OUTCOME SECTION.
       1100-START.
           MOVE SPACES TO CA-MESSAGE
           EVALUATE TRUE
               WHEN ID-STARTED
                   MOVE 0 TO CA-RETURN-CODE
                   STRING "Instruction claimed - carry on with "
                          FUNCTION TRIM(ID-OPERATION)
                       DELIMITED BY SIZE INTO CA-MESSAGE
               WHEN ID-COMPLETED
                   MOVE 4 TO CA-RETURN-CODE
                   MOVE "IDM0006" TO CA-REASON-CODE
                   STRING "This instruction was already completed - "
                          "the original result is "
                          FUNCTION TRIM(ID-RESULT-REF)
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   ADD 1 TO CA-MSG-COUNT
               WHEN ID-INPROGRESS
      *>           an uncertain outcome is neither success nor a safe
      *>           to repeat failure - the user must establish it
                   MOVE 6 TO CA-RETURN-CODE
                   MOVE "IDM0007" TO CA-REASON-CODE
                   STRING "Another attempt at this instruction is "
                          "still running - establish its outcome "
                          "before starting a replacement"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   ADD 1 TO CA-MSG-COUNT
               WHEN ID-FAILED
                   MOVE 8 TO CA-RETURN-CODE
                   MOVE "IDM0008" TO CA-REASON-CODE
                   STRING "The earlier attempt failed: "
                          FUNCTION TRIM(ID-RESULT-MESSAGE)
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   ADD 1 TO CA-MSG-COUNT
               WHEN OTHER
                   MOVE 12 TO CA-RETURN-CODE
                   MOVE "IDM0009" TO CA-REASON-CODE
                   STRING "Unknown idempotency outcome "
                          FUNCTION TRIM(ID-OUTCOME)
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   ADD 1 TO CA-MSG-COUNT
           END-EVALUATE.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       2000-END SECTION.
       2000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_IdempotentEnd ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE ID-STATUS TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE ID-RESULT-REF TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE ID-RESULT-MESSAGE TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           IF ID-RESULT-REF = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(3)
           END-IF
           IF ID-RESULT-MESSAGE = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(4)
           END-IF
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 64 TO SQLIO-COL-WIDTH(1)
           MOVE 60 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 60 TO SQLIO-COL-WIDTH(4)
           MOVE 200 TO SQLIO-COL-WIDTH(5)
           MOVE 23 TO SQLIO-COL-WIDTH(6)
           MOVE 23 TO SQLIO-COL-WIDTH(7)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-END-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2000-EXIT
           END-IF
           MOVE WS-E-STATUS TO ID-OUTCOME
           MOVE WS-E-REF TO ID-RESULT-REF
           MOVE WS-E-STARTED TO ID-STARTED-AT
           MOVE WS-E-ENDED TO ID-ENDED-AT
           MOVE SPACES TO CA-MESSAGE
           STRING "Instruction recorded as "
                  FUNCTION TRIM(ID-STATUS)
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
           MOVE "IDM0004" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "IDEMPOT does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in IDEMPOT" TO CA-MESSAGE
           END-IF
           IF CA-MESSAGE(1:8) = "STERP-E-"
              OR CA-MESSAGE(1:8) = "STERP-W-"
               MOVE "IDM0010" TO CA-REASON-CODE
               MOVE 8 TO CA-RETURN-CODE
           ELSE
               MOVE "SQL0001" TO CA-REASON-CODE
               MOVE 12 TO CA-RETURN-CODE
           END-IF
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
