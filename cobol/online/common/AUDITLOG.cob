      *>================================================================
      *> PROGRAM-ID : AUDITLOG
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : WRITE ONE ATTRIBUTABLE AUDIT ROW (COM-013).
      *>              EVERY MATERIAL CREATION, CHANGE, APPROVAL,
      *>              REJECTION, RELEASE, POSTING, REVERSAL AND
      *>              OVERRIDE CALLS THIS SO THAT A REVIEWER CAN SEE
      *>              WHO CHANGED WHAT, WHEN, WHY AND WITH WHOSE
      *>              AUTHORITY.  THE ROW IS WRITTEN ON THE COBOL
      *>              CONNECTION, SO IT JOINS ANY BEGN / COMT
      *>              TRANSACTION THAT THE CALLING PROGRAM HAS OPEN -
      *>              AN AUDIT ROW FOR A ROLLED BACK POSTING IS ROLLED
      *>              BACK WITH IT (COM-014).
      *> CALLED BY  : EVERY MODULE AND THE .NET CLIENT
      *> INTERFACE  : CALL "AUDITLOG" USING WS-AUDITLOG-CA
      *>              COPYBOOK AUDITLOGCA
      *> CALLS      : SQLIO -> core.usp_WriteAudit
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2009-11-19 KMP  CR2301 AUDIT TRAIL (AUDIT FINDING 2009/14)
      *> 2018-05-25 ANW  CR3102 SENSITIVE FLAG FOR PERSONAL AND BANKING
      *>                 DATA (UK GDPR)
      *> 2026-10-05 STERP core.usp_WriteAudit
      *>================================================================
      *> REQ: COM-013, COM-019
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AUDITLOG.
       AUTHOR. K M PATEL.
       DATE-WRITTEN. 19/11/2009.
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
       01  WS-ROW.
           05  WS-AUDIT-ID             PIC X(20).
       01  WS-I                        PIC S9(4) VALUE 0.
       LINKAGE SECTION.
       COPY AUDITLOGCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "WRITE"    PERFORM 1000-WRITE-AUDIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR
           MOVE "AUDITLOG" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "COM-013" TO CA-REQ-REF
           MOVE 0 TO CA-MSG-COUNT
           MOVE 0 TO AU-AUDIT-ID
           IF AU-SOURCE = SPACES OR AU-SOURCE = LOW-VALUES
               MOVE "COBOL" TO AU-SOURCE
           END-IF
           IF AU-SENSITIVE NOT = "Y"
               MOVE "N" TO AU-SENSITIVE
           END-IF
           MOVE FUNCTION UPPER-CASE(AU-ACTION) TO AU-ACTION.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-USER = SPACES OR CA-USER = LOW-VALUES
               MOVE "AUD0001" TO CA-REASON-CODE
               MOVE "An audit row must name the user responsible"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AU-ENTITY-TYPE = SPACES OR AU-ENTITY-KEY = SPACES
               MOVE "AUD0002" TO CA-REASON-CODE
               MOVE "An audit row must identify the record changed"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AU-ACTION = SPACES OR AU-ACTION = LOW-VALUES
               MOVE "AUD0003" TO CA-REASON-CODE
               MOVE "An audit row must state the action taken"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-WRITE-AUDIT SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_WriteAudit "
                  "?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 13 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 13
               MOVE SPACES TO SQLIO-PARM-VAL(WS-I)
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE AU-ENTITY-TYPE TO SQLIO-PARM-VAL(3)
           MOVE AU-ENTITY-KEY TO SQLIO-PARM-VAL(4)
           MOVE AU-ACTION TO SQLIO-PARM-VAL(5)
           MOVE AU-FIELD-NAME TO SQLIO-PARM-VAL(6)
           MOVE AU-OLD-VALUE TO SQLIO-PARM-VAL(7)
           MOVE AU-NEW-VALUE TO SQLIO-PARM-VAL(8)
           MOVE AU-REASON TO SQLIO-PARM-VAL(9)
           MOVE AU-AUTHORITY-REF TO SQLIO-PARM-VAL(10)
           MOVE AU-SOURCE TO SQLIO-PARM-VAL(11)
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(12)
           IF AU-SENSITIVE = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(13)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(13)
           END-IF
      *>   optional columns are sent as NULL rather than as spaces, so
      *>   that an audit enquiry can tell "not recorded" from "blank"
           PERFORM 1100-NULL-OPTIONALS
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1000-EXIT
           END-IF
           COMPUTE AU-AUDIT-ID = FUNCTION NUMVAL(WS-AUDIT-ID)
           MOVE SPACES TO CA-MESSAGE
           STRING "Audited " FUNCTION TRIM(AU-ACTION) " of "
                  FUNCTION TRIM(AU-ENTITY-TYPE) " "
                  FUNCTION TRIM(AU-ENTITY-KEY)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       1000-EXIT.
           EXIT.

       1100-NULL-OPTIONALS SECTION.
       1100-START.
           PERFORM VARYING WS-I FROM 6 BY 1 UNTIL WS-I > 12
               IF SQLIO-PARM-VAL(WS-I) = SPACES
                   MOVE "Y" TO SQLIO-PARM-NULL(WS-I)
               END-IF
           END-PERFORM.
       1100-EXIT.
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
           MOVE "AUD0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "AUDITLOG does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in AUDITLOG" TO CA-MESSAGE
           END-IF
           IF CA-MESSAGE(1:8) = "STERP-E-"
              OR CA-MESSAGE(1:8) = "STERP-W-"
               MOVE "AUD0008" TO CA-REASON-CODE
               MOVE 8 TO CA-RETURN-CODE
           ELSE
               MOVE "SQL0001" TO CA-REASON-CODE
               MOVE 12 TO CA-RETURN-CODE
           END-IF
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
