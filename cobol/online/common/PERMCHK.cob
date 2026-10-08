      *>================================================================
      *> PROGRAM-ID : PERMCHK
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : AUTHORITY CHECK FOR AN ACTION, A COMPANY AND A
      *>              SITE (COM-002).  HIDING A MENU OPTION IS NOT THE
      *>              CONTROL - EVERY BUSINESS ACTION AND EVERY
      *>              SENSITIVE ENQUIRY OR EXPORT ASKS HERE FIRST, AND
      *>              AN UNAUTHORISED ATTEMPT HAS NO BUSINESS EFFECT
      *>              AND IS EXPLAINED.
      *>              A REFUSAL IS RETURN CODE 06 (NEEDS AUTHORITY),
      *>              WHICH THE CLIENT PRESENTS DIFFERENTLY FROM A
      *>              VALIDATION ERROR (08) OR A SYSTEM FAILURE (12) -
      *>              COM-017.
      *> CALLED BY  : EVERY MODULE AND THE .NET CLIENT
      *> INTERFACE  : CALL "PERMCHK" USING WS-PERMCHK-CA
      *>              COPYBOOK PERMCHKCA
      *> CALLS      : SQLIO -> core.usp_CheckPermission
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2002-04-08 KMP  CR1520 ROLE BASED ACCESS REPLACES THE OLD
      *>                 THREE LEVEL USER CLASS
      *> 2026-10-05 STERP core.usp_CheckPermission, SITE SCOPE
      *>================================================================
      *> REQ: COM-002, COM-019
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERMCHK.
       AUTHOR. K M PATEL.
       DATE-WRITTEN. 08/04/2002.
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
           05  WS-K-USER               PIC X(20).
           05  WS-K-PERM               PIC X(40).
           05  WS-K-COMPANY            PIC X(10).
           05  WS-K-SITE               PIC X(10).
           05  WS-K-ALLOWED            PIC X(4).
           05  WS-K-REASON             PIC X(200).
       LINKAGE SECTION.
       COPY PERMCHKCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "CHECK"    PERFORM 1000-CHECK
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR
           MOVE "PERMCHK" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "COM-002" TO CA-REQ-REF
           MOVE 0 TO CA-MSG-COUNT
           MOVE "N" TO PK-ALLOWED
           MOVE SPACES TO PK-REASON-TEXT
           IF PK-CHECK-USER = SPACES OR PK-CHECK-USER = LOW-VALUES
               MOVE CA-USER TO PK-CHECK-USER
           END-IF
           IF PK-CHECK-SITE = LOW-VALUES
               MOVE SPACES TO PK-CHECK-SITE
           END-IF
           IF PK-CHECK-SITE = SPACES
               MOVE CA-SITE TO PK-CHECK-SITE
           END-IF
           MOVE FUNCTION UPPER-CASE(PK-PERMISSION) TO PK-PERMISSION.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF PK-CHECK-USER = SPACES OR PK-CHECK-USER = LOW-VALUES
               MOVE "PRM0001" TO CA-REASON-CODE
               MOVE "A user code is required for an authority check"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF PK-PERMISSION = SPACES OR PK-PERMISSION = LOW-VALUES
               MOVE "PRM0002" TO CA-REASON-CODE
               MOVE "A permission code is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-COMPANY = SPACES OR CA-COMPANY = LOW-VALUES
               MOVE "PRM0003" TO CA-REASON-CODE
               MOVE "Authority is always scoped to a company"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-CHECK SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_CheckPermission ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE PK-CHECK-USER TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE PK-PERMISSION TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           IF PK-CHECK-SITE = SPACES
               MOVE SPACES TO SQLIO-PARM-VAL(4)
               MOVE "Y" TO SQLIO-PARM-NULL(4)
           ELSE
               MOVE PK-CHECK-SITE TO SQLIO-PARM-VAL(4)
               MOVE "N" TO SQLIO-PARM-NULL(4)
           END-IF
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           MOVE 10 TO SQLIO-COL-WIDTH(4)
           MOVE 4 TO SQLIO-COL-WIDTH(5)
           MOVE 200 TO SQLIO-COL-WIDTH(6)
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "N" TO PK-ALLOWED
               MOVE "The authority check returned no answer"
                   TO PK-REASON-TEXT
               MOVE PK-REASON-TEXT TO CA-MESSAGE
               MOVE "PRM0005" TO CA-REASON-CODE
               MOVE 12 TO CA-RETURN-CODE
               GO TO 1000-EXIT
           END-IF
           MOVE WS-K-REASON TO PK-REASON-TEXT
           IF WS-K-ALLOWED(1:1) = "1"
               MOVE "Y" TO PK-ALLOWED
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(PK-CHECK-USER) " holds "
                      FUNCTION TRIM(PK-PERMISSION) " in "
                      FUNCTION TRIM(CA-COMPANY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
      *>       06 = needs authority.  Not 08: the request is valid,
      *>       the user simply may not do it (COM-002 / COM-017).
               MOVE "N" TO PK-ALLOWED
               MOVE 6 TO CA-RETURN-CODE
               MOVE "PRM0006" TO CA-REASON-CODE
               IF PK-REASON-TEXT = SPACES
                   MOVE SPACES TO CA-MESSAGE
                   STRING FUNCTION TRIM(PK-CHECK-USER)
                          " does not hold "
                          FUNCTION TRIM(PK-PERMISSION)
                       DELIMITED BY SIZE INTO CA-MESSAGE
               ELSE
                   MOVE PK-REASON-TEXT TO CA-MESSAGE
               END-IF
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE "N" TO PK-ALLOWED
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "PRM0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "PERMCHK does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE "N" TO PK-ALLOWED
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in PERMCHK" TO CA-MESSAGE
           END-IF
           MOVE CA-MESSAGE TO PK-REASON-TEXT
           IF CA-MESSAGE(1:8) = "STERP-E-"
              OR CA-MESSAGE(1:8) = "STERP-W-"
               MOVE "PRM0007" TO CA-REASON-CODE
               MOVE 6 TO CA-RETURN-CODE
           ELSE
               MOVE "SQL0001" TO CA-REASON-CODE
               MOVE 12 TO CA-RETURN-CODE
           END-IF
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
