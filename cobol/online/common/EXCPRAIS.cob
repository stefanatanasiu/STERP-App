      *>================================================================
      *> PROGRAM-ID : EXCPRAIS
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : RAISE AN ACTIONABLE EXCEPTION ON A WORKLIST
      *>              (COM-017, COM-022).  HOLDS, WARNINGS AND HARD
      *>              ERRORS STAY DISTINGUISHABLE: THE SEVERITY IS
      *>              CHECKED HERE AND AGAIN IN THE PROCEDURE.
      *>              EVERY ITEM CARRIES AN OWNER, A PRIORITY AND,
      *>              WHERE RELEVANT, A DUE DATE, SO THAT IT CAN BE
      *>              AGED AND CHASED.  RAISING THE SAME CATEGORY FOR
      *>              THE SAME DOCUMENT AGAIN REFRESHES THE OPEN ITEM
      *>              RATHER THAN BREEDING DUPLICATES.
      *>              A NOTIFICATION IS NOT APPROVAL AND NOT PROOF
      *>              THAT ITS RECIPIENT ACTED - RESOLUTION IS A
      *>              SEPARATE, EVIDENCED STEP (usp_ResolveException).
      *> CALLED BY  : EVERY MODULE AND THE .NET CLIENT
      *> INTERFACE  : CALL "EXCPRAIS" USING WS-EXCPRAIS-CA
      *>              COPYBOOK EXCPRAISCA
      *> CALLS      : SQLIO -> core.usp_RaiseException
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2007-03-14 ANW  CR2150 EXCEPTION WORKLIST REPLACES THE NIGHTLY
      *>                 "ERRORS AND OMISSIONS" PRINTOUT
      *> 2026-10-05 STERP core.ExceptionItem, OWNER AND DUE DATE
      *>================================================================
      *> REQ: COM-017, COM-022
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXCPRAIS.
       AUTHOR. A N WRIGHT.
       DATE-WRITTEN. 14/03/2007.
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
       01  WS-I                        PIC S9(4) VALUE 0.
       01  WS-ROW.
           05  WS-E-ID                 PIC X(20).
           05  WS-E-COMPANY            PIC X(10).
           05  WS-E-MODULE             PIC X(6).
           05  WS-E-SEVERITY           PIC X(12).
           05  WS-E-CATEGORY           PIC X(40).
           05  WS-E-DOCTYPE            PIC X(20).
           05  WS-E-DOCREF             PIC X(40).
           05  WS-E-MESSAGE            PIC X(200).
           05  WS-E-OWNER-USER         PIC X(20).
           05  WS-E-OWNER-ROLE         PIC X(20).
           05  WS-E-PRIORITY           PIC X(8).
           05  WS-E-RAISED             PIC X(23).
           05  WS-E-DUE                PIC X(23).
           05  WS-E-STATUS             PIC X(12).
       LINKAGE SECTION.
       COPY EXCPRAISCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "RAISE"    PERFORM 1000-RAISE
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR
           MOVE "EXCPRAIS" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "COM-022" TO CA-REQ-REF
           MOVE 0 TO CA-MSG-COUNT
           MOVE 0 TO EX-EXCEPTION-ID
           MOVE SPACES TO EX-STATUS EX-RAISED-AT
           MOVE FUNCTION UPPER-CASE(EX-SEVERITY) TO EX-SEVERITY
           MOVE FUNCTION UPPER-CASE(EX-MODULE) TO EX-MODULE
           IF EX-PRIORITY < 1 OR EX-PRIORITY > 9
               MOVE 5 TO EX-PRIORITY
           END-IF.
       0100-EXIT.
           EXIT.

      *>   COM-017 - the three severities must stay distinguishable
       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES OR CA-COMPANY = LOW-VALUES
               MOVE "EXC0001" TO CA-REASON-CODE
               MOVE "An exception must belong to a company"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF NOT EX-HOLD AND NOT EX-WARNING AND NOT EX-ERROR
               MOVE "EXC0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Severity must be HOLD, WARNING or ERROR, not "
                      FUNCTION TRIM(EX-SEVERITY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF EX-CATEGORY = SPACES OR EX-CATEGORY = LOW-VALUES
               MOVE "EXC0003" TO CA-REASON-CODE
               MOVE "An exception must state its category"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF EX-MESSAGE-TEXT = SPACES
               MOVE "EXC0004" TO CA-REASON-CODE
               MOVE "An exception must explain the affected action"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF EX-OWNER-USER = SPACES AND EX-OWNER-ROLE = SPACES
      *>       COM-022 - an exception with no owner never gets worked
               MOVE "EXC0005" TO CA-REASON-CODE
               MOVE "An exception needs an owning user or role"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF EX-DUE-AT NOT = 0
              AND FUNCTION TEST-DATE-YYYYMMDD(EX-DUE-AT) NOT = 0
               MOVE "EXC0006" TO CA-REASON-CODE
               MOVE "The due date is not a valid calendar date"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-RAISE SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_RaiseException "
                  "?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 16 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 16
               MOVE SPACES TO SQLIO-PARM-VAL(WS-I)
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE EX-MODULE TO SQLIO-PARM-VAL(2)
           MOVE EX-SEVERITY TO SQLIO-PARM-VAL(3)
           MOVE EX-CATEGORY TO SQLIO-PARM-VAL(4)
           MOVE EX-MESSAGE-TEXT TO SQLIO-PARM-VAL(5)
           MOVE EX-DOCUMENT-TYPE TO SQLIO-PARM-VAL(6)
           MOVE EX-DOCUMENT-REF TO SQLIO-PARM-VAL(7)
           MOVE CA-SITE TO SQLIO-PARM-VAL(8)
           MOVE EX-OWNER-USER TO SQLIO-PARM-VAL(9)
           MOVE EX-OWNER-ROLE TO SQLIO-PARM-VAL(10)
           MOVE EX-PRIORITY TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(11)
           IF EX-DUE-AT = 0
               MOVE "Y" TO SQLIO-PARM-NULL(12)
           ELSE
               MOVE EX-DUE-AT TO SQLIO-PARM-VAL(12)
           END-IF
           IF EX-AMOUNT-GBP = 0
               MOVE "Y" TO SQLIO-PARM-NULL(13)
           ELSE
               MOVE EX-AMOUNT-GBP TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT)
                   TO SQLIO-PARM-VAL(13)
           END-IF
           IF EX-QUANTITY = 0
               MOVE "Y" TO SQLIO-PARM-NULL(14)
           ELSE
               MOVE EX-QUANTITY TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT)
                   TO SQLIO-PARM-VAL(14)
           END-IF
           MOVE CA-USER TO SQLIO-PARM-VAL(15)
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(16)
           PERFORM 1100-NULL-OPTIONALS
           MOVE 14 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 6 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 40 TO SQLIO-COL-WIDTH(5)
           MOVE 20 TO SQLIO-COL-WIDTH(6)
           MOVE 40 TO SQLIO-COL-WIDTH(7)
           MOVE 200 TO SQLIO-COL-WIDTH(8)
           MOVE 20 TO SQLIO-COL-WIDTH(9)
           MOVE 20 TO SQLIO-COL-WIDTH(10)
           MOVE 8 TO SQLIO-COL-WIDTH(11)
           MOVE 23 TO SQLIO-COL-WIDTH(12)
           MOVE 23 TO SQLIO-COL-WIDTH(13)
           MOVE 12 TO SQLIO-COL-WIDTH(14)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1000-EXIT
           END-IF
           COMPUTE EX-EXCEPTION-ID = FUNCTION NUMVAL(WS-E-ID)
           MOVE WS-E-STATUS TO EX-STATUS
           MOVE WS-E-RAISED TO EX-RAISED-AT
      *>   the caller's own return code mirrors the severity so that a
      *>   hold and a hard error cannot be confused by the client
           EVALUATE TRUE
               WHEN EX-HOLD    MOVE 6 TO CA-RETURN-CODE
               WHEN EX-WARNING MOVE 4 TO CA-RETURN-CODE
               WHEN OTHER      MOVE 0 TO CA-RETURN-CODE
           END-EVALUATE
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(EX-SEVERITY) " raised on worklist: "
                  FUNCTION TRIM(EX-MESSAGE-TEXT)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       1000-EXIT.
           EXIT.

       1100-NULL-OPTIONALS SECTION.
       1100-START.
           PERFORM VARYING WS-I FROM 6 BY 1 UNTIL WS-I > 10
               IF SQLIO-PARM-VAL(WS-I) = SPACES
                   MOVE "Y" TO SQLIO-PARM-NULL(WS-I)
               END-IF
           END-PERFORM
           PERFORM VARYING WS-I FROM 15 BY 1 UNTIL WS-I > 16
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
           MOVE "EXC0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "EXCPRAIS does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in EXCPRAIS" TO CA-MESSAGE
           END-IF
           IF CA-MESSAGE(1:8) = "STERP-E-"
              OR CA-MESSAGE(1:8) = "STERP-W-"
               MOVE "EXC0008" TO CA-REASON-CODE
               MOVE 8 TO CA-RETURN-CODE
           ELSE
               MOVE "SQL0001" TO CA-REASON-CODE
               MOVE 12 TO CA-RETURN-CODE
           END-IF
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
