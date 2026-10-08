      *>================================================================
      *> PROGRAM-ID : CMDARGS
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : PARSE THE KEY=VALUE COMMAND LINE USED BY EVERY
      *>              STERP BATCH PROGRAM AND JOB STREAM, FOR EXAMPLE
      *>                AGEDDEBT CO=STC ASAT=20261004 USER=ARCLERK1
      *>              KEYS ARE FOLDED TO UPPER CASE; A VALUE MAY BE
      *>              QUOTED WITH DOUBLE QUOTES WHEN IT CONTAINS
      *>              SPACES.  THE COMMON KEYS ARE ALSO RETURNED IN
      *>              THEIR OWN FIELDS.  A MALFORMED ARGUMENT IS
      *>              REPORTED, NOT SILENTLY DROPPED (COM-018).
      *> CALLED BY  : BATCH MAIN PROGRAMS IN EVERY MODULE
      *> INTERFACE  : CALL "CMDARGS" USING WS-CMDARGS-CA
      *>              COPYBOOK CMDARGSCA
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-03-02 DJM  CR0151 PARAMETER CARDS FOR THE BATCH SUITE -
      *>                 ORIGINALLY READ FROM SYSIN, NOW THE COMMAND
      *>                 LINE OF THE EXE
      *> 2026-10-05 STERP QUOTED VALUES, NAMED COMMON KEYS
      *>================================================================
      *> REQ: COM-018
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CMDARGS.
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
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-LEN                  PIC S9(4) VALUE 0.
           05  WS-CH                   PIC X VALUE SPACE.
           05  WS-TOKEN                PIC X(80) VALUE SPACES.
           05  WS-TOKEN-LEN            PIC S9(4) VALUE 0.
           05  WS-EQ-POS               PIC S9(4) VALUE 0.
           05  WS-KEY                  PIC X(12) VALUE SPACES.
           05  WS-VAL                  PIC X(60) VALUE SPACES.
           05  WS-IN-QUOTE             PIC X VALUE "N".
           05  WS-TOKEN-NO             PIC S9(4) VALUE 0.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-NUM-ED               PIC Z(3)9.
       01  WS-LINE                     PIC X(256) VALUE SPACES.
       LINKAGE SECTION.
       COPY CMDARGSCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           EVALUATE CA-FUNCTION
               WHEN "CMDLINE"  PERFORM 1000-READ-COMMAND-LINE
                               PERFORM 2000-PARSE
               WHEN "PARSE"    PERFORM 2000-PARSE
               WHEN "GET"      PERFORM 3000-LOOKUP
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE WS-WORK
           MOVE "N" TO WS-ERROR
           MOVE "CMDARGS" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "COM-018" TO CA-REQ-REF
           MOVE 0 TO CA-MSG-COUNT.
       0100-EXIT.
           EXIT.

       1000-READ-COMMAND-LINE SECTION.
       1000-START.
           MOVE SPACES TO WS-LINE
           ACCEPT WS-LINE FROM COMMAND-LINE
           MOVE WS-LINE TO CM-COMMAND-LINE.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> TOKENISE.  A TOKEN ENDS AT A SPACE UNLESS A DOUBLE QUOTE IS
      *> OPEN.  AN EMPTY COMMAND LINE IS NOT AN ERROR - SOME REPORTS
      *> RUN ENTIRELY ON THEIR DEFAULTS.
      *>----------------------------------------------------------------
       2000-PARSE SECTION.
       2000-START.
           MOVE 0 TO CM-PAIR-CNT
           MOVE 0 TO CM-DROPPED
           MOVE SPACES TO CM-CO CM-SITE CM-USER CM-ASAT
           MOVE SPACES TO CM-FROM CM-TO CM-PERIOD CM-MODE CM-OUT
           MOVE "N" TO CM-FOUND
           MOVE SPACES TO CM-LOOKUP-VALUE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 20
               MOVE SPACES TO CM-KEY(WS-I)
               MOVE SPACES TO CM-VALUE(WS-I)
           END-PERFORM
           MOVE CM-COMMAND-LINE TO WS-LINE
           MOVE SPACES TO WS-TOKEN
           MOVE 0 TO WS-TOKEN-LEN
           MOVE 0 TO WS-TOKEN-NO
           MOVE "N" TO WS-IN-QUOTE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 256
               MOVE WS-LINE(WS-I:1) TO WS-CH
               EVALUATE TRUE
                   WHEN WS-CH = QUOTE
                       IF WS-IN-QUOTE = "Y"
                           MOVE "N" TO WS-IN-QUOTE
                       ELSE
                           MOVE "Y" TO WS-IN-QUOTE
                       END-IF
                   WHEN (WS-CH = SPACE OR WS-CH = LOW-VALUE)
                        AND WS-IN-QUOTE = "N"
                       IF WS-TOKEN-LEN > 0
                           PERFORM 2100-STORE-TOKEN
                       END-IF
                   WHEN WS-TOKEN-LEN < 80
                       ADD 1 TO WS-TOKEN-LEN
                       MOVE WS-CH TO WS-TOKEN(WS-TOKEN-LEN:1)
               END-EVALUATE
           END-PERFORM
           IF WS-TOKEN-LEN > 0
               PERFORM 2100-STORE-TOKEN
           END-IF
           IF WS-IN-QUOTE = "Y"
               MOVE "CMD0003" TO CA-REASON-CODE
               MOVE "Unclosed double quote in the command line"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           IF CM-DROPPED > 0
               MOVE 4 TO CA-RETURN-CODE
               MOVE "CMD0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               MOVE CM-DROPPED TO WS-NUM-ED
               STRING FUNCTION TRIM(WS-NUM-ED)
                      " argument(s) ignored - only 20 are held"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           ELSE
               MOVE SPACES TO CA-MESSAGE
               MOVE CM-PAIR-CNT TO WS-NUM-ED
               STRING FUNCTION TRIM(WS-NUM-ED)
                      " argument(s) read"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       2000-EXIT.
           EXIT.

       2100-STORE-TOKEN SECTION.
       2100-START.
           ADD 1 TO WS-TOKEN-NO
           MOVE 0 TO WS-EQ-POS
           PERFORM VARYING WS-J FROM 1 BY 1
               UNTIL WS-J > WS-TOKEN-LEN OR WS-EQ-POS > 0
               IF WS-TOKEN(WS-J:1) = "="
                   MOVE WS-J TO WS-EQ-POS
               END-IF
           END-PERFORM
           IF WS-EQ-POS = 0
      *>       some job streams repeat the program name first
               IF WS-TOKEN-NO = 1
                   PERFORM 2190-CLEAR-TOKEN
                   GO TO 2100-EXIT
               END-IF
               MOVE "CMD0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Argument [" FUNCTION TRIM(WS-TOKEN)
                      "] is not in KEY=VALUE form"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               PERFORM 2190-CLEAR-TOKEN
               GO TO 2100-EXIT
           END-IF
           IF WS-EQ-POS = 1
               MOVE "CMD0002" TO CA-REASON-CODE
               MOVE "An argument key cannot be empty" TO CA-MESSAGE
               PERFORM 9000-REJECT
               PERFORM 2190-CLEAR-TOKEN
               GO TO 2100-EXIT
           END-IF
           MOVE SPACES TO WS-KEY WS-VAL
           COMPUTE WS-LEN = WS-EQ-POS - 1
           IF WS-LEN > 12
               MOVE 12 TO WS-LEN
           END-IF
           MOVE WS-TOKEN(1:WS-LEN) TO WS-KEY
           MOVE FUNCTION UPPER-CASE(WS-KEY) TO WS-KEY
           COMPUTE WS-LEN = WS-TOKEN-LEN - WS-EQ-POS
           IF WS-LEN > 60
               MOVE 60 TO WS-LEN
           END-IF
           IF WS-LEN > 0
               MOVE WS-TOKEN(WS-EQ-POS + 1:WS-LEN) TO WS-VAL
           END-IF
           IF CM-PAIR-CNT < 20
               ADD 1 TO CM-PAIR-CNT
               MOVE WS-KEY TO CM-KEY(CM-PAIR-CNT)
               MOVE WS-VAL TO CM-VALUE(CM-PAIR-CNT)
           ELSE
               ADD 1 TO CM-DROPPED
           END-IF
           PERFORM 2200-NAMED-KEYS
           PERFORM 2190-CLEAR-TOKEN.
       2100-EXIT.
           EXIT.

       2190-CLEAR-TOKEN SECTION.
       2190-START.
           MOVE SPACES TO WS-TOKEN
           MOVE 0 TO WS-TOKEN-LEN.
       2190-EXIT.
           EXIT.

       2200-NAMED-KEYS SECTION.
       2200-START.
           EVALUATE WS-KEY
               WHEN "CO"      MOVE WS-VAL TO CM-CO
               WHEN "COMPANY" MOVE WS-VAL TO CM-CO
               WHEN "SITE"    MOVE WS-VAL TO CM-SITE
               WHEN "USER"    MOVE WS-VAL TO CM-USER
               WHEN "ASAT"    MOVE WS-VAL TO CM-ASAT
               WHEN "FROM"    MOVE WS-VAL TO CM-FROM
               WHEN "TO"      MOVE WS-VAL TO CM-TO
               WHEN "PERIOD"  MOVE WS-VAL TO CM-PERIOD
               WHEN "MODE"    MOVE FUNCTION UPPER-CASE(WS-VAL)
                                   TO CM-MODE
               WHEN "OUT"     MOVE WS-VAL TO CM-OUT
               WHEN OTHER     CONTINUE
           END-EVALUATE.
       2200-EXIT.
           EXIT.

       3000-LOOKUP SECTION.
       3000-START.
           MOVE "N" TO CM-FOUND
           MOVE SPACES TO CM-LOOKUP-VALUE
           MOVE FUNCTION UPPER-CASE(CM-LOOKUP-KEY) TO CM-LOOKUP-KEY
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > CM-PAIR-CNT OR CM-FOUND = "Y"
               IF CM-KEY(WS-I) = CM-LOOKUP-KEY
                   MOVE "Y" TO CM-FOUND
                   MOVE CM-VALUE(WS-I) TO CM-LOOKUP-VALUE
               END-IF
           END-PERFORM
           IF CM-FOUND = "N"
               MOVE 16 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No argument " FUNCTION TRIM(CM-LOOKUP-KEY)
                      " was supplied"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(CM-LOOKUP-KEY) " = "
                      FUNCTION TRIM(CM-LOOKUP-VALUE)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       3000-EXIT.
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
           MOVE "CMD0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "CMDARGS does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
