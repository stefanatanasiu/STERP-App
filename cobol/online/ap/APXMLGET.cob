      *>================================================================
      *> PROGRAM-ID : APXMLGET
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : TAKES ONE RECORD OF A STRUCTURED SUPPLIER
      *>              E-INVOICE APART.  THE PURCHASE LEDGER HAS NO XML
      *>              PARSER - IT READS THE DOCUMENT A LINE AT A TIME
      *>              AND ASKS THIS ROUTINE WHAT IS ON THE LINE, WHICH
      *>              IS ENOUGH FOR THE FLAT PEPPOL BIS BILLING 3.0
      *>              LAYOUT OUR ACCESS POINT DELIVERS (D-26).  A
      *>              DOCUMENT THAT PUTS SEVERAL ELEMENTS ON ONE LINE
      *>              IS HANDLED BY THE FIND FUNCTION.
      *>              SEE docs\tech-debt\ap.md - A REAL PARSER IS THE
      *>              MODERNISATION ANSWER.
      *> CALLED BY  : APEINV, TAPEINV
      *> INTERFACE  : CALL "APXMLGET" USING WS-XML-CA
      *>              COPYBOOK APXMLGTC
      *> CALLS      : nothing - no database, no files
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2026-10-06 STERP AP-031 ORIGINAL
      *>================================================================
      *> REQ: AP-031
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APXMLGET.
       AUTHOR. STERP.
       DATE-WRITTEN. 06/10/2026.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SCAN.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-LEN                  PIC S9(4) VALUE 0.
           05  WS-P1                   PIC S9(4) VALUE 0.
           05  WS-P2                   PIC S9(4) VALUE 0.
           05  WS-P3                   PIC S9(4) VALUE 0.
           05  WS-P4                   PIC S9(4) VALUE 0.
           05  WS-NAME-LEN             PIC S9(4) VALUE 0.
           05  WS-VAL-LEN              PIC S9(4) VALUE 0.
           05  WS-FOUND                PIC X VALUE "N".
           05  WS-WORK-VAL             PIC X(200) VALUE SPACES.
           05  WS-OUT-VAL              PIC X(200) VALUE SPACES.
           05  WS-OPEN-TAG             PIC X(44) VALUE SPACES.
           05  WS-CLOSE-TAG            PIC X(44) VALUE SPACES.
           05  WS-TAG-LEN              PIC S9(4) VALUE 0.
           05  WS-DATE-TEXT            PIC X(10) VALUE SPACES.
           05  WS-DATE-R REDEFINES WS-DATE-TEXT.
               10  WS-DT-YYYY          PIC 9(4).
               10  FILLER              PIC X.
               10  WS-DT-MM            PIC 9(2).
               10  FILLER              PIC X.
               10  WS-DT-DD            PIC 9(2).
       LINKAGE SECTION.
       COPY APXMLGTC.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-START.
           MOVE 0 TO XG-RC
           MOVE SPACES TO XG-REASON
           EVALUATE TRUE
               WHEN XG-F-ELEMENT   PERFORM 1000-ELEMENT
               WHEN XG-F-FIND      PERFORM 2000-FIND
               WHEN XG-F-DATE      PERFORM 3000-DATE
               WHEN XG-F-NUM       PERFORM 4000-NUM
               WHEN OTHER
                   MOVE 08 TO XG-RC
                   MOVE "APXG001" TO XG-REASON
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
      *>   the first element on the record, whatever it is called
       1000-ELEMENT SECTION.
       1000-START.
           PERFORM 1050-CLEAR
           PERFORM 1100-RECORD-LENGTH
           IF WS-LEN = 0
               MOVE 04 TO XG-RC
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-FIND-ANGLE
           IF WS-FOUND NOT = "Y"
               MOVE 04 TO XG-RC
               GO TO 1000-EXIT
           END-IF
           PERFORM 1300-TAG-NAME
           IF XG-RC NOT = 0
               GO TO 1000-EXIT
           END-IF
           PERFORM 1400-TAG-VALUE
           PERFORM 1500-DECODE.
       1000-EXIT.
           EXIT.

       1050-CLEAR SECTION.
       1050-START.
           MOVE SPACES TO XG-TAG-FOUND
           MOVE SPACES TO XG-VALUE
           MOVE SPACES TO XG-ATTRIBUTES
           MOVE 0 TO XG-VALUE-LEN
           MOVE "N" TO XG-IS-OPEN
           MOVE "N" TO XG-IS-CLOSE
           MOVE "N" TO XG-IS-EMPTY
           MOVE "N" TO WS-FOUND
           MOVE SPACES TO WS-WORK-VAL
           MOVE 0 TO WS-P1 WS-P2 WS-P3 WS-P4.
       1050-EXIT.
           EXIT.

       1100-RECORD-LENGTH SECTION.
       1100-START.
           MOVE 0 TO WS-LEN
           PERFORM VARYING WS-I FROM 300 BY -1
               UNTIL WS-I < 1 OR WS-LEN > 0
               IF XG-RECORD(WS-I:1) NOT = SPACE
                   MOVE WS-I TO WS-LEN
               END-IF
           END-PERFORM.
       1100-EXIT.
           EXIT.

       1200-FIND-ANGLE SECTION.
       1200-START.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-LEN OR WS-FOUND = "Y"
               IF XG-RECORD(WS-I:1) = "<"
                   MOVE "Y" TO WS-FOUND
                   COMPUTE WS-P1 = WS-I + 1
               END-IF
           END-PERFORM.
       1200-EXIT.
           EXIT.

      *>   a closing tag, a declaration and a comment are all reported
      *>   rather than rejected - the caller decides what matters
       1300-TAG-NAME SECTION.
       1300-START.
           IF WS-P1 > WS-LEN
               MOVE 04 TO XG-RC
               GO TO 1300-EXIT
           END-IF
           IF XG-RECORD(WS-P1:1) = "/"
               MOVE "Y" TO XG-IS-CLOSE
               ADD 1 TO WS-P1
           END-IF
           IF XG-RECORD(WS-P1:1) = "?" OR XG-RECORD(WS-P1:1) = "!"
               MOVE 04 TO XG-RC
               GO TO 1300-EXIT
           END-IF
           MOVE "N" TO WS-FOUND
           PERFORM VARYING WS-I FROM WS-P1 BY 1
               UNTIL WS-I > WS-LEN OR WS-FOUND = "Y"
               IF XG-RECORD(WS-I:1) = ">" OR XG-RECORD(WS-I:1) = SPACE
                   MOVE "Y" TO WS-FOUND
                   MOVE WS-I TO WS-P2
               END-IF
           END-PERFORM
           IF WS-FOUND NOT = "Y"
               MOVE 08 TO XG-RC
               MOVE "APXG002" TO XG-REASON
               GO TO 1300-EXIT
           END-IF
           COMPUTE WS-NAME-LEN = WS-P2 - WS-P1
           IF WS-NAME-LEN < 1 OR WS-NAME-LEN > 40
               MOVE 08 TO XG-RC
               MOVE "APXG003" TO XG-REASON
               GO TO 1300-EXIT
           END-IF
           MOVE XG-RECORD(WS-P1:WS-NAME-LEN) TO XG-TAG-FOUND
           MOVE "N" TO WS-FOUND
           PERFORM VARYING WS-I FROM WS-P2 BY 1
               UNTIL WS-I > WS-LEN OR WS-FOUND = "Y"
               IF XG-RECORD(WS-I:1) = ">"
                   MOVE "Y" TO WS-FOUND
                   MOVE WS-I TO WS-P3
               END-IF
           END-PERFORM
           IF WS-FOUND NOT = "Y"
               MOVE 08 TO XG-RC
               MOVE "APXG004" TO XG-REASON
               GO TO 1300-EXIT
           END-IF
           IF WS-P3 > WS-P2
               COMPUTE WS-J = WS-P3 - WS-P2
               IF WS-J > 100
                   MOVE 100 TO WS-J
               END-IF
               MOVE XG-RECORD(WS-P2:WS-J) TO XG-ATTRIBUTES
           END-IF
           IF XG-RECORD(WS-P3 - 1:1) = "/"
               MOVE "Y" TO XG-IS-EMPTY
           END-IF.
       1300-EXIT.
           EXIT.

      *>   value between > and the next </ on the same record; no
      *>   closing tag means a container was opened
       1400-TAG-VALUE SECTION.
       1400-START.
           IF XG-IS-CLOSE = "Y" OR XG-IS-EMPTY = "Y"
               GO TO 1400-EXIT
           END-IF
           MOVE "N" TO WS-FOUND
           COMPUTE WS-I = WS-P3 + 1
           PERFORM UNTIL WS-I NOT < WS-LEN OR WS-FOUND = "Y"
               IF XG-RECORD(WS-I:2) = "</"
                   MOVE "Y" TO WS-FOUND
                   MOVE WS-I TO WS-P4
               END-IF
               ADD 1 TO WS-I
           END-PERFORM
           IF WS-FOUND NOT = "Y"
               MOVE "Y" TO XG-IS-OPEN
               GO TO 1400-EXIT
           END-IF
           COMPUTE WS-VAL-LEN = WS-P4 - WS-P3 - 1
           IF WS-VAL-LEN < 1
               GO TO 1400-EXIT
           END-IF
           IF WS-VAL-LEN > 200
               MOVE 200 TO WS-VAL-LEN
           END-IF
           COMPUTE WS-J = WS-P3 + 1
           MOVE XG-RECORD(WS-J:WS-VAL-LEN) TO WS-WORK-VAL.
       1400-EXIT.
           EXIT.

      *>   the five predefined entities; anything else travels as it
      *>   arrived, which is how the 1998 EDI translator behaved
       1500-DECODE SECTION.
       1500-START.
           MOVE SPACES TO WS-OUT-VAL
           MOVE 0 TO WS-J
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 200
               EVALUATE TRUE
                   WHEN WS-WORK-VAL(WS-I:5) = "&amp;"
                       ADD 1 TO WS-J
                       MOVE "&" TO WS-OUT-VAL(WS-J:1)
                       ADD 5 TO WS-I
                   WHEN WS-WORK-VAL(WS-I:4) = "&lt;"
                       ADD 1 TO WS-J
                       MOVE "<" TO WS-OUT-VAL(WS-J:1)
                       ADD 4 TO WS-I
                   WHEN WS-WORK-VAL(WS-I:4) = "&gt;"
                       ADD 1 TO WS-J
                       MOVE ">" TO WS-OUT-VAL(WS-J:1)
                       ADD 4 TO WS-I
                   WHEN WS-WORK-VAL(WS-I:6) = "&quot;"
                       ADD 1 TO WS-J
                       MOVE QUOTE TO WS-OUT-VAL(WS-J:1)
                       ADD 6 TO WS-I
                   WHEN WS-WORK-VAL(WS-I:6) = "&apos;"
                       ADD 1 TO WS-J
                       MOVE "'" TO WS-OUT-VAL(WS-J:1)
                       ADD 6 TO WS-I
                   WHEN OTHER
                       ADD 1 TO WS-J
                       MOVE WS-WORK-VAL(WS-I:1) TO WS-OUT-VAL(WS-J:1)
                       ADD 1 TO WS-I
               END-EVALUATE
           END-PERFORM
           MOVE WS-OUT-VAL TO XG-VALUE
           MOVE 0 TO XG-VALUE-LEN
           PERFORM VARYING WS-I FROM 200 BY -1
               UNTIL WS-I < 1 OR XG-VALUE-LEN > 0
               IF XG-VALUE(WS-I:1) NOT = SPACE
                   MOVE WS-I TO XG-VALUE-LEN
               END-IF
           END-PERFORM.
       1500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   <tag>value</tag> anywhere on the record
       2000-FIND SECTION.
       2000-START.
           PERFORM 1050-CLEAR
           PERFORM 1100-RECORD-LENGTH
           MOVE 0 TO WS-TAG-LEN
           PERFORM VARYING WS-I FROM 40 BY -1
               UNTIL WS-I < 1 OR WS-TAG-LEN > 0
               IF XG-TAG-WANTED(WS-I:1) NOT = SPACE
                   MOVE WS-I TO WS-TAG-LEN
               END-IF
           END-PERFORM
           IF WS-TAG-LEN = 0 OR WS-LEN = 0
               MOVE 04 TO XG-RC
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO WS-OPEN-TAG
           MOVE SPACES TO WS-CLOSE-TAG
           STRING "<" XG-TAG-WANTED(1:WS-TAG-LEN) ">"
               DELIMITED BY SIZE INTO WS-OPEN-TAG
           STRING "</" XG-TAG-WANTED(1:WS-TAG-LEN) ">"
               DELIMITED BY SIZE INTO WS-CLOSE-TAG
           MOVE "N" TO WS-FOUND
           COMPUTE WS-J = WS-TAG-LEN + 2
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-LEN OR WS-FOUND = "Y"
               IF XG-RECORD(WS-I:WS-J) = WS-OPEN-TAG(1:WS-J)
                   MOVE "Y" TO WS-FOUND
                   COMPUTE WS-P3 = WS-I + WS-J - 1
               END-IF
           END-PERFORM
           IF WS-FOUND NOT = "Y"
               MOVE 04 TO XG-RC
               GO TO 2000-EXIT
           END-IF
           MOVE XG-TAG-WANTED TO XG-TAG-FOUND
           PERFORM 1400-TAG-VALUE
           PERFORM 1500-DECODE.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       3000-DATE SECTION.
       3000-START.
           MOVE 0 TO XG-DATE
           MOVE XG-VALUE(1:10) TO WS-DATE-TEXT
           IF WS-DATE-TEXT(5:1) NOT = "-" OR WS-DATE-TEXT(8:1) NOT = "-"
               MOVE 08 TO XG-RC
               MOVE "APXG005" TO XG-REASON
               GO TO 3000-EXIT
           END-IF
           IF WS-DATE-TEXT(1:4) IS NOT NUMERIC
              OR WS-DATE-TEXT(6:2) IS NOT NUMERIC
              OR WS-DATE-TEXT(9:2) IS NOT NUMERIC
               MOVE 08 TO XG-RC
               MOVE "APXG006" TO XG-REASON
               GO TO 3000-EXIT
           END-IF
           COMPUTE XG-DATE = (WS-DT-YYYY * 10000) + (WS-DT-MM * 100)
                             + WS-DT-DD.
       3000-EXIT.
           EXIT.

       4000-NUM SECTION.
       4000-START.
           MOVE 0 TO XG-NUMERIC
           IF XG-VALUE = SPACES
               MOVE 04 TO XG-RC
               GO TO 4000-EXIT
           END-IF
           MOVE 0 TO WS-J
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 200
               IF XG-VALUE(WS-I:1) NOT = SPACE
                   IF XG-VALUE(WS-I:1) IS NOT NUMERIC
                      AND XG-VALUE(WS-I:1) NOT = "."
                      AND XG-VALUE(WS-I:1) NOT = "-"
                      AND XG-VALUE(WS-I:1) NOT = "+"
                       ADD 1 TO WS-J
                   END-IF
               END-IF
           END-PERFORM
           IF WS-J > 0
               MOVE 08 TO XG-RC
               MOVE "APXG007" TO XG-REASON
               GO TO 4000-EXIT
           END-IF
           COMPUTE XG-NUMERIC = FUNCTION NUMVAL(XG-VALUE).
       4000-EXIT.
           EXIT.
