      *>================================================================
      *> PROGRAM-ID : STRUTIL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : TEXT HANDLING SHARED BY EVERY MODULE -
      *>              * UPPER CASING AND VALIDATION OF BUSINESS CODES
      *>                (D-21: A-Z 0-9 - . / _ , UP TO 40 CHARACTERS,
      *>                UNIQUE WITHOUT REGARD TO CASE, STORED UPPER)
      *>              * NORMALISING DOCUMENT REFERENCES SO THAT
      *>                DUPLICATE DETECTION CANNOT BE DEFEATED BY
      *>                SPACES, PUNCTUATION OR LEADING ZEROS (COM-004)
      *>              * MONEY AND QUANTITY FORMATTING FOR MESSAGES AND
      *>                REPORTS, WITH AN EXPLICIT CURRENCY (COM-024)
      *> POUND SIGN : COBOL SOURCES AND COMMAREAS ARE SINGLE BYTE.  THE
      *>              POUND SIGN IS X"A3" IN ISO-8859-1 / WINDOWS-1252
      *>              BUT TWO BYTES IN UTF-8, SO EMBEDDING IT IN FIXED
      *>              WIDTH TEXT WOULD SHIFT EVERY COLUMN BEHIND IT.
      *>              THE DEFAULT IS THEREFORE THE TEXT "GBP"; X"A3" IS
      *>              ONLY PRODUCED WHEN THE CALLER ASKS FOR IT.
      *> CALLED BY  : EVERY MODULE AND THE .NET CLIENT
      *> INTERFACE  : CALL "STRUTIL" USING WS-STRUTIL-CA
      *>              COPYBOOK STRUTILCA
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1995-04-20 DJM  CR0388 PART NUMBER VALIDATION (STOCK CONTROL)
      *> 2001-10-15 KMP  CR1405 REFERENCE MATCHING FOR THE BACS RETURNS
      *>                 FILE - SUPPLIERS SEND THE SAME REFERENCE WITH
      *>                 AND WITHOUT PUNCTUATION
      *> 2012-04-23 ANW  REWRITE FOR GNUCOBOL
      *> 2026-10-05 STERP D-21 CHARACTER SET, POUND SIGN POLICY
      *>================================================================
      *> REQ: COM-004, COM-024
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRUTIL.
       AUTHOR. D J MATHERS.
       DATE-WRITTEN. 20/04/1995.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY STMONEY.
       01  WS-CONST.
      *>   D-21 - the only characters allowed in a business code
           05  WS-ALLOWED              PIC X(40) VALUE
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-./_".
           05  WS-DIGITS-ONLY          PIC X(10) VALUE "0123456789".
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-K                    PIC S9(4) VALUE 0.
           05  WS-LEN                  PIC S9(4) VALUE 0.
           05  WS-MAX                  PIC S9(4) VALUE 40.
           05  WS-CH                   PIC X VALUE SPACE.
           05  WS-PREV                 PIC X VALUE SPACE.
           05  WS-POS                  PIC S9(4) VALUE 0.
           05  WS-FIRST-DIG            PIC S9(4) VALUE 0.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-UPPER                PIC X(200) VALUE SPACES.
           05  WS-BUILD                PIC X(200) VALUE SPACES.
           05  WS-TAIL                 PIC X(200) VALUE SPACES.
           05  WS-ALL-DIGITS           PIC X VALUE "N".
       01  WS-EDIT.
           05  WS-ED-MONEY             PIC ---,---,---,---,--9.99.
           05  WS-ED-QTY               PIC ---,---,---,---,--9.9(6).
           05  WS-ED-TEXT              PIC X(30) VALUE SPACES.
           05  WS-SIGN-TEXT            PIC X VALUE SPACE.
       LINKAGE SECTION.
       COPY STRUTILCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           EVALUATE CA-FUNCTION
               WHEN "UPPER"    PERFORM 1000-UPPER
               WHEN "CODE"     PERFORM 1100-VALIDATE-CODE
               WHEN "DOCREF"   PERFORM 2000-NORMALISE-REF
               WHEN "MONEY"    PERFORM 3000-FORMAT-MONEY
               WHEN "QTY"      PERFORM 3100-FORMAT-QTY
               WHEN "SQUEEZE"  PERFORM 4000-SQUEEZE
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE WS-WORK WS-EDIT
           MOVE "N" TO WS-ERROR
           MOVE "STRUTIL" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "COM-004" TO CA-REQ-REF
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO SU-TEXT-OUT
           MOVE "Y" TO SU-VALID
           MOVE "N" TO SU-CHANGED
           MOVE SPACE TO SU-BAD-CHAR
           MOVE 0 TO SU-BAD-POS
           MOVE 0 TO SU-LENGTH-OUT
           IF SU-MAX-LEN <= 0 OR SU-MAX-LEN > 200
               MOVE 40 TO SU-MAX-LEN
           END-IF
           MOVE SU-MAX-LEN TO WS-MAX
           IF SU-CURRENCY = SPACES OR SU-CURRENCY = LOW-VALUES
               MOVE "GBP" TO SU-CURRENCY
           END-IF
           IF SU-ZERO-POLICY = SPACES OR SU-ZERO-POLICY = LOW-VALUES
               MOVE "KEEP" TO SU-ZERO-POLICY
           END-IF
           MOVE FUNCTION UPPER-CASE(SU-TEXT-IN) TO WS-UPPER.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-UPPER SECTION.
       1000-START.
           MOVE WS-UPPER TO SU-TEXT-OUT
           PERFORM 8000-OUT-LENGTH
           IF SU-TEXT-OUT NOT = SU-TEXT-IN
               MOVE "Y" TO SU-CHANGED
           END-IF
           MOVE "Folded to upper case" TO CA-MESSAGE.
       1000-EXIT.
           EXIT.

      *>   D-21 - codes are up to 40 characters from A-Z 0-9 - . / _ ,
      *>   unique without regard to case and stored in upper case
       1100-VALIDATE-CODE SECTION.
       1100-START.
           MOVE WS-UPPER TO SU-TEXT-OUT
           PERFORM 8000-OUT-LENGTH
           MOVE SU-LENGTH-OUT TO WS-LEN
           IF WS-LEN = 0
               MOVE "N" TO SU-VALID
               MOVE "STR0001" TO CA-REASON-CODE
               MOVE "A code is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1100-EXIT
           END-IF
           IF WS-LEN > WS-MAX
               MOVE "N" TO SU-VALID
               MOVE WS-MAX TO SU-BAD-POS
               MOVE "STR0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               MOVE WS-MAX TO STM-ED-INT
               STRING "Code is longer than "
                      FUNCTION TRIM(STM-ED-INT) " characters"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1100-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-LEN OR SU-VALID = "N"
               MOVE SU-TEXT-OUT(WS-I:1) TO WS-CH
               MOVE 0 TO WS-POS
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > 40 OR WS-POS > 0
                   IF WS-ALLOWED(WS-J:1) = WS-CH
                       MOVE WS-J TO WS-POS
                   END-IF
               END-PERFORM
               IF WS-POS = 0
                   MOVE "N" TO SU-VALID
                   MOVE WS-CH TO SU-BAD-CHAR
                   MOVE WS-I TO SU-BAD-POS
               END-IF
           END-PERFORM
           IF SU-VALID = "N"
               MOVE "STR0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               MOVE SU-BAD-POS TO STM-ED-INT
               STRING "Character [" SU-BAD-CHAR "] at position "
                      FUNCTION TRIM(STM-ED-INT)
                      " is not allowed in a code "
                      "(A-Z 0-9 and - . / _ only)"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1100-EXIT
           END-IF
           IF SU-TEXT-OUT NOT = SU-TEXT-IN
               MOVE "Y" TO SU-CHANGED
               MOVE 4 TO CA-RETURN-CODE
               MOVE "STR0004" TO CA-REASON-CODE
               MOVE "Code accepted and stored in upper case"
                   TO CA-MESSAGE
           ELSE
               MOVE "Code accepted" TO CA-MESSAGE
           END-IF.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> DOCUMENT REFERENCE NORMALISATION (COM-004)
      *> Keeps only A-Z and 0-9, then optionally drops the leading
      *> zeros of the trailing numeric part, so that
      *>   "SI-000123", "si 000123" and "SI/000123" all normalise to
      *>   "SI123" under STRIP and to "SI000123" under KEEP.
      *> A reference that is all digits keeps at least one digit.
      *>----------------------------------------------------------------
       2000-NORMALISE-REF SECTION.
       2000-START.
           MOVE SPACES TO WS-BUILD
           MOVE 0 TO WS-K
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 200
               MOVE WS-UPPER(WS-I:1) TO WS-CH
               IF (WS-CH >= "A" AND WS-CH <= "Z")
                  OR (WS-CH >= "0" AND WS-CH <= "9")
                   ADD 1 TO WS-K
                   MOVE WS-CH TO WS-BUILD(WS-K:1)
               END-IF
           END-PERFORM
           IF WS-K = 0
               MOVE "STR0005" TO CA-REASON-CODE
               MOVE "The reference has no letters or digits"
                   TO CA-MESSAGE
               MOVE "N" TO SU-VALID
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           IF SU-ZERO-STRIP
               PERFORM 2100-STRIP-LEADING-ZEROS
           END-IF
           MOVE WS-BUILD TO SU-TEXT-OUT
           PERFORM 8000-OUT-LENGTH
           IF SU-TEXT-OUT NOT = SU-TEXT-IN
               MOVE "Y" TO SU-CHANGED
               MOVE 4 TO CA-RETURN-CODE
               MOVE "STR0006" TO CA-REASON-CODE
           END-IF
           MOVE SPACES TO CA-MESSAGE
           STRING "Reference normalised to "
                  FUNCTION TRIM(SU-TEXT-OUT)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

       2100-STRIP-LEADING-ZEROS SECTION.
       2100-START.
      *>   find the first character of the trailing run of digits
           MOVE 0 TO WS-FIRST-DIG
           PERFORM VARYING WS-I FROM WS-K BY -1 UNTIL WS-I < 1
               MOVE WS-BUILD(WS-I:1) TO WS-CH
               IF WS-CH >= "0" AND WS-CH <= "9"
                   MOVE WS-I TO WS-FIRST-DIG
               ELSE
                   MOVE 0 TO WS-I
               END-IF
           END-PERFORM
           IF WS-FIRST-DIG = 0
               GO TO 2100-EXIT
           END-IF
      *>   skip the zeros but always leave one digit standing
           MOVE WS-FIRST-DIG TO WS-J
           PERFORM UNTIL WS-J >= WS-K
                      OR WS-BUILD(WS-J:1) NOT = "0"
               ADD 1 TO WS-J
           END-PERFORM
           IF WS-J > WS-FIRST-DIG
               MOVE SPACES TO WS-TAIL
               COMPUTE WS-LEN = WS-K - WS-J + 1
               MOVE WS-BUILD(WS-J:WS-LEN) TO WS-TAIL
               MOVE SPACES TO WS-BUILD(WS-FIRST-DIG:)
               MOVE WS-TAIL(1:WS-LEN) TO WS-BUILD(WS-FIRST-DIG:WS-LEN)
               COMPUTE WS-K = WS-FIRST-DIG + WS-LEN - 1
           END-IF.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> FORMATTING (COM-024 - money and units are always explicit)
      *>----------------------------------------------------------------
       3000-FORMAT-MONEY SECTION.
       3000-START.
           MOVE SU-AMOUNT TO WS-ED-MONEY
           MOVE SPACES TO SU-TEXT-OUT
           IF SU-SYMBOL = "Y" AND SU-CURRENCY = "GBP"
      *>       X"A3" is the pound sign in ISO-8859-1 / Windows-1252
               STRING STM-POUND FUNCTION TRIM(WS-ED-MONEY)
                   DELIMITED BY SIZE INTO SU-TEXT-OUT
           ELSE
               STRING FUNCTION TRIM(SU-CURRENCY) " "
                      FUNCTION TRIM(WS-ED-MONEY)
                   DELIMITED BY SIZE INTO SU-TEXT-OUT
           END-IF
           PERFORM 8000-OUT-LENGTH
           MOVE "Amount formatted" TO CA-MESSAGE.
       3000-EXIT.
           EXIT.

      *>   D-09 - quantities carry six decimals; trailing zeros are
      *>   noise on a screen or a report but must never be rounded away
       3100-FORMAT-QTY SECTION.
       3100-START.
           MOVE SU-AMOUNT TO WS-ED-QTY
           MOVE SPACES TO WS-BUILD
           MOVE FUNCTION TRIM(WS-ED-QTY) TO WS-BUILD
           IF SU-ZERO-STRIP
               PERFORM 3200-TRIM-DECIMALS
           END-IF
           MOVE WS-BUILD TO SU-TEXT-OUT
           PERFORM 8000-OUT-LENGTH
           MOVE "Quantity formatted" TO CA-MESSAGE.
       3100-EXIT.
           EXIT.

       3200-TRIM-DECIMALS SECTION.
       3200-START.
           MOVE 0 TO WS-LEN
           PERFORM VARYING WS-I FROM 200 BY -1
               UNTIL WS-I < 1 OR WS-LEN > 0
               IF WS-BUILD(WS-I:1) NOT = SPACE
                   MOVE WS-I TO WS-LEN
               END-IF
           END-PERFORM
           MOVE 0 TO WS-POS
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-LEN OR WS-POS > 0
               IF WS-BUILD(WS-I:1) = "."
                   MOVE WS-I TO WS-POS
               END-IF
           END-PERFORM
           IF WS-POS = 0
               GO TO 3200-EXIT
           END-IF
           PERFORM UNTIL WS-LEN <= WS-POS
                      OR WS-BUILD(WS-LEN:1) NOT = "0"
               MOVE SPACE TO WS-BUILD(WS-LEN:1)
               SUBTRACT 1 FROM WS-LEN
           END-PERFORM
           IF WS-LEN = WS-POS
               MOVE SPACE TO WS-BUILD(WS-LEN:1)
           END-IF.
       3200-EXIT.
           EXIT.

       4000-SQUEEZE SECTION.
       4000-START.
           MOVE SPACES TO WS-BUILD
           MOVE 0 TO WS-K
           MOVE SPACE TO WS-PREV
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 200
               MOVE SU-TEXT-IN(WS-I:1) TO WS-CH
               IF WS-CH = SPACE OR WS-CH = LOW-VALUE
                   IF WS-K > 0 AND WS-PREV NOT = SPACE
                       ADD 1 TO WS-K
                       MOVE SPACE TO WS-BUILD(WS-K:1)
                       MOVE SPACE TO WS-PREV
                   END-IF
               ELSE
                   ADD 1 TO WS-K
                   MOVE WS-CH TO WS-BUILD(WS-K:1)
                   MOVE WS-CH TO WS-PREV
               END-IF
           END-PERFORM
           IF WS-K > 0 AND WS-BUILD(WS-K:1) = SPACE
               MOVE SPACE TO WS-BUILD(WS-K:1)
           END-IF
           MOVE WS-BUILD TO SU-TEXT-OUT
           PERFORM 8000-OUT-LENGTH
           IF SU-TEXT-OUT NOT = SU-TEXT-IN
               MOVE "Y" TO SU-CHANGED
           END-IF
           MOVE "Spaces squeezed" TO CA-MESSAGE.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-OUT-LENGTH SECTION.
       8000-START.
           MOVE 0 TO SU-LENGTH-OUT
           PERFORM VARYING WS-I FROM 200 BY -1
               UNTIL WS-I < 1 OR SU-LENGTH-OUT > 0
               IF SU-TEXT-OUT(WS-I:1) NOT = SPACE
                  AND SU-TEXT-OUT(WS-I:1) NOT = LOW-VALUE
                   MOVE WS-I TO SU-LENGTH-OUT
               END-IF
           END-PERFORM.
       8000-EXIT.
           EXIT.

       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE "N" TO SU-VALID
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "STR0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "STRUTIL does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
