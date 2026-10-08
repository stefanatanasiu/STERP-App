      *>================================================================
      *> TSTRUTIL - unit tests for STRUTIL (codes, references, money)
      *> D-21 allows A-Z 0-9 and - . / _ in a business code, up to 40
      *> characters, unique without regard to case and stored upper.
      *> COM-004 requires that similar identifiers do not silently
      *> merge, which is what reference normalisation is for.
      *> COM-024 requires money and units to be explicit.
      *>================================================================
      *> REQ: COM-004, COM-024   AC: AC-003
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSTRUTIL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       01  WS-POUND                    PIC X VALUE X"A3".
       01  WS-EXPECT-SYM               PIC X(20) VALUE SPACES.
       COPY STRUTILCA REPLACING ==DFHCOMMAREA== BY ==WS-SU-CA==.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSTRUTIL" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-CODE-TESTS
           PERFORM 2000-REFERENCE-TESTS
           PERFORM 3000-MONEY-TESTS
           PERFORM 4000-QUANTITY-TESTS
           PERFORM 5000-MISC-TESTS
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
       1000-CODE-TESTS.
           MOVE "D-21 a lower case item code is stored upper case"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "brk-disc/fr.01" TO SU-TEXT-IN OF WS-SU-CA
           PERFORM 8100-CALL-CODE
           MOVE "BRK-DISC/FR.01" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "folding to upper case is reported as a change"
               TO TK-NAME
           IF SU-CHANGED OF WS-SU-CA = "Y"
              AND SU-VALID OF WS-SU-CA = "Y"
              AND CA-RETURN-CODE OF WS-SU-CA = 4
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "D-21 underscore and full stop are allowed" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "ASSY_900.A/2-B" TO SU-TEXT-IN OF WS-SU-CA
           PERFORM 8100-CALL-CODE
           IF SU-VALID OF WS-SU-CA = "Y"
              AND CA-RETURN-CODE OF WS-SU-CA = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SU-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "D-21 a space inside a code is rejected" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "BRK DISC" TO SU-TEXT-IN OF WS-SU-CA
           PERFORM 8100-CALL-CODE
           IF SU-VALID OF WS-SU-CA = "N"
              AND CA-REASON-CODE OF WS-SU-CA = "STR0003"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "D-21 an ampersand is rejected and located" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "AB&CD" TO SU-TEXT-IN OF WS-SU-CA
           PERFORM 8100-CALL-CODE
           IF SU-VALID OF WS-SU-CA = "N"
              AND SU-BAD-CHAR OF WS-SU-CA = "&"
              AND SU-BAD-POS OF WS-SU-CA = 3
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SU-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "D-21 forty characters are allowed" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "ABCDEFGHIJABCDEFGHIJABCDEFGHIJABCDEFGHIJ"
               TO SU-TEXT-IN OF WS-SU-CA
           PERFORM 8100-CALL-CODE
           IF SU-VALID OF WS-SU-CA = "Y"
              AND SU-LENGTH-OUT OF WS-SU-CA = 40
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "D-21 forty one characters are rejected" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "ABCDEFGHIJABCDEFGHIJABCDEFGHIJABCDEFGHIJX"
               TO SU-TEXT-IN OF WS-SU-CA
           PERFORM 8100-CALL-CODE
           IF SU-VALID OF WS-SU-CA = "N"
              AND CA-REASON-CODE OF WS-SU-CA = "STR0002"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "a shorter limit can be imposed (20 for parties)"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE 20 TO SU-MAX-LEN OF WS-SU-CA
           MOVE "ABCDEFGHIJABCDEFGHIJX" TO SU-TEXT-IN OF WS-SU-CA
           PERFORM 8100-CALL-CODE
           IF SU-VALID OF WS-SU-CA = "N"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "an empty code is rejected" TO TK-NAME
           PERFORM 8000-RESET
           MOVE SPACES TO SU-TEXT-IN OF WS-SU-CA
           PERFORM 8100-CALL-CODE
           IF SU-VALID OF WS-SU-CA = "N"
              AND CA-REASON-CODE OF WS-SU-CA = "STR0001"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> COM-004 - similar references must not silently merge, so the
      *> normal form is what duplicate detection compares
      *>----------------------------------------------------------------
       2000-REFERENCE-TESTS.
           MOVE "SI-000123 normalises to SI123 under STRIP" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "STRIP" TO SU-ZERO-POLICY OF WS-SU-CA
           MOVE "SI-000123" TO SU-TEXT-IN OF WS-SU-CA
           PERFORM 8200-CALL-DOCREF
           MOVE "SI123" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "si 000123 normalises to the same thing" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "STRIP" TO SU-ZERO-POLICY OF WS-SU-CA
           MOVE "si 000123" TO SU-TEXT-IN OF WS-SU-CA
           PERFORM 8200-CALL-DOCREF
           MOVE "SI123" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SI/000123 normalises to the same thing" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "STRIP" TO SU-ZERO-POLICY OF WS-SU-CA
           MOVE "SI/000123" TO SU-TEXT-IN OF WS-SU-CA
           PERFORM 8200-CALL-DOCREF
           MOVE "SI123" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "KEEP leaves the leading zeros in place" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "KEEP" TO SU-ZERO-POLICY OF WS-SU-CA
           MOVE "SI-000123" TO SU-TEXT-IN OF WS-SU-CA
           PERFORM 8200-CALL-DOCREF
           MOVE "SI000123" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "an all zero number keeps one digit" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "STRIP" TO SU-ZERO-POLICY OF WS-SU-CA
           MOVE "SI-0000" TO SU-TEXT-IN OF WS-SU-CA
           PERFORM 8200-CALL-DOCREF
           MOVE "SI0" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "a purely numeric reference still normalises"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "STRIP" TO SU-ZERO-POLICY OF WS-SU-CA
           MOVE "0001234" TO SU-TEXT-IN OF WS-SU-CA
           PERFORM 8200-CALL-DOCREF
           MOVE "1234" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "COM-004 SI123 and SI1234 stay different" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "STRIP" TO SU-ZERO-POLICY OF WS-SU-CA
           MOVE "SI-0001234" TO SU-TEXT-IN OF WS-SU-CA
           PERFORM 8200-CALL-DOCREF
           MOVE "SI1234" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "a reference with no letters or digits is rejected"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "---" TO SU-TEXT-IN OF WS-SU-CA
           PERFORM 8200-CALL-DOCREF
           IF CA-RETURN-CODE OF WS-SU-CA = 8
              AND CA-REASON-CODE OF WS-SU-CA = "STR0005"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> COM-024 - money is shown with an explicit currency.  The
      *> default is the text GBP because the pound sign is two bytes in
      *> UTF-8 and would shift fixed width columns; X"A3" is produced
      *> only when the caller asks for the symbol.
      *>----------------------------------------------------------------
       3000-MONEY-TESTS.
           MOVE "money defaults to GBP 1,234.56" TO TK-NAME
           PERFORM 8000-RESET
           MOVE 1234.56 TO SU-AMOUNT OF WS-SU-CA
           PERFORM 8300-CALL-MONEY
           MOVE "GBP 1,234.56" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "the pound symbol is a single X'A3' byte on request"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE 1234.56 TO SU-AMOUNT OF WS-SU-CA
           MOVE "Y" TO SU-SYMBOL OF WS-SU-CA
           PERFORM 8300-CALL-MONEY
           MOVE SPACES TO WS-EXPECT-SYM
           STRING WS-POUND "1,234.56" DELIMITED BY SIZE
               INTO WS-EXPECT-SYM
           MOVE WS-EXPECT-SYM TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "the symbol form is nine characters long" TO TK-NAME
           MOVE 9 TO TK-EXPECTED-N
           MOVE SU-LENGTH-OUT OF WS-SU-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "a negative amount keeps its sign" TO TK-NAME
           PERFORM 8000-RESET
           MOVE -1250.00 TO SU-AMOUNT OF WS-SU-CA
           PERFORM 8300-CALL-MONEY
           MOVE "GBP -1,250.00" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "a foreign currency names itself" TO TK-NAME
           PERFORM 8000-RESET
           MOVE 1150.00 TO SU-AMOUNT OF WS-SU-CA
           MOVE "EUR" TO SU-CURRENCY OF WS-SU-CA
           MOVE "Y" TO SU-SYMBOL OF WS-SU-CA
           PERFORM 8300-CALL-MONEY
           MOVE "EUR 1,150.00" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "zero formats as GBP 0.00, not blank" TO TK-NAME
           PERFORM 8000-RESET
           MOVE 0 TO SU-AMOUNT OF WS-SU-CA
           PERFORM 8300-CALL-MONEY
           MOVE "GBP 0.00" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "millions are grouped correctly" TO TK-NAME
           PERFORM 8000-RESET
           MOVE 1234567.89 TO SU-AMOUNT OF WS-SU-CA
           PERFORM 8300-CALL-MONEY
           MOVE "GBP 1,234,567.89" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> D-09 - quantities carry six decimals
      *>----------------------------------------------------------------
       4000-QUANTITY-TESTS.
           MOVE "a quantity keeps six decimals by default" TO TK-NAME
           PERFORM 8000-RESET
           MOVE 12.5 TO SU-AMOUNT OF WS-SU-CA
           PERFORM 8400-CALL-QTY
           MOVE "12.500000" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "trailing zeros can be trimmed for display" TO TK-NAME
           PERFORM 8000-RESET
           MOVE 12.5 TO SU-AMOUNT OF WS-SU-CA
           MOVE "STRIP" TO SU-ZERO-POLICY OF WS-SU-CA
           PERFORM 8400-CALL-QTY
           MOVE "12.5" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "a whole quantity trims to no decimal point at all"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE 12 TO SU-AMOUNT OF WS-SU-CA
           MOVE "STRIP" TO SU-ZERO-POLICY OF WS-SU-CA
           PERFORM 8400-CALL-QTY
           MOVE "12" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "a six decimal quantity is never rounded away"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE 0.000001 TO SU-AMOUNT OF WS-SU-CA
           MOVE "STRIP" TO SU-ZERO-POLICY OF WS-SU-CA
           PERFORM 8400-CALL-QTY
           MOVE "0.000001" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
       5000-MISC-TESTS.
           MOVE "UPPER folds mixed case text" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "Stellar Components Ltd" TO SU-TEXT-IN OF WS-SU-CA
           MOVE "UPPER" TO CA-FUNCTION OF WS-SU-CA
           CALL "STRUTIL" USING WS-SU-CA
           MOVE "STELLAR COMPONENTS LTD" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SQUEEZE collapses runs of spaces" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "  Stellar    Components   Ltd  "
               TO SU-TEXT-IN OF WS-SU-CA
           MOVE "SQUEEZE" TO CA-FUNCTION OF WS-SU-CA
           CALL "STRUTIL" USING WS-SU-CA
           MOVE "Stellar Components Ltd" TO TK-EXPECTED-X
           MOVE SU-TEXT-OUT OF WS-SU-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "an unknown function is rejected (STR0009)" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "SOUNDEX" TO CA-FUNCTION OF WS-SU-CA
           CALL "STRUTIL" USING WS-SU-CA
           IF CA-RETURN-CODE OF WS-SU-CA = 8
              AND CA-REASON-CODE OF WS-SU-CA = "STR0009"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
       8000-RESET.
           INITIALIZE WS-SU-CA
           MOVE "STC" TO CA-COMPANY OF WS-SU-CA.

       8100-CALL-CODE.
           MOVE "CODE" TO CA-FUNCTION OF WS-SU-CA
           CALL "STRUTIL" USING WS-SU-CA.

       8200-CALL-DOCREF.
           MOVE "DOCREF" TO CA-FUNCTION OF WS-SU-CA
           CALL "STRUTIL" USING WS-SU-CA.

       8300-CALL-MONEY.
           MOVE "MONEY" TO CA-FUNCTION OF WS-SU-CA
           CALL "STRUTIL" USING WS-SU-CA.

       8400-CALL-QTY.
           MOVE "QTY" TO CA-FUNCTION OF WS-SU-CA
           CALL "STRUTIL" USING WS-SU-CA.

       COPY TESTKITP.
