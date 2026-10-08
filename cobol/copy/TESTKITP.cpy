      *>================================================================
      *> TESTKITP - STERP COBOL unit test kit, PROCEDURE part
      *> Must be the last thing in the PROCEDURE DIVISION.
      *>================================================================
       TK-BEGIN.
           MOVE 0 TO TK-COUNT TK-PASSED TK-FAILED
           DISPLAY "# suite " FUNCTION TRIM(TK-SUITE)
               UPON SYSOUT.
       TK-PASS.
           ADD 1 TO TK-COUNT TK-PASSED
           MOVE TK-COUNT TO TK-COUNT-ED
           DISPLAY "ok " FUNCTION TRIM(TK-COUNT-ED) " - "
                   FUNCTION TRIM(TK-NAME) UPON SYSOUT
           MOVE SPACES TO TK-DETAIL.
       TK-FAIL.
           ADD 1 TO TK-COUNT TK-FAILED
           MOVE TK-COUNT TO TK-COUNT-ED
           DISPLAY "not ok " FUNCTION TRIM(TK-COUNT-ED) " - "
                   FUNCTION TRIM(TK-NAME) UPON SYSOUT
           IF TK-DETAIL NOT = SPACES
               DISPLAY "#   " FUNCTION TRIM(TK-DETAIL)
                   UPON SYSOUT
           END-IF
           MOVE SPACES TO TK-DETAIL.
       TK-SKIP.
           ADD 1 TO TK-COUNT TK-PASSED
           MOVE TK-COUNT TO TK-COUNT-ED
           DISPLAY "ok " FUNCTION TRIM(TK-COUNT-ED) " - "
                   FUNCTION TRIM(TK-NAME) " # SKIP "
                   FUNCTION TRIM(TK-DETAIL) UPON SYSOUT
           MOVE SPACES TO TK-DETAIL.
       TK-ASSERT-NUM.
           IF TK-ACTUAL-N = TK-EXPECTED-N
               PERFORM TK-PASS
           ELSE
               MOVE SPACES TO TK-DETAIL
               MOVE TK-EXPECTED-N TO TK-NUM-ED
               STRING "expected " FUNCTION TRIM(TK-NUM-ED)
                   DELIMITED BY SIZE INTO TK-DETAIL
               MOVE TK-ACTUAL-N TO TK-NUM-ED
               STRING FUNCTION TRIM(TK-DETAIL) " got "
                   FUNCTION TRIM(TK-NUM-ED)
                   DELIMITED BY SIZE INTO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.
       TK-ASSERT-STR.
           IF TK-ACTUAL-X = TK-EXPECTED-X
               PERFORM TK-PASS
           ELSE
               MOVE SPACES TO TK-DETAIL
               STRING "expected [" FUNCTION TRIM(TK-EXPECTED-X)
                   "] got [" FUNCTION TRIM(TK-ACTUAL-X) "]"
                   DELIMITED BY SIZE INTO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.
       TK-END.
           MOVE TK-COUNT TO TK-COUNT-ED
           DISPLAY "1.." FUNCTION TRIM(TK-COUNT-ED)
               UPON SYSOUT
           DISPLAY "# " FUNCTION TRIM(TK-SUITE) " passed " TK-PASSED
                   " failed " TK-FAILED UPON SYSOUT
           MOVE TK-FAILED TO RETURN-CODE.
