      *>================================================================
      *> TESTKITW - STERP COBOL unit test kit, WORKING-STORAGE part
      *> Usage: COPY TESTKITW. in WORKING-STORAGE,
      *>        COPY TESTKITP. at the END of the PROCEDURE DIVISION.
      *>   MOVE "suite" TO TK-SUITE  PERFORM TK-BEGIN
      *>   MOVE "test name" TO TK-NAME
      *>   IF condition PERFORM TK-PASS ELSE PERFORM TK-FAIL END-IF
      *>   (optional MOVE text TO TK-DETAIL before TK-FAIL)
      *>   MOVE reason TO TK-DETAIL  PERFORM TK-SKIP  (TAP "# SKIP",
      *>        counts as ok - use when a prerequisite is not deployed)
      *>   PERFORM TK-ASSERT-NUM with TK-EXPECTED-N / TK-ACTUAL-N
      *>   PERFORM TK-ASSERT-STR with TK-EXPECTED-X / TK-ACTUAL-X
      *>   PERFORM TK-END   (sets RETURN-CODE = failures)
      *>================================================================
       01  TK-AREA.
           05  TK-SUITE                PIC X(30) VALUE SPACES.
           05  TK-NAME                 PIC X(100) VALUE SPACES.
           05  TK-DETAIL               PIC X(200) VALUE SPACES.
           05  TK-COUNT                PIC 9(5) VALUE 0.
           05  TK-PASSED               PIC 9(5) VALUE 0.
           05  TK-FAILED               PIC 9(5) VALUE 0.
           05  TK-COUNT-ED             PIC Z(4)9.
           05  TK-EXPECTED-N           PIC S9(13)V9(6) VALUE 0.
           05  TK-ACTUAL-N             PIC S9(13)V9(6) VALUE 0.
           05  TK-EXPECTED-X           PIC X(100) VALUE SPACES.
           05  TK-ACTUAL-X             PIC X(100) VALUE SPACES.
           05  TK-NUM-ED               PIC -(13)9.9(6).
