      *>================================================================
      *> TRNDALL - unit tests for RNDALLOC (rounding and allocation)
      *>           and for the STFUNCS user-defined function library.
      *> Worked numbers come from the acceptance catalogue:
      *>   AC-006  round 1.005 and -1.005 to two decimals -> 1.01/-1.01
      *>           allocate 1.00 over three lines -> 0.34/0.33/0.33 and
      *>           the negatives, totalling the source exactly
      *>           six decimal unit price before final rounding
      *> Output protocol (TAP): "ok n - text" / "not ok n - text".
      *>================================================================
      *> REQ: COM-011   AC: AC-006
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRNDALL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ROUND-AWAY
           FUNCTION SIGN-OF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY STFNARGS.
       01  WS-I                        PIC S9(9) COMP-5 VALUE 0.
       01  WS-TOTAL                    PIC S9(13)V9(6) VALUE 0.
       01  WS-OK                       PIC X VALUE "Y".
       COPY RNDALLOCCA REPLACING ==DFHCOMMAREA== BY ==WS-RND-CA==.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TRNDALL" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-ROUNDING-TESTS
           PERFORM 2000-PRECISION-TESTS
           PERFORM 3000-ALLOCATION-TESTS
           PERFORM 4000-ERROR-TESTS
           PERFORM 5000-FUNCTION-TESTS
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
       1000-ROUNDING-TESTS.
           MOVE "AC-006 round 1.005 to 2 dp gives 1.01" TO TK-NAME
           MOVE 1.005 TO RA-VALUE OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           PERFORM 8000-CALL-ROUND
           MOVE 1.01 TO TK-EXPECTED-N
           MOVE RA-RESULT OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-006 round -1.005 to 2 dp gives -1.01" TO TK-NAME
           MOVE -1.005 TO RA-VALUE OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           PERFORM 8000-CALL-ROUND
           MOVE -1.01 TO TK-EXPECTED-N
           MOVE RA-RESULT OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "round 2.675 to 2 dp gives 2.68 (decimal, not binary)"
               TO TK-NAME
           MOVE 2.675 TO RA-VALUE OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           PERFORM 8000-CALL-ROUND
           MOVE 2.68 TO TK-EXPECTED-N
           MOVE RA-RESULT OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "round 1.0049 to 2 dp stays 1.00" TO TK-NAME
           MOVE 1.0049 TO RA-VALUE OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           PERFORM 8000-CALL-ROUND
           MOVE 1.00 TO TK-EXPECTED-N
           MOVE RA-RESULT OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "round 0.5 to 0 dp gives 1 (half away from zero)"
               TO TK-NAME
           MOVE 0.5 TO RA-VALUE OF WS-RND-CA
           MOVE 0 TO RA-PLACES OF WS-RND-CA
           PERFORM 8000-CALL-ROUND
           MOVE 1 TO TK-EXPECTED-N
           MOVE RA-RESULT OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "round -0.5 to 0 dp gives -1" TO TK-NAME
           MOVE -0.5 TO RA-VALUE OF WS-RND-CA
           MOVE 0 TO RA-PLACES OF WS-RND-CA
           PERFORM 8000-CALL-ROUND
           MOVE -1 TO TK-EXPECTED-N
           MOVE RA-RESULT OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "round -2.5 to 0 dp gives -3, not banker's -2"
               TO TK-NAME
           MOVE -2.5 TO RA-VALUE OF WS-RND-CA
           MOVE 0 TO RA-PLACES OF WS-RND-CA
           PERFORM 8000-CALL-ROUND
           MOVE -3 TO TK-EXPECTED-N
           MOVE RA-RESULT OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "round 0.0000005 to 6 dp gives 0.000001" TO TK-NAME
           MOVE 0.0000005 TO RA-VALUE OF WS-RND-CA
           MOVE 6 TO RA-PLACES OF WS-RND-CA
           PERFORM 8000-CALL-ROUND
           MOVE 0.000001 TO TK-EXPECTED-N
           MOVE RA-RESULT OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "truncate 1.009 to 2 dp gives 1.00" TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "TRUNC" TO CA-FUNCTION OF WS-RND-CA
           MOVE 1.009 TO RA-VALUE OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           MOVE 1.00 TO TK-EXPECTED-N
           MOVE RA-RESULT OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "truncate -1.009 to 2 dp gives -1.00 (toward zero)"
               TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "TRUNC" TO CA-FUNCTION OF WS-RND-CA
           MOVE -1.009 TO RA-VALUE OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           MOVE -1.00 TO TK-EXPECTED-N
           MOVE RA-RESULT OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> AC-006 "calculate a six decimal unit price before final
      *> currency rounding - no premature unit price rounding changes
      *> the required final total".  GBP 100.00 over 7 units is
      *> 14.285714 each; 7 x 14.285714 = 99.999998 which rounds back to
      *> 100.00, whereas 7 x 14.29 would give 100.03.
      *>----------------------------------------------------------------
       2000-PRECISION-TESTS.
           MOVE "AC-006 100.00 over 7 gives unit price 14.285714"
               TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "UNITPRC" TO CA-FUNCTION OF WS-RND-CA
           MOVE 100.00 TO RA-VALUE OF WS-RND-CA
           MOVE 7 TO RA-VALUE2 OF WS-RND-CA
           MOVE 6 TO RA-PLACES OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           MOVE 14.285714 TO TK-EXPECTED-N
           MOVE RA-RESULT OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-006 7 x 14.285714 rounds back to 100.00"
               TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "EXTEND" TO CA-FUNCTION OF WS-RND-CA
           MOVE 7 TO RA-VALUE OF WS-RND-CA
           MOVE 14.285714 TO RA-VALUE2 OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE RA-RESULT OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-006 extension keeps the unrounded working value"
               TO TK-NAME
           MOVE 99.999998 TO TK-EXPECTED-N
           MOVE RA-UNROUNDED OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "prematurely rounded price 14.29 would give 100.03"
               TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "EXTEND" TO CA-FUNCTION OF WS-RND-CA
           MOVE 7 TO RA-VALUE OF WS-RND-CA
           MOVE 14.29 TO RA-VALUE2 OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           MOVE 100.03 TO TK-EXPECTED-N
           MOVE RA-RESULT OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "unit price 100.00 over 3 is 33.333333 at 6 dp"
               TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "UNITPRC" TO CA-FUNCTION OF WS-RND-CA
           MOVE 100.00 TO RA-VALUE OF WS-RND-CA
           MOVE 3 TO RA-VALUE2 OF WS-RND-CA
           MOVE 6 TO RA-PLACES OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           MOVE 33.333333 TO TK-EXPECTED-N
           MOVE RA-RESULT OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
       3000-ALLOCATION-TESTS.
           MOVE "AC-006 allocate 1.00 equally over 3 lines" TO TK-NAME
           MOVE 1.00 TO RA-AMOUNT OF WS-RND-CA
           MOVE 3 TO RA-LINE-CNT OF WS-RND-CA
           PERFORM 8100-CALL-ALLOC-EQUAL
           IF RA-PART OF WS-RND-CA(1) = 0.34
              AND RA-PART OF WS-RND-CA(2) = 0.33
              AND RA-PART OF WS-RND-CA(3) = 0.33
               PERFORM TK-PASS
           ELSE
               PERFORM 8900-SHOW-PARTS
               PERFORM TK-FAIL
           END-IF

           MOVE "AC-006 the three parts total 1.00 exactly" TO TK-NAME
           MOVE 1.00 TO TK-EXPECTED-N
           MOVE RA-ALLOCATED OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "allocation residual is zero" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE RA-RESIDUAL OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "exactly one line takes the spare penny" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE RA-BUMPED OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-006 allocate -1.00 equally over 3 lines" TO TK-NAME
           MOVE -1.00 TO RA-AMOUNT OF WS-RND-CA
           MOVE 3 TO RA-LINE-CNT OF WS-RND-CA
           PERFORM 8100-CALL-ALLOC-EQUAL
           IF RA-PART OF WS-RND-CA(1) = -0.34
              AND RA-PART OF WS-RND-CA(2) = -0.33
              AND RA-PART OF WS-RND-CA(3) = -0.33
               PERFORM TK-PASS
           ELSE
               PERFORM 8900-SHOW-PARTS
               PERFORM TK-FAIL
           END-IF

           MOVE "AC-006 the negative parts total -1.00 exactly"
               TO TK-NAME
           MOVE -1.00 TO TK-EXPECTED-N
           MOVE RA-ALLOCATED OF WS-RND-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "allocate 100.00 equally over 3 gives 33.34/33.33/33.33"
               TO TK-NAME
           MOVE 100.00 TO RA-AMOUNT OF WS-RND-CA
           MOVE 3 TO RA-LINE-CNT OF WS-RND-CA
           PERFORM 8100-CALL-ALLOC-EQUAL
           IF RA-PART OF WS-RND-CA(1) = 33.34
              AND RA-PART OF WS-RND-CA(2) = 33.33
              AND RA-PART OF WS-RND-CA(3) = 33.33
              AND RA-ALLOCATED OF WS-RND-CA = 100.00
               PERFORM TK-PASS
           ELSE
               PERFORM 8900-SHOW-PARTS
               PERFORM TK-FAIL
           END-IF

           MOVE "allocate 0.01 over 3 gives 0.01/0.00/0.00" TO TK-NAME
           MOVE 0.01 TO RA-AMOUNT OF WS-RND-CA
           MOVE 3 TO RA-LINE-CNT OF WS-RND-CA
           PERFORM 8100-CALL-ALLOC-EQUAL
           IF RA-PART OF WS-RND-CA(1) = 0.01
              AND RA-PART OF WS-RND-CA(2) = 0
              AND RA-PART OF WS-RND-CA(3) = 0
               PERFORM TK-PASS
           ELSE
               PERFORM 8900-SHOW-PARTS
               PERFORM TK-FAIL
           END-IF

           MOVE "weighted allocation 10.00 over 1:2:3" TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "ALLOC" TO CA-FUNCTION OF WS-RND-CA
           MOVE "WEIGHT" TO RA-MODE OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           MOVE 10.00 TO RA-AMOUNT OF WS-RND-CA
           MOVE 3 TO RA-LINE-CNT OF WS-RND-CA
           MOVE 1 TO RA-WEIGHT OF WS-RND-CA(1)
           MOVE 2 TO RA-WEIGHT OF WS-RND-CA(2)
           MOVE 3 TO RA-WEIGHT OF WS-RND-CA(3)
           CALL "RNDALLOC" USING WS-RND-CA
           IF RA-PART OF WS-RND-CA(1) = 1.67
              AND RA-PART OF WS-RND-CA(2) = 3.33
              AND RA-PART OF WS-RND-CA(3) = 5.00
              AND RA-ALLOCATED OF WS-RND-CA = 10.00
               PERFORM TK-PASS
           ELSE
               PERFORM 8900-SHOW-PARTS
               PERFORM TK-FAIL
           END-IF

      *>   realistic settlement discount of GBP 50.00 spread over three
      *>   invoice lines of 120.00, 80.00 and 55.55 (largest remainder)
           MOVE "document discount 50.00 over 120.00/80.00/55.55"
               TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "ALLOC" TO CA-FUNCTION OF WS-RND-CA
           MOVE "WEIGHT" TO RA-MODE OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           MOVE 50.00 TO RA-AMOUNT OF WS-RND-CA
           MOVE 3 TO RA-LINE-CNT OF WS-RND-CA
           MOVE 120.00 TO RA-WEIGHT OF WS-RND-CA(1)
           MOVE 80.00 TO RA-WEIGHT OF WS-RND-CA(2)
           MOVE 55.55 TO RA-WEIGHT OF WS-RND-CA(3)
           CALL "RNDALLOC" USING WS-RND-CA
           IF RA-PART OF WS-RND-CA(1) = 23.48
              AND RA-PART OF WS-RND-CA(2) = 15.65
              AND RA-PART OF WS-RND-CA(3) = 10.87
              AND RA-ALLOCATED OF WS-RND-CA = 50.00
               PERFORM TK-PASS
           ELSE
               PERFORM 8900-SHOW-PARTS
               PERFORM TK-FAIL
           END-IF

           MOVE "zero weight line receives nothing" TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "ALLOC" TO CA-FUNCTION OF WS-RND-CA
           MOVE "WEIGHT" TO RA-MODE OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           MOVE 9.99 TO RA-AMOUNT OF WS-RND-CA
           MOVE 3 TO RA-LINE-CNT OF WS-RND-CA
           MOVE 1 TO RA-WEIGHT OF WS-RND-CA(1)
           MOVE 0 TO RA-WEIGHT OF WS-RND-CA(2)
           MOVE 1 TO RA-WEIGHT OF WS-RND-CA(3)
           CALL "RNDALLOC" USING WS-RND-CA
           IF RA-PART OF WS-RND-CA(2) = 0
              AND RA-ALLOCATED OF WS-RND-CA = 9.99
               PERFORM TK-PASS
           ELSE
               PERFORM 8900-SHOW-PARTS
               PERFORM TK-FAIL
           END-IF

      *>   the 500 line limit, with a total that does not divide evenly
           MOVE "allocate 100.00 over the full 500 line table"
               TO TK-NAME
           MOVE 100.00 TO RA-AMOUNT OF WS-RND-CA
           MOVE 500 TO RA-LINE-CNT OF WS-RND-CA
           PERFORM 8100-CALL-ALLOC-EQUAL
           MOVE 0 TO WS-TOTAL
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 500
               ADD RA-PART OF WS-RND-CA(WS-I) TO WS-TOTAL
           END-PERFORM
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE WS-TOTAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "allocate 1.23 over 500 lines still totals 1.23"
               TO TK-NAME
           MOVE 1.23 TO RA-AMOUNT OF WS-RND-CA
           MOVE 500 TO RA-LINE-CNT OF WS-RND-CA
           PERFORM 8100-CALL-ALLOC-EQUAL
           MOVE 0 TO WS-TOTAL
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 500
               ADD RA-PART OF WS-RND-CA(WS-I) TO WS-TOTAL
           END-PERFORM
           MOVE 1.23 TO TK-EXPECTED-N
           MOVE WS-TOTAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "six decimal quantity allocation (D-09)" TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "ALLOC" TO CA-FUNCTION OF WS-RND-CA
           MOVE "EQUAL" TO RA-MODE OF WS-RND-CA
           MOVE 6 TO RA-PLACES OF WS-RND-CA
           MOVE 1.000000 TO RA-AMOUNT OF WS-RND-CA
           MOVE 3 TO RA-LINE-CNT OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           IF RA-PART OF WS-RND-CA(1) = 0.333334
              AND RA-PART OF WS-RND-CA(2) = 0.333333
              AND RA-PART OF WS-RND-CA(3) = 0.333333
               PERFORM TK-PASS
           ELSE
               PERFORM 8900-SHOW-PARTS
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> COM-017 - an online service must answer, never fall over
      *>----------------------------------------------------------------
       4000-ERROR-TESTS.
           MOVE "too many decimal places is rejected with RND0001"
               TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "ROUND" TO CA-FUNCTION OF WS-RND-CA
           MOVE 9 TO RA-PLACES OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           IF CA-RETURN-CODE OF WS-RND-CA = 8
              AND CA-REASON-CODE OF WS-RND-CA = "RND0001"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "zero line count is rejected with RND0002" TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "ALLOC" TO CA-FUNCTION OF WS-RND-CA
           MOVE "EQUAL" TO RA-MODE OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           MOVE 0 TO RA-LINE-CNT OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           IF CA-RETURN-CODE OF WS-RND-CA = 8
              AND CA-REASON-CODE OF WS-RND-CA = "RND0002"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "unknown allocation mode is rejected with RND0003"
               TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "ALLOC" TO CA-FUNCTION OF WS-RND-CA
           MOVE "PRORATA" TO RA-MODE OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           MOVE 2 TO RA-LINE-CNT OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           IF CA-RETURN-CODE OF WS-RND-CA = 8
              AND CA-REASON-CODE OF WS-RND-CA = "RND0003"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "negative weight is rejected with RND0004" TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "ALLOC" TO CA-FUNCTION OF WS-RND-CA
           MOVE "WEIGHT" TO RA-MODE OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           MOVE 10.00 TO RA-AMOUNT OF WS-RND-CA
           MOVE 2 TO RA-LINE-CNT OF WS-RND-CA
           MOVE -1 TO RA-WEIGHT OF WS-RND-CA(1)
           MOVE 3 TO RA-WEIGHT OF WS-RND-CA(2)
           CALL "RNDALLOC" USING WS-RND-CA
           IF CA-RETURN-CODE OF WS-RND-CA = 8
              AND CA-REASON-CODE OF WS-RND-CA = "RND0004"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "all weights zero is rejected with RND0005" TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "ALLOC" TO CA-FUNCTION OF WS-RND-CA
           MOVE "WEIGHT" TO RA-MODE OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           MOVE 10.00 TO RA-AMOUNT OF WS-RND-CA
           MOVE 2 TO RA-LINE-CNT OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           IF CA-RETURN-CODE OF WS-RND-CA = 8
              AND CA-REASON-CODE OF WS-RND-CA = "RND0005"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "zero divisor is not treated as one (RND0006)"
               TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "UNITPRC" TO CA-FUNCTION OF WS-RND-CA
           MOVE 100.00 TO RA-VALUE OF WS-RND-CA
           MOVE 0 TO RA-VALUE2 OF WS-RND-CA
           MOVE 6 TO RA-PLACES OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           IF CA-RETURN-CODE OF WS-RND-CA = 8
              AND CA-REASON-CODE OF WS-RND-CA = "RND0006"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "unknown function is rejected with RND0008" TO TK-NAME
           INITIALIZE WS-RND-CA
           MOVE "SPLIT" TO CA-FUNCTION OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           IF CA-RETURN-CODE OF WS-RND-CA = 8
              AND CA-REASON-CODE OF WS-RND-CA = "RND0008"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> The STFUNCS library itself, called the way STFNARGS requires.
      *>----------------------------------------------------------------
       5000-FUNCTION-TESTS.
           MOVE "FUNCTION ROUND-AWAY links under -fstatic-call"
               TO TK-NAME
           MOVE 1.005 TO FNA-VALUE
           MOVE 2 TO FNA-PLACES
           COMPUTE FNA-RESULT =
               FUNCTION ROUND-AWAY(FNA-VALUE, FNA-PLACES)
           MOVE 1.01 TO TK-EXPECTED-N
           MOVE FNA-RESULT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "FUNCTION SIGN-OF returns -1 / 0 / +1" TO TK-NAME
           MOVE "Y" TO WS-OK
           MOVE -0.000001 TO FNA-VALUE
           IF FUNCTION SIGN-OF(FNA-VALUE) NOT = -1
               MOVE "N" TO WS-OK
           END-IF
           MOVE 0 TO FNA-VALUE
           IF FUNCTION SIGN-OF(FNA-VALUE) NOT = 0
               MOVE "N" TO WS-OK
           END-IF
           MOVE 12345.678 TO FNA-VALUE
           IF FUNCTION SIGN-OF(FNA-VALUE) NOT = 1
               MOVE "N" TO WS-OK
           END-IF
           IF WS-OK = "Y" PERFORM TK-PASS ELSE PERFORM TK-FAIL END-IF.

      *>----------------------------------------------------------------
      *> HELPERS
      *>----------------------------------------------------------------
       8000-CALL-ROUND.
           MOVE "ROUND" TO CA-FUNCTION OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA.

       8100-CALL-ALLOC-EQUAL.
           MOVE "ALLOC" TO CA-FUNCTION OF WS-RND-CA
           MOVE "EQUAL" TO RA-MODE OF WS-RND-CA
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           MOVE 0 TO CA-RETURN-CODE OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA.

       8900-SHOW-PARTS.
           MOVE SPACES TO TK-DETAIL
           MOVE RA-PART OF WS-RND-CA(1) TO TK-NUM-ED
           STRING "part1=" FUNCTION TRIM(TK-NUM-ED)
               DELIMITED BY SIZE INTO TK-DETAIL
           MOVE RA-PART OF WS-RND-CA(2) TO TK-NUM-ED
           STRING FUNCTION TRIM(TK-DETAIL) " part2="
                  FUNCTION TRIM(TK-NUM-ED)
               DELIMITED BY SIZE INTO TK-DETAIL
           MOVE RA-PART OF WS-RND-CA(3) TO TK-NUM-ED
           STRING FUNCTION TRIM(TK-DETAIL) " part3="
                  FUNCTION TRIM(TK-NUM-ED)
               DELIMITED BY SIZE INTO TK-DETAIL
           MOVE RA-ALLOCATED OF WS-RND-CA TO TK-NUM-ED
           STRING FUNCTION TRIM(TK-DETAIL) " total="
                  FUNCTION TRIM(TK-NUM-ED)
               DELIMITED BY SIZE INTO TK-DETAIL.

       COPY TESTKITP.
