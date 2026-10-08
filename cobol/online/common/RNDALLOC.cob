      *>================================================================
      *> PROGRAM-ID : RNDALLOC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : FINANCIAL ROUNDING AND ALLOCATION.  THE SINGLE
      *>              PLACE IN STERP WHERE A MONETARY VALUE IS ROUNDED
      *>              OR A TOTAL IS SPREAD OVER DOCUMENT LINES.
      *>              RULES - SPEC 01 SECTION 8 (COM-011):
      *>                * ROUND TO THE NEAREST MINOR UNIT, EXACT HALVES
      *>                  AWAY FROM ZERO          1.005 ->  1.01
      *>                                         -1.005 -> -1.01
      *>                * UNIT PRICES CARRY AT LEAST SIX DECIMALS AND
      *>                  WORKINGS AT LEAST TWELVE BEFORE A DEFINED
      *>                  ROUNDING BOUNDARY
      *>                * ALLOCATION USES PROPORTIONAL UNROUNDED SHARES,
      *>                  ALLOCATES WHOLE MINOR UNITS, THEN DISTRIBUTES
      *>                  THE REMAINING MINOR UNITS BY LARGEST
      *>                  FRACTIONAL REMAINDER.  EQUAL REMAINDERS ARE
      *>                  RESOLVED BY STABLE DOCUMENT LINE ORDER
      *>                * NEGATIVE AMOUNTS ARE ALLOCATED ON THE ABSOLUTE
      *>                  VALUE AND THE SIGN IS RESTORED
      *>                * THE PARTS ALWAYS TOTAL THE SOURCE EXACTLY
      *> CALLED BY  : EVERY MODULE (AR, AP, SO, PO, FIN, CST, KIT, BOM)
      *>              AND THE .NET CLIENT THROUGH THE COMMAREA BRIDGE
      *> INTERFACE  : CALL "RNDALLOC" USING WS-RNDALLOC-CA
      *>              COPYBOOK RNDALLOCCA
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-02-04 RJH  CR0004 PENNY ROUNDING FOR THE SALES LEDGER
      *> 1994-11-23 DJM  CR0291 SETTLEMENT DISCOUNT SPREAD, LARGEST
      *>                 REMAINDER METHOD (AUDIT QUERY 94/07)
      *> 1999-04-18 KMP  CR1102 500 LINE LIMIT RAISED FROM 200
      *> 2008-06-02 ANW  CR2240 SIX DECIMAL UNIT PRICES
      *> 2012-04-23 ANW  REWRITE FOR GNUCOBOL - EVALUATE, NO GO TO
      *> 2026-10-05 STERP TWELVE DECIMAL WORKINGS, EXTEND FUNCTION,
      *>                 NEGATIVE ALLOCATION SIGN RESTORE (AC-006)
      *>================================================================
      *> REQ: COM-011   AC: AC-006
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RNDALLOC.
       AUTHOR. R J HARGREAVES.
       DATE-WRITTEN. 04/02/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       REPOSITORY.
           FUNCTION ROUND-AWAY
           FUNCTION SIGN-OF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY STMONEY.
       COPY STFNARGS.
       01  WS-CONTROL.
           05  WS-I                    PIC S9(9) COMP-5 VALUE 0.
           05  WS-J                    PIC S9(9) COMP-5 VALUE 0.
           05  WS-BEST                 PIC S9(9) COMP-5 VALUE 0.
           05  WS-LINES                PIC S9(9) COMP-5 VALUE 0.
           05  WS-PLACES               PIC S9(4) VALUE 0.
           05  WS-FACTOR               PIC 9(9) VALUE 1.
           05  WS-SIGN                 PIC S9(4) VALUE 1.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
       01  WS-ALLOC.
           05  WS-ABS-AMOUNT           PIC S9(13)V9(6) VALUE 0.
           05  WS-TARGET-UNITS         PIC S9(20) VALUE 0.
           05  WS-TOTAL-WEIGHT         PIC S9(16)V9(6) VALUE 0.
           05  WS-ASSIGNED             PIC S9(20) VALUE 0.
           05  WS-REMAINDER            PIC S9(9) COMP-5 VALUE 0.
           05  WS-RAW                  PIC S9(20)V9(9) VALUE 0.
           05  WS-BASE                 PIC S9(20) VALUE 0.
           05  WS-BEST-FRAC            PIC S9(4)V9(9) VALUE 0.
           05  WS-CHECK-TOTAL          PIC S9(13)V9(6) VALUE 0.
       01  WS-LINE-WORK.
           05  WS-W OCCURS 500 TIMES.
               10  WS-W-UNITS          PIC S9(20) VALUE 0.
               10  WS-W-FRAC           PIC S9(4)V9(9) VALUE 0.
               10  WS-W-BUMPED         PIC X VALUE "N".
       01  WS-MSG-NUM                  PIC -(12)9.9(6).
       LINKAGE SECTION.
       COPY RNDALLOCCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "ROUND"    PERFORM 1000-ROUND
               WHEN "TRUNC"    PERFORM 1100-TRUNCATE
               WHEN "UNITPRC"  PERFORM 1200-UNIT-PRICE
               WHEN "EXTEND"   PERFORM 1300-EXTEND
               WHEN "ALLOC"    PERFORM 2000-ALLOCATE
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
      *> HOUSEKEEPING.  WORKING-STORAGE SURVIVES BETWEEN CALLS INSIDE
      *> THE ENGINE DLL, SO EVERY FIELD IS RESET HERE.
      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE WS-CONTROL WS-ALLOC
           MOVE "N" TO WS-ERROR
           MOVE "RNDALLOC" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "COM-011" TO CA-REQ-REF
           MOVE 0 TO CA-MSG-COUNT
           MOVE 500 TO RA-MAX-LINES
           MOVE RA-PLACES TO WS-PLACES.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> INPUT VALIDATION.  AN ONLINE SERVICE NEVER FAILS ON BAD INPUT -
      *> IT ANSWERS 08 WITH A STABLE REASON CODE (COM-017).
      *>----------------------------------------------------------------
       0200-VALIDATE SECTION.
       0200-START.
           IF WS-PLACES < 0 OR WS-PLACES > STM-MAX-PLACES
               MOVE "RND0001" TO CA-REASON-CODE
               MOVE "Decimal places must be between 0 and 6"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           COMPUTE WS-FACTOR = STM-FACTOR(WS-PLACES + 1)
           IF CA-FUNCTION = "ALLOC"
               PERFORM 0300-VALIDATE-ALLOC
           END-IF.
       0200-EXIT.
           EXIT.

       0300-VALIDATE-ALLOC SECTION.
       0300-START.
           MOVE RA-LINE-CNT TO WS-LINES
           IF WS-LINES < 1 OR WS-LINES > 500
               MOVE "RND0002" TO CA-REASON-CODE
               MOVE "Line count must be between 1 and 500"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0300-EXIT
           END-IF
           IF NOT RA-EQUAL AND NOT RA-WEIGHTED
               MOVE "RND0003" TO CA-REASON-CODE
               MOVE "Allocation mode must be EQUAL or WEIGHT"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0300-EXIT
           END-IF
      *>   equal allocation is weighted allocation with every weight 1
           IF RA-EQUAL
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LINES
                   MOVE 1 TO RA-WEIGHT(WS-I)
               END-PERFORM
           END-IF
           MOVE 0 TO WS-TOTAL-WEIGHT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LINES
               IF RA-WEIGHT(WS-I) < 0
                   MOVE "RND0004" TO CA-REASON-CODE
                   MOVE "Allocation weights cannot be negative"
                       TO CA-MESSAGE
                   PERFORM 9000-REJECT
                   GO TO 0300-EXIT
               END-IF
               ADD RA-WEIGHT(WS-I) TO WS-TOTAL-WEIGHT
           END-PERFORM
           IF WS-TOTAL-WEIGHT = 0
               MOVE "RND0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Total allocation weight is zero - there is "
                      "nothing to allocate to"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0300-EXIT
           END-IF.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> ROUND - NEAREST MINOR UNIT, EXACT HALVES AWAY FROM ZERO.
      *> SCALING AND TRUNCATION ARE USED RATHER THAN THE ROUNDED PHRASE
      *> BECAUSE RA-PLACES IS ONLY KNOWN AT RUN TIME.
      *>----------------------------------------------------------------
       1000-ROUND SECTION.
       1000-START.
           MOVE RA-VALUE TO RA-UNROUNDED
           MOVE RA-VALUE TO FNA-VALUE
           PERFORM 8000-ROUND-WORK
           MOVE FNA-RESULT TO RA-RESULT
           MOVE "Rounded to nearest minor unit, halves away from zero"
               TO CA-MESSAGE.
       1000-EXIT.
           EXIT.

       1100-TRUNCATE SECTION.
       1100-START.
           MOVE RA-VALUE TO RA-UNROUNDED
           COMPUTE WS-TARGET-UNITS = RA-VALUE * WS-FACTOR
           COMPUTE RA-RESULT = WS-TARGET-UNITS / WS-FACTOR
           MOVE "Truncated toward zero" TO CA-MESSAGE.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> UNITPRC - RA-VALUE / RA-VALUE2 HELD TO RA-PLACES DECIMALS.
      *> CONVERSION FACTORS AND PRICE-PER QUANTITIES ARE STRICTLY
      *> POSITIVE; A ZERO DIVISOR IS INVALID, NOT EQUIVALENT TO ONE.
      *>----------------------------------------------------------------
       1200-UNIT-PRICE SECTION.
       1200-START.
           IF RA-VALUE2 = 0
               MOVE "RND0006" TO CA-REASON-CODE
               MOVE "Divisor is zero - a missing divisor is not one"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1200-EXIT
           END-IF
           COMPUTE RA-UNROUNDED = RA-VALUE / RA-VALUE2
           MOVE RA-UNROUNDED TO FNA-VALUE
           PERFORM 8000-ROUND-WORK
           MOVE FNA-RESULT TO RA-RESULT
           MOVE "Unit price held to the requested precision"
               TO CA-MESSAGE.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> EXTEND - QUANTITY TIMES UNIT PRICE.  THE PRODUCT IS FORMED AT
      *> TWELVE DECIMALS AND ONLY THEN ROUNDED, SO THAT NO PREMATURE
      *> UNIT PRICE ROUNDING CHANGES THE REQUIRED FINAL TOTAL (AC-006).
      *>----------------------------------------------------------------
       1300-EXTEND SECTION.
       1300-START.
           COMPUTE RA-UNROUNDED = RA-VALUE * RA-VALUE2
           MOVE RA-UNROUNDED TO FNA-VALUE
           PERFORM 8000-ROUND-WORK
           MOVE FNA-RESULT TO RA-RESULT
           MOVE "Extended at twelve decimals then rounded once"
               TO CA-MESSAGE.
       1300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> ALLOCATE - THE LARGEST REMAINDER METHOD ON ABSOLUTE AMOUNTS.
      *>----------------------------------------------------------------
       2000-ALLOCATE SECTION.
       2000-START.
           PERFORM 2100-PREPARE
           PERFORM 2200-WHOLE-UNITS
           PERFORM 2300-DISTRIBUTE-REST
           PERFORM 2400-BUILD-PARTS
           PERFORM 2500-PROVE-TOTAL.
       2000-EXIT.
           EXIT.

       2100-PREPARE SECTION.
       2100-START.
           MOVE RA-AMOUNT TO FNA-VALUE
           COMPUTE WS-SIGN = FUNCTION SIGN-OF(FNA-VALUE)
           IF WS-SIGN = 0
               MOVE 1 TO WS-SIGN
           END-IF
           COMPUTE WS-ABS-AMOUNT = RA-AMOUNT * WS-SIGN
      *>   the source must already sit on a whole minor unit
           MOVE WS-ABS-AMOUNT TO FNA-VALUE
           PERFORM 8000-ROUND-WORK
           COMPUTE WS-TARGET-UNITS = FNA-RESULT * WS-FACTOR
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LINES
               MOVE 0 TO WS-W-UNITS(WS-I)
               MOVE 0 TO WS-W-FRAC(WS-I)
               MOVE "N" TO WS-W-BUMPED(WS-I)
               MOVE 0 TO RA-PART(WS-I)
           END-PERFORM.
       2100-EXIT.
           EXIT.

      *>   proportional unrounded share, whole minor units only
       2200-WHOLE-UNITS SECTION.
       2200-START.
           MOVE 0 TO WS-ASSIGNED
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LINES
               COMPUTE WS-RAW =
                   WS-TARGET-UNITS * RA-WEIGHT(WS-I) / WS-TOTAL-WEIGHT
               COMPUTE WS-BASE = WS-RAW
               COMPUTE WS-W-FRAC(WS-I) = WS-RAW - WS-BASE
               MOVE WS-BASE TO WS-W-UNITS(WS-I)
               ADD WS-BASE TO WS-ASSIGNED
           END-PERFORM
           COMPUTE WS-REMAINDER = WS-TARGET-UNITS - WS-ASSIGNED.
       2200-EXIT.
           EXIT.

      *>   hand the spare minor units to the largest fractional
      *>   remainders; equal remainders go to the earlier line, which
      *>   makes the result reproducible for a given line order
       2300-DISTRIBUTE-REST SECTION.
       2300-START.
           MOVE 0 TO RA-BUMPED
           PERFORM WS-REMAINDER TIMES
               MOVE 0 TO WS-BEST
               MOVE -1 TO WS-BEST-FRAC
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-LINES
                   IF WS-W-BUMPED(WS-J) = "N"
                      AND WS-W-FRAC(WS-J) > WS-BEST-FRAC
                       MOVE WS-J TO WS-BEST
                       MOVE WS-W-FRAC(WS-J) TO WS-BEST-FRAC
                   END-IF
               END-PERFORM
               IF WS-BEST > 0
                   ADD 1 TO WS-W-UNITS(WS-BEST)
                   MOVE "Y" TO WS-W-BUMPED(WS-BEST)
                   ADD 1 TO RA-BUMPED
               END-IF
           END-PERFORM.
       2300-EXIT.
           EXIT.

       2400-BUILD-PARTS SECTION.
       2400-START.
           MOVE 0 TO RA-ALLOCATED
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LINES
               COMPUTE RA-PART(WS-I) =
                   WS-W-UNITS(WS-I) / WS-FACTOR * WS-SIGN
               ADD RA-PART(WS-I) TO RA-ALLOCATED
           END-PERFORM.
       2400-EXIT.
           EXIT.

      *>   control total check - the parts must equal the source to the
      *>   last minor unit (COM-011 "preserve control totals")
       2500-PROVE-TOTAL SECTION.
       2500-START.
           MOVE RA-AMOUNT TO FNA-VALUE
           PERFORM 8000-ROUND-WORK
           MOVE FNA-RESULT TO WS-CHECK-TOTAL
           COMPUTE RA-RESIDUAL = WS-CHECK-TOTAL - RA-ALLOCATED
           IF RA-RESIDUAL NOT = 0
               MOVE "RND0009" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               MOVE RA-RESIDUAL TO WS-MSG-NUM
               STRING "Allocation does not reconcile, residual "
                      FUNCTION TRIM(WS-MSG-NUM)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 12 TO CA-RETURN-CODE
           ELSE
               MOVE SPACES TO CA-MESSAGE
               MOVE RA-BUMPED TO STM-ED-INT
               STRING "Allocated in full, "
                      FUNCTION TRIM(STM-ED-INT)
                      " line(s) took a spare minor unit"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       2500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> SHARED HELPER.  SEE STFNARGS FOR WHY THE ARGUMENTS HAVE TO BE
      *> MOVED INTO FNA-VALUE / FNA-PLACES RATHER THAN PASSED DIRECTLY.
      *>----------------------------------------------------------------
       8000-ROUND-WORK SECTION.
       8000-START.
           MOVE WS-PLACES TO FNA-PLACES
           COMPUTE FNA-RESULT =
               FUNCTION ROUND-AWAY(FNA-VALUE, FNA-PLACES).
       8000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> ERROR EXITS
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
           MOVE "RND0008" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "RNDALLOC does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
