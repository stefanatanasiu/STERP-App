      *>================================================================
      *> PROGRAM-ID : FXCONV
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : FOREIGN CURRENCY CONVERSION AND REALISED EXCHANGE
      *>              DIFFERENCES (COM-010, COM-011, D-14).
      *>              RATES ARE QUOTED AS FOREIGN CURRENCY UNITS PER ONE
      *>              POUND STERLING, THE UK MARKET AND HMRC CONVENTION.
      *>                  STERLING = FOREIGN / RATE
      *>                  FOREIGN  = STERLING * RATE
      *>              COM-010 - A MISSING, ZERO OR AMBIGUOUS RATE BLOCKS
      *>              MONETARY CONFIRMATION.  THERE IS NO ASSUMED PARITY
      *>              CONVERSION AND THE DIRECTION OF A QUOTATION IS
      *>              NEVER INFERRED FROM ITS MAGNITUDE - A CALLER THAT
      *>              HOLDS THE INVERSE MUST SAY SO AND ASK FOR IT TO BE
      *>              TURNED ROUND, AND BOTH QUOTATIONS ARE RETAINED.
      *>              WORKINGS ARE KEPT AT TWELVE DECIMALS AND ROUNDED
      *>              ONCE THROUGH RNDALLOC; THE RESIDUAL IS REPORTED
      *>              SEPARATELY SO THAT IT CANNOT MASQUERADE AS
      *>              REVENUE, COST OR VAT.
      *> CALLED BY  : FIN, AR, AP, SO, PO AND THE .NET CLIENT
      *> INTERFACE  : CALL "FXCONV" USING WS-FXCONV-CA
      *>              COPYBOOK FXCONVCA
      *> CALLS      : RNDALLOC, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1992-09-30 RJH  CR0098 ECU AND USD CONVERSION FOR EXPORT SALES
      *> 1998-05-06 DJM  CR0987 EURO CONVERSION (TRIANGULATION) - THE
      *>                 TRIANGULATION PARAGRAPHS WERE REMOVED IN 2003
      *> 2003-06-11 KMP  CR1702 REALISED GAIN / LOSS ON SETTLEMENT
      *> 2012-04-23 ANW  REWRITE FOR GNUCOBOL
      *> 2026-10-05 STERP D-14 BANK OF ENGLAND DAILY SPOT, REVERSED
      *>                 QUOTATION REJECTED UNLESS EXPLICITLY ALLOWED
      *>================================================================
      *> REQ: COM-010, COM-011   AC: AC-005
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FXCONV.
       AUTHOR. R J HARGREAVES.
       DATE-WRITTEN. 30/09/1992.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STMONEY.
       01  WS-FLAGS.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-RATE-FOUND           PIC X VALUE "N".
           05  WS-PROBE-DONE           PIC X VALUE "N".
           05  WS-RATE-TABLE           PIC X(2) VALUE "NO".
               88  WS-NO-RATE-TABLE              VALUE "NO".
       01  WS-WORK.
           05  WS-GBP1                 PIC S9(13)V9(12) VALUE 0.
           05  WS-GBP2                 PIC S9(13)V9(12) VALUE 0.
           05  WS-PLACES               PIC S9(4) VALUE 2.
           05  WS-RATE-ED              PIC -(5)9.9(8).
       01  WS-SQL-RATE.
           05  WS-R-DATE               PIC X(8).
           05  WS-R-UNITS              PIC X(30).
           05  WS-R-SOURCE             PIC X(20).
       01  WS-SQL-PROBE                PIC X(2).
       COPY RNDALLOCCA REPLACING ==DFHCOMMAREA== BY ==WS-RND-CA==
                       LEADING ==CA-== BY ==RNH-==.
       LINKAGE SECTION.
       COPY FXCONVCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE-CURRENCY
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "GETRATE"  PERFORM 1000-GET-RATE
               WHEN "TOGBP"    PERFORM 2000-TO-STERLING
               WHEN "TOFCY"    PERFORM 2100-TO-FOREIGN
               WHEN "REALISED" PERFORM 3000-REALISED
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE WS-WORK
           MOVE "N" TO WS-ERROR
           MOVE "N" TO WS-RATE-FOUND
           MOVE "FXCONV" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "COM-010" TO CA-REQ-REF
           MOVE 0 TO CA-MSG-COUNT
           MOVE 0 TO FX-UNROUNDED FX-RESIDUAL
           MOVE SPACES TO FX-RATE-TEXT
           MOVE FUNCTION UPPER-CASE(FX-CURRENCY) TO FX-CURRENCY
           MOVE FUNCTION UPPER-CASE(FX-POSITION) TO FX-POSITION
           MOVE FUNCTION UPPER-CASE(FX-QUOTE-BASIS) TO FX-QUOTE-BASIS
           IF FX-RATE-TYPE = SPACES OR FX-RATE-TYPE = LOW-VALUES
               MOVE "SPOT" TO FX-RATE-TYPE
           END-IF
           MOVE FUNCTION UPPER-CASE(FX-RATE-TYPE) TO FX-RATE-TYPE
           IF FX-QUOTE-BASIS = SPACES OR FX-QUOTE-BASIS = LOW-VALUES
               MOVE "FCYPERGBP" TO FX-QUOTE-BASIS
           END-IF
           IF FX-POSITION = SPACES OR FX-POSITION = LOW-VALUES
               MOVE "ASSET" TO FX-POSITION
           END-IF
           IF FX-PLACES = 0
               MOVE 2 TO FX-PLACES
           END-IF
           MOVE FX-PLACES TO WS-PLACES.
       0100-EXIT.
           EXIT.

       0200-VALIDATE-CURRENCY SECTION.
       0200-START.
           IF FX-CURRENCY = SPACES OR FX-CURRENCY = LOW-VALUES
               MOVE "FX0005" TO CA-REASON-CODE
               MOVE "A currency code is required - parity is never "
                   TO CA-MESSAGE
               STRING FUNCTION TRIM(CA-MESSAGE) "assumed"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF FX-PLACES < 0 OR FX-PLACES > 6
               MOVE "FX0006" TO CA-REASON-CODE
               MOVE "Decimal places must be between 0 and 6"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> RATE LOOKUP - D-14 TAKES THE BANK OF ENGLAND DAILY SPOT RATE
      *> FOR THE DOCUMENT DATE, OR THE PREVIOUS PUBLISHED RATE IF NONE
      *> IS PUBLISHED FOR THAT DATE.
      *>----------------------------------------------------------------
       1000-GET-RATE SECTION.
       1000-START.
           IF FX-CURRENCY = "GBP"
               MOVE 1 TO FX-RATE
               MOVE 1 TO FX-RATE-ENTERED
               MOVE FX-RATE-DATE TO FX-RATE-FOUND-DATE
               MOVE "BASE CURRENCY" TO FX-RATE-SOURCE
               MOVE "GBP is the base currency - rate 1.00000000"
                   TO CA-MESSAGE
               PERFORM 5000-RATE-TEXT
               GO TO 1000-EXIT
           END-IF
           IF FUNCTION TEST-DATE-YYYYMMDD(FX-RATE-DATE) NOT = 0
               MOVE "FX0007" TO CA-REASON-CODE
               MOVE "A valid rate date is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1000-EXIT
           END-IF
           PERFORM 6000-READ-RATE
           IF WS-RATE-FOUND = "N"
               MOVE 16 TO CA-RETURN-CODE
               MOVE "FX0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No " FUNCTION TRIM(FX-RATE-TYPE)
                      " rate is held for " FX-CURRENCY
                      " on or before " FX-RATE-DATE
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 1000-EXIT
           END-IF
           PERFORM 1500-CHECK-RATE
           IF WS-INPUT-BAD
               GO TO 1000-EXIT
           END-IF
           PERFORM 5000-RATE-TEXT
           IF FX-RATE-FOUND-DATE < FX-RATE-DATE
               MOVE 4 TO CA-RETURN-CODE
               MOVE "FX0008" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No rate published on " FX-RATE-DATE
                      " - using the rate of " FX-RATE-FOUND-DATE
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           ELSE
               MOVE SPACES TO CA-MESSAGE
               STRING "Rate " FUNCTION TRIM(FX-RATE-TEXT)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       1000-EXIT.
           EXIT.

      *>   COM-010 - missing, zero or ambiguous rates block confirmation
       1500-CHECK-RATE SECTION.
       1500-START.
           IF FX-RATE-ENTERED = 0
               MOVE FX-RATE TO FX-RATE-ENTERED
           END-IF
           IF FX-RATE = 0
               MOVE "FX0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "An exchange rate is required for " FX-CURRENCY
                      " - a missing rate is not parity"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1500-EXIT
           END-IF
           IF FX-RATE < 0
               MOVE "FX0002" TO CA-REASON-CODE
               MOVE "An exchange rate must be positive" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1500-EXIT
           END-IF
           IF FX-GBP-PER-FCY
               IF FX-ALLOW-INVERT NOT = "Y"
                   MOVE "FX0003" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Rate quoted as sterling per unit of "
                          FX-CURRENCY
                          " - STERP quotes foreign units per GBP 1"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   PERFORM 9000-REJECT
                   GO TO 1500-EXIT
               END-IF
      *>       keep the entered quotation and the converted rate
               MOVE FX-RATE TO FX-RATE-ENTERED
               COMPUTE FX-RATE = 1 / FX-RATE-ENTERED
               MOVE "FCYPERGBP" TO FX-QUOTE-BASIS
               MOVE 4 TO CA-RETURN-CODE
               MOVE "FX0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               MOVE FX-RATE-ENTERED TO WS-RATE-ED
               STRING "Entered rate " FUNCTION TRIM(WS-RATE-ED)
                      " was quoted the other way round and has been "
                      "turned round on request"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       1500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> CONVERSION
      *>----------------------------------------------------------------
       2000-TO-STERLING SECTION.
       2000-START.
           PERFORM 2500-ENSURE-RATE
           IF WS-INPUT-BAD OR CA-RETURN-CODE = 16
               GO TO 2000-EXIT
           END-IF
           COMPUTE FX-UNROUNDED = FX-FOREIGN-AMOUNT / FX-RATE
           MOVE FX-UNROUNDED TO FX-GBP-AMOUNT
           PERFORM 4000-ROUND-RESULT
           MOVE FX-GBP-AMOUNT TO STM-ED-MONEY
           MOVE SPACES TO CA-MESSAGE
           STRING FX-CURRENCY " converted at "
                  FUNCTION TRIM(FX-RATE-TEXT) " to GBP "
                  FUNCTION TRIM(STM-ED-MONEY)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

       2100-TO-FOREIGN SECTION.
       2100-START.
           PERFORM 2500-ENSURE-RATE
           IF WS-INPUT-BAD OR CA-RETURN-CODE = 16
               GO TO 2100-EXIT
           END-IF
           COMPUTE FX-UNROUNDED = FX-GBP-AMOUNT * FX-RATE
           MOVE FX-UNROUNDED TO FX-FOREIGN-AMOUNT
           INITIALIZE WS-RND-CA
           MOVE "ROUND" TO RNH-FUNCTION
           MOVE WS-PLACES TO RA-PLACES
           MOVE FX-UNROUNDED TO RA-VALUE
           CALL "RNDALLOC" USING WS-RND-CA
           MOVE RA-RESULT TO FX-FOREIGN-AMOUNT
           COMPUTE FX-RESIDUAL = FX-UNROUNDED - FX-FOREIGN-AMOUNT
           MOVE FX-FOREIGN-AMOUNT TO STM-ED-MONEY
           MOVE SPACES TO CA-MESSAGE
           STRING "GBP converted at " FUNCTION TRIM(FX-RATE-TEXT)
                  " to " FX-CURRENCY " "
                  FUNCTION TRIM(STM-ED-MONEY)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2100-EXIT.
           EXIT.

      *>   use the supplied rate when there is one, otherwise look it up
       2500-ENSURE-RATE SECTION.
       2500-START.
           IF FX-CURRENCY = "GBP"
               MOVE 1 TO FX-RATE FX-RATE-ENTERED
               MOVE "BASE CURRENCY" TO FX-RATE-SOURCE
               PERFORM 5000-RATE-TEXT
               GO TO 2500-EXIT
           END-IF
           IF FX-RATE = 0 AND FX-RATE-DATE NOT = 0
               PERFORM 1000-GET-RATE
               GO TO 2500-EXIT
           END-IF
           PERFORM 1500-CHECK-RATE
           IF NOT WS-INPUT-BAD
               PERFORM 5000-RATE-TEXT
           END-IF.
       2500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REALISED EXCHANGE DIFFERENCE ON SETTLEMENT (D-14)
      *>   booked   sterling = foreign / FX-RATE
      *>   settled  sterling = foreign / FX-RATE-2
      *>   an asset that settles for more sterling than it was booked
      *>   at makes a gain; a liability that costs more makes a loss
      *>----------------------------------------------------------------
       3000-REALISED SECTION.
       3000-START.
           PERFORM 1500-CHECK-RATE
           IF WS-INPUT-BAD
               GO TO 3000-EXIT
           END-IF
           IF FX-RATE-2 <= 0
               MOVE "FX0009" TO CA-REASON-CODE
               MOVE "A positive settlement rate is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           COMPUTE WS-GBP1 = FX-FOREIGN-AMOUNT / FX-RATE
           COMPUTE WS-GBP2 = FX-FOREIGN-AMOUNT / FX-RATE-2
           INITIALIZE WS-RND-CA
           MOVE "ROUND" TO RNH-FUNCTION
           MOVE WS-PLACES TO RA-PLACES
           MOVE WS-GBP1 TO RA-VALUE
           CALL "RNDALLOC" USING WS-RND-CA
           MOVE RA-RESULT TO FX-GBP-AMOUNT
           INITIALIZE WS-RND-CA
           MOVE "ROUND" TO RNH-FUNCTION
           MOVE WS-PLACES TO RA-PLACES
           MOVE WS-GBP2 TO RA-VALUE
           CALL "RNDALLOC" USING WS-RND-CA
           MOVE RA-RESULT TO FX-GBP-AMOUNT-2
           IF FX-LIABILITY
               COMPUTE FX-REALISED = FX-GBP-AMOUNT - FX-GBP-AMOUNT-2
           ELSE
               COMPUTE FX-REALISED = FX-GBP-AMOUNT-2 - FX-GBP-AMOUNT
           END-IF
           MOVE FX-REALISED TO STM-ED-MONEY
           MOVE SPACES TO CA-MESSAGE
           IF FX-REALISED < 0
               STRING "Realised exchange loss of GBP "
                      FUNCTION TRIM(STM-ED-MONEY)
                      " on this " FUNCTION TRIM(FX-POSITION)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               STRING "Realised exchange gain of GBP "
                      FUNCTION TRIM(STM-ED-MONEY)
                      " on this " FUNCTION TRIM(FX-POSITION)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> COM-011 - round once, and report the residual separately so
      *> that it cannot be mistaken for revenue, cost or VAT
      *>----------------------------------------------------------------
       4000-ROUND-RESULT SECTION.
       4000-START.
           INITIALIZE WS-RND-CA
           MOVE "ROUND" TO RNH-FUNCTION
           MOVE WS-PLACES TO RA-PLACES
           MOVE FX-UNROUNDED TO RA-VALUE
           CALL "RNDALLOC" USING WS-RND-CA
           MOVE RA-RESULT TO FX-GBP-AMOUNT
           COMPUTE FX-RESIDUAL = FX-UNROUNDED - FX-GBP-AMOUNT.
       4000-EXIT.
           EXIT.

       5000-RATE-TEXT SECTION.
       5000-START.
           MOVE FX-RATE TO WS-RATE-ED
           MOVE SPACES TO FX-RATE-TEXT
           STRING FUNCTION TRIM(WS-RATE-ED) " " FX-CURRENCY
                  " per GBP 1"
               DELIMITED BY SIZE INTO FX-RATE-TEXT.
       5000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> core.FxRate ACCESS
      *>----------------------------------------------------------------
       6000-READ-RATE SECTION.
       6000-START.
           MOVE "N" TO WS-RATE-FOUND
           PERFORM 6100-PROBE-TABLE
           IF WS-NO-RATE-TABLE
               GO TO 6000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 CONVERT(char(8), RateDate, 112), "
                  "CONVERT(varchar(30), UnitsPerGBP), "
                  "ISNULL(Source, '') FROM core.FxRate "
                  "WHERE CurrencyCode = ? AND RateType = ? "
                  "AND RateDate <= ? ORDER BY RateDate DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE FX-CURRENCY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE FX-RATE-TYPE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE FX-RATE-DATE TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 20 TO SQLIO-COL-WIDTH(3)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SQL-RATE
           IF SQLIO-ERROR
               MOVE "NO" TO WS-RATE-TABLE
               GO TO 6000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 6000-EXIT
           END-IF
           MOVE "Y" TO WS-RATE-FOUND
           MOVE WS-R-DATE TO FX-RATE-FOUND-DATE
           COMPUTE FX-RATE = FUNCTION NUMVAL(WS-R-UNITS)
           MOVE FX-RATE TO FX-RATE-ENTERED
           MOVE "FCYPERGBP" TO FX-QUOTE-BASIS
           IF WS-R-SOURCE = SPACES
               MOVE "core.FxRate" TO FX-RATE-SOURCE
           ELSE
               MOVE WS-R-SOURCE TO FX-RATE-SOURCE
           END-IF.
       6000-EXIT.
           EXIT.

       6100-PROBE-TABLE SECTION.
       6100-START.
           IF WS-PROBE-DONE = "Y"
               GO TO 6100-EXIT
           END-IF
           MOVE "Y" TO WS-PROBE-DONE
           MOVE "NO" TO WS-RATE-TABLE
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CASE WHEN COL_LENGTH('core.FxRate',"
                  "'UnitsPerGBP') IS NOT NULL THEN 'OK' "
                  "ELSE 'NO' END"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE "SNGL" TO SQLIO-FUNC
           MOVE 5 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-COL-WIDTH(1)
           CALL "SQLIO" USING SQLIO-CA WS-SQL-PROBE
           IF SQLIO-OK
               MOVE WS-SQL-PROBE TO WS-RATE-TABLE
           END-IF.
       6100-EXIT.
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
           MOVE "FX0010" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "FXCONV does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
