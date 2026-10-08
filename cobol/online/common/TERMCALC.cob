      *>================================================================
      *> PROGRAM-ID : TERMCALC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : PAYMENT TERMS - DUE DATE AND SETTLEMENT DISCOUNT
      *>              DATE FOR SALES AND PURCHASE DOCUMENTS (COM-012).
      *>              THE BASELINE SUPPORTS DAYS FROM THE INVOICE DATE,
      *>              A SPECIFIED DAY OF THE FOLLOWING MONTH, AND END OF
      *>              MONTH PLUS A NUMBER OF DAYS.  A DUE DAY THAT DOES
      *>              NOT EXIST IN THE TARGET MONTH BECOMES THAT MONTH'S
      *>              FINAL DAY - SPEC 01 SECTION 9.
      *>              WHERE NO TERMS ARE AGREED, THE LATE PAYMENT OF
      *>              COMMERCIAL DEBTS (INTEREST) ACT 1998 DEFAULT
      *>              APPLIES: 30 DAYS AFTER THE LATER OF SUPPLY AND THE
      *>              CUSTOMER RECEIVING NOTICE OF THE AMOUNT.
      *>              D-25 - TERMS LONGER THAN THE PARAMETER LIMIT
      *>              (BASELINE 60 DAYS) ARE FLAGGED FOR REVIEW.
      *>              CALENDAR DAY CALCULATION IS THE BASELINE; AN
      *>              APPROVED BUSINESS DAY POLICY (BACS) MUST STATE THE
      *>              CALENDAR AND THE ADJUSTMENT DIRECTION (COM-026).
      *> CALLED BY  : AR, AP, SO, PO, FIN AND THE .NET CLIENT
      *> INTERFACE  : CALL "TERMCALC" USING WS-TERMCALC-CA
      *>              COPYBOOK TERMCALCCA
      *> CALLS      : DATECALC, RNDALLOC, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-02-04 RJH  CR0007 DUE DATE FROM TERMS CODE (SALES LEDGER)
      *> 1994-07-11 DJM  CR0266 END OF MONTH TERMS FOR THE MOTOR TRADE
      *> 1998-11-02 KMP  CR1041 SETTLEMENT DISCOUNT DEADLINE
      *> 2013-05-20 ANW  CR2702 LATE PAYMENT ACT STATUTORY DEFAULT
      *> 2026-10-05 STERP D-25 TERM LIMIT FLAG, BACS ADJUSTMENT OPTION,
      *>                 TERMS READ FROM core.PaymentTerms
      *>================================================================
      *> REQ: COM-012, COM-026, COM-009   AC: AC-005
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TERMCALC.
       AUTHOR. R J HARGREAVES.
       DATE-WRITTEN. 04/02/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STDATES.
       COPY STMONEY.
       01  WS-FLAGS.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-TERMS-FOUND          PIC X VALUE "N".
           05  WS-PROBE-DONE           PIC X VALUE "N".
           05  WS-TERMS-TABLE          PIC X(2) VALUE "NO".
               88  WS-TERMS-BY-COMPANY           VALUE "CO".
               88  WS-TERMS-GLOBAL               VALUE "GL".
               88  WS-NO-TERMS-TABLE             VALUE "NO".
       01  WS-WORK.
           05  WS-BASE-DATE            PIC 9(8) VALUE 0.
           05  WS-LATER-DATE           PIC 9(8) VALUE 0.
           05  WS-DISC-BASE            PIC S9(13)V9(6) VALUE 0.
           05  WS-DAYS                 PIC S9(4) VALUE 0.
           05  WS-DOM                  PIC S9(4) VALUE 0.
           05  WS-NUM-ED               PIC -(4)9.
       01  WS-SQL-TERMS.
           05  WS-T-BASIS              PIC X(16).
           05  WS-T-DAYS               PIC X(8).
           05  WS-T-DOM                PIC X(8).
           05  WS-T-DISC-DAYS          PIC X(8).
           05  WS-T-DISC-PCT           PIC X(16).
       01  WS-SQL-PROBE                PIC X(2).
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DC-CA==
                       LEADING ==CA-== BY ==DCH-==.
       COPY RNDALLOCCA REPLACING ==DFHCOMMAREA== BY ==WS-RND-CA==
                       LEADING ==CA-== BY ==RNH-==.
       LINKAGE SECTION.
       COPY TERMCALCCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           EVALUATE CA-FUNCTION
               WHEN "DUEDATE"  PERFORM 1000-DUE-DATE
               WHEN "GETTERMS" PERFORM 1100-GET-TERMS-ONLY
               WHEN "DISCOUNT" PERFORM 4000-DISCOUNT-AMOUNT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE WS-WORK
           MOVE "N" TO WS-ERROR
           MOVE "N" TO WS-TERMS-FOUND
           MOVE "TERMCALC" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "COM-012" TO CA-REQ-REF
           MOVE 0 TO CA-MSG-COUNT
           MOVE 0 TO TC-DUE-DATE TC-DUE-DATE-RAW TC-DISCOUNT-DATE
           MOVE 0 TO TC-DISCOUNT-AMT
           MOVE 0 TO TC-TERM-DAYS
           MOVE "N" TO TC-LONG-TERM-FLAG
           MOVE SPACES TO TC-SOURCE TC-TERMS-DESC
           MOVE SPACES TO TC-DUE-DATE-UK TC-DISC-DATE-UK
           IF TC-MAX-TERM-DAYS = 0
               MOVE 60 TO TC-MAX-TERM-DAYS
           END-IF
           IF TC-CALENDAR = SPACES OR TC-CALENDAR = LOW-VALUES
               MOVE "BACS" TO TC-CALENDAR
           END-IF
           IF TC-BACS-DIRECTION = SPACES
              OR TC-BACS-DIRECTION = LOW-VALUES
               MOVE "NEXT" TO TC-BACS-DIRECTION
           END-IF
           IF TC-DISCOUNT-BASIS = SPACES
              OR TC-DISCOUNT-BASIS = LOW-VALUES
               MOVE "NET" TO TC-DISCOUNT-BASIS
           END-IF
           MOVE FUNCTION UPPER-CASE(TC-BASIS-TYPE) TO TC-BASIS-TYPE
           MOVE FUNCTION UPPER-CASE(TC-TERMS-CODE) TO TC-TERMS-CODE.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> MAIN FLOW
      *>----------------------------------------------------------------
       1000-DUE-DATE SECTION.
       1000-START.
           PERFORM 1200-VALIDATE-DATES
           IF WS-INPUT-BAD
               GO TO 1000-EXIT
           END-IF
           PERFORM 1300-RESOLVE-TERMS
           IF WS-INPUT-BAD
               GO TO 1000-EXIT
           END-IF
           PERFORM 2000-APPLY-BASIS
           IF WS-INPUT-BAD
               GO TO 1000-EXIT
           END-IF
           MOVE TC-DUE-DATE TO TC-DUE-DATE-RAW
           PERFORM 3000-DISCOUNT-DATE
           IF TC-BACS-ADJUST = "Y"
               PERFORM 3100-BACS-ADJUST
           END-IF
           PERFORM 3200-TERM-LENGTH
           PERFORM 4000-DISCOUNT-AMOUNT
           PERFORM 5000-FORMAT-DATES
           PERFORM 5100-DESCRIBE-TERMS.
       1000-EXIT.
           EXIT.

       1100-GET-TERMS-ONLY SECTION.
       1100-START.
           IF TC-TERMS-CODE = SPACES
               MOVE "TRM0001" TO CA-REASON-CODE
               MOVE "A payment terms code is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1100-EXIT
           END-IF
           PERFORM 6000-READ-TERMS
           IF WS-TERMS-FOUND = "N"
               MOVE 16 TO CA-RETURN-CODE
               MOVE "TRM0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Payment terms "
                      FUNCTION TRIM(TC-TERMS-CODE)
                      " are not held for this company"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 1100-EXIT
           END-IF
           MOVE "TERMS" TO TC-SOURCE
           PERFORM 5100-DESCRIBE-TERMS.
       1100-EXIT.
           EXIT.

       1200-VALIDATE-DATES SECTION.
       1200-START.
      *>   the invoice date is the normal anchor; supply and receipt
      *>   matter only for the statutory default
           IF TC-INVOICE-DATE = 0 AND TC-SUPPLY-DATE NOT = 0
               MOVE TC-SUPPLY-DATE TO TC-INVOICE-DATE
           END-IF
           IF FUNCTION TEST-DATE-YYYYMMDD(TC-INVOICE-DATE) NOT = 0
               MOVE "TRM0003" TO CA-REASON-CODE
               MOVE "The invoice date is not a valid calendar date"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1200-EXIT
           END-IF
           IF TC-SUPPLY-DATE NOT = 0
              AND FUNCTION TEST-DATE-YYYYMMDD(TC-SUPPLY-DATE) NOT = 0
               MOVE "TRM0003" TO CA-REASON-CODE
               MOVE "The supply date is not a valid calendar date"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1200-EXIT
           END-IF
           IF TC-RECEIVED-DATE NOT = 0
              AND FUNCTION TEST-DATE-YYYYMMDD(TC-RECEIVED-DATE) NOT = 0
               MOVE "TRM0003" TO CA-REASON-CODE
               MOVE "The received date is not a valid calendar date"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       1200-EXIT.
           EXIT.

      *>   COM-012 - a document snapshots the resolved basis, so that a
      *>   later change to the customer's terms cannot move its due date
       1300-RESOLVE-TERMS SECTION.
       1300-START.
           IF TC-TERMS-CODE NOT = SPACES
               PERFORM 6000-READ-TERMS
               IF WS-TERMS-FOUND = "Y"
                   MOVE "TERMS" TO TC-SOURCE
                   GO TO 1300-EXIT
               END-IF
      *>       an unknown terms code must not silently invent a date
               MOVE 4 TO CA-RETURN-CODE
               MOVE "TRM0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Payment terms "
                      FUNCTION TRIM(TC-TERMS-CODE)
                      " not found - the statutory default applies"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "STATUTORY" TO TC-BASIS-TYPE
               MOVE "STATUTORY" TO TC-SOURCE
               GO TO 1300-EXIT
           END-IF
           IF TC-BASIS-TYPE NOT = SPACES
              AND TC-BASIS-TYPE NOT = "STATUTORY"
               MOVE "EXPLICIT" TO TC-SOURCE
               GO TO 1300-EXIT
           END-IF
           MOVE "STATUTORY" TO TC-BASIS-TYPE
           MOVE "STATUTORY" TO TC-SOURCE.
       1300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> DUE DATE RULES
      *>----------------------------------------------------------------
       2000-APPLY-BASIS SECTION.
       2000-START.
           EVALUATE TC-BASIS-TYPE
               WHEN "INVOICE"        PERFORM 2100-DAYS-FROM-INVOICE
               WHEN "EOM"            PERFORM 2200-END-OF-MONTH-TERMS
               WHEN "DAYOFNEXTMONTH" PERFORM 2300-DAY-OF-NEXT-MONTH
               WHEN "STATUTORY"      PERFORM 2400-STATUTORY
               WHEN OTHER
                   MOVE "TRM0004" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Payment terms basis "
                          FUNCTION TRIM(TC-BASIS-TYPE)
                          " is not supported"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   PERFORM 9000-REJECT
           END-EVALUATE.
       2000-EXIT.
           EXIT.

       2100-DAYS-FROM-INVOICE SECTION.
       2100-START.
           MOVE TC-INVOICE-DATE TO WS-BASE-DATE
           MOVE TC-DAYS TO WS-DAYS
           PERFORM 7000-ADD-DAYS
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TC-DUE-DATE.
       2100-EXIT.
           EXIT.

      *>   "30 days end of month" - the month end first, then the days
       2200-END-OF-MONTH-TERMS SECTION.
       2200-START.
           INITIALIZE WS-DC-CA
           MOVE "EOMONTH" TO DCH-FUNCTION
           MOVE TC-INVOICE-DATE TO DC-DATE-1 OF WS-DC-CA
           MOVE 0 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           IF DCH-RETURN-CODE NOT = 0
               PERFORM 9200-DATE-FAILED
               GO TO 2200-EXIT
           END-IF
           MOVE DC-RESULT-DATE OF WS-DC-CA TO WS-BASE-DATE
           MOVE TC-DAYS TO WS-DAYS
           PERFORM 7000-ADD-DAYS
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TC-DUE-DATE.
       2200-EXIT.
           EXIT.

      *>   AC-005 - terms "day 31 of the following month" applied to a
      *>   January invoice give 28 February 2027 and 29 February 2028
       2300-DAY-OF-NEXT-MONTH SECTION.
       2300-START.
           MOVE TC-DAY-OF-MONTH TO WS-DOM
           IF WS-DOM < 1 OR WS-DOM > 31
               MOVE "TRM0005" TO CA-REASON-CODE
               MOVE "Day of month must be between 1 and 31"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2300-EXIT
           END-IF
      *>   move to the first of the following month, then take its end
           MOVE TC-INVOICE-DATE TO STD-DATE-N
           MOVE 1 TO STD-DD
           MOVE STD-DATE-N TO WS-BASE-DATE
           INITIALIZE WS-DC-CA
           MOVE "EOMONTH" TO DCH-FUNCTION
           MOVE WS-BASE-DATE TO DC-DATE-1 OF WS-DC-CA
           MOVE 1 TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           IF DCH-RETURN-CODE NOT = 0
               PERFORM 9200-DATE-FAILED
               GO TO 2300-EXIT
           END-IF
           MOVE DC-RESULT-DATE OF WS-DC-CA TO STD-DATE-N
           IF WS-DOM < STD-DD
               MOVE WS-DOM TO STD-DD
           END-IF
           MOVE STD-DATE-N TO TC-DUE-DATE.
       2300-EXIT.
           EXIT.

      *>   Late Payment of Commercial Debts (Interest) Act 1998:
      *>   30 days after the later of supply and receipt of the invoice
       2400-STATUTORY SECTION.
       2400-START.
           MOVE TC-INVOICE-DATE TO WS-LATER-DATE
           IF TC-SUPPLY-DATE > WS-LATER-DATE
               MOVE TC-SUPPLY-DATE TO WS-LATER-DATE
           END-IF
           IF TC-RECEIVED-DATE > WS-LATER-DATE
               MOVE TC-RECEIVED-DATE TO WS-LATER-DATE
           END-IF
           MOVE WS-LATER-DATE TO WS-BASE-DATE
           MOVE 30 TO WS-DAYS
           MOVE 30 TO TC-DAYS
           MOVE "STATUTORY" TO TC-BASIS-TYPE
           PERFORM 7000-ADD-DAYS
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TC-DUE-DATE.
       2400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> DISCOUNT, BACS AND TERM LENGTH
      *>----------------------------------------------------------------
       3000-DISCOUNT-DATE SECTION.
       3000-START.
           IF TC-DISCOUNT-DAYS <= 0
               MOVE 0 TO TC-DISCOUNT-DATE
               GO TO 3000-EXIT
           END-IF
           MOVE TC-INVOICE-DATE TO WS-BASE-DATE
           MOVE TC-DISCOUNT-DAYS TO WS-DAYS
           PERFORM 7000-ADD-DAYS
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TC-DISCOUNT-DATE.
       3000-EXIT.
           EXIT.

       3100-BACS-ADJUST SECTION.
       3100-START.
           INITIALIZE WS-DC-CA
           MOVE "ADJUST" TO DCH-FUNCTION
           MOVE TC-DUE-DATE TO DC-DATE-1 OF WS-DC-CA
           MOVE TC-CALENDAR TO DC-CALENDAR OF WS-DC-CA
           MOVE TC-BACS-DIRECTION TO DC-DIRECTION OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           IF DCH-RETURN-CODE > 4
               PERFORM 9200-DATE-FAILED
               GO TO 3100-EXIT
           END-IF
           MOVE DC-RESULT-DATE OF WS-DC-CA TO TC-DUE-DATE
           IF DC-ADJUSTED OF WS-DC-CA = "Y"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "TRM0006" TO CA-REASON-CODE
               MOVE DCH-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       3100-EXIT.
           EXIT.

      *>   D-25 - agreed terms longer than the limit (baseline 60 days)
      *>   must be expressly agreed and not grossly unfair; flag them
       3200-TERM-LENGTH SECTION.
       3200-START.
           INITIALIZE WS-DC-CA
           MOVE "DAYSBTWN" TO DCH-FUNCTION
           MOVE TC-INVOICE-DATE TO DC-DATE-1 OF WS-DC-CA
           MOVE TC-DUE-DATE TO DC-DATE-2 OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           MOVE DC-RESULT-NUM OF WS-DC-CA TO TC-TERM-DAYS
           IF TC-TERM-DAYS > TC-MAX-TERM-DAYS
               MOVE "Y" TO TC-LONG-TERM-FLAG
               MOVE 4 TO CA-RETURN-CODE
               MOVE "TRM0007" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               MOVE TC-TERM-DAYS TO WS-NUM-ED
               STRING "Payment term of " FUNCTION TRIM(WS-NUM-ED)
                      " days needs express agreement - review it"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       3200-EXIT.
           EXIT.

      *>   the eligible amount records its VAT basis (spec 01 section 9)
       4000-DISCOUNT-AMOUNT SECTION.
       4000-START.
           MOVE 0 TO TC-DISCOUNT-AMT
           IF TC-DISCOUNT-PCT = 0
               GO TO 4000-EXIT
           END-IF
           IF TC-DISCOUNT-BASIS = "GROSS"
               COMPUTE WS-DISC-BASE = TC-NET-AMOUNT + TC-VAT-AMOUNT
           ELSE
               MOVE TC-NET-AMOUNT TO WS-DISC-BASE
           END-IF
           INITIALIZE WS-RND-CA
           MOVE "ROUND" TO RNH-FUNCTION
           MOVE 2 TO RA-PLACES OF WS-RND-CA
           COMPUTE RA-VALUE OF WS-RND-CA =
               WS-DISC-BASE * TC-DISCOUNT-PCT / 100
           CALL "RNDALLOC" USING WS-RND-CA
           MOVE RA-RESULT OF WS-RND-CA TO TC-DISCOUNT-AMT.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> PRESENTATION
      *>----------------------------------------------------------------
       5000-FORMAT-DATES SECTION.
       5000-START.
           IF TC-DUE-DATE NOT = 0
               INITIALIZE WS-DC-CA
               MOVE "FORMAT" TO DCH-FUNCTION
               MOVE TC-DUE-DATE TO DC-DATE-1 OF WS-DC-CA
               MOVE "DDMMYYYY" TO DC-FORMAT-OUT OF WS-DC-CA
               CALL "DATECALC" USING WS-DC-CA
               MOVE DC-TEXT-OUT OF WS-DC-CA TO TC-DUE-DATE-UK
           END-IF
           IF TC-DISCOUNT-DATE NOT = 0
               INITIALIZE WS-DC-CA
               MOVE "FORMAT" TO DCH-FUNCTION
               MOVE TC-DISCOUNT-DATE TO DC-DATE-1 OF WS-DC-CA
               MOVE "DDMMYYYY" TO DC-FORMAT-OUT OF WS-DC-CA
               CALL "DATECALC" USING WS-DC-CA
               MOVE DC-TEXT-OUT OF WS-DC-CA TO TC-DISC-DATE-UK
           END-IF.
       5000-EXIT.
           EXIT.

       5100-DESCRIBE-TERMS SECTION.
       5100-START.
           MOVE SPACES TO TC-TERMS-DESC
           EVALUATE TC-BASIS-TYPE
               WHEN "INVOICE"
                   MOVE TC-DAYS TO WS-NUM-ED
                   STRING FUNCTION TRIM(WS-NUM-ED)
                          " days from invoice date"
                       DELIMITED BY SIZE INTO TC-TERMS-DESC
               WHEN "EOM"
                   MOVE TC-DAYS TO WS-NUM-ED
                   STRING FUNCTION TRIM(WS-NUM-ED)
                          " days end of month"
                       DELIMITED BY SIZE INTO TC-TERMS-DESC
               WHEN "DAYOFNEXTMONTH"
                   MOVE TC-DAY-OF-MONTH TO WS-NUM-ED
                   STRING "Day " FUNCTION TRIM(WS-NUM-ED)
                          " of the following month"
                       DELIMITED BY SIZE INTO TC-TERMS-DESC
               WHEN "STATUTORY"
                   STRING "30 days - Late Payment Act default"
                       DELIMITED BY SIZE INTO TC-TERMS-DESC
               WHEN OTHER
                   MOVE TC-BASIS-TYPE TO TC-TERMS-DESC
           END-EVALUATE
           IF CA-MESSAGE = SPACES
               MOVE SPACES TO CA-MESSAGE
               STRING "Due " FUNCTION TRIM(TC-DUE-DATE-UK)
                      " - " FUNCTION TRIM(TC-TERMS-DESC)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       5100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> core.PaymentTerms ACCESS
      *>----------------------------------------------------------------
       6000-READ-TERMS SECTION.
       6000-START.
           MOVE "N" TO WS-TERMS-FOUND
           PERFORM 6100-PROBE-TABLE
           IF WS-NO-TERMS-TABLE
               GO TO 6000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           IF WS-TERMS-BY-COMPANY
               STRING "SELECT TOP 1 BasisType, Days, DayOfMonth, "
                      "DiscountDays, DiscountPct "
                      "FROM core.PaymentTerms "
                      "WHERE TermsCode = ? "
                      "AND CompanyCode IN (?, '*') "
                      "ORDER BY CASE WHEN CompanyCode = '*' "
                      "THEN 2 ELSE 1 END"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 2 TO SQLIO-PARM-CNT
               MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
               MOVE "N" TO SQLIO-PARM-NULL(2)
           ELSE
               STRING "SELECT TOP 1 BasisType, Days, DayOfMonth, "
                      "DiscountDays, DiscountPct "
                      "FROM core.PaymentTerms WHERE TermsCode = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 1 TO SQLIO-PARM-CNT
           END-IF
           MOVE TC-TERMS-CODE TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 16 TO SQLIO-COL-WIDTH(1)
           MOVE 8 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-COL-WIDTH(3)
           MOVE 8 TO SQLIO-COL-WIDTH(4)
           MOVE 16 TO SQLIO-COL-WIDTH(5)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SQL-TERMS
           IF SQLIO-ERROR
      *>       a terms lookup failure must not pretend the code is bad
               MOVE "NO" TO WS-TERMS-TABLE
               GO TO 6000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 6000-EXIT
           END-IF
           MOVE "Y" TO WS-TERMS-FOUND
           MOVE FUNCTION UPPER-CASE(WS-T-BASIS) TO TC-BASIS-TYPE
           COMPUTE TC-DAYS = FUNCTION NUMVAL(WS-T-DAYS)
           COMPUTE TC-DAY-OF-MONTH = FUNCTION NUMVAL(WS-T-DOM)
           COMPUTE TC-DISCOUNT-DAYS = FUNCTION NUMVAL(WS-T-DISC-DAYS)
           COMPUTE TC-DISCOUNT-PCT = FUNCTION NUMVAL(WS-T-DISC-PCT).
       6000-EXIT.
           EXIT.

      *>   COL_LENGTH answers NULL rather than failing when the object
      *>   is not there, so this probe is safe on an empty database
       6100-PROBE-TABLE SECTION.
       6100-START.
           IF WS-PROBE-DONE = "Y"
               GO TO 6100-EXIT
           END-IF
           MOVE "Y" TO WS-PROBE-DONE
           MOVE "NO" TO WS-TERMS-TABLE
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CASE WHEN COL_LENGTH('core.PaymentTerms',"
                  "'TermsCode') IS NULL THEN 'NO' WHEN "
                  "COL_LENGTH('core.PaymentTerms','CompanyCode') "
                  "IS NOT NULL THEN 'CO' ELSE 'GL' END"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE "SNGL" TO SQLIO-FUNC
           MOVE 3 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-COL-WIDTH(1)
           CALL "SQLIO" USING SQLIO-CA WS-SQL-PROBE
           IF SQLIO-OK
               MOVE WS-SQL-PROBE TO WS-TERMS-TABLE
           END-IF.
       6100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       7000-ADD-DAYS SECTION.
       7000-START.
           INITIALIZE WS-DC-CA
           MOVE "ADDDAYS" TO DCH-FUNCTION
           MOVE WS-BASE-DATE TO DC-DATE-1 OF WS-DC-CA
           MOVE WS-DAYS TO DC-NUMBER OF WS-DC-CA
           CALL "DATECALC" USING WS-DC-CA
           IF DCH-RETURN-CODE NOT = 0
               PERFORM 9200-DATE-FAILED
           END-IF.
       7000-EXIT.
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
           MOVE "TRM0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "TERMCALC does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

       9200-DATE-FAILED SECTION.
       9200-START.
           MOVE "TRM0008" TO CA-REASON-CODE
           MOVE DCH-MESSAGE TO CA-MESSAGE
           PERFORM 9000-REJECT.
       9200-EXIT.
           EXIT.
