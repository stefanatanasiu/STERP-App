      *>================================================================
      *> PROGRAM-ID : SOVAT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT
      *> PURPOSE    : THE UK VAT LIABILITY OF A SALES LINE, TAKEN
      *>              FROM THE ITEM'S VAT CLASSIFICATION, THE
      *>              CUSTOMER'S STATUS AND LOCATION, THE PLACE OF
      *>              SUPPLY AND THE DELIVERY EVIDENCE.  THE ANSWER
      *>              CARRIES ITS OWN EVIDENCE REQUIREMENT AND SAYS
      *>              WHETHER INVOICING IS BLOCKED.  A MISSING
      *>              CONDITION BLOCKS; IT NEVER DEFAULTS TO ZERO.
      *> CALLED BY  : SALES ORDER ENTRY, QUOTATION ENTRY, DESPATCH
      *>              CONFIRMATION, INVOICING, THE .NET CLIENT
      *> INTERFACE  : CALL "SOVAT" USING WS-SOVAT-CA
      *>              COPYBOOK SOVATCA
      *> CALLS      : SQLIO (inv.ItemVatClass, fin.VatCode,
      *>              fin.VatRate, ar.Customer), PARMGET
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-02-28 RJH  CR0271 VAT LIABILITY BY PRODUCT GROUP
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT TAX POINT DATES
      *> 2007-06-01 DJM  CR1521 MOBILE TELEPHONE DOMESTIC REVERSE
      *>                 CHARGE, 5000 POUND DE MINIMIS PER INVOICE
      *> 2021-01-04 DLS  CR3004 BREXIT - GB EXPORTS AND NI PROTOCOL
      *>                 (EC SALES LIST LOGIC REMOVED, SEE 7100)
      *> 2026-10-06 STERP SO-025, SO-026 EVIDENCE DRIVEN ZERO RATING
      *>================================================================
      *> REQ: SO-025, SO-026, SO-003
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOVAT.
       AUTHOR. R J HOLLOWAY.
       DATE-WRITTEN. 28/02/1994.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARM-CA==.
       01  WS-CONTROL.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-SQL-FAILED           PIC X VALUE "N".
               88  WS-SQL-BAD                    VALUE "Y".
           05  WS-DECIDED              PIC X VALUE "N".
               88  WS-IS-DECIDED                 VALUE "Y".
           05  WS-IX                   PIC S9(9) COMP-5 VALUE 0.
           05  WS-JX                   PIC S9(9) COMP-5 VALUE 0.
           05  WS-DATE-N               PIC 9(8) VALUE 0.
           05  WS-DEADLINE             PIC 9(8) VALUE 0.
           05  WS-YYYY                 PIC 9(4) VALUE 0.
           05  WS-MM                   PIC 9(2) VALUE 0.
           05  WS-DD                   PIC 9(2) VALUE 0.
           05  WS-MONTHS               PIC S9(4) COMP-5 VALUE 3.
           05  WS-TEXT                 PIC X(200) VALUE SPACES.
       01  WS-EDIT.
           05  WS-MSG-NUM              PIC -(11)9.99.
           05  WS-MSG-PCT              PIC -(4)9.9(4).
       01  WS-ROW.
           05  WS-R-COL1               PIC X(200).
           05  WS-R-COL2               PIC X(200).
           05  WS-R-COL3               PIC X(200).
           05  WS-R-COL4               PIC X(200).
           05  WS-R-COL5               PIC X(200).
       LINKAGE SECTION.
       COPY SOVATCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           PERFORM 0200-VALIDATE THRU 0200-EXIT
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION OF DFHCOMMAREA
               WHEN "DECIDE"    PERFORM 1000-DETERMINE THRU 1000-EXIT
               WHEN "DETERMIN"  PERFORM 1000-DETERMINE THRU 1000-EXIT
               WHEN "EXPLAIN"   PERFORM 1000-DETERMINE THRU 1000-EXIT
               WHEN "EVIDENCE"  PERFORM 5000-EVIDENCE THRU 5000-EXIT
               WHEN "OVERRIDE"  PERFORM 6000-OVERRIDE THRU 6000-EXIT
               WHEN OTHER    PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE.
           MOVE "SOVAT" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "SO-025" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE "N" TO WS-ERROR WS-SQL-FAILED WS-DECIDED
           MOVE 0 TO WS-IX WS-JX WS-DATE-N WS-DEADLINE
           MOVE FUNCTION UPPER-CASE(CA-FUNCTION OF DFHCOMMAREA)
               TO CA-FUNCTION OF DFHCOMMAREA
           MOVE FUNCTION UPPER-CASE(SV-LINE-TYPE) TO SV-LINE-TYPE
           MOVE FUNCTION UPPER-CASE(SV-CUST-LOCATION)
               TO SV-CUST-LOCATION
           MOVE FUNCTION UPPER-CASE(SV-DELIVER-LOCATION)
               TO SV-DELIVER-LOCATION
           MOVE FUNCTION UPPER-CASE(SV-ITEM-VAT-CODE)
               TO SV-ITEM-VAT-CODE
           MOVE FUNCTION UPPER-CASE(SV-OVERRIDE-CODE)
               TO SV-OVERRIDE-CODE
           IF SV-LINE-TYPE = SPACES
               MOVE "GOODS" TO SV-LINE-TYPE
           END-IF
           IF SV-CUST-LOCATION = SPACES
               MOVE "GB" TO SV-CUST-LOCATION
           END-IF
           IF SV-DELIVER-LOCATION = SPACES
               MOVE SV-CUST-LOCATION TO SV-DELIVER-LOCATION
           END-IF
           IF SV-PLACE-OF-SUPPLY = SPACES
               MOVE "GB" TO SV-PLACE-OF-SUPPLY
           END-IF
           IF SV-SUPPLY-DATE = 0
               MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO SV-SUPPLY-DATE
           END-IF
           IF SV-EVIDENCE-MONTHS < 1 OR SV-EVIDENCE-MONTHS > 12
               MOVE 3 TO SV-EVIDENCE-MONTHS
           END-IF
           IF SV-CURRENCY = SPACES
               MOVE "GBP" TO SV-CURRENCY
           END-IF
           IF SV-NET-AMOUNT-GBP = 0
               MOVE SV-NET-AMOUNT TO SV-NET-AMOUNT-GBP
           END-IF
           MOVE SPACES TO SV-VAT-CODE SV-LIABILITY SV-MECHANISM
           MOVE SPACES TO SV-INVOICE-WORDING SV-EVIDENCE-REQD
           MOVE SPACES TO SV-BLOCK-REASON SV-BLOCK-TEXT
           MOVE SPACES TO SV-TREATMENT-BASIS
           MOVE 0 TO SV-VAT-RATE-PCT SV-VAT-AMOUNT
           MOVE 0 TO SV-PROOF-DEADLINE
           MOVE "NA" TO SV-EVIDENCE-STATUS
           MOVE "N" TO SV-BLOCK-INVOICE SV-SELF-ACCOUNTS
           MOVE "N" TO SV-DRC-APPLIES SV-OVERRIDE-APPLIED
           MOVE 0 TO SV-RULE-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 10
               MOVE 0 TO SV-R-SEQ(WS-IX)
               MOVE SPACES TO SV-R-TEXT(WS-IX)
           END-PERFORM
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 10
               MOVE 200 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE.
           IF CA-COMPANY OF DFHCOMMAREA = SPACES OR SV-CUSTOMER = SPACES
               MOVE "VAT0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A company and a customer are required"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF SV-CUST-LOCATION NOT = "GB"
               AND SV-CUST-LOCATION NOT = "XI"
               AND SV-CUST-LOCATION NOT = "EU"
               AND SV-CUST-LOCATION NOT = "ROW"
               MOVE "VAT0011" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "Customer VAT location "
                      FUNCTION TRIM(SV-CUST-LOCATION)
                      " is not one of GB, XI, EU or ROW"
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF SV-DELIVER-LOCATION NOT = "GB"
               AND SV-DELIVER-LOCATION NOT = "XI"
               AND SV-DELIVER-LOCATION NOT = "EU"
               AND SV-DELIVER-LOCATION NOT = "ROW"
               MOVE "VAT0011" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "Delivery VAT location "
                      FUNCTION TRIM(SV-DELIVER-LOCATION)
                      " is not one of GB, XI, EU or ROW"
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  THE DECISION.  THE RULES ARE TESTED IN A FIXED ORDER
      *>       AND EVERY TEST THAT FIRES IS WRITTEN TO THE TRACE SO
      *>       THAT THE VAT OWNER CAN SEE WHY (SO-025).
      *>----------------------------------------------------------------
       1000-DETERMINE.
           PERFORM 2000-ITEM-CLASS THRU 2000-EXIT
           IF WS-INPUT-BAD
               GO TO 1000-EXIT
           END-IF
           PERFORM 3000-DE-MINIMIS THRU 3000-EXIT
           PERFORM 7100-NI-EU-CHECK THRU 7100-EXIT
           IF WS-IS-DECIDED
               GO TO 1000-EXIT
           END-IF
           PERFORM 7150-DOMESTIC-CODE THRU 7150-EXIT
           IF WS-IS-DECIDED
               GO TO 1000-EXIT
           END-IF
           PERFORM 7200-OVERSEAS-SERVICE THRU 7200-EXIT
           IF WS-IS-DECIDED
               PERFORM 8000-FINISH THRU 8000-EXIT
               GO TO 1000-EXIT
           END-IF
           PERFORM 7300-EXPORT-OF-GOODS THRU 7300-EXIT
           IF WS-IS-DECIDED
               PERFORM 8000-FINISH THRU 8000-EXIT
               GO TO 1000-EXIT
           END-IF
           PERFORM 7400-DOMESTIC-REVERSE THRU 7400-EXIT
           IF WS-IS-DECIDED
               PERFORM 8000-FINISH THRU 8000-EXIT
               GO TO 1000-EXIT
           END-IF
           PERFORM 7500-UK-SUPPLY THRU 7500-EXIT
           PERFORM 8000-FINISH THRU 8000-EXIT.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  THE ITEM'S SALES VAT CLASSIFICATION.  WITHOUT ONE THERE
      *>       IS NO LIABILITY TO REASON ABOUT AND THE LINE IS
      *>       REJECTED RATHER THAN TREATED AS ZERO RATED.
      *>----------------------------------------------------------------
       2000-ITEM-CLASS.
           IF SV-ITEM-VAT-CODE NOT = SPACES
               MOVE "COMMAREA" TO SV-ITEM-VAT-SOURCE
               GO TO 2000-EXIT
           END-IF
           IF NOT SV-READ-DB
               MOVE "VAT0002" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "No VAT classification was supplied for item "
                      FUNCTION TRIM(SV-ITEM-CODE)
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 v.VatCode, ISNULL(v.EvidenceRef,'')"
                  " FROM inv.ItemVatClass v WHERE v.CompanyCode = ?"
                  " AND v.ItemCode = ? AND v.Direction = 'SALE' AND "
                  "v.EffectiveFrom <= CONVERT(date, ?, 112) AND "
                  "(v.EffectiveTo IS NULL OR v.EffectiveTo >= "
                  "CONVERT(date, ?, 112)) ORDER BY v.EffectiveFrom "
                  "DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE SV-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE SV-SUPPLY-DATE TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(3)
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "VAT0002" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "No effective sales VAT classification for "
                      "item " FUNCTION TRIM(SV-ITEM-CODE)
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           MOVE WS-R-COL1(1:4) TO SV-ITEM-VAT-CODE
           MOVE "ITEMVATCLASS" TO SV-ITEM-VAT-SOURCE.
       2000-EXIT.
           EXIT.

      *>   THE DOMESTIC REVERSE CHARGE DE MINIMIS.  CR1521 PUT IT AT
      *>   5000 POUNDS AND THE PARAMETER HAS KEPT IT THERE.
       3000-DE-MINIMIS.
           IF SV-DRC-DEMINIMIS > 0
               GO TO 3000-EXIT
           END-IF
           MOVE 5000.00 TO SV-DRC-DEMINIMIS
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-PARM-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "SO.VAT.DRC.DEMINIMIS" TO PM-PARAM-CODE
           MOVE SV-SUPPLY-DATE TO PM-AS-OF
           MOVE "5000" TO PM-DEFAULT-TEXT
           MOVE "Y" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARM-CA
           IF CA-RETURN-CODE OF WS-PARM-CA < 8
               AND PM-VALUE-NUM > 0
               MOVE PM-VALUE-NUM TO SV-DRC-DEMINIMIS
           END-IF.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000  EXPORT EVIDENCE.  ZERO RATING AN EXPORT IS CONDITIONAL
      *>       ON PROOF OF EXPORT BEING HELD WITHIN THE PERMITTED
      *>       PERIOD, BASELINE THREE MONTHS (SO-026).
      *>----------------------------------------------------------------
       5000-EVIDENCE.
           MOVE SV-SUPPLY-DATE TO WS-DATE-N
           IF SV-DESPATCH-DATE NOT = 0
               MOVE SV-DESPATCH-DATE TO WS-DATE-N
           END-IF
           PERFORM 5100-ADD-MONTHS THRU 5100-EXIT
           MOVE WS-DEADLINE TO SV-PROOF-DEADLINE
           MOVE SPACES TO SV-EVIDENCE-REQD
           STRING "Commercial and official evidence of export "
                  "within three months of the supply"
               DELIMITED BY SIZE INTO SV-EVIDENCE-REQD
           IF SV-HAS-PROOF AND SV-PROOF-TYPE NOT = SPACES
               MOVE "OBTAINED" TO SV-EVIDENCE-STATUS
               GO TO 5000-EXIT
           END-IF
           IF CA-BUSINESS-DATE OF DFHCOMMAREA > WS-DEADLINE
               MOVE "OVERDUE" TO SV-EVIDENCE-STATUS
           ELSE
               MOVE "PENDING" TO SV-EVIDENCE-STATUS
           END-IF.
       5000-EXIT.
           EXIT.

      *>   CRUDE MONTH ARITHMETIC KEPT FROM THE 1994 PROGRAM.  THE
      *>   DAY IS CLAMPED TO 28 SO THAT A DEADLINE NEVER FALLS ON A
      *>   DATE THAT DOES NOT EXIST.
      *>   TODO SO-TD: replace with CALL "DATECALC" ADDMON
       5100-ADD-MONTHS.
           COMPUTE WS-YYYY = WS-DATE-N / 10000
           COMPUTE WS-MM = FUNCTION MOD(WS-DATE-N / 100, 100)
           COMPUTE WS-DD = FUNCTION MOD(WS-DATE-N, 100)
           COMPUTE WS-MM = WS-MM + SV-EVIDENCE-MONTHS
           PERFORM UNTIL WS-MM <= 12
               COMPUTE WS-MM = WS-MM - 12
               ADD 1 TO WS-YYYY
           END-PERFORM
           IF WS-DD > 28
               MOVE 28 TO WS-DD
           END-IF
           COMPUTE WS-DEADLINE = WS-YYYY * 10000 + WS-MM * 100
                               + WS-DD.
       5100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 6000  AN AUTHORISED OVERRIDE.  THE DETERMINED TREATMENT IS
      *>       STILL CALCULATED FIRST SO THAT THE RECORD SHOWS WHAT
      *>       WAS OVERRIDDEN AND WHY.
      *>----------------------------------------------------------------
       6000-OVERRIDE.
           IF SV-OVERRIDE-CODE = SPACES
               OR SV-OVERRIDE-REASON = SPACES
               OR SV-OVERRIDE-APPROVL = SPACES
               MOVE "VAT0007" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "A VAT override needs a code, a written "
                      "reason and an approval reference"
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 6000-EXIT
           END-IF
           PERFORM 1000-DETERMINE THRU 1000-EXIT
           IF WS-INPUT-BAD
               GO TO 6000-EXIT
           END-IF
           MOVE SV-OVERRIDE-CODE TO SV-VAT-CODE
           MOVE "Y" TO SV-OVERRIDE-APPLIED
           MOVE "OVERRIDE" TO SV-TREATMENT-BASIS
           MOVE "N" TO SV-BLOCK-INVOICE
           MOVE SPACES TO SV-BLOCK-REASON SV-BLOCK-TEXT
           PERFORM 4000-RATE-AND-WORDING THRU 4000-EXIT
           MOVE SPACES TO WS-TEXT
           STRING "Authorised override to "
                  FUNCTION TRIM(SV-OVERRIDE-CODE) " by "
                  FUNCTION TRIM(SV-OVERRIDE-APPROVL) " - "
                  FUNCTION TRIM(SV-OVERRIDE-REASON)
               DELIMITED BY SIZE INTO WS-TEXT
           PERFORM 9400-ADD-RULE THRU 9400-EXIT
           PERFORM 8000-FINISH THRU 8000-EXIT
           IF CA-RETURN-CODE OF DFHCOMMAREA < 4
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "VAT0007" TO CA-REASON-CODE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       6000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 7100  NORTHERN IRELAND AND THE EU.  MOVEMENTS OF GOODS
      *>       BETWEEN NI AND THE EU ARE OUTSIDE THE BASELINE (D-23).
      *>----------------------------------------------------------------
       7100-NI-EU-CHECK.
           IF NOT SV-IS-GOODS
               GO TO 7100-EXIT
           END-IF
           IF (SV-DELIVER-LOCATION = "XI"
               AND SV-CUST-LOCATION = "EU")
               OR (SV-DELIVER-LOCATION = "EU"
               AND SV-CUST-LOCATION = "XI")
               MOVE "VAT0009" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "Goods movements between Northern Ireland and"
                      " the EU are outside the baseline (D-23)"
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF DFHCOMMAREA TO SV-BLOCK-TEXT
               MOVE "VAT0009" TO SV-BLOCK-REASON
               MOVE "Y" TO SV-BLOCK-INVOICE
               MOVE "Y" TO WS-DECIDED
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       7100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 7150  A DESPATCH TO AN EU OR REST OF WORLD ADDRESS CANNOT USE
      *>       A DOMESTIC VAT CODE WITHOUT AUTHORISED EVIDENCE.
      *>----------------------------------------------------------------
       7150-DOMESTIC-CODE.
           IF NOT SV-IS-GOODS
               GO TO 7150-EXIT
           END-IF
           IF SV-DELIVER-LOCATION = "GB"
               OR SV-DELIVER-LOCATION = "XI"
               GO TO 7150-EXIT
           END-IF
           IF SV-OVERRIDE-CODE NOT = "S"
               AND SV-OVERRIDE-CODE NOT = "R"
               AND SV-OVERRIDE-CODE NOT = "Z"
               AND SV-OVERRIDE-CODE NOT = "E"
               GO TO 7150-EXIT
           END-IF
           IF SV-OVERRIDE-APPROVL NOT = SPACES
               GO TO 7150-EXIT
           END-IF
           MOVE "VAT0006" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE "VAT0006" TO SV-BLOCK-REASON
           MOVE "Y" TO SV-BLOCK-INVOICE
           MOVE SPACES TO SV-BLOCK-TEXT
           STRING "A domestic VAT code cannot be used for a "
                  "delivery outside the UK without authorised "
                  "evidence"
               DELIMITED BY SIZE INTO SV-BLOCK-TEXT
           MOVE SV-BLOCK-TEXT TO CA-MESSAGE OF DFHCOMMAREA
           MOVE SV-BLOCK-TEXT TO WS-TEXT
           PERFORM 9400-ADD-RULE THRU 9400-EXIT
           MOVE "Y" TO WS-DECIDED
           PERFORM 9000-REJECT THRU 9000-EXIT.
       7150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 7200  SERVICES TO AN OVERSEAS BUSINESS CUSTOMER.  THE GENERAL
      *>       PLACE OF SUPPLY RULE PUTS THESE OUTSIDE THE SCOPE OF UK
      *>       VAT AND THE CUSTOMER ACCOUNTS FOR ANY VAT DUE.
      *>----------------------------------------------------------------
       7200-OVERSEAS-SERVICE.
           IF NOT SV-IS-SERVICE
               GO TO 7200-EXIT
           END-IF
           IF SV-CUST-LOCATION = "GB" OR SV-CUST-LOCATION = "XI"
               GO TO 7200-EXIT
           END-IF
           IF NOT SV-IS-BUSINESS
               MOVE SPACES TO WS-TEXT
               STRING "Services to an overseas consumer stay in the"
                      " UK under the general rule"
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9400-ADD-RULE THRU 9400-EXIT
               GO TO 7200-EXIT
           END-IF
           MOVE "O" TO SV-VAT-CODE
           MOVE "OUTSIDE" TO SV-LIABILITY
           MOVE "RCSERVICE" TO SV-MECHANISM
           MOVE "Y" TO SV-SELF-ACCOUNTS
           MOVE "PLACEOFSUPPLY" TO SV-TREATMENT-BASIS
           MOVE "Y" TO WS-DECIDED
           MOVE SPACES TO WS-TEXT
           STRING "Service supplied to a business customer in "
                  FUNCTION TRIM(SV-CUST-LOCATION)
                  " - outside the scope of UK VAT"
               DELIMITED BY SIZE INTO WS-TEXT
           PERFORM 9400-ADD-RULE THRU 9400-EXIT
           MOVE SPACES TO SV-EVIDENCE-REQD
           STRING "Evidence that the customer belongs outside the "
                  "UK and is in business"
               DELIMITED BY SIZE INTO SV-EVIDENCE-REQD
      *>   a reverse charge sale with no valid customer VAT number
      *>   blocks invoicing - it does not fall back to zero
           IF SV-CUST-VAT-REG-NO = SPACES
               AND SV-CUST-LOCATION = "EU"
               PERFORM 7900-NO-VAT-NUMBER THRU 7900-EXIT
           END-IF.
       7200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 7300  EXPORT OF GOODS OUT OF GREAT BRITAIN.  ZERO RATED, BUT
      *>       ONLY WITH PROOF OF EXPORT HELD IN TIME (SO-026).
      *>----------------------------------------------------------------
       7300-EXPORT-OF-GOODS.
           IF NOT SV-IS-GOODS AND NOT SV-IS-CHARGE
               GO TO 7300-EXIT
           END-IF
           IF SV-DELIVER-LOCATION = "GB"
               OR SV-DELIVER-LOCATION = "XI"
               GO TO 7300-EXIT
           END-IF
           MOVE "EXP" TO SV-VAT-CODE
           MOVE "ZERO" TO SV-LIABILITY
           MOVE "EXPORT" TO SV-MECHANISM
           MOVE "EXPORTEVIDENCE" TO SV-TREATMENT-BASIS
           MOVE "Y" TO WS-DECIDED
           MOVE SPACES TO WS-TEXT
           STRING "Goods exported from Great Britain to "
                  FUNCTION TRIM(SV-DELIVER-LOCATION)
                  " - zero rated subject to proof of export"
               DELIMITED BY SIZE INTO WS-TEXT
           PERFORM 9400-ADD-RULE THRU 9400-EXIT
           PERFORM 5000-EVIDENCE THRU 5000-EXIT
           EVALUATE SV-EVIDENCE-STATUS
               WHEN "OBTAINED"
                   MOVE SPACES TO WS-TEXT
                   STRING "Proof of export "
                          FUNCTION TRIM(SV-PROOF-TYPE)
                          " is held - zero rating is confirmed"
                       DELIMITED BY SIZE INTO WS-TEXT
                   PERFORM 9400-ADD-RULE THRU 9400-EXIT
               WHEN "PENDING"
                   MOVE "Y" TO SV-BLOCK-INVOICE
                   MOVE "VAT0004" TO SV-BLOCK-REASON
                   MOVE SPACES TO SV-BLOCK-TEXT
                   MOVE SV-PROOF-DEADLINE TO WS-DATE-N
                   STRING "Zero rating is pending proof of export, "
                          "due by " WS-DATE-N
                       DELIMITED BY SIZE INTO SV-BLOCK-TEXT
                   MOVE "VAT0004" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SV-BLOCK-TEXT TO CA-MESSAGE OF DFHCOMMAREA
                   MOVE 6 TO CA-RETURN-CODE OF DFHCOMMAREA
                   ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               WHEN "OVERDUE"
                   MOVE "Y" TO SV-BLOCK-INVOICE
                   MOVE "VAT0005" TO SV-BLOCK-REASON
                   MOVE SPACES TO SV-BLOCK-TEXT
                   MOVE SV-PROOF-DEADLINE TO WS-DATE-N
                   STRING "Proof of export was due by " WS-DATE-N
                          " - UK VAT must now be accounted for"
                       DELIMITED BY SIZE INTO SV-BLOCK-TEXT
                   MOVE "VAT0005" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SV-BLOCK-TEXT TO CA-MESSAGE OF DFHCOMMAREA
                   MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
                   ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-EVALUATE.
       7300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 7400  THE DOMESTIC REVERSE CHARGE FOR SPECIFIED GOODS SOLD TO
      *>       A VAT REGISTERED RESELLER ABOVE THE DE MINIMIS.
      *>----------------------------------------------------------------
       7400-DOMESTIC-REVERSE.
           IF NOT SV-IS-GOODS
               GO TO 7400-EXIT
           END-IF
           IF NOT SV-IS-DRC-GOODS
               GO TO 7400-EXIT
           END-IF
           IF NOT SV-DRC-CUSTOMER
               MOVE SPACES TO WS-TEXT
               STRING "Customer is not a VAT registered reseller - "
                      "the domestic reverse charge does not apply"
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9400-ADD-RULE THRU 9400-EXIT
               GO TO 7400-EXIT
           END-IF
           IF SV-NET-AMOUNT-GBP <= SV-DRC-DEMINIMIS
               MOVE SPACES TO WS-TEXT
               MOVE SV-DRC-DEMINIMIS TO WS-MSG-NUM
               STRING "Below the de minimis of "
                      FUNCTION TRIM(WS-MSG-NUM)
                      " so the normal UK liability applies"
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9400-ADD-RULE THRU 9400-EXIT
               GO TO 7400-EXIT
           END-IF
           MOVE "DRC" TO SV-VAT-CODE
           MOVE "STANDARD" TO SV-LIABILITY
           MOVE "DRC" TO SV-MECHANISM
           MOVE "Y" TO SV-SELF-ACCOUNTS
           MOVE "Y" TO SV-DRC-APPLIES
           MOVE "DOMESTICREVERSE" TO SV-TREATMENT-BASIS
           MOVE "Y" TO WS-DECIDED
           MOVE SPACES TO WS-TEXT
           MOVE SV-DRC-DEMINIMIS TO WS-MSG-NUM
           STRING "Specified goods to a VAT registered reseller "
                  "above " FUNCTION TRIM(WS-MSG-NUM)
                  " - domestic reverse charge"
               DELIMITED BY SIZE INTO WS-TEXT
           PERFORM 9400-ADD-RULE THRU 9400-EXIT
           MOVE SPACES TO SV-EVIDENCE-REQD
           STRING "A valid customer VAT registration number and "
                  "confirmation that the customer is not an end user"
               DELIMITED BY SIZE INTO SV-EVIDENCE-REQD
           IF SV-CUST-VAT-REG-NO = SPACES
               PERFORM 7900-NO-VAT-NUMBER THRU 7900-EXIT
           END-IF.
       7400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 7500  THE ORDINARY UK SUPPLY, INCLUDING NORTHERN IRELAND,
      *>       WHICH IS A UK SUPPLY WITH UK VAT (D-23).
      *>----------------------------------------------------------------
       7500-UK-SUPPLY.
           MOVE SV-ITEM-VAT-CODE TO SV-VAT-CODE
           MOVE "ITEMCLASS" TO SV-TREATMENT-BASIS
           MOVE "NORMAL" TO SV-MECHANISM
           EVALUATE SV-VAT-CODE
               WHEN "S" MOVE "STANDARD" TO SV-LIABILITY
               WHEN "R" MOVE "REDUCED" TO SV-LIABILITY
               WHEN "Z" MOVE "ZERO" TO SV-LIABILITY
               WHEN "E" MOVE "EXEMPT" TO SV-LIABILITY
               WHEN "O" MOVE "OUTSIDE" TO SV-LIABILITY
               WHEN OTHER MOVE "STANDARD" TO SV-LIABILITY
           END-EVALUATE
           MOVE SPACES TO WS-TEXT
           IF SV-DELIVER-LOCATION = "XI"
               STRING "Northern Ireland is a UK supply with UK VAT "
                      "- item classification "
                      FUNCTION TRIM(SV-VAT-CODE) " applies"
                   DELIMITED BY SIZE INTO WS-TEXT
           ELSE
               STRING "UK domestic supply - item classification "
                      FUNCTION TRIM(SV-VAT-CODE) " applies"
                   DELIMITED BY SIZE INTO WS-TEXT
           END-IF
           PERFORM 9400-ADD-RULE THRU 9400-EXIT.
       7500-EXIT.
           EXIT.

       7900-NO-VAT-NUMBER.
           MOVE "Y" TO SV-BLOCK-INVOICE
           MOVE "VAT0003" TO SV-BLOCK-REASON
           MOVE SPACES TO SV-BLOCK-TEXT
           STRING "A reverse charge sale needs a valid customer VAT "
                  "registration number before it can be invoiced"
               DELIMITED BY SIZE INTO SV-BLOCK-TEXT
           MOVE "VAT0003" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SV-BLOCK-TEXT TO CA-MESSAGE OF DFHCOMMAREA
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SV-BLOCK-TEXT TO WS-TEXT
           PERFORM 9400-ADD-RULE THRU 9400-EXIT.
       7900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  THE RATE AND THE INVOICE WORDING.  THE EFFECTIVE DATED
      *>       RATE COMES FROM fin.VatRate WHEN THE DATABASE IS
      *>       AVAILABLE; THE BASELINE TABLE BELOW IS THE FALLBACK.
      *>----------------------------------------------------------------
       4000-RATE-AND-WORDING.
           EVALUATE SV-VAT-CODE
               WHEN "S"   MOVE 20.0000 TO SV-VAT-RATE-PCT
               WHEN "R"   MOVE 5.0000 TO SV-VAT-RATE-PCT
               WHEN OTHER MOVE 0.0000 TO SV-VAT-RATE-PCT
           END-EVALUATE
           PERFORM 4200-BASE-WORDING THRU 4200-EXIT
           IF SV-READ-DB
               PERFORM 4100-READ-VAT-CODE THRU 4100-EXIT
           END-IF
           COMPUTE SV-VAT-AMOUNT ROUNDED =
               SV-NET-AMOUNT * SV-VAT-RATE-PCT / 100.
       4000-EXIT.
           EXIT.

       4100-READ-VAT-CODE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 c.Liability, c.Mechanism, "
                  "ISNULL(c.InvoiceWording,''), "
                  "ISNULL(c.EvidenceRequired,''), "
                  "(SELECT TOP 1 CONVERT(varchar(20), r.RatePct) "
                  "FROM fin.VatRate r WHERE r.VatCode = c.VatCode "
                  "AND r.EffectiveFrom <= CONVERT(date, ?, 112) "
                  "ORDER BY r.EffectiveFrom DESC) FROM fin.VatCode c"
                  " WHERE c.VatCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE SV-SUPPLY-DATE TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(1)
           MOVE SV-VAT-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 4100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 4100-EXIT
           END-IF
           MOVE WS-R-COL1(1:12) TO SV-LIABILITY
           MOVE WS-R-COL2(1:12) TO SV-MECHANISM
           IF WS-R-COL3 NOT = SPACES
               MOVE WS-R-COL3(1:200) TO SV-INVOICE-WORDING
           END-IF
           IF WS-R-COL4 NOT = SPACES
               MOVE WS-R-COL4(1:100) TO SV-EVIDENCE-REQD
           END-IF
           IF WS-R-COL5 = SPACES
               IF SV-VAT-CODE = "S" OR SV-VAT-CODE = "R"
                   MOVE "VAT0010" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
                   STRING "No VAT rate is held for code "
                          FUNCTION TRIM(SV-VAT-CODE)
                          " on the supply date"
                       DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
                   PERFORM 9000-REJECT THRU 9000-EXIT
               END-IF
           ELSE
               COMPUTE SV-VAT-RATE-PCT = FUNCTION NUMVAL(WS-R-COL5)
           END-IF.
       4100-EXIT.
           EXIT.

       4200-BASE-WORDING.
           MOVE SPACES TO SV-INVOICE-WORDING
           EVALUATE SV-VAT-CODE
               WHEN "S"
                   STRING "VAT at the standard rate of 20%"
                       DELIMITED BY SIZE INTO SV-INVOICE-WORDING
               WHEN "R"
                   STRING "VAT at the reduced rate of 5%"
                       DELIMITED BY SIZE INTO SV-INVOICE-WORDING
               WHEN "Z"
                   STRING "Zero-rated supply"
                       DELIMITED BY SIZE INTO SV-INVOICE-WORDING
               WHEN "E"
                   STRING "Exempt supply - no VAT is chargeable"
                       DELIMITED BY SIZE INTO SV-INVOICE-WORDING
               WHEN "O"
                   STRING "Outside the scope of UK VAT - customer "
                          "to account for any VAT due"
                       DELIMITED BY SIZE INTO SV-INVOICE-WORDING
               WHEN "RC"
                   STRING "Reverse charge: customer to account for "
                          "any VAT due"
                       DELIMITED BY SIZE INTO SV-INVOICE-WORDING
               WHEN "DRC"
                   STRING "Reverse charge: customer to pay the VAT "
                          "to HMRC (VAT Act 1994 Section 55A "
                          "applies)"
                       DELIMITED BY SIZE INTO SV-INVOICE-WORDING
               WHEN "EXP"
                   STRING "Zero-rated export of goods - evidence of "
                          "export is retained"
                       DELIMITED BY SIZE INTO SV-INVOICE-WORDING
               WHEN OTHER
                   STRING "VAT treatment " FUNCTION TRIM(SV-VAT-CODE)
                       DELIMITED BY SIZE INTO SV-INVOICE-WORDING
           END-EVALUATE.
       4200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 8000  FINISH - RATE, WORDING, AMOUNT AND THE CLOSING MESSAGE.
      *>----------------------------------------------------------------
       8000-FINISH.
           IF SV-OVERRIDE-APPLIED NOT = "Y"
               PERFORM 4000-RATE-AND-WORDING THRU 4000-EXIT
           END-IF
           IF SV-EVIDENCE-REQD = SPACES
               MOVE "None beyond the sales invoice"
                   TO SV-EVIDENCE-REQD
           END-IF
           IF CA-MESSAGE OF DFHCOMMAREA = SPACES
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               MOVE SV-VAT-RATE-PCT TO WS-MSG-PCT
               STRING "VAT code " FUNCTION TRIM(SV-VAT-CODE)
                      " at " FUNCTION TRIM(WS-MSG-PCT)
                      "% - " FUNCTION TRIM(SV-INVOICE-WORDING)
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
           END-IF.
       8000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 9000  ERROR EXITS AND THE RULE TRACE
      *>----------------------------------------------------------------
       9000-REJECT.
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE "Y" TO WS-ERROR
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE "VAT0008" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           STRING "Function " FUNCTION TRIM(CA-FUNCTION OF DFHCOMMAREA)
                  " is not DECIDE, EXPLAIN, EVIDENCE or OVERRIDE"
               DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE "Y" TO WS-ERROR
           MOVE "Y" TO WS-SQL-FAILED
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE OF DFHCOMMAREA
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in SOVAT" TO CA-MESSAGE OF DFHCOMMAREA
           END-IF
           MOVE "SQL0001" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA.
       9300-EXIT.
           EXIT.

       9400-ADD-RULE.
           IF SV-RULE-CNT >= 10
               GO TO 9400-EXIT
           END-IF
           ADD 1 TO SV-RULE-CNT
           MOVE SV-RULE-CNT TO WS-IX
           MOVE WS-IX TO SV-R-SEQ(WS-IX)
           MOVE WS-TEXT(1:90) TO SV-R-TEXT(WS-IX)
           MOVE SPACES TO WS-TEXT.
       9400-EXIT.
           EXIT.
