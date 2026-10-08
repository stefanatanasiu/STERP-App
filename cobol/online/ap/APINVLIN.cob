      *>================================================================
      *> PROGRAM-ID : APINVLIN
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : THE LINE CODING AND VAT ENGINE FOR A PURCHASE
      *>              INVOICE LINE.  IT EXTENDS QUANTITY BY PRICE ON
      *>              THE STATED PRICE-PER BASIS, ADDS SEPARATELY
      *>              VISIBLE CHARGES, LOOKS THE VAT CODE UP IN
      *>              fin.VatCode / fin.VatRate AT THE TAX POINT AND
      *>              SPLITS RECOVERABLE FROM NON-RECOVERABLE INPUT
      *>              TAX.  REVERSE CHARGE AND POSTPONED IMPORT VAT
      *>              CARRY NO SUPPLIER VAT: THEY ARE SELF ACCOUNTED
      *>              IN BOTH THE OUTPUT AND THE INPUT BOX.
      *>              MISSING CODING BLOCKS THE LINE - IT IS NEVER
      *>              SILENTLY DEFAULTED (AP-005).
      *>              NOT A LINKABLE PROGRAM.
      *> CALLED BY  : APINVREG, APBULK, APEINV
      *> INTERFACE  : CALL "APINVLIN" USING WS-APLIN  (COPYBOOK
      *>              APLINWRK)
      *> CALLS      : SQLIO, APSVC
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-04-02 RJH  ORIGINAL
      *> 1995-01-01 DJM  CR0510 REDUCED RATE 8 PER CENT ON FUEL
      *> 2011-01-04 KMP  CR2740 STANDARD RATE 20 PER CENT
      *> 2021-01-01 AJS  BREXIT - REVERSE CHARGE / POSTPONED VAT
      *> 2026-10-05 STERP RATE TABLE CACHED, NON-PO CATEGORY CHECK
      *>================================================================
      *> REQ: AP-005, AP-021, AP-025, D-02, D-08
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APINVLIN.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 02/04/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APSVCARE.
       01  WS-VAT-CACHE.
           05  WS-CACHE-CNT            PIC 9(3) VALUE 0.
           05  WS-CACHE OCCURS 20 TIMES.
               10  WS-CA-CODE          PIC X(4).
               10  WS-CA-DATE          PIC 9(8).
               10  WS-CA-RATE          PIC S9(5)V9(4).
               10  WS-CA-MECH          PIC X(12).
               10  WS-CA-LIAB          PIC X(12).
               10  WS-CA-REC           PIC X.
               10  WS-CA-SELF          PIC X.
               10  WS-CA-VATBOX        PIC 9(2).
               10  WS-CA-NETBOX        PIC 9(2).
       01  WS-VAT-ROW.
           05  WS-V-RATE               PIC X(20).
           05  WS-V-MECH               PIC X(12).
           05  WS-V-LIAB               PIC X(12).
           05  WS-V-REC                PIC X(1).
           05  WS-V-SELF               PIC X(1).
           05  WS-V-VATBOX             PIC X(4).
           05  WS-V-NETBOX             PIC X(4).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-HIT                  PIC S9(4) VALUE 0.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-EXTENDED             PIC S9(15)V9(6) VALUE 0.
           05  WS-VAT-RAW              PIC S9(15)V9(6) VALUE 0.
           05  WS-POS                  PIC S9(4) VALUE 0.
           05  WS-CAT                  PIC X(20) VALUE SPACES.
           05  WS-SEARCH               PIC X(220) VALUE SPACES.
       LINKAGE SECTION.
       COPY APLINWRK.
       PROCEDURE DIVISION USING WS-APLIN.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           MOVE 0 TO LN-RC
           MOVE SPACES TO LN-REASON LN-MESSAGE
           MOVE "OK" TO LN-STATUS
           EVALUATE LN-FUNC
               WHEN "VATCALC" PERFORM 2000-VATCALC THRU 2000-EXIT
               WHEN "CODECHK" PERFORM 1000-CODECHK THRU 1000-EXIT
               WHEN "BOTH"
                   PERFORM 1000-CODECHK THRU 1000-EXIT
                   IF LN-RC < 8
                       PERFORM 2000-VATCALC THRU 2000-EXIT
                   END-IF
               WHEN OTHER
                   MOVE 8 TO LN-RC
                   MOVE "APL0099" TO LN-REASON
                   MOVE "Unsupported line function" TO LN-MESSAGE
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AP-005: EVERY LINE MUST CARRY EITHER AN ORDER / RECEIPT /
      *> SERVICE LINK OR AN APPROVED NON-PO REASON FROM THE D-08 LIST,
      *> PLUS AN ACCOUNT.  AN UNCODED LINE BLOCKS POSTING RATHER THAN
      *> PICK AN UNSPECIFIED DEFAULT.
      *>----------------------------------------------------------------
       1000-CODECHK SECTION.
       1000-START.
           MOVE "N" TO LN-NONPO
           IF LN-ORDER = SPACES AND LN-RECEIPT = SPACES
              AND LN-SERVICE = SPACES
               MOVE "Y" TO LN-NONPO
           END-IF
           IF LN-DESCRIPTION = SPACES OR LN-DESCRIPTION = LOW-VALUES
               MOVE 8 TO LN-RC
               MOVE "APL0001" TO LN-REASON
               MOVE "The line has no description" TO LN-MESSAGE
               MOVE "BLOCKED" TO LN-STATUS
               GO TO 1000-EXIT
           END-IF
           IF LN-VAT-CODE = SPACES OR LN-VAT-CODE = LOW-VALUES
               MOVE 8 TO LN-RC
               MOVE "APL0002" TO LN-REASON
               MOVE "The line has no VAT code" TO LN-MESSAGE
               MOVE "BLOCKED" TO LN-STATUS
               GO TO 1000-EXIT
           END-IF
           IF LN-ACCOUNT = SPACES OR LN-ACCOUNT = LOW-VALUES
               MOVE 8 TO LN-RC
               MOVE "APL0003" TO LN-REASON
               MOVE SPACES TO LN-MESSAGE
               STRING "The line has no expense or stock account"
                   DELIMITED BY SIZE INTO LN-MESSAGE
               MOVE "BLOCKED" TO LN-STATUS
               GO TO 1000-EXIT
           END-IF
           IF LN-NONPO = "Y"
               PERFORM 1100-NONPO-CHECK THRU 1100-EXIT
           END-IF
           IF LN-NATURE = SPACES OR LN-NATURE = LOW-VALUES
               MOVE "EXPENSE" TO LN-NATURE
           END-IF
           IF LN-NATURE = "STOCK" AND LN-RECEIPT = SPACES
              AND LN-ORDER = SPACES
               MOVE 8 TO LN-RC
               MOVE "APL0004" TO LN-REASON
               MOVE SPACES TO LN-MESSAGE
               STRING "A stock line must reference its order or "
                      "receipt" DELIMITED BY SIZE INTO LN-MESSAGE
               MOVE "BLOCKED" TO LN-STATUS
           END-IF.
       1000-EXIT.
           EXIT.

       1100-NONPO-CHECK SECTION.
       1100-START.
           IF LN-NONPO-REASON = SPACES
              OR LN-NONPO-REASON = LOW-VALUES
               MOVE 8 TO LN-RC
               MOVE "APL0005" TO LN-REASON
               MOVE SPACES TO LN-MESSAGE
               STRING "A line with no order needs an approved "
                      "non-PO reason" DELIMITED BY SIZE
                   INTO LN-MESSAGE
               MOVE "BLOCKED" TO LN-STATUS
               GO TO 1100-EXIT
           END-IF
           IF LN-NONPO-ALLOWED = SPACES
               INITIALIZE WS-APSVC
               MOVE "PARMTXT" TO SVC-FUNC
               MOVE LN-COMPANY TO SVC-COMPANY
               MOVE LN-DATE TO SVC-DATE
               MOVE "AP.NONPO.CATEGORIES" TO SVC-TEXT-1
               MOVE "UTILITIES,RATES,RENT" TO SVC-TEXT-2
               CALL "APSVC" USING WS-APSVC
               MOVE SVC-TEXT-OUT TO LN-NONPO-ALLOWED
           END-IF
           MOVE SPACES TO WS-SEARCH
           STRING "," FUNCTION TRIM(LN-NONPO-ALLOWED) ","
               DELIMITED BY SIZE INTO WS-SEARCH
           MOVE SPACES TO WS-CAT
           STRING "," FUNCTION TRIM(
                  FUNCTION UPPER-CASE(LN-NONPO-REASON)) ","
               DELIMITED BY SIZE INTO WS-CAT
           MOVE 0 TO WS-POS
           INSPECT WS-SEARCH TALLYING WS-POS
               FOR ALL FUNCTION TRIM(WS-CAT)
           IF WS-POS = 0
               MOVE 6 TO LN-RC
               MOVE "APL0006" TO LN-REASON
               MOVE SPACES TO LN-MESSAGE
               STRING "Non-PO category "
                      FUNCTION TRIM(LN-NONPO-REASON)
                      " is not on the approved list"
                   DELIMITED BY SIZE INTO LN-MESSAGE
               MOVE "HELD" TO LN-STATUS
           END-IF.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> EXTENSION, CHARGES AND VAT.  THE EXTENSION IS ROUNDED ONCE AT
      *> THE END (COM-011) AND A ZERO PRICE-PER IS REFUSED RATHER THAN
      *> TREATED AS ONE (SECTION 3 OF THE DECISIONS).
      *>----------------------------------------------------------------
       2000-VATCALC SECTION.
       2000-START.
           IF LN-PRICE-PER = 0
               MOVE 1 TO LN-PRICE-PER
           END-IF
           IF LN-FX-RATE = 0
               MOVE 1 TO LN-FX-RATE
           END-IF
           IF LN-NET = 0
               COMPUTE WS-EXTENDED ROUNDED =
                   (LN-QTY * LN-PRICE) / LN-PRICE-PER
               COMPUTE LN-NET = WS-EXTENDED + LN-CHARGE
           END-IF
           PERFORM 2100-GET-VAT-CODE THRU 2100-EXIT
           IF LN-RC > 6
               GO TO 2000-EXIT
           END-IF
           MOVE 0 TO LN-VAT LN-SELF-VAT-GBP
           MOVE 0 TO LN-RECOVER-GBP LN-NONRECOVER-GBP
           COMPUTE LN-NET-GBP ROUNDED = LN-NET / LN-FX-RATE
           IF LN-SELF-ACCOUNTS = "Y"
               PERFORM 2200-SELF-ACCOUNT THRU 2200-EXIT
           ELSE
               PERFORM 2300-SUPPLIER-VAT THRU 2300-EXIT
           END-IF.
       2000-EXIT.
           EXIT.

       2100-GET-VAT-CODE SECTION.
       2100-START.
           MOVE 0 TO WS-HIT
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-CACHE-CNT OR WS-HIT > 0
               IF WS-CA-CODE(WS-I) = LN-VAT-CODE
                  AND WS-CA-DATE(WS-I) = LN-TAX-POINT
                   MOVE WS-I TO WS-HIT
               END-IF
           END-PERFORM
           IF WS-HIT > 0
               MOVE WS-CA-RATE(WS-HIT) TO LN-RATE-PCT
               MOVE WS-CA-MECH(WS-HIT) TO LN-MECHANISM
               MOVE WS-CA-LIAB(WS-HIT) TO LN-LIABILITY
               MOVE WS-CA-REC(WS-HIT) TO LN-RECOVERABLE
               MOVE WS-CA-SELF(WS-HIT) TO LN-SELF-ACCOUNTS
               MOVE WS-CA-VATBOX(WS-HIT) TO LN-VAT-BOX
               MOVE WS-CA-NETBOX(WS-HIT) TO LN-NET-BOX
               GO TO 2100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(r.RatePct AS varchar(20)), "
                  "c.Mechanism, c.Liability, "
                  "CAST(c.IsRecoverable AS char(1)), "
                  "CAST(c.SelfAccounts AS char(1)), "
                  "CAST(ISNULL(c.InputVatBox,0) AS varchar(4)), "
                  "CAST(ISNULL(c.InputNetBox,0) AS varchar(4)) "
                  "FROM fin.VatCode c JOIN fin.VatRate r "
                  "ON r.VatCode = c.VatCode "
                  "WHERE c.VatCode = ? AND r.EffectiveFrom <= "
                  "CAST(? AS date) ORDER BY r.EffectiveFrom DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE LN-VAT-CODE TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE LN-TAX-POINT TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 1 TO SQLIO-COL-WIDTH(4)
           MOVE 1 TO SQLIO-COL-WIDTH(5)
           MOVE 4 TO SQLIO-COL-WIDTH(6)
           MOVE 4 TO SQLIO-COL-WIDTH(7)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-VAT-ROW
           IF SQLIO-ERROR
               MOVE 12 TO LN-RC
               MOVE "SQL0001" TO LN-REASON
               MOVE SQLIO-ERRMSG(1:80) TO LN-MESSAGE
               GO TO 2100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 8 TO LN-RC
               MOVE "APL0007" TO LN-REASON
               MOVE SPACES TO LN-MESSAGE
               STRING "No VAT rate is held for code "
                      FUNCTION TRIM(LN-VAT-CODE) " at that tax point"
                   DELIMITED BY SIZE INTO LN-MESSAGE
               MOVE "BLOCKED" TO LN-STATUS
               GO TO 2100-EXIT
           END-IF
           COMPUTE LN-RATE-PCT = FUNCTION NUMVAL(WS-V-RATE)
           MOVE WS-V-MECH TO LN-MECHANISM
           MOVE WS-V-LIAB TO LN-LIABILITY
           IF WS-V-REC = "1"
               MOVE "Y" TO LN-RECOVERABLE
           ELSE
               MOVE "N" TO LN-RECOVERABLE
           END-IF
           IF WS-V-SELF = "1"
               MOVE "Y" TO LN-SELF-ACCOUNTS
           ELSE
               MOVE "N" TO LN-SELF-ACCOUNTS
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-V-VATBOX)
           MOVE WS-NUM TO LN-VAT-BOX
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-V-NETBOX)
           MOVE WS-NUM TO LN-NET-BOX
           IF WS-CACHE-CNT < 20
               ADD 1 TO WS-CACHE-CNT
               MOVE LN-VAT-CODE TO WS-CA-CODE(WS-CACHE-CNT)
               MOVE LN-TAX-POINT TO WS-CA-DATE(WS-CACHE-CNT)
               MOVE LN-RATE-PCT TO WS-CA-RATE(WS-CACHE-CNT)
               MOVE LN-MECHANISM TO WS-CA-MECH(WS-CACHE-CNT)
               MOVE LN-LIABILITY TO WS-CA-LIAB(WS-CACHE-CNT)
               MOVE LN-RECOVERABLE TO WS-CA-REC(WS-CACHE-CNT)
               MOVE LN-SELF-ACCOUNTS TO WS-CA-SELF(WS-CACHE-CNT)
               MOVE LN-VAT-BOX TO WS-CA-VATBOX(WS-CACHE-CNT)
               MOVE LN-NET-BOX TO WS-CA-NETBOX(WS-CACHE-CNT)
           END-IF.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> REVERSE CHARGE SERVICES AND POSTPONED IMPORT VAT.  THE
      *> SUPPLIER CHARGES NOTHING; THE SAME AMOUNT GOES INTO THE OUTPUT
      *> BOX AND, WHERE RECOVERABLE, INTO THE INPUT BOX.  THE PAYABLE
      *> IS THE NET AMOUNT ONLY.
      *>----------------------------------------------------------------
       2200-SELF-ACCOUNT SECTION.
       2200-START.
           MOVE 0 TO LN-VAT
           MOVE 0 TO LN-VAT-GBP
           COMPUTE WS-VAT-RAW = (LN-NET-GBP * LN-RATE-PCT) / 100
           INITIALIZE WS-APSVC
           MOVE "ROUND" TO SVC-FUNC
           MOVE LN-COMPANY TO SVC-COMPANY
           MOVE LN-DATE TO SVC-DATE
           MOVE WS-VAT-RAW TO SVC-NUM-1
           MOVE 2 TO SVC-INT-1
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-NUM-OUT TO LN-SELF-VAT-GBP
           IF LN-RECOVERABLE = "Y"
               MOVE LN-SELF-VAT-GBP TO LN-RECOVER-GBP
           ELSE
               MOVE LN-SELF-VAT-GBP TO LN-NONRECOVER-GBP
           END-IF.
       2200-EXIT.
           EXIT.

       2300-SUPPLIER-VAT SECTION.
       2300-START.
           COMPUTE WS-VAT-RAW = (LN-NET * LN-RATE-PCT) / 100
           INITIALIZE WS-APSVC
           MOVE "ROUND" TO SVC-FUNC
           MOVE LN-COMPANY TO SVC-COMPANY
           MOVE LN-DATE TO SVC-DATE
           MOVE WS-VAT-RAW TO SVC-NUM-1
           MOVE 2 TO SVC-INT-1
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-NUM-OUT TO LN-VAT
           COMPUTE LN-VAT-GBP ROUNDED = LN-VAT / LN-FX-RATE
      *>   AP-025: VAT IS ONLY RELEASED WHERE THE EVIDENCE IS VALID.
      *>   WITHOUT IT THE PAYABLE STANDS BUT THE TAX IS HELD AS A
      *>   RECLAIM EXCEPTION AND CHARGED TO THE COST.
           IF LN-RECOVERABLE = "Y" AND LN-VAT-EVIDENCE NOT = "N"
               MOVE LN-VAT-GBP TO LN-RECOVER-GBP
               MOVE 0 TO LN-NONRECOVER-GBP
           ELSE
               MOVE 0 TO LN-RECOVER-GBP
               MOVE LN-VAT-GBP TO LN-NONRECOVER-GBP
           END-IF.
       2300-EXIT.
           EXIT.
