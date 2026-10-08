      *>================================================================
      *> PROGRAM-ID : RETURNS
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : CUSTOMER RETURN AUTHORISATION, RECEIPT,
      *>              INSPECTION AND DISPOSITION.  A RETURN
      *>              AUTHORISATION IS NOT A RECEIPT AND IT IS NOT A
      *>              CREDIT: IT RESERVES A REFERENCE AND A DECISION.
      *>              GOODS THAT ARRIVE BACK ARE UNAVAILABLE UNTIL A
      *>              PERMITTED DISPOSITION IS RECORDED, SO THEY ARE
      *>              TAKEN INTO AN INSPECTION CONDITION AND PUT ON A
      *>              QUARANTINE HOLD THROUGH STKMOVE - NEVER STRAIGHT
      *>              INTO FREE STOCK.  ONLY RESTOCK PUTS THEM BACK
      *>              INTO SALEABLE STOCK, AT THE ORIGINAL UNIT COST,
      *>              WITH ANY DIFFERENCE HELD AS A VARIANCE.
      *>              A RETURN NEVER REOPENS THE ORIGINAL ORDER.
      *> CALLED BY  : STERP DESKTOP CLIENT (SALES RETURNS), SOCREDIT
      *> INTERFACE  : CALL "RETURNS" USING DFHCOMMAREA (COPY RETURNSCA)
      *> CALLS      : NUMGEN, PARMGET, DATECALC, STKMOVE, AUDITLOG,
      *>              SQLIO -> so.ReturnAuthorisation, so.ReturnLine
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-02-18 RJH  CR0601 SALES RETURNS AND RMA CONTROL
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS ON RETURN DATES
      *> 2004-09-22 DJM  CR1221 QUARANTINE BEFORE DISPOSITION
      *> 2011-06-02 TMW  CR1904 WARRANTY WINDOW FROM A PARAMETER
      *> 2026-10-06 STERP SO-017 SO-018 D-12 RETURN WINDOW AND HOLD
      *>================================================================
      *> REQ: SO-017, SO-018
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RETURNS.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 18/02/1997.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARM-CA==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATE-CA==.
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==.
       COPY STKMOVECA REPLACING ==DFHCOMMAREA== BY ==WS-MOVE-CA==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDIT-CA==.
       01  WS-CONTROL.
           05  WS-FUNC                 PIC X(8).
           05  WS-CO                   PIC X(10).
           05  WS-SITE                 PIC X(10).
           05  WS-USER                 PIC X(20).
           05  WS-BDATE                PIC 9(8).
           05  WS-CORR                 PIC X(36).
      *>   CR2605.  One correlation id may only name ONE idempotent
      *>   business instruction.  The receipt movement completes
      *>   core.IdempotencyKey for the caller's id through
      *>   inv.usp_PostStockMovement, so every later movement of the
      *>   same return was short-circuited and handed back the receipt.
      *>   Each movement therefore derives its own sub-correlation id.
           05  WS-SUB-CORR             PIC X(36) VALUE SPACES.
           05  WS-SUB-SUFFIX           PIC X(8) VALUE SPACES.
           05  WS-SUB-SEQ              PIC 9(3) VALUE 0.
           05  WS-BASE-LEN             PIC S9(4) VALUE 0.
           05  WS-SUF-LEN              PIC S9(4) VALUE 0.
           05  WS-ERROR                PIC X VALUE "N".
           05  WS-IN-TXN               PIC X VALUE "N".
           05  WS-RC                   PIC 9(2) VALUE 0.
           05  WS-REASON               PIC X(8).
           05  WS-MSG                  PIC X(200).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-N                    PIC S9(15)V9(6) VALUE 0.
           05  WS-QTY1                 PIC S9(13)V9(6) VALUE 0.
           05  WS-QTY2                 PIC S9(13)V9(6) VALUE 0.
           05  WS-AMT1                 PIC S9(13)V99 VALUE 0.
           05  WS-AMT2                 PIC S9(13)V99 VALUE 0.
           05  WS-LINES-READ           PIC S9(4) VALUE 0.
           05  WS-QTY-ED               PIC -(9)9.9(6).
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-INT-ED               PIC -(8)9.
           05  WS-DATE-ED              PIC X(10).
       01  WS-HEAD-ROW.
           05  WH-RETURN-ID            PIC X(24).
           05  WH-CUSTOMER             PIC X(20).
           05  WH-STATUS               PIC X(12).
           05  WH-REQ-DATE             PIC X(8).
           05  WH-RECV-DATE            PIC X(8).
           05  WH-SITE                 PIC X(10).
           05  WH-ORDER                PIC X(30).
           05  WH-FAULTY               PIC X(2).
           05  WH-WINDOW               PIC X(2).
           05  WH-APPROVAL             PIC X(24).
           05  WH-CREDIT-NOTE          PIC X(30).
           05  WH-DESPATCH             PIC X(30).
           05  WH-INVOICE              PIC X(30).
       01  WS-LINE-ROW.
           05  WL-LINE-NUM             PIC X(12).
           05  WL-ITEM                 PIC X(40).
           05  WL-REVISION             PIC X(4).
           05  WL-LOT                  PIC X(30).
           05  WL-SERIAL               PIC X(40).
           05  WL-UOM                  PIC X(10).
           05  WL-REQ-QTY              PIC X(24).
           05  WL-AUTH-QTY             PIC X(24).
           05  WL-RECV-QTY             PIC X(24).
           05  WL-ACC-QTY              PIC X(24).
           05  WL-REJ-QTY              PIC X(24).
           05  WL-CRED-QTY             PIC X(24).
           05  WL-DISPOSITION          PIC X(12).
           05  WL-ORIG-COST            PIC X(24).
           05  WL-HOLD-ID              PIC X(24).
           05  WL-VAT-CODE             PIC X(4).
       01  WS-ONE-COL.
           05  WS-C1                   PIC X(40).
       LINKAGE SECTION.
       COPY RETURNSCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 1000-INITIALISE THRU 1000-EXIT
           IF WS-ERROR = "N"
               EVALUATE WS-FUNC
                   WHEN "REQUEST"
                       PERFORM 2000-REQUEST THRU 2000-EXIT
                   WHEN "AUTHORIS"
                       PERFORM 3000-AUTHORISE THRU 3000-EXIT
                   WHEN "RECEIVE"
                       PERFORM 4000-RECEIVE THRU 4000-EXIT
                   WHEN "INSPECT"
                       PERFORM 5000-INSPECTION THRU 5000-EXIT
                   WHEN "DISPOSE"
                       PERFORM 6000-DISPOSE THRU 6000-EXIT
                   WHEN "CLOSE"
                       PERFORM 7000-CLOSE THRU 7000-EXIT
                   WHEN OTHER
                       PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
               END-EVALUATE
           END-IF
           PERFORM 9400-REPORT THRU 9400-EXIT.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 1000  EVERY CALL STARTS CLEAN.  WORKING STORAGE SURVIVES
      *>       BETWEEN CALLS IN THIS RUN UNIT, SO NOTHING MAY BE
      *>       ASSUMED FROM THE PREVIOUS CALLER.
      *>----------------------------------------------------------------
       1000-INITIALISE SECTION.
       1000-START.
           MOVE "N" TO WS-ERROR
           MOVE "N" TO WS-IN-TXN
           MOVE 0 TO WS-RC WS-I WS-J WS-LINES-READ
           MOVE SPACES TO WS-REASON WS-MSG
           MOVE "RETURNS" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "SO-017" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE FUNCTION UPPER-CASE(CA-FUNCTION OF DFHCOMMAREA)
               TO WS-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO WS-CO
           MOVE CA-SITE OF DFHCOMMAREA TO WS-SITE
           MOVE CA-USER OF DFHCOMMAREA TO WS-USER
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO WS-BDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO WS-CORR
      *>   a return authorisation has no stock and no money effect
           MOVE "N" TO RT-STOCK-MOVED
           MOVE "N" TO RT-CREDIT-RAISED
           MOVE "N" TO RT-ORDER-REOPENED
           MOVE 0 TO RT-MOVEMENT-COUNT
           IF WS-CO = SPACES
               MOVE "RET0001" TO WS-REASON
               MOVE "A company code is required" TO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           IF RT-SITE = SPACES
               MOVE WS-SITE TO RT-SITE
           END-IF
           IF RT-HOLD-CONDITION = SPACES
               MOVE "QUARANTINE" TO RT-HOLD-CONDITION
           END-IF
           IF RT-SALEABLE-COND = SPACES
               MOVE "ACCEPTED" TO RT-SALEABLE-COND
           END-IF
           IF RT-INSPECT-DAYS = 0
               MOVE 5 TO RT-INSPECT-DAYS
           END-IF
           IF RT-LINE-CNT > 20
               MOVE 20 TO RT-LINE-CNT
           END-IF
           IF WS-BDATE = 0
               MOVE 20261006 TO WS-BDATE
               MOVE WS-BDATE TO CA-BUSINESS-DATE OF DFHCOMMAREA
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 2000  REQUEST.  D-12 GIVES THE CUSTOMER 30 DAYS FROM DELIVERY
      *>       FOR NON FAULTY GOODS AND THE WARRANTY PERIOD FOR FAULTY
      *>       GOODS.  A LATE REQUEST IS STILL WRITTEN DOWN - REFUSING
      *>       TO RECORD IT WOULD LOSE THE EVIDENCE - BUT IT IS HELD
      *>       WITH REASON RET0004 UNTIL SOMEONE WITH AUTHORITY
      *>       APPROVES IT.
      *>----------------------------------------------------------------
       2000-REQUEST SECTION.
       2000-START.
           IF RT-CUSTOMER = SPACES
               MOVE "RET0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A return needs the customer it came from"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF RT-ORIG-ORDER = SPACES AND RT-ORIG-DESPATCH = SPACES
              AND RT-ORIG-INVOICE = SPACES
               MOVE "RET0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A return must quote the original order, "
                      "despatch or invoice it relates to"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF RT-LINE-CNT = 0
               MOVE "RET0001" TO WS-REASON
               MOVE "A return must list the goods" TO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF RT-REQUEST-DATE = 0
               MOVE WS-BDATE TO RT-REQUEST-DATE
           END-IF
           IF RT-REASON-CODE = SPACES
               MOVE "UNSTATED" TO RT-REASON-CODE
           END-IF
           PERFORM 2050-CHECK-LINES THRU 2050-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-WINDOW-TEST THRU 2100-EXIT
           PERFORM 2200-DRAW-NUMBER THRU 2200-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           PERFORM 2300-WRITE-REQUEST THRU 2300-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           MOVE "REQUESTED" TO RT-STATUS
           PERFORM 8700-AUDIT THRU 8700-EXIT
           IF RT-IN-WINDOW
               MOVE SPACES TO WS-MSG
               STRING "Return " FUNCTION TRIM(RT-RETURN-REF)
                      " requested; no stock has moved and no credit "
                      "has been raised"
                   DELIMITED BY SIZE INTO WS-MSG
               MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           END-IF.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2050-CHECK-LINES SECTION.
       2050-START.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > RT-LINE-CNT OR WS-ERROR = "Y"
               IF RT-L-LINE-NUM(WS-I) = 0
                   MOVE WS-I TO RT-L-LINE-NUM(WS-I)
               END-IF
               IF RT-L-ITEM(WS-I) = SPACES
                   MOVE "RET0001" TO WS-REASON
                   MOVE SPACES TO WS-MSG
                   MOVE WS-I TO WS-INT-ED
                   STRING "Return line " FUNCTION TRIM(WS-INT-ED)
                          " has no item code"
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM 9000-REJECT THRU 9000-EXIT
               END-IF
               IF RT-L-REQ-QTY(WS-I) <= 0
                   MOVE "RET0005" TO WS-REASON
                   MOVE SPACES TO WS-MSG
                   MOVE WS-I TO WS-INT-ED
                   STRING "Return line " FUNCTION TRIM(WS-INT-ED)
                          " must state a quantity above zero"
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM 9000-REJECT THRU 9000-EXIT
               END-IF
               MOVE "N" TO RT-L-SALEABLE(WS-I)
               MOVE 0 TO RT-L-MOVE-ID(WS-I)
           END-PERFORM.
       2050-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 2100  THE D-12 WINDOW.  NON FAULTY GOODS: 30 DAYS FROM
      *>       DELIVERY.  FAULTY GOODS: THE WARRANTY PERIOD, DEFAULT
      *>       12 MONTHS, FROM PARAMETER SO.RETURN.WARRANTY.MONTHS.
      *>       THE AGREEMENT MAY SAY OTHERWISE, WHICH IS WHY BOTH
      *>       LIMITS ARE PARAMETERS AND NOT CONSTANTS.
      *>----------------------------------------------------------------
       2100-WINDOW-TEST SECTION.
       2100-START.
           MOVE "Y" TO RT-WITHIN-WINDOW
           MOVE "N" TO RT-APPROVAL-NEEDED
           MOVE 0 TO RT-DAYS-SINCE-DEL
           IF RT-DELIVERY-DATE = 0
      *>       with no delivery date the window cannot be proved, so
      *>       the request is treated as needing authority
               MOVE "N" TO RT-WITHIN-WINDOW
               MOVE "RET0004" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "No delivery date is held, so the return "
                      "window cannot be evidenced"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 2150-WINDOW-OUTCOME THRU 2150-EXIT
               GO TO 2100-EXIT
           END-IF
           IF RT-FAULTY
               MOVE "Y" TO RT-WARRANTY-CLAIM
               PERFORM 2110-WARRANTY-MONTHS THRU 2110-EXIT
               PERFORM 2120-ADD-MONTHS THRU 2120-EXIT
           ELSE
               MOVE "N" TO RT-WARRANTY-CLAIM
               PERFORM 2115-WINDOW-DAYS THRU 2115-EXIT
               PERFORM 2125-ADD-DAYS THRU 2125-EXIT
           END-IF
           PERFORM 2130-DAYS-SINCE THRU 2130-EXIT
           IF RT-REQUEST-DATE > RT-WINDOW-END-DATE
               MOVE "N" TO RT-WITHIN-WINDOW
               MOVE "RET0004" TO WS-REASON
               MOVE SPACES TO WS-MSG
               MOVE RT-DAYS-SINCE-DEL TO WS-INT-ED
               IF RT-FAULTY
                   MOVE RT-WARRANTY-MONTHS TO WS-I
                   MOVE WS-I TO WS-DATE-ED
                   STRING "The goods were delivered "
                          FUNCTION TRIM(WS-INT-ED)
                          " days ago, outside the warranty period of "
                          FUNCTION TRIM(WS-DATE-ED)
                          " months; an approval is required"
                       DELIMITED BY SIZE INTO WS-MSG
               ELSE
                   MOVE RT-WINDOW-DAYS TO WS-I
                   MOVE WS-I TO WS-DATE-ED
                   STRING "The goods were delivered "
                          FUNCTION TRIM(WS-INT-ED)
                          " days ago, outside the "
                          FUNCTION TRIM(WS-DATE-ED)
                          " day window for non-faulty goods; an "
                          "approval is required"
                       DELIMITED BY SIZE INTO WS-MSG
               END-IF
               PERFORM 2150-WINDOW-OUTCOME THRU 2150-EXIT
           END-IF.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2110-WARRANTY-MONTHS SECTION.
       2110-START.
           IF RT-WARRANTY-MONTHS > 0
               GO TO 2110-EXIT
           END-IF
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE WS-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE WS-USER TO CA-USER OF WS-PARM-CA
           MOVE RT-REQUEST-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "SO.RETURN.WARRANTY.MONTHS" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "12" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           MOVE PM-VALUE-NUM TO WS-N
           MOVE WS-N TO RT-WARRANTY-MONTHS
           IF RT-WARRANTY-MONTHS <= 0
               MOVE 12 TO RT-WARRANTY-MONTHS
           END-IF.
       2110-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2115-WINDOW-DAYS SECTION.
       2115-START.
           IF RT-WINDOW-DAYS > 0
               GO TO 2115-EXIT
           END-IF
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE WS-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE WS-USER TO CA-USER OF WS-PARM-CA
           MOVE RT-REQUEST-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "SO.RETURN.WINDOW.DAYS" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "30" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           MOVE PM-VALUE-NUM TO WS-N
           MOVE WS-N TO RT-WINDOW-DAYS
           IF RT-WINDOW-DAYS <= 0
               MOVE 30 TO RT-WINDOW-DAYS
           END-IF.
       2115-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2120-ADD-MONTHS SECTION.
       2120-START.
           INITIALIZE WS-DATE-CA
           MOVE "ADDMONTH" TO CA-FUNCTION OF WS-DATE-CA
           MOVE WS-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE WS-USER TO CA-USER OF WS-DATE-CA
           MOVE RT-DELIVERY-DATE TO DC-DATE-1
           MOVE RT-WARRANTY-MONTHS TO DC-NUMBER
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA > 4
               MOVE RT-DELIVERY-DATE TO RT-WINDOW-END-DATE
           ELSE
               MOVE DC-RESULT-DATE TO RT-WINDOW-END-DATE
           END-IF.
       2120-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2125-ADD-DAYS SECTION.
       2125-START.
           INITIALIZE WS-DATE-CA
           MOVE "ADDDAYS" TO CA-FUNCTION OF WS-DATE-CA
           MOVE WS-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE WS-USER TO CA-USER OF WS-DATE-CA
           MOVE RT-DELIVERY-DATE TO DC-DATE-1
           MOVE RT-WINDOW-DAYS TO DC-NUMBER
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA > 4
               MOVE RT-DELIVERY-DATE TO RT-WINDOW-END-DATE
           ELSE
               MOVE DC-RESULT-DATE TO RT-WINDOW-END-DATE
           END-IF.
       2125-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2130-DAYS-SINCE SECTION.
       2130-START.
           INITIALIZE WS-DATE-CA
           MOVE "DAYSBTWN" TO CA-FUNCTION OF WS-DATE-CA
           MOVE WS-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE WS-USER TO CA-USER OF WS-DATE-CA
           MOVE RT-DELIVERY-DATE TO DC-DATE-1
           MOVE RT-REQUEST-DATE TO DC-DATE-2
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA > 4
               MOVE 0 TO RT-DAYS-SINCE-DEL
           ELSE
               MOVE DC-RESULT-NUM TO RT-DAYS-SINCE-DEL
           END-IF.
       2130-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 2150  OUTSIDE THE WINDOW.  WITH AN APPROVAL IT IS A WARNING
      *>       AND THE REQUEST PROCEEDS; WITHOUT ONE IT IS A HOLD.
      *>----------------------------------------------------------------
       2150-WINDOW-OUTCOME SECTION.
       2150-START.
           MOVE "Y" TO RT-APPROVAL-NEEDED
           IF RT-APPROVAL-ID > 0
               MOVE "N" TO RT-APPROVAL-NEEDED
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
           ELSE
               IF WS-RC < 6
                   MOVE 6 TO WS-RC
               END-IF
           END-IF.
       2150-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 2200  THE AUTHORISATION NUMBER.  SERIES SORMA IS THE SALES
      *>       RETURN MATERIAL AUTHORISATION SERIES; WHERE A COMPANY
      *>       STILL CARRIES THE OLDER SRMA SERIES THAT IS USED
      *>       INSTEAD, SO AN UNCONVERTED COMPANY IS NOT STOPPED.
      *>----------------------------------------------------------------
       2200-DRAW-NUMBER SECTION.
       2200-START.
           IF RT-RETURN-REF NOT = SPACES
               GO TO 2200-EXIT
           END-IF
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUM-CA
           MOVE WS-CO TO CA-COMPANY OF WS-NUM-CA
           MOVE WS-USER TO CA-USER OF WS-NUM-CA
           MOVE WS-CORR TO CA-CORRELATION-ID OF WS-NUM-CA
           MOVE RT-REQUEST-DATE TO CA-BUSINESS-DATE OF WS-NUM-CA
           MOVE "SRMA" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF CA-RETURN-CODE OF WS-NUM-CA > 4
               MOVE CA-REASON-CODE OF WS-NUM-CA TO WS-REASON
               MOVE CA-MESSAGE OF WS-NUM-CA TO WS-MSG
               MOVE "Y" TO WS-ERROR
               MOVE CA-RETURN-CODE OF WS-NUM-CA TO WS-RC
               GO TO 2200-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO RT-RETURN-REF.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2300-WRITE-REQUEST SECTION.
       2300-START.
           PERFORM 8600-BEGIN THRU 8600-EXIT
           IF WS-ERROR = "Y"
               GO TO 2300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.ReturnAuthorisation (CompanyCode, "
                  "SiteCode, ReturnRef, CustomerCode, "
                  "OriginalOrderNumber, OriginalDespatchRef, "
                  "OriginalInvoiceNumber, RequestedDate, ReasonCode, "
                  "Reason, IsFaulty, WithinReturnWindow, "
                  "WarrantyClaim, ReplacementDecision, Status, "
                  "ApprovalId, CreatedBy) VALUES (?, ?, ?, ?, "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), "
                  "CONVERT(date, ?, 112), ?, NULLIF(?,''), ?, ?, ?, "
                  "'NONE', 'REQUESTED', NULLIF(?,'0'), ?); "
                  "SELECT CAST(SCOPE_IDENTITY() AS varchar(24))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE RT-SITE TO SQLIO-PARM-VAL(2)
           MOVE RT-RETURN-REF TO SQLIO-PARM-VAL(3)
           MOVE RT-CUSTOMER TO SQLIO-PARM-VAL(4)
           MOVE RT-ORIG-ORDER TO SQLIO-PARM-VAL(5)
           MOVE RT-ORIG-DESPATCH TO SQLIO-PARM-VAL(6)
           MOVE RT-ORIG-INVOICE TO SQLIO-PARM-VAL(7)
           MOVE RT-REQUEST-DATE TO SQLIO-PARM-VAL(8)
           MOVE RT-REASON-CODE TO SQLIO-PARM-VAL(9)
           MOVE RT-REASON TO SQLIO-PARM-VAL(10)
           IF RT-FAULTY
               MOVE "1" TO SQLIO-PARM-VAL(11)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(11)
           END-IF
           IF RT-IN-WINDOW
               MOVE "1" TO SQLIO-PARM-VAL(12)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(12)
           END-IF
           IF RT-WARRANTY-CLAIM = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(13)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(13)
           END-IF
           MOVE RT-APPROVAL-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(14)
           MOVE WS-USER TO SQLIO-PARM-VAL(15)
           MOVE 15 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               PERFORM 8620-ROLLBACK THRU 8620-EXIT
               GO TO 2300-EXIT
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WS-C1)
           MOVE WS-N TO RT-RETURN-ID
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > RT-LINE-CNT OR WS-ERROR = "Y"
               PERFORM 2350-WRITE-LINE THRU 2350-EXIT
           END-PERFORM
           IF WS-ERROR = "Y"
               PERFORM 8620-ROLLBACK THRU 8620-EXIT
               GO TO 2300-EXIT
           END-IF
           PERFORM 8610-COMMIT THRU 8610-EXIT.
       2300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2350-WRITE-LINE SECTION.
       2350-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.ReturnLine (ReturnId, LineNum, "
                  "ItemCode, Revision, LotCode, SerialNo, "
                  "RequestedQty, AuthorisedQty, UomCode, "
                  "OriginalUnitCostGBP, CreditUnitPrice, VatCode) "
                  "VALUES (?, ?, ?, NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), ?, 0, NULLIF(?,''), "
                  "NULLIF(?,'0'), NULLIF(?,'0'), NULLIF(?,''))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE RT-RETURN-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE RT-L-LINE-NUM(WS-I) TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE RT-L-ITEM(WS-I) TO SQLIO-PARM-VAL(3)
           MOVE RT-L-REVISION(WS-I) TO SQLIO-PARM-VAL(4)
           MOVE RT-L-LOT(WS-I) TO SQLIO-PARM-VAL(5)
           MOVE RT-L-SERIAL(WS-I) TO SQLIO-PARM-VAL(6)
           MOVE RT-L-REQ-QTY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE RT-L-UOM(WS-I) TO SQLIO-PARM-VAL(8)
           MOVE RT-L-ORIG-COST(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE RT-L-CRED-PRICE(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE RT-L-VAT-CODE(WS-I) TO SQLIO-PARM-VAL(11)
           MOVE 11 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       2350-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 3000  AUTHORISE.  QUANTITIES ARE AGREED AND THE REFERENCE IS
      *>       RELEASED TO THE CUSTOMER.  NOTHING PHYSICAL AND NOTHING
      *>       FINANCIAL HAPPENS HERE EITHER (SO-017).
      *>----------------------------------------------------------------
       3000-AUTHORISE SECTION.
       3000-START.
           PERFORM 8000-LOAD-HEADER THRU 8000-EXIT
           IF WS-ERROR = "Y"
               GO TO 3000-EXIT
           END-IF
           IF RT-STATUS NOT = "REQUESTED"
               MOVE "RET0003" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Return " FUNCTION TRIM(RT-RETURN-REF)
                      " is " FUNCTION TRIM(RT-STATUS)
                      " and can no longer be authorised"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           IF RT-WITHIN-WINDOW = "N" AND RT-APPROVAL-ID = 0
               MOVE "RET0004" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "This return was requested outside the "
                      "permitted window and needs an approval "
                      "before it can be authorised"
                   DELIMITED BY SIZE INTO WS-MSG
               MOVE 6 TO WS-RC
               MOVE "Y" TO WS-ERROR
               GO TO 3000-EXIT
           END-IF
           PERFORM 8100-LOAD-LINES THRU 8100-EXIT
           IF WS-ERROR = "Y"
               GO TO 3000-EXIT
           END-IF
           IF RT-AUTH-DATE = 0
               MOVE WS-BDATE TO RT-AUTH-DATE
           END-IF
           IF RT-AUTHORISED-BY = SPACES
               MOVE WS-USER TO RT-AUTHORISED-BY
           END-IF
           PERFORM 8600-BEGIN THRU 8600-EXIT
           IF WS-ERROR = "Y"
               GO TO 3000-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > RT-LINE-CNT OR WS-ERROR = "Y"
               IF RT-L-AUTH-QTY(WS-I) <= 0
                   MOVE RT-L-REQ-QTY(WS-I) TO RT-L-AUTH-QTY(WS-I)
               END-IF
               IF RT-L-AUTH-QTY(WS-I) > RT-L-REQ-QTY(WS-I)
                   MOVE "RET0005" TO WS-REASON
                   MOVE SPACES TO WS-MSG
                   MOVE RT-L-LINE-NUM(WS-I) TO WS-INT-ED
                   STRING "Line " FUNCTION TRIM(WS-INT-ED)
                          " cannot authorise more than was requested"
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM 9000-REJECT THRU 9000-EXIT
               ELSE
                   MOVE SPACES TO SQLIO-STMT
                   STRING "UPDATE so.ReturnLine SET AuthorisedQty "
                          "= ? WHERE ReturnId = ? AND LineNum = ?"
                       DELIMITED BY SIZE INTO SQLIO-STMT
                   MOVE RT-L-AUTH-QTY(WS-I) TO SQLIO-NUM-EDIT
                   MOVE FUNCTION TRIM(SQLIO-NUM-EDIT)
                       TO SQLIO-PARM-VAL(1)
                   MOVE RT-RETURN-ID TO WS-INT-ED
                   MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(2)
                   MOVE RT-L-LINE-NUM(WS-I) TO WS-INT-ED
                   MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
                   MOVE 3 TO SQLIO-PARM-CNT
                   PERFORM 8650-NO-NULLS THRU 8650-EXIT
                   PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
               END-IF
           END-PERFORM
           IF WS-ERROR = "Y"
               PERFORM 8620-ROLLBACK THRU 8620-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.ReturnAuthorisation SET Status = "
                  "'AUTHORISED', AuthorisedDate = CONVERT(date, ?, "
                  "112), AuthorisedBy = ?, ApprovalId = "
                  "NULLIF(?,'0'), ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE ReturnId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE RT-AUTH-DATE TO SQLIO-PARM-VAL(1)
           MOVE RT-AUTHORISED-BY TO SQLIO-PARM-VAL(2)
           MOVE RT-APPROVAL-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE WS-USER TO SQLIO-PARM-VAL(4)
           MOVE RT-RETURN-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(5)
           MOVE 5 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-ERROR = "Y"
               PERFORM 8620-ROLLBACK THRU 8620-EXIT
               GO TO 3000-EXIT
           END-IF
           PERFORM 8610-COMMIT THRU 8610-EXIT
           MOVE "AUTHORISED" TO RT-STATUS
           PERFORM 8700-AUDIT THRU 8700-EXIT
           MOVE SPACES TO WS-MSG
           STRING "Return " FUNCTION TRIM(RT-RETURN-REF)
                  " is authorised; the goods have not been received "
                  "and no credit has been raised"
               DELIMITED BY SIZE INTO WS-MSG
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 4000  RECEIVE.  THE GOODS COME BACK INTO AN INSPECTION
      *>       CONDITION AT THE RETURNS LOCATION AND ARE IMMEDIATELY
      *>       PUT ON A QUARANTINE HOLD.  THEY ARE OWNED BUT THEY ARE
      *>       NOT AVAILABLE TO SELL, SO NOTHING CAN BE PROMISED FROM
      *>       THEM UNTIL A DISPOSITION SAYS SO (SO-017).
      *>----------------------------------------------------------------
       4000-RECEIVE SECTION.
       4000-START.
           PERFORM 8000-LOAD-HEADER THRU 8000-EXIT
           IF WS-ERROR = "Y"
               GO TO 4000-EXIT
           END-IF
           IF RT-STATUS NOT = "AUTHORISED" AND RT-STATUS NOT =
              "RECEIVED"
               MOVE "RET0003" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Goods may only be received against an "
                      "authorised return; " FUNCTION TRIM(
                      RT-RETURN-REF) " is "
                      FUNCTION TRIM(RT-STATUS)
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           PERFORM 8100-LOAD-LINES THRU 8100-EXIT
           IF WS-ERROR = "Y"
               GO TO 4000-EXIT
           END-IF
           IF RT-RECEIVED-DATE = 0
               MOVE WS-BDATE TO RT-RECEIVED-DATE
           END-IF
           PERFORM 4050-INSPECT-DUE THRU 4050-EXIT
           MOVE 0 TO RT-QTY-ON-HOLD
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > RT-LINE-CNT OR WS-ERROR = "Y"
               IF RT-L-RECV-QTY(WS-I) > 0
                   PERFORM 4100-RECEIVE-LINE THRU 4100-EXIT
               END-IF
           END-PERFORM
           IF WS-ERROR = "Y"
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.ReturnAuthorisation SET Status = "
                  "'RECEIVED', ReceivedDate = CONVERT(date, ?, 112), "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE ReturnId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE RT-RECEIVED-DATE TO SQLIO-PARM-VAL(1)
           MOVE WS-USER TO SQLIO-PARM-VAL(2)
           MOVE RT-RETURN-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           MOVE "RECEIVED" TO RT-STATUS
           PERFORM 8700-AUDIT THRU 8700-EXIT
           MOVE RT-QTY-ON-HOLD TO WS-QTY-ED
           MOVE SPACES TO WS-MSG
           STRING "Received " FUNCTION TRIM(WS-QTY-ED)
                  " on quarantine hold; the goods are unavailable "
                  "until a disposition is recorded"
               DELIMITED BY SIZE INTO WS-MSG
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4050-INSPECT-DUE SECTION.
       4050-START.
           INITIALIZE WS-DATE-CA
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DATE-CA
           MOVE WS-CO TO CA-COMPANY OF WS-DATE-CA
           MOVE WS-USER TO CA-USER OF WS-DATE-CA
           MOVE RT-RECEIVED-DATE TO DC-DATE-1
           MOVE RT-INSPECT-DAYS TO DC-NUMBER
           MOVE "ENG" TO DC-CALENDAR
           CALL "DATECALC" USING WS-DATE-CA
           IF CA-RETURN-CODE OF WS-DATE-CA > 4
               MOVE RT-RECEIVED-DATE TO RT-INSPECT-DUE
           ELSE
               MOVE DC-RESULT-DATE TO RT-INSPECT-DUE
           END-IF.
       4050-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4100-RECEIVE-LINE SECTION.
       4100-START.
           IF RT-L-AUTH-QTY(WS-I) > 0 AND
              RT-L-RECV-QTY(WS-I) > RT-L-AUTH-QTY(WS-I)
               MOVE "RET0005" TO WS-REASON
               MOVE SPACES TO WS-MSG
               MOVE RT-L-LINE-NUM(WS-I) TO WS-INT-ED
               STRING "Line " FUNCTION TRIM(WS-INT-ED)
                      " received more than was authorised"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4100-EXIT
           END-IF
           PERFORM 8300-MOVE-INTO-RETURNS THRU 8300-EXIT
           IF WS-ERROR = "Y"
               GO TO 4100-EXIT
           END-IF
           PERFORM 8350-OPEN-QUALITY-HOLD THRU 8350-EXIT
           IF WS-ERROR = "Y"
               GO TO 4100-EXIT
           END-IF
           ADD RT-L-RECV-QTY(WS-I) TO RT-QTY-ON-HOLD
           MOVE "N" TO RT-L-SALEABLE(WS-I)
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.ReturnLine SET ReceivedQty = "
                  "ReceivedQty + ?, MovementId = NULLIF(?,'0'), "
                  "QualityHoldId = NULLIF(?,'0') WHERE ReturnId = ? "
                  "AND LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE RT-L-RECV-QTY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE RT-L-MOVE-ID(WS-I) TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE RT-L-HOLD-ID(WS-I) TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE RT-RETURN-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE RT-L-LINE-NUM(WS-I) TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(5)
           MOVE 5 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       4100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 5000  INSPECTION.  D-12 WANTS IT WITHIN FIVE WORKING DAYS OF
      *>       RECEIPT.  EACH LINE IS SPLIT INTO ACCEPTED AND REJECTED
      *>       QUANTITY AND GIVEN A DISPOSITION.  ONLY RESTOCK WILL
      *>       LATER RETURN GOODS TO SALEABLE STOCK.
      *>----------------------------------------------------------------
       5000-INSPECTION SECTION.
       5000-START.
           PERFORM 8000-LOAD-HEADER THRU 8000-EXIT
           IF WS-ERROR = "Y"
               GO TO 5000-EXIT
           END-IF
           IF RT-STATUS NOT = "RECEIVED" AND RT-STATUS NOT =
              "INSPECTED"
               MOVE "RET0003" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Only received goods can be inspected; "
                      FUNCTION TRIM(RT-RETURN-REF) " is "
                      FUNCTION TRIM(RT-STATUS)
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           PERFORM 8100-LOAD-LINES THRU 8100-EXIT
           IF WS-ERROR = "Y"
               GO TO 5000-EXIT
           END-IF
           IF RT-INSPECT-DATE = 0
               MOVE WS-BDATE TO RT-INSPECT-DATE
           END-IF
           IF RT-RECEIVED-DATE NOT = 0
               PERFORM 4050-INSPECT-DUE THRU 4050-EXIT
               IF RT-INSPECT-DATE > RT-INSPECT-DUE
                   MOVE "RET0008" TO WS-REASON
                   MOVE RT-INSPECT-DUE TO WS-DATE-ED
                   MOVE SPACES TO WS-MSG
                   STRING "The inspection was due by "
                          FUNCTION TRIM(WS-DATE-ED)
                          "; it is recorded late for review"
                       DELIMITED BY SIZE INTO WS-MSG
                   IF WS-RC < 4
                       MOVE 4 TO WS-RC
                   END-IF
               END-IF
           END-IF
           MOVE 0 TO RT-TOTAL-VALUE-GBP
           MOVE 0 TO RT-TOTAL-VAR-GBP
           PERFORM 8600-BEGIN THRU 8600-EXIT
           IF WS-ERROR = "Y"
               GO TO 5000-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > RT-LINE-CNT OR WS-ERROR = "Y"
               PERFORM 5100-INSPECT-LINE THRU 5100-EXIT
           END-PERFORM
           IF WS-ERROR = "Y"
               PERFORM 8620-ROLLBACK THRU 8620-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.ReturnAuthorisation SET Status = "
                  "'INSPECTED', InspectedDate = CONVERT(date, ?, "
                  "112), ReplacementDecision = ?, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE ReturnId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE RT-INSPECT-DATE TO SQLIO-PARM-VAL(1)
           IF RT-REPLACE-DECISION = SPACES
               MOVE "NONE" TO RT-REPLACE-DECISION
           END-IF
           MOVE RT-REPLACE-DECISION TO SQLIO-PARM-VAL(2)
           MOVE WS-USER TO SQLIO-PARM-VAL(3)
           MOVE RT-RETURN-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE 4 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-ERROR = "Y"
               PERFORM 8620-ROLLBACK THRU 8620-EXIT
               GO TO 5000-EXIT
           END-IF
           PERFORM 8610-COMMIT THRU 8610-EXIT
           MOVE "INSPECTED" TO RT-STATUS
           PERFORM 8700-AUDIT THRU 8700-EXIT
           MOVE SPACES TO WS-MSG
           STRING "Inspection recorded; a credit note may now be "
                  "raised through SOCREDIT if one is due"
               DELIMITED BY SIZE INTO WS-MSG
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       5100-INSPECT-LINE SECTION.
       5100-START.
           IF RT-L-DISPOSITION(WS-I) = SPACES
               MOVE "HOLD" TO RT-L-DISPOSITION(WS-I)
           END-IF
           EVALUATE RT-L-DISPOSITION(WS-I)
               WHEN "RESTOCK"    CONTINUE
               WHEN "REWORK"     CONTINUE
               WHEN "SUPPRETURN" CONTINUE
               WHEN "SCRAP"      CONTINUE
               WHEN "HOLD"       CONTINUE
               WHEN OTHER
                   MOVE "RET0006" TO WS-REASON
                   MOVE SPACES TO WS-MSG
                   STRING "Disposition "
                          FUNCTION TRIM(RT-L-DISPOSITION(WS-I))
                          " is not one of RESTOCK, REWORK, "
                          "SUPPRETURN, SCRAP or HOLD"
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM 9000-REJECT THRU 9000-EXIT
                   GO TO 5100-EXIT
           END-EVALUATE
           COMPUTE WS-QTY1 = RT-L-ACC-QTY(WS-I) + RT-L-REJ-QTY(WS-I)
           IF WS-QTY1 = 0
               MOVE RT-L-RECV-QTY(WS-I) TO RT-L-ACC-QTY(WS-I)
               MOVE RT-L-RECV-QTY(WS-I) TO WS-QTY1
           END-IF
           IF WS-QTY1 > RT-L-RECV-QTY(WS-I)
               MOVE "RET0005" TO WS-REASON
               MOVE SPACES TO WS-MSG
               MOVE RT-L-LINE-NUM(WS-I) TO WS-INT-ED
               STRING "Line " FUNCTION TRIM(WS-INT-ED)
                      " inspected more than was received"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5100-EXIT
           END-IF
      *>   value at the original unit cost; a different current cost
      *>   is a variance, it is never hidden inside the return value
           COMPUTE WS-AMT1 ROUNDED =
               RT-L-ACC-QTY(WS-I) * RT-L-ORIG-COST(WS-I)
           MOVE WS-AMT1 TO RT-L-VALUE-GBP(WS-I)
           IF RT-L-CURR-COST(WS-I) > 0
               COMPUTE WS-AMT2 ROUNDED = RT-L-ACC-QTY(WS-I) *
                   (RT-L-CURR-COST(WS-I) - RT-L-ORIG-COST(WS-I))
           ELSE
               MOVE 0 TO WS-AMT2
           END-IF
           MOVE WS-AMT2 TO RT-L-VAR-GBP(WS-I)
           ADD WS-AMT1 TO RT-TOTAL-VALUE-GBP
           ADD WS-AMT2 TO RT-TOTAL-VAR-GBP
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.ReturnLine SET AcceptedQty = ?, "
                  "RejectedQty = ?, Disposition = ?, "
                  "ReturnValueGBP = ?, VarianceGBP = ? "
                  "WHERE ReturnId = ? AND LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE RT-L-ACC-QTY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE RT-L-REJ-QTY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE RT-L-DISPOSITION(WS-I) TO SQLIO-PARM-VAL(3)
           MOVE RT-L-VALUE-GBP(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE RT-L-VAR-GBP(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE RT-RETURN-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(6)
           MOVE RT-L-LINE-NUM(WS-I) TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(7)
           MOVE 7 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       5100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 6000  DISPOSE.  THE QUARANTINE HOLD IS CLOSED WITH ITS
      *>       OUTCOME.  ONLY RESTOCK RELEASES THE GOODS BACK INTO THE
      *>       SALEABLE CONDITION, AT THE ORIGINAL UNIT COST.  REWORK
      *>       AND SUPPRETURN LEAVE THEM REJECTED FOR THE KIT OR
      *>       PURCHASING TEAM; SCRAP ALSO REMOVES THEM; HOLD LEAVES
      *>       THEM EXACTLY WHERE THEY ARE.
      *>----------------------------------------------------------------
       6000-DISPOSE SECTION.
       6000-START.
           PERFORM 8000-LOAD-HEADER THRU 8000-EXIT
           IF WS-ERROR = "Y"
               GO TO 6000-EXIT
           END-IF
           IF RT-STATUS NOT = "INSPECTED"
               MOVE "RET0010" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "The goods may not be moved before the return "
                      "has been inspected and a disposition agreed"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 6000-EXIT
           END-IF
           PERFORM 8100-LOAD-LINES THRU 8100-EXIT
           IF WS-ERROR = "Y"
               GO TO 6000-EXIT
           END-IF
           MOVE 0 TO RT-QTY-TO-SALEABLE
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > RT-LINE-CNT OR WS-ERROR = "Y"
               IF RT-L-ACC-QTY(WS-I) > 0 OR RT-L-REJ-QTY(WS-I) > 0
                   PERFORM 6100-DISPOSE-LINE THRU 6100-EXIT
               END-IF
           END-PERFORM
           IF WS-ERROR = "Y"
               GO TO 6000-EXIT
           END-IF
           MOVE RT-QTY-TO-SALEABLE TO WS-QTY-ED
           MOVE SPACES TO WS-MSG
           STRING "Dispositions carried out; "
                  FUNCTION TRIM(WS-QTY-ED)
                  " returned to saleable stock at the original cost"
               DELIMITED BY SIZE INTO WS-MSG
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           PERFORM 8700-AUDIT THRU 8700-EXIT.
       6000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       6100-DISPOSE-LINE SECTION.
       6100-START.
           EVALUATE RT-L-DISPOSITION(WS-I)
               WHEN "RESTOCK"
                   MOVE "ACCEPT" TO SM-HOLD-OUTCOME
                   PERFORM 8400-RELEASE-HOLD THRU 8400-EXIT
                   IF WS-ERROR = "N"
                       MOVE "Y" TO RT-L-SALEABLE(WS-I)
                       ADD RT-L-ACC-QTY(WS-I) TO RT-QTY-TO-SALEABLE
                       MOVE SPACES TO RT-L-MESSAGE(WS-I)
                       STRING "Returned to saleable stock at the "
                              "original unit cost"
                           DELIMITED BY SIZE
                           INTO RT-L-MESSAGE(WS-I)
                   END-IF
               WHEN "SCRAP"
                   MOVE "SCRAP" TO SM-HOLD-OUTCOME
                   PERFORM 8400-RELEASE-HOLD THRU 8400-EXIT
                   IF WS-ERROR = "N"
                       PERFORM 8450-SCRAP THRU 8450-EXIT
                   END-IF
                   MOVE "N" TO RT-L-SALEABLE(WS-I)
               WHEN "SUPPRETURN"
                   MOVE "REJECT" TO SM-HOLD-OUTCOME
                   PERFORM 8400-RELEASE-HOLD THRU 8400-EXIT
                   MOVE "N" TO RT-L-SALEABLE(WS-I)
                   MOVE SPACES TO RT-L-MESSAGE(WS-I)
                   STRING "Held as rejected for a supplier return; "
                          "a supplier RMA is still required"
                       DELIMITED BY SIZE INTO RT-L-MESSAGE(WS-I)
               WHEN "REWORK"
                   MOVE "REJECT" TO SM-HOLD-OUTCOME
                   PERFORM 8400-RELEASE-HOLD THRU 8400-EXIT
                   MOVE "N" TO RT-L-SALEABLE(WS-I)
                   MOVE SPACES TO RT-L-MESSAGE(WS-I)
                   STRING "Held as rejected for rework as a new "
                          "execution kit"
                       DELIMITED BY SIZE INTO RT-L-MESSAGE(WS-I)
               WHEN OTHER
                   MOVE "N" TO RT-L-SALEABLE(WS-I)
                   MOVE SPACES TO RT-L-MESSAGE(WS-I)
                   STRING "Left on quarantine hold; not available "
                          "to sell"
                       DELIMITED BY SIZE INTO RT-L-MESSAGE(WS-I)
           END-EVALUATE.
       6100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 7000  CLOSE.  THE RETURN ENDS HERE.  THE ORIGINAL ORDER IS
      *>       DELIBERATELY LEFT ALONE: SO-017 SAYS A RETURN DOES NOT
      *>       AUTOMATICALLY REOPEN IT, AND THE REMAINING DEMAND OF
      *>       SECTION 8 OF THE SPECIFICATION IS UNCHANGED BY IT.
      *>----------------------------------------------------------------
       7000-CLOSE SECTION.
       7000-START.
           PERFORM 8000-LOAD-HEADER THRU 8000-EXIT
           IF WS-ERROR = "Y"
               GO TO 7000-EXIT
           END-IF
           IF RT-STATUS = "CLOSED"
               MOVE 4 TO WS-RC
               MOVE "RET0003" TO WS-REASON
               MOVE "That return is already closed" TO WS-MSG
               GO TO 7000-EXIT
           END-IF
           IF RT-CLOSED-DATE = 0
               MOVE WS-BDATE TO RT-CLOSED-DATE
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.ReturnAuthorisation SET Status = "
                  "'CLOSED', ReplacementDecision = ?, "
                  "ReplacementOrderNumber = NULLIF(?,''), "
                  "CreditNoteNumber = NULLIF(?,''), ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE ReturnId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           IF RT-REPLACE-DECISION = SPACES
               MOVE "NONE" TO RT-REPLACE-DECISION
           END-IF
           MOVE RT-REPLACE-DECISION TO SQLIO-PARM-VAL(1)
           MOVE RT-REPLACE-ORDER TO SQLIO-PARM-VAL(2)
           MOVE RT-CREDIT-NOTE TO SQLIO-PARM-VAL(3)
           MOVE WS-USER TO SQLIO-PARM-VAL(4)
           MOVE RT-RETURN-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(5)
           MOVE 5 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-ERROR = "Y"
               GO TO 7000-EXIT
           END-IF
           MOVE "CLOSED" TO RT-STATUS
           MOVE "N" TO RT-ORDER-REOPENED
           PERFORM 8700-AUDIT THRU 8700-EXIT
           MOVE SPACES TO WS-MSG
           STRING "Return " FUNCTION TRIM(RT-RETURN-REF)
                  " is closed; order "
                  FUNCTION TRIM(RT-ORIG-ORDER)
                  " has not been reopened"
               DELIMITED BY SIZE INTO WS-MSG
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA.
       7000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8000-LOAD-HEADER SECTION.
       8000-START.
           IF RT-RETURN-REF = SPACES
               MOVE "RET0002" TO WS-REASON
               MOVE "A return reference is required" TO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 8000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ReturnId AS varchar(24)), "
                  "CustomerCode, Status, CONVERT(char(8), "
                  "RequestedDate, 112), ISNULL(CONVERT(char(8), "
                  "ReceivedDate, 112),''), SiteCode, "
                  "ISNULL(OriginalOrderNumber,''), CAST(IsFaulty AS "
                  "char(1)), CAST(WithinReturnWindow AS char(1)), "
                  "ISNULL(CAST(ApprovalId AS varchar(24)),'0'), "
                  "ISNULL(CreditNoteNumber,''), "
                  "ISNULL(OriginalDespatchRef,''), "
                  "ISNULL(OriginalInvoiceNumber,'') "
                  "FROM so.ReturnAuthorisation WHERE CompanyCode = ? "
                  "AND ReturnRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE RT-RETURN-REF TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 13 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 8 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 10 TO SQLIO-COL-WIDTH(6)
           MOVE 30 TO SQLIO-COL-WIDTH(7)
           MOVE 2 TO SQLIO-COL-WIDTH(8)
           MOVE 2 TO SQLIO-COL-WIDTH(9)
           MOVE 24 TO SQLIO-COL-WIDTH(10)
           MOVE 30 TO SQLIO-COL-WIDTH(11)
           MOVE 30 TO SQLIO-COL-WIDTH(12)
           MOVE 30 TO SQLIO-COL-WIDTH(13)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-HEAD-ROW
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 8000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO WS-RC
               MOVE "Y" TO WS-ERROR
               MOVE "RET0002" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Return " FUNCTION TRIM(RT-RETURN-REF)
                      " is not held for this company"
                   DELIMITED BY SIZE INTO WS-MSG
               GO TO 8000-EXIT
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WH-RETURN-ID)
           MOVE WS-N TO RT-RETURN-ID
           MOVE WH-CUSTOMER TO RT-CUSTOMER
           MOVE WH-STATUS TO RT-STATUS
           MOVE WH-SITE TO RT-SITE
           MOVE WH-ORDER TO RT-ORIG-ORDER
           MOVE WH-DESPATCH TO RT-ORIG-DESPATCH
           MOVE WH-INVOICE TO RT-ORIG-INVOICE
           MOVE WH-CREDIT-NOTE TO RT-CREDIT-NOTE
           IF WH-REQ-DATE NOT = SPACES
               COMPUTE WS-N = FUNCTION NUMVAL(WH-REQ-DATE)
               MOVE WS-N TO RT-REQUEST-DATE
           END-IF
           IF WH-RECV-DATE NOT = SPACES
               COMPUTE WS-N = FUNCTION NUMVAL(WH-RECV-DATE)
               MOVE WS-N TO RT-RECEIVED-DATE
           END-IF
           IF WH-FAULTY(1:1) = "1"
               MOVE "Y" TO RT-IS-FAULTY
           ELSE
               MOVE "N" TO RT-IS-FAULTY
           END-IF
           IF WH-WINDOW(1:1) = "1"
               MOVE "Y" TO RT-WITHIN-WINDOW
           ELSE
               MOVE "N" TO RT-WITHIN-WINDOW
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WH-APPROVAL)
           IF RT-APPROVAL-ID = 0
               MOVE WS-N TO RT-APPROVAL-ID
           END-IF.
       8000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 8100  THE HELD LINES ARE READ AND MERGED WITH WHATEVER THE
      *>       CALLER SUPPLIED, SO A CALLER MAY NAME ONLY THE LINES IT
      *>       IS ACTING ON AND STILL GET A COMPLETE ANSWER BACK.
      *>----------------------------------------------------------------
       8100-LOAD-LINES SECTION.
       8100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(LineNum AS varchar(12)), ItemCode, "
                  "ISNULL(Revision,''), ISNULL(LotCode,''), "
                  "ISNULL(SerialNo,''), ISNULL(UomCode,''), "
                  "CAST(RequestedQty AS varchar(24)), "
                  "CAST(AuthorisedQty AS varchar(24)), "
                  "CAST(ReceivedQty AS varchar(24)), "
                  "CAST(AcceptedQty AS varchar(24)), "
                  "CAST(RejectedQty AS varchar(24)), "
                  "CAST(CreditedQty AS varchar(24)), "
                  "ISNULL(Disposition,''), "
                  "ISNULL(CAST(OriginalUnitCostGBP AS "
                  "varchar(24)),'0'), ISNULL(CAST(QualityHoldId AS "
                  "varchar(24)),'0'), ISNULL(VatCode,'') "
                  "FROM so.ReturnLine WHERE ReturnId = ? "
                  "ORDER BY LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE RT-RETURN-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 16 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 4 TO SQLIO-COL-WIDTH(3)
           MOVE 30 TO SQLIO-COL-WIDTH(4)
           MOVE 40 TO SQLIO-COL-WIDTH(5)
           MOVE 10 TO SQLIO-COL-WIDTH(6)
           PERFORM VARYING WS-J FROM 7 BY 1 UNTIL WS-J > 12
               MOVE 24 TO SQLIO-COL-WIDTH(WS-J)
           END-PERFORM
           MOVE 12 TO SQLIO-COL-WIDTH(13)
           MOVE 24 TO SQLIO-COL-WIDTH(14)
           MOVE 24 TO SQLIO-COL-WIDTH(15)
           MOVE 4 TO SQLIO-COL-WIDTH(16)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 8100-EXIT
           END-IF
           MOVE 0 TO WS-LINES-READ
           PERFORM 8150-FETCH-LINE THRU 8150-EXIT
               UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
                     OR WS-LINES-READ >= 20
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           IF WS-LINES-READ > 0
               MOVE WS-LINES-READ TO RT-LINE-CNT
           END-IF.
       8100-EXIT.
           EXIT.

       8150-FETCH-LINE SECTION.
       8150-START.
           MOVE 2 TO SQLIO-CURSOR
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               GO TO 8150-EXIT
           END-IF
           ADD 1 TO WS-LINES-READ
           MOVE WS-LINES-READ TO WS-J
           COMPUTE WS-N = FUNCTION NUMVAL(WL-LINE-NUM)
           MOVE WS-N TO RT-L-LINE-NUM(WS-J)
           MOVE WL-ITEM TO RT-L-ITEM(WS-J)
           MOVE WL-REVISION TO RT-L-REVISION(WS-J)
           MOVE WL-LOT TO RT-L-LOT(WS-J)
           MOVE WL-SERIAL TO RT-L-SERIAL(WS-J)
           MOVE WL-UOM TO RT-L-UOM(WS-J)
           COMPUTE WS-N = FUNCTION NUMVAL(WL-REQ-QTY)
           MOVE WS-N TO RT-L-REQ-QTY(WS-J)
           COMPUTE WS-N = FUNCTION NUMVAL(WL-AUTH-QTY)
           IF RT-L-AUTH-QTY(WS-J) = 0 OR WS-FUNC NOT = "AUTHORIS"
               MOVE WS-N TO RT-L-AUTH-QTY(WS-J)
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WL-RECV-QTY)
           IF WS-FUNC NOT = "RECEIVE"
               MOVE WS-N TO RT-L-RECV-QTY(WS-J)
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WL-ACC-QTY)
           IF WS-FUNC NOT = "INSPECT"
               MOVE WS-N TO RT-L-ACC-QTY(WS-J)
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WL-REJ-QTY)
           IF WS-FUNC NOT = "INSPECT"
               MOVE WS-N TO RT-L-REJ-QTY(WS-J)
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WL-CRED-QTY)
           MOVE WS-N TO RT-L-CRED-QTY(WS-J)
           IF RT-L-DISPOSITION(WS-J) = SPACES
               MOVE WL-DISPOSITION TO RT-L-DISPOSITION(WS-J)
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WL-ORIG-COST)
           IF RT-L-ORIG-COST(WS-J) = 0
               MOVE WS-N TO RT-L-ORIG-COST(WS-J)
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WL-HOLD-ID)
           IF RT-L-HOLD-ID(WS-J) = 0
               MOVE WS-N TO RT-L-HOLD-ID(WS-J)
           END-IF
           IF RT-L-VAT-CODE(WS-J) = SPACES
               MOVE WL-VAT-CODE TO RT-L-VAT-CODE(WS-J)
           END-IF.
       8150-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 8300  THE PHYSICAL RECEIPT.  THE GOODS COME BACK IN AN
      *>       INSPECTION CONDITION AT THE RETURNS LOCATION, WHICH IS
      *>       NOT FREE STOCK.  STKMOVE OWNS THE BALANCE, THE MOVEMENT
      *>       AND ITS EVIDENCE (ITM-015).
      *>----------------------------------------------------------------
       8300-MOVE-INTO-RETURNS SECTION.
       8300-START.
           INITIALIZE WS-MOVE-CA
           MOVE "RETURN" TO CA-FUNCTION OF WS-MOVE-CA
           MOVE WS-CO TO CA-COMPANY OF WS-MOVE-CA
           MOVE RT-SITE TO CA-SITE OF WS-MOVE-CA
           MOVE WS-USER TO CA-USER OF WS-MOVE-CA
           MOVE RT-RECEIVED-DATE TO CA-BUSINESS-DATE OF WS-MOVE-CA
           MOVE SPACES TO WS-SUB-SUFFIX
           MOVE RT-L-LINE-NUM(WS-I) TO WS-SUB-SEQ
           STRING "-RCV" WS-SUB-SEQ DELIMITED BY SIZE
               INTO WS-SUB-SUFFIX
           PERFORM 9700-SUB-CORR THRU 9700-EXIT
           MOVE WS-SUB-CORR TO CA-CORRELATION-ID OF WS-MOVE-CA
           MOVE RT-L-ITEM(WS-I) TO SM-ITEM-CODE
           MOVE RT-L-REVISION(WS-I) TO SM-REVISION
           MOVE RT-SITE TO SM-SITE
           MOVE RT-WAREHOUSE TO SM-WAREHOUSE
           MOVE RT-QUARANTINE-LOC TO SM-LOCATION
           MOVE RT-L-LOT(WS-I) TO SM-LOT-CODE
           MOVE RT-L-SERIAL(WS-I) TO SM-SERIAL-NO
           MOVE "INSPECTION" TO SM-CONDITION
           MOVE RT-L-RECV-QTY(WS-I) TO SM-QUANTITY
           MOVE RT-L-UOM(WS-I) TO SM-UOM-CODE
           MOVE RT-L-ORIG-COST(WS-I) TO SM-UNIT-COST
           MOVE RT-RECEIVED-DATE TO SM-MOVEMENT-DATE
           MOVE RT-RECEIVED-DATE TO SM-POSTING-DATE
           MOVE "CUSTRET" TO SM-REASON-CODE
           MOVE SPACES TO SM-REASON
           STRING "Customer return " FUNCTION TRIM(RT-RETURN-REF)
               DELIMITED BY SIZE INTO SM-REASON
           MOVE "SO" TO SM-SOURCE-MODULE
           MOVE "RETURNAUTH" TO SM-SOURCE-DOC-TYPE
           MOVE RT-RETURN-REF TO SM-SOURCE-DOC-REF
           MOVE RT-L-LINE-NUM(WS-I) TO SM-SOURCE-DOC-LINE
           CALL "STKMOVE" USING WS-MOVE-CA
           IF CA-RETURN-CODE OF WS-MOVE-CA > 4
               MOVE "RET0007" TO WS-REASON
               MOVE CA-MESSAGE OF WS-MOVE-CA TO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 8300-EXIT
           END-IF
           MOVE SM-MOVEMENT-ID TO RT-L-MOVE-ID(WS-I)
           MOVE "Y" TO RT-STOCK-MOVED
           ADD 1 TO RT-MOVEMENT-COUNT.
       8300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8350-OPEN-QUALITY-HOLD SECTION.
       8350-START.
           INITIALIZE WS-MOVE-CA
           MOVE "HOLD" TO CA-FUNCTION OF WS-MOVE-CA
           MOVE WS-CO TO CA-COMPANY OF WS-MOVE-CA
           MOVE RT-SITE TO CA-SITE OF WS-MOVE-CA
           MOVE WS-USER TO CA-USER OF WS-MOVE-CA
           MOVE RT-RECEIVED-DATE TO CA-BUSINESS-DATE OF WS-MOVE-CA
           MOVE SPACES TO WS-SUB-SUFFIX
           MOVE RT-L-LINE-NUM(WS-I) TO WS-SUB-SEQ
           STRING "-HLD" WS-SUB-SEQ DELIMITED BY SIZE
               INTO WS-SUB-SUFFIX
           PERFORM 9700-SUB-CORR THRU 9700-EXIT
           MOVE WS-SUB-CORR TO CA-CORRELATION-ID OF WS-MOVE-CA
           MOVE RT-L-ITEM(WS-I) TO SM-ITEM-CODE
           MOVE RT-L-REVISION(WS-I) TO SM-REVISION
           MOVE RT-SITE TO SM-SITE
           MOVE RT-WAREHOUSE TO SM-WAREHOUSE
           MOVE RT-QUARANTINE-LOC TO SM-LOCATION
           MOVE RT-L-LOT(WS-I) TO SM-LOT-CODE
           MOVE RT-L-SERIAL(WS-I) TO SM-SERIAL-NO
           MOVE "INSPECTION" TO SM-CONDITION
           MOVE RT-HOLD-CONDITION TO SM-TO-CONDITION
           MOVE "QUARANTINE" TO SM-HOLD-TYPE
           MOVE RT-L-RECV-QTY(WS-I) TO SM-QUANTITY
           MOVE RT-L-UOM(WS-I) TO SM-UOM-CODE
           MOVE RT-RECEIVED-DATE TO SM-MOVEMENT-DATE
           MOVE RT-RECEIVED-DATE TO SM-POSTING-DATE
           MOVE "CUSTRET" TO SM-REASON-CODE
           MOVE SPACES TO SM-REASON
           STRING "Awaiting inspection of return "
                  FUNCTION TRIM(RT-RETURN-REF)
               DELIMITED BY SIZE INTO SM-REASON
           MOVE "SO" TO SM-SOURCE-MODULE
           MOVE "RETURNAUTH" TO SM-SOURCE-DOC-TYPE
           MOVE RT-RETURN-REF TO SM-SOURCE-DOC-REF
           MOVE RT-L-LINE-NUM(WS-I) TO SM-SOURCE-DOC-LINE
           CALL "STKMOVE" USING WS-MOVE-CA
           IF CA-RETURN-CODE OF WS-MOVE-CA > 4
               MOVE "RET0007" TO WS-REASON
               MOVE CA-MESSAGE OF WS-MOVE-CA TO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 8350-EXIT
           END-IF
           MOVE SM-HOLD-ID TO RT-L-HOLD-ID(WS-I)
           ADD 1 TO RT-MOVEMENT-COUNT.
       8350-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 8400  CLOSING THE HOLD.  THE OUTCOME DECIDES THE CONDITION
      *>       THE STOCK LANDS IN, SO ONLY AN ACCEPT PUTS IT BACK INTO
      *>       SALEABLE STOCK.  THE ORIGINAL UNIT COST IS CARRIED SO
      *>       THE RESTOCK DOES NOT REVALUE THE ITEM.
      *>----------------------------------------------------------------
       8400-RELEASE-HOLD SECTION.
       8400-START.
           IF RT-L-HOLD-ID(WS-I) = 0
               MOVE SPACES TO RT-L-MESSAGE(WS-I)
               STRING "No quality hold is held for this line; "
                      "nothing has been released"
                   DELIMITED BY SIZE INTO RT-L-MESSAGE(WS-I)
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
                   MOVE "RET0010" TO WS-REASON
                   MOVE RT-L-MESSAGE(WS-I) TO WS-MSG
               END-IF
               GO TO 8400-EXIT
           END-IF
           MOVE SM-HOLD-OUTCOME TO WS-C1
           INITIALIZE WS-MOVE-CA
           MOVE "RELEASE" TO CA-FUNCTION OF WS-MOVE-CA
           MOVE WS-CO TO CA-COMPANY OF WS-MOVE-CA
           MOVE RT-SITE TO CA-SITE OF WS-MOVE-CA
           MOVE WS-USER TO CA-USER OF WS-MOVE-CA
           MOVE WS-BDATE TO CA-BUSINESS-DATE OF WS-MOVE-CA
           MOVE SPACES TO WS-SUB-SUFFIX
           MOVE RT-L-LINE-NUM(WS-I) TO WS-SUB-SEQ
           STRING "-REL" WS-SUB-SEQ DELIMITED BY SIZE
               INTO WS-SUB-SUFFIX
           PERFORM 9700-SUB-CORR THRU 9700-EXIT
           MOVE WS-SUB-CORR TO CA-CORRELATION-ID OF WS-MOVE-CA
           MOVE WS-C1(1:12) TO SM-HOLD-OUTCOME
           MOVE RT-L-HOLD-ID(WS-I) TO SM-HOLD-ID
           MOVE RT-L-ITEM(WS-I) TO SM-ITEM-CODE
           MOVE RT-L-REVISION(WS-I) TO SM-REVISION
           MOVE RT-SITE TO SM-SITE
           MOVE RT-WAREHOUSE TO SM-WAREHOUSE
           MOVE RT-QUARANTINE-LOC TO SM-LOCATION
           MOVE RT-L-LOT(WS-I) TO SM-LOT-CODE
           MOVE RT-L-SERIAL(WS-I) TO SM-SERIAL-NO
           MOVE RT-HOLD-CONDITION TO SM-CONDITION
           IF SM-HOLD-OUTCOME = "ACCEPT"
               MOVE RT-SALEABLE-COND TO SM-TO-CONDITION
               MOVE RT-L-ACC-QTY(WS-I) TO SM-QUANTITY
           ELSE
               MOVE "REJECTED" TO SM-TO-CONDITION
               COMPUTE SM-QUANTITY =
                   RT-L-ACC-QTY(WS-I) + RT-L-REJ-QTY(WS-I)
           END-IF
           MOVE RT-L-UOM(WS-I) TO SM-UOM-CODE
           MOVE RT-L-ORIG-COST(WS-I) TO SM-UNIT-COST
           MOVE WS-BDATE TO SM-MOVEMENT-DATE
           MOVE WS-BDATE TO SM-POSTING-DATE
           MOVE RT-L-DISPOSITION(WS-I) TO SM-REASON-CODE
           MOVE SPACES TO SM-REASON
           STRING "Disposition "
                  FUNCTION TRIM(RT-L-DISPOSITION(WS-I))
                  " of return " FUNCTION TRIM(RT-RETURN-REF)
               DELIMITED BY SIZE INTO SM-REASON
           MOVE "SO" TO SM-SOURCE-MODULE
           MOVE "RETURNAUTH" TO SM-SOURCE-DOC-TYPE
           MOVE RT-RETURN-REF TO SM-SOURCE-DOC-REF
           MOVE RT-L-LINE-NUM(WS-I) TO SM-SOURCE-DOC-LINE
           CALL "STKMOVE" USING WS-MOVE-CA
           IF CA-RETURN-CODE OF WS-MOVE-CA > 4
               MOVE "RET0007" TO WS-REASON
               MOVE CA-MESSAGE OF WS-MOVE-CA TO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 8400-EXIT
           END-IF
           MOVE "Y" TO RT-STOCK-MOVED
           ADD 1 TO RT-MOVEMENT-COUNT.
       8400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8450-SCRAP SECTION.
       8450-START.
           INITIALIZE WS-MOVE-CA
           MOVE "SCRAP" TO CA-FUNCTION OF WS-MOVE-CA
           MOVE WS-CO TO CA-COMPANY OF WS-MOVE-CA
           MOVE RT-SITE TO CA-SITE OF WS-MOVE-CA
           MOVE WS-USER TO CA-USER OF WS-MOVE-CA
           MOVE WS-BDATE TO CA-BUSINESS-DATE OF WS-MOVE-CA
           MOVE SPACES TO WS-SUB-SUFFIX
           MOVE RT-L-LINE-NUM(WS-I) TO WS-SUB-SEQ
           STRING "-SCR" WS-SUB-SEQ DELIMITED BY SIZE
               INTO WS-SUB-SUFFIX
           PERFORM 9700-SUB-CORR THRU 9700-EXIT
           MOVE WS-SUB-CORR TO CA-CORRELATION-ID OF WS-MOVE-CA
           MOVE RT-L-ITEM(WS-I) TO SM-ITEM-CODE
           MOVE RT-L-REVISION(WS-I) TO SM-REVISION
           MOVE RT-SITE TO SM-SITE
           MOVE RT-WAREHOUSE TO SM-WAREHOUSE
           MOVE RT-QUARANTINE-LOC TO SM-LOCATION
           MOVE RT-L-LOT(WS-I) TO SM-LOT-CODE
           MOVE RT-L-SERIAL(WS-I) TO SM-SERIAL-NO
           MOVE "REJECTED" TO SM-CONDITION
           COMPUTE SM-QUANTITY =
               RT-L-ACC-QTY(WS-I) + RT-L-REJ-QTY(WS-I)
           MOVE RT-L-UOM(WS-I) TO SM-UOM-CODE
           MOVE RT-L-ORIG-COST(WS-I) TO SM-UNIT-COST
           MOVE WS-BDATE TO SM-MOVEMENT-DATE
           MOVE WS-BDATE TO SM-POSTING-DATE
           MOVE "RETSCRAP" TO SM-REASON-CODE
           MOVE SPACES TO SM-REASON
           STRING "Scrapped on inspection of return "
                  FUNCTION TRIM(RT-RETURN-REF)
               DELIMITED BY SIZE INTO SM-REASON
           MOVE "SO" TO SM-SOURCE-MODULE
           MOVE "RETURNAUTH" TO SM-SOURCE-DOC-TYPE
           MOVE RT-RETURN-REF TO SM-SOURCE-DOC-REF
           MOVE RT-L-LINE-NUM(WS-I) TO SM-SOURCE-DOC-LINE
           CALL "STKMOVE" USING WS-MOVE-CA
           IF CA-RETURN-CODE OF WS-MOVE-CA > 4
               MOVE "RET0007" TO WS-REASON
               MOVE CA-MESSAGE OF WS-MOVE-CA TO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 8450-EXIT
           END-IF
           MOVE SPACES TO RT-L-MESSAGE(WS-I)
           STRING "Scrapped; not returned to saleable stock"
               DELIMITED BY SIZE INTO RT-L-MESSAGE(WS-I)
           ADD 1 TO RT-MOVEMENT-COUNT.
       8450-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8500-EXEC-UPDATE SECTION.
       8500-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
           END-IF
           MOVE 1 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       8500-EXIT.
           EXIT.

       8600-BEGIN SECTION.
       8600-START.
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
           ELSE
               MOVE "Y" TO WS-IN-TXN
           END-IF.
       8600-EXIT.
           EXIT.

       8610-COMMIT SECTION.
       8610-START.
           IF WS-IN-TXN = "Y"
               MOVE "COMT" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
               MOVE "N" TO WS-IN-TXN
           END-IF.
       8610-EXIT.
           EXIT.

       8620-ROLLBACK SECTION.
       8620-START.
           IF WS-IN-TXN = "Y"
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
               MOVE "N" TO WS-IN-TXN
           END-IF.
       8620-EXIT.
           EXIT.

       8650-NO-NULLS SECTION.
       8650-START.
           PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM.
       8650-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8700-AUDIT SECTION.
       8700-START.
           INITIALIZE WS-AUDIT-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUDIT-CA
           MOVE WS-CO TO CA-COMPANY OF WS-AUDIT-CA
           MOVE RT-SITE TO CA-SITE OF WS-AUDIT-CA
           MOVE WS-USER TO CA-USER OF WS-AUDIT-CA
           MOVE WS-BDATE TO CA-BUSINESS-DATE OF WS-AUDIT-CA
           MOVE WS-CORR TO CA-CORRELATION-ID OF WS-AUDIT-CA
           MOVE "so.ReturnAuthorisation" TO AU-ENTITY-TYPE
           MOVE RT-RETURN-REF TO AU-ENTITY-KEY
           MOVE WS-FUNC TO AU-ACTION
           MOVE "Status" TO AU-FIELD-NAME
           MOVE RT-STATUS TO AU-NEW-VALUE
           MOVE RT-REASON-CODE TO AU-REASON
           MOVE "RETURNS" TO AU-SOURCE
           MOVE "N" TO AU-SENSITIVE
           IF RT-APPROVAL-ID > 0
               MOVE RT-APPROVAL-ID TO WS-INT-ED
               MOVE FUNCTION TRIM(WS-INT-ED) TO AU-AUTHORITY-REF
           END-IF
           CALL "AUDITLOG" USING WS-AUDIT-CA.
       8700-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8900-SQL-FAILED SECTION.
       8900-START.
           MOVE "Y" TO WS-ERROR
           MOVE 12 TO WS-RC
           MOVE "SQL0001" TO WS-REASON
           MOVE SQLIO-ERRMSG(1:200) TO WS-MSG
           IF WS-MSG = SPACES
               MOVE "The database refused the request" TO WS-MSG
           END-IF.
       8900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           IF WS-RC < 8
               MOVE 8 TO WS-RC
           END-IF.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "RET0009" TO WS-REASON
           MOVE SPACES TO WS-MSG
           STRING "Function " FUNCTION TRIM(WS-FUNC)
                  " is not supported by RETURNS; use REQUEST, "
                  "AUTHORISE, RECEIVE, INSPECT, DISPOSE or CLOSE"
               DELIMITED BY SIZE INTO WS-MSG
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 9400  ONE PLACE DECIDES WHAT THE CALLER IS TOLD, SO A HOLD
      *>       NEVER ARRIVES LOOKING LIKE A SUCCESS.
      *>----------------------------------------------------------------
       9400-REPORT SECTION.
       9400-START.
           PERFORM 8620-ROLLBACK THRU 8620-EXIT
           MOVE WS-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           IF WS-REASON NOT = SPACES
               MOVE WS-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           END-IF
           IF WS-MSG NOT = SPACES
               MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           END-IF
           IF CA-MESSAGE OF DFHCOMMAREA NOT = SPACES
               MOVE 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF
           IF RT-STOCK-MOVED = SPACES
               MOVE "N" TO RT-STOCK-MOVED
           END-IF
           MOVE "N" TO RT-ORDER-REOPENED.
       9400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 9700  COM-005.  A derived correlation id for one sub-instruction
      *>       of this return.  The caller's id is truncated only if the
      *>       suffix would not otherwise fit the 36 character field.
      *>----------------------------------------------------------------
       9700-SUB-CORR SECTION.
       9700-START.
           MOVE 0 TO WS-BASE-LEN
           INSPECT FUNCTION REVERSE(WS-CORR)
               TALLYING WS-BASE-LEN FOR LEADING SPACES
           COMPUTE WS-BASE-LEN = 36 - WS-BASE-LEN
           MOVE 0 TO WS-SUF-LEN
           INSPECT FUNCTION REVERSE(WS-SUB-SUFFIX)
               TALLYING WS-SUF-LEN FOR LEADING SPACES
           COMPUTE WS-SUF-LEN = 8 - WS-SUF-LEN
           IF WS-BASE-LEN < 1
               MOVE 1 TO WS-BASE-LEN
           END-IF
           IF WS-BASE-LEN + WS-SUF-LEN > 36
               COMPUTE WS-BASE-LEN = 36 - WS-SUF-LEN
           END-IF
           MOVE SPACES TO WS-SUB-CORR
           STRING WS-CORR(1:WS-BASE-LEN)
                  WS-SUB-SUFFIX(1:WS-SUF-LEN)
               DELIMITED BY SIZE INTO WS-SUB-CORR.
       9700-EXIT.
           EXIT.
