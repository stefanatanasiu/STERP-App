      *>================================================================
      *> PROGRAM-ID : SORESV
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : THE RESERVATION DRIVER FOR SALES DEMAND (SO-009).
      *>              IT OWNS THE SALES RULES - WHICH STOCK IS
      *>              SALEABLE, WHICH LOT MAY STILL BE SHIPPED, WHAT
      *>              THE CUSTOMER'S REMAINING SHELF LIFE DEMANDS AND
      *>              HOW A SHORTFALL IS REPORTED TO THE ORDER DESK.
      *>
      *>              IT DOES NOT OWN THE ARITHMETIC.  "IS THERE
      *>              ENOUGH FREE STOCK AND MAY I HAVE IT?" IS
      *>              ANSWERED ONCE, INSIDE inv.usp_ReserveStock UNDER
      *>              UPDLOCK, REACHED THROUGH CALL "STKRESV".  THIS
      *>              PROGRAM NEVER READS FREE STOCK, DECIDES FOR
      *>              ITSELF AND THEN WRITES A RESERVATION - THE 1998
      *>              VERSION DID EXACTLY THAT AND OVERSOLD THE RDG
      *>              WAREHOUSE EVERY MONTH END (CR1188).  TWO ORDERS
      *>              THEREFORE CANNOT RESERVE THE SAME UNITS TWICE.
      *>
      *>              QUARANTINED, EXPIRED, REJECTED AND THIRD PARTY
      *>              OWNED MATERIAL IS NOT ORDINARY SALEABLE STOCK
      *>              (REASON SRV0006), AND LOT EXPIRY IS JUDGED AT
      *>              THE PLANNED DESPATCH DATE PLUS THE CUSTOMER'S
      *>              REMAINING SHELF LIFE REQUIREMENT.
      *> CALLED BY  : SALES ORDER ENTRY, SORELSE, PICKGEN, DESPATCH,
      *>              THE ALLOCATION WORKLIST, THE .NET CLIENT
      *> INTERFACE  : CALL "SORESV" USING WS-SORESV-CA
      *>              COPYBOOK SORESVCA
      *> CALLS      : STKRESV, AVAILQ, LOTSER, DATECALC, SQLIO (reads)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-04-02 RJH  CR0918 SALES ALLOCATION (ACUCOBOL)
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS THROUGHOUT
      *> 2008-10-14 KMP  CR1188 OVERSELL - THE ARITHMETIC MOVED INTO
      *>                 THE DATABASE UNDER UPDLOCK, NEVER COMPUTED HERE
      *> 2012-02-20 ANW  CR2740 CUSTOMER REMAINING SHELF LIFE
      *> 2026-10-06 STERP SO-009 SALEABILITY AND EXPIRY AT DESPATCH
      *>================================================================
      *> REQ: SO-009
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORESV.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 02/04/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STKRESVCA REPLACING ==DFHCOMMAREA== BY ==WS-SKR-CA==.
       COPY AVAILQCA REPLACING ==DFHCOMMAREA== BY ==WS-AVQ-CA==.
       COPY LOTSERCA REPLACING ==DFHCOMMAREA== BY ==WS-LTS-CA==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DTC-CA==.
      *>   The sales demand type.  Specification 05 writes it SALESORD
      *>   in prose; the value the reservation table accepts, and the
      *>   one inv.usp_ReserveStock expects, is SALESORDER.
       01  WS-CONST.
           05  WS-DEMAND-SALES         PIC X(12) VALUE "SALESORDER".
       01  WS-ROW.
           05  WS-COL OCCURS 12 TIMES  PIC X(100).
       01  WS-WORK.
           05  WS-BAD                  PIC X VALUE "N".
               88  WS-BAD-INPUT                  VALUE "Y".
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-QTY                  PIC S9(13)V9(6) VALUE 0.
           05  WS-TEXT                 PIC X(200) VALUE SPACES.
           05  WS-TEXT2                PIC X(200) VALUE SPACES.
           05  WS-QTY-ED               PIC Z(9)9.9(6).
           05  WS-INT-ED               PIC -(9)9.
           05  WS-LINE-FOUND           PIC X VALUE "N".
       LINKAGE SECTION.
       COPY SORESVCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       1000-MAIN.
           PERFORM 1100-INITIALISE THRU 1100-EXIT
           PERFORM 1200-VALIDATE THRU 1200-EXIT
           IF WS-BAD-INPUT
               GO TO 1000-RETURN
           END-IF
           EVALUATE CA-FUNCTION OF DFHCOMMAREA
               WHEN "RESERVE"
                   PERFORM 2000-RESERVE THRU 2000-EXIT
               WHEN "RELEASE"
                   PERFORM 5000-RELEASE THRU 5000-EXIT
               WHEN "CONSUME"
                   PERFORM 6000-CONSUME THRU 6000-EXIT
               WHEN "REVALID"
                   PERFORM 7000-REVALIDATE THRU 7000-EXIT
               WHEN "ENQUIRE"
                   PERFORM 8000-ENQUIRE THRU 8000-EXIT
               WHEN OTHER
                   PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       1000-RETURN.
           GOBACK.

      *>----------------------------------------------------------------
       1100-INITIALISE.
           MOVE "N" TO WS-BAD
           MOVE "N" TO WS-LINE-FOUND
           MOVE 0 TO WS-I WS-J WS-QTY
           MOVE SPACES TO WS-TEXT WS-TEXT2
           MOVE "SORESV" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "SO-009" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE FUNCTION UPPER-CASE(SV-ITEM-CODE) TO SV-ITEM-CODE
           MOVE 0 TO SV-QTY-RESERVED SV-QTY-SHORTFALL
           MOVE 0 TO SV-QTY-CONSUMED
           MOVE 0 TO SV-QTY-FREE-BEFORE SV-QTY-FREE-AFTER
           MOVE 0 TO SV-QTY-QUARANTINE SV-QTY-EXPIRED
           MOVE 0 TO SV-QTY-REJECTED SV-QTY-THIRDPARTY
           MOVE 0 TO SV-QTY-UNSALEABLE
           MOVE 0 TO SV-RESERVATION-ID SV-INVALID-CNT SV-LINE-CNT
           MOVE 0 TO SV-LOT-EXPIRY SV-DAYS-REMAINING
           MOVE 0 TO SV-SHELF-TEST-DATE
           MOVE 0 TO SV-ACCEPTED-QTY SV-CANCELLED-QTY
           MOVE 0 TO SV-DESPATCHED-QTY SV-OUTSTANDING-QTY
           MOVE SPACES TO SV-STATUS SV-INVALID-REASON
           MOVE SPACES TO SV-UNSALEABLE-TEXT
           MOVE SPACES TO SV-ORDER-STATUS SV-LINE-STATUS
           MOVE "N" TO SV-LOT-USABLE
           MOVE "N" TO SV-EXPIRY-CHECKED
      *>   the demand identity is fixed by SO-009 and never guessed
           MOVE WS-DEMAND-SALES TO SV-DEMAND-TYPE
           IF SV-DEMAND-REF = SPACES
               MOVE SV-ORDER-NUMBER TO SV-DEMAND-REF
           END-IF
           IF SV-DEMAND-LINE = 0
               MOVE SV-LINE-NUM TO SV-DEMAND-LINE
           END-IF
           IF SV-FIRMNESS = SPACES
               MOVE "FIRM" TO SV-FIRMNESS
           END-IF
           IF SV-PRIORITY = 0
               MOVE 5 TO SV-PRIORITY
           END-IF
           IF SV-DESPATCH-DATE = 0
               IF SV-REQUIRED-DATE = 0
                   MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
                       TO SV-DESPATCH-DATE
               ELSE
                   MOVE SV-REQUIRED-DATE TO SV-DESPATCH-DATE
               END-IF
           END-IF
           IF SV-REQUIRED-DATE = 0
               MOVE SV-DESPATCH-DATE TO SV-REQUIRED-DATE
           END-IF
           IF SV-SHELF-DAYS-REQ < 0
               MOVE 0 TO SV-SHELF-DAYS-REQ
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 12
               MOVE 100 TO SQLIO-COL-WIDTH(WS-I)
           END-PERFORM.
       1100-EXIT.
           EXIT.

       1200-VALIDATE.
           IF CA-COMPANY OF DFHCOMMAREA = SPACES
              OR SV-ITEM-CODE = SPACES
               MOVE "SRV0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "A company code and an item code are required "
                      "to reserve sales demand."
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1200-EXIT
           END-IF
           IF CA-FUNCTION OF DFHCOMMAREA = "ENQUIRE"
               GO TO 1200-EXIT
           END-IF
           IF SV-DEMAND-REF = SPACES
               MOVE "SRV0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "A reservation must name the sales order it "
                      "is held for."
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1200-EXIT
           END-IF
           IF CA-FUNCTION OF DFHCOMMAREA = "RESERVE"
              OR CA-FUNCTION OF DFHCOMMAREA = "CONSUME"
               IF SV-QTY-REQUESTED <= 0
                   MOVE "SRV0002" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SPACES TO WS-TEXT
                   STRING "A quantity greater than zero is required."
                       DELIMITED BY SIZE INTO WS-TEXT
                   PERFORM 9000-REJECT THRU 9000-EXIT
               END-IF
           END-IF.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  RESERVE.  The order line is checked first so that the
      *>       demand, site and revision travel with the reservation;
      *>       then saleability and shelf life; then - and only then -
      *>       STKRESV does the concurrency safe arithmetic.
      *>----------------------------------------------------------------
       2000-RESERVE.
           PERFORM 2100-READ-ORDER-LINE THRU 2100-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-SALEABILITY THRU 2200-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 2300-SHELF-LIFE THRU 2300-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           MOVE "RESERVE" TO WS-TEXT2
           PERFORM 4000-CALL-STKRESV THRU 4000-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 2900-RESERVE-RESULT THRU 2900-EXIT.
       2000-EXIT.
           EXIT.

      *>   The order line states the demand.  A cancelled or closed
      *>   line has no demand left to reserve against (SRV0005), and a
      *>   line that is already fully despatched has none either.
       2100-READ-ORDER-LINE.
           IF SV-ORDER-NUMBER = SPACES
               GO TO 2100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT l.Status, ISNULL(l.ItemCode,''), "
                  "ISNULL(l.Revision,''), "
                  "CONVERT(varchar(30), l.AcceptedQty), "
                  "CONVERT(varchar(30), l.CancelledQty), "
                  "CONVERT(varchar(30), l.DespatchedQty), "
                  "o.ReleaseStatus, ISNULL(o.SiteCode,''), "
                  "o.CustomerCode "
                  "FROM so.SalesOrderLine l JOIN so.SalesOrder o "
                  "ON o.SalesOrderId = l.SalesOrderId "
                  "WHERE l.CompanyCode = ? AND l.OrderNumber = ? "
                  "AND l.LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE SV-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE SV-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 9 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
      *>       An order that is not yet on the database is a valid
      *>       case for a what-if allocation, so this is a warning,
      *>       not a refusal; the reservation still names the demand.
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "SRV0004" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Order line was not found; the reservation is "
                      "held against the quoted demand reference."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               GO TO 2100-EXIT
           END-IF
           MOVE "Y" TO WS-LINE-FOUND
           MOVE WS-COL(1) TO SV-LINE-STATUS
           MOVE WS-COL(7) TO SV-ORDER-STATUS
           COMPUTE SV-ACCEPTED-QTY = FUNCTION NUMVAL(WS-COL(4))
           COMPUTE SV-CANCELLED-QTY = FUNCTION NUMVAL(WS-COL(5))
           COMPUTE SV-DESPATCHED-QTY = FUNCTION NUMVAL(WS-COL(6))
           COMPUTE SV-OUTSTANDING-QTY = SV-ACCEPTED-QTY
               - SV-CANCELLED-QTY - SV-DESPATCHED-QTY
           IF SV-OUTSTANDING-QTY < 0
               MOVE 0 TO SV-OUTSTANDING-QTY
           END-IF
      *>   demand, site and revision are preserved, not overwritten
           IF SV-REVISION = SPACES
               MOVE WS-COL(3) TO SV-REVISION
           END-IF
           IF SV-SITE = SPACES
               MOVE WS-COL(8) TO SV-SITE
           END-IF
           IF SV-CUSTOMER = SPACES
               MOVE WS-COL(9) TO SV-CUSTOMER
           END-IF
           IF SV-LINE-STATUS = "CANCELLED" OR SV-LINE-STATUS = "CLOSED"
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "SRV0005" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Order line " FUNCTION TRIM(SV-ORDER-NUMBER)
                      " is " FUNCTION TRIM(SV-LINE-STATUS)
                      " and has no demand left to reserve."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2200  SALEABILITY (specification 05 section 6).  Quarantined,
      *>       expired, rejected and third party owned material is in
      *>       our custody but it is not ordinary saleable stock.  It
      *>       is reported separately so the order desk can see why the
      *>       shelf looks fuller than the allocation allows.
      *>----------------------------------------------------------------
       2200-SALEABILITY.
           INITIALIZE WS-AVQ-CA
           MOVE "AVAIL" TO CA-FUNCTION OF WS-AVQ-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-AVQ-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-AVQ-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-AVQ-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-AVQ-CA
           MOVE SV-ITEM-CODE TO AV-ITEM-CODE
           MOVE SV-REVISION TO AV-REVISION
           MOVE SV-SITE TO AV-SITE
           MOVE SV-WAREHOUSE TO AV-WAREHOUSE
           MOVE SV-LOT-CODE TO AV-LOT-CODE
           MOVE "OWNED" TO AV-OWNER-TYPE
           MOVE SV-DESPATCH-DATE TO AV-AS-AT
           MOVE WS-DEMAND-SALES TO AV-DEMAND-TYPE
           MOVE SV-DEMAND-REF TO AV-DEMAND-REF
           MOVE SV-DEMAND-LINE TO AV-DEMAND-LINE
           CALL "AVAILQ" USING WS-AVQ-CA
           IF CA-RETURN-CODE OF WS-AVQ-CA > 4
      *>       the picture is unavailable; STKRESV is still the
      *>       authority on free stock, so carry on but say so
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "SRV0006" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "The saleability picture could not be read; "
                      "the reservation is still bounded by free stock."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 2200-EXIT
           END-IF
           MOVE AV-QTY-QUARANTINE TO SV-QTY-QUARANTINE
           MOVE AV-QTY-EXPIRED TO SV-QTY-EXPIRED
           MOVE AV-QTY-REJECTED TO SV-QTY-REJECTED
           MOVE AV-QTY-THIRDPARTY TO SV-QTY-THIRDPARTY
           COMPUTE SV-QTY-UNSALEABLE = AV-QTY-QUARANTINE
               + AV-QTY-EXPIRED + AV-QTY-REJECTED
               + AV-QTY-THIRDPARTY
           MOVE AV-QTY-FREE TO SV-QTY-FREE-BEFORE
           IF SV-QTY-UNSALEABLE > 0
               MOVE SV-QTY-UNSALEABLE TO WS-QTY-ED
               MOVE SPACES TO WS-TEXT
               STRING "Quarantined, expired, rejected or third party "
                      "owned stock of " FUNCTION TRIM(WS-QTY-ED)
                      " is held but is not saleable."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO SV-UNSALEABLE-TEXT
           END-IF
      *>   a named lot that is itself unsaleable is refused outright
           IF SV-LOT-CODE NOT = SPACES
              AND AV-QTY-FREE <= 0
              AND SV-QTY-UNSALEABLE > 0
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "SRV0006" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Lot " FUNCTION TRIM(SV-LOT-CODE)
                      " is quarantined, expired, rejected or not "
                      "owned, so it cannot be sold."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       2200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2300  SHELF LIFE.  The test date is the PLANNED DESPATCH DATE
      *>       plus the customer's remaining shelf life requirement -
      *>       not today, and not the order date.  A lot that expires
      *>       between the two is useless to this customer even though
      *>       it is perfectly good stock today (CR2740).
      *>----------------------------------------------------------------
       2300-SHELF-LIFE.
           IF SV-LOT-CODE = SPACES
               GO TO 2300-EXIT
           END-IF
           MOVE SV-DESPATCH-DATE TO SV-SHELF-TEST-DATE
           IF SV-SHELF-DAYS-REQ > 0
               INITIALIZE WS-DTC-CA
               MOVE "ADDDAYS" TO CA-FUNCTION OF WS-DTC-CA
               MOVE CA-COMPANY OF DFHCOMMAREA
                   TO CA-COMPANY OF WS-DTC-CA
               MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-DTC-CA
               MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
                   TO CA-BUSINESS-DATE OF WS-DTC-CA
               MOVE SV-DESPATCH-DATE TO DC-DATE-1
               MOVE SV-SHELF-DAYS-REQ TO DC-NUMBER
               CALL "DATECALC" USING WS-DTC-CA
               IF CA-RETURN-CODE OF WS-DTC-CA < 8
                   MOVE DC-RESULT-DATE TO SV-SHELF-TEST-DATE
               END-IF
           END-IF
           INITIALIZE WS-LTS-CA
           MOVE "EXPCHK" TO CA-FUNCTION OF WS-LTS-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-LTS-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-LTS-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-LTS-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-LTS-CA
           MOVE SV-ITEM-CODE TO LS-ITEM-CODE
           MOVE SV-REVISION TO LS-REVISION
           MOVE SV-LOT-CODE TO LS-LOT-CODE
           MOVE SV-SERIAL-NO TO LS-SERIAL-NO
           MOVE SV-SITE TO LS-SITE
           MOVE "O" TO LS-DIRECTION
           MOVE SV-QTY-REQUESTED TO LS-QUANTITY
           MOVE SV-SHELF-TEST-DATE TO LS-AS-AT
           MOVE SV-SHELF-DAYS-REQ TO LS-MIN-REMAIN-DAYS
           CALL "LOTSER" USING WS-LTS-CA
           MOVE "Y" TO SV-EXPIRY-CHECKED
           MOVE LS-EXPIRY-DATE TO SV-LOT-EXPIRY
           MOVE LS-DAYS-REMAINING TO SV-DAYS-REMAINING
           MOVE LS-USABLE TO SV-LOT-USABLE
           IF CA-RETURN-CODE OF WS-LTS-CA = 16
      *>       the lot is unknown to the traceability service; leave
      *>       the decision to STKRESV rather than invent an answer
               MOVE "N" TO SV-EXPIRY-CHECKED
               GO TO 2300-EXIT
           END-IF
           IF LS-USABLE NOT = "Y"
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "SRV0007" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Lot " FUNCTION TRIM(SV-LOT-CODE)
                      " does not satisfy the remaining shelf life "
                      "required at the planned despatch date."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       2300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  THE ONLY ROUTE TO A RESERVATION.  Every field that
      *>       restricts the material - site, revision, warehouse,
      *>       location, lot, serial - is handed over unchanged so the
      *>       restriction is honoured by the locking procedure and not
      *>       re-interpreted here.  WS-TEXT2 selects the function.
      *>----------------------------------------------------------------
       4000-CALL-STKRESV.
           INITIALIZE WS-SKR-CA
           MOVE WS-TEXT2 TO CA-FUNCTION OF WS-SKR-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-SKR-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-SKR-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-SKR-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-SKR-CA
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA
               TO CA-CORRELATION-ID OF WS-SKR-CA
           MOVE SV-ITEM-CODE TO SR-ITEM-CODE
           MOVE SV-REVISION TO SR-REVISION
           MOVE SV-SITE TO SR-SITE
           MOVE SV-WAREHOUSE TO SR-WAREHOUSE
           MOVE SV-LOCATION TO SR-LOCATION
           MOVE SV-LOT-CODE TO SR-LOT-CODE
           MOVE SV-SERIAL-NO TO SR-SERIAL-NO
           MOVE WS-DEMAND-SALES TO SR-DEMAND-TYPE
           MOVE SV-DEMAND-REF TO SR-DEMAND-REF
           MOVE SV-DEMAND-LINE TO SR-DEMAND-LINE
           MOVE SV-FIRMNESS TO SR-FIRMNESS
           MOVE SV-PRIORITY TO SR-PRIORITY
           MOVE SV-REQUIRED-DATE TO SR-REQUIRED-DATE
           MOVE SV-QTY-REQUESTED TO SR-QTY-REQUESTED
           MOVE SV-ALLOW-PARTIAL TO SR-ALLOW-PARTIAL
           CALL "STKRESV" USING WS-SKR-CA
           MOVE SR-QTY-RESERVED TO SV-QTY-RESERVED
           MOVE SR-QTY-SHORTFALL TO SV-QTY-SHORTFALL
           MOVE SR-QTY-CONSUMED TO SV-QTY-CONSUMED
           MOVE SR-QTY-FREE-BEFORE TO SV-QTY-FREE-BEFORE
           MOVE SR-QTY-FREE-AFTER TO SV-QTY-FREE-AFTER
           MOVE SR-RESERVATION-ID TO SV-RESERVATION-ID
           MOVE SR-INVALID-CNT TO SV-INVALID-CNT
           MOVE SR-INVALID-REASON TO SV-INVALID-REASON
           MOVE SR-STATUS TO SV-STATUS
           PERFORM 4100-COPY-LINES THRU 4100-EXIT
           IF CA-RETURN-CODE OF WS-SKR-CA >= 8
               MOVE CA-RETURN-CODE OF WS-SKR-CA
                   TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-SKR-CA
                   TO CA-MESSAGE OF DFHCOMMAREA
               IF CA-REASON-CODE OF WS-SKR-CA = "RSV0003"
                   MOVE "SRV0003" TO CA-REASON-CODE OF DFHCOMMAREA
               ELSE
                   MOVE CA-REASON-CODE OF WS-SKR-CA
                       TO CA-REASON-CODE OF DFHCOMMAREA
               END-IF
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       4000-EXIT.
           EXIT.

       4100-COPY-LINES.
           MOVE SR-LINE-CNT TO SV-LINE-CNT
           IF SR-LINE-CNT = 0
               GO TO 4100-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > SR-LINE-CNT
               MOVE SR-L-RES-ID(WS-I) TO SV-L-RES-ID(WS-I)
               MOVE SR-L-ITEM(WS-I) TO SV-L-ITEM(WS-I)
               MOVE SR-L-SITE(WS-I) TO SV-L-SITE(WS-I)
               MOVE SR-L-LOCATION(WS-I) TO SV-L-LOCATION(WS-I)
               MOVE SR-L-LOT(WS-I) TO SV-L-LOT(WS-I)
               MOVE SR-L-DEM-REF(WS-I) TO SV-L-DEM-REF(WS-I)
               MOVE SR-L-DEM-LINE(WS-I) TO SV-L-DEM-LINE(WS-I)
               MOVE SR-L-QTY(WS-I) TO SV-L-QTY(WS-I)
               MOVE SR-L-PICKED(WS-I) TO SV-L-PICKED(WS-I)
               MOVE SR-L-CONSUMED(WS-I) TO SV-L-CONSUMED(WS-I)
               MOVE SR-L-STATUS(WS-I) TO SV-L-STATUS(WS-I)
               MOVE SR-L-REQUIRED(WS-I) TO SV-L-REQUIRED(WS-I)
           END-PERFORM.
       4100-EXIT.
           EXIT.

      *>   A partial allocation is a legitimate answer, but it is
      *>   never silent: the shortfall is a number, a reason code and
      *>   a sentence the order desk can read to the customer.
       2900-RESERVE-RESULT.
           MOVE SV-QTY-RESERVED TO WS-QTY-ED
           MOVE SPACES TO WS-TEXT
           STRING "Reserved " FUNCTION TRIM(WS-QTY-ED) " of "
                  FUNCTION TRIM(SV-ITEM-CODE) " for order "
                  FUNCTION TRIM(SV-DEMAND-REF)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
           IF SV-QTY-SHORTFALL > 0
               MOVE SV-QTY-SHORTFALL TO WS-QTY-ED
               MOVE SPACES TO WS-TEXT2
               STRING FUNCTION TRIM(WS-TEXT) "; short by "
                      FUNCTION TRIM(WS-QTY-ED)
                      " which remains outstanding demand."
                   DELIMITED BY SIZE INTO WS-TEXT2
               MOVE WS-TEXT2 TO CA-MESSAGE OF DFHCOMMAREA
               IF CA-RETURN-CODE OF DFHCOMMAREA < 4
                   MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               END-IF
               MOVE "SRV0003" TO CA-REASON-CODE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF
           IF SV-QTY-RESERVED > 0
               MOVE "ACTIVE" TO SV-STATUS
           END-IF
           IF SV-UNSALEABLE-TEXT NOT = SPACES
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       2900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       5000-RELEASE.
           MOVE "RELEASE" TO WS-TEXT2
           PERFORM 4000-CALL-STKRESV THRU 4000-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 5000-EXIT
           END-IF
           MOVE "RELEASED" TO SV-STATUS
           MOVE SPACES TO WS-TEXT
           STRING "The reservation held for order "
                  FUNCTION TRIM(SV-DEMAND-REF)
                  " has been released; the demand itself is "
                  "unchanged."
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       5000-EXIT.
           EXIT.

      *>   CONSUME is final use.  STKRESV revalidates before it
      *>   consumes, so a reservation pointing at newly quarantined or
      *>   expired material is refused instead of quietly shipped.
       6000-CONSUME.
           MOVE "CONSUME" TO WS-TEXT2
           PERFORM 4000-CALL-STKRESV THRU 4000-EXIT
           IF CA-RETURN-CODE OF WS-SKR-CA = 6
               MOVE 6 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "SRV0009" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-SKR-CA
                   TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               GO TO 6000-EXIT
           END-IF
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 6000-EXIT
           END-IF
           MOVE "CONSUMED" TO SV-STATUS
           MOVE SV-QTY-CONSUMED TO WS-QTY-ED
           MOVE SPACES TO WS-TEXT
           STRING "Consumed " FUNCTION TRIM(WS-QTY-ED) " of "
                  FUNCTION TRIM(SV-ITEM-CODE) " against order "
                  FUNCTION TRIM(SV-DEMAND-REF)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       6000-EXIT.
           EXIT.

       7000-REVALIDATE.
           MOVE "REVALID" TO WS-TEXT2
           PERFORM 4000-CALL-STKRESV THRU 4000-EXIT
           IF CA-RETURN-CODE OF DFHCOMMAREA >= 8
               GO TO 7000-EXIT
           END-IF
           IF SV-INVALID-CNT > 0
               MOVE 6 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "SRV0009" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SV-INVALID-CNT TO WS-INT-ED
               MOVE SPACES TO WS-TEXT
               STRING FUNCTION TRIM(WS-INT-ED)
                      " reservation(s) for order "
                      FUNCTION TRIM(SV-DEMAND-REF)
                      " are no longer valid; reallocate before "
                      "picking."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               MOVE "INVALID" TO SV-STATUS
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           ELSE
               MOVE "ACTIVE" TO SV-STATUS
               MOVE SPACES TO WS-TEXT
               STRING "All reservations for order "
                      FUNCTION TRIM(SV-DEMAND-REF) " remain valid."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
           END-IF.
       7000-EXIT.
           EXIT.

       8000-ENQUIRE.
           MOVE "ENQUIRE" TO WS-TEXT2
           PERFORM 4000-CALL-STKRESV THRU 4000-EXIT
           IF CA-RETURN-CODE OF WS-SKR-CA = 16
               MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "SRV0004" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "No reservations are held for order "
                      FUNCTION TRIM(SV-DEMAND-REF)
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 8000-EXIT
           END-IF
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 8000-EXIT
           END-IF
           MOVE 0 TO WS-QTY
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > SV-LINE-CNT
               IF SV-L-STATUS(WS-I) = "ACTIVE"
                   COMPUTE WS-QTY = WS-QTY + SV-L-QTY(WS-I)
                       - SV-L-CONSUMED(WS-I)
               END-IF
           END-PERFORM
           MOVE WS-QTY TO SV-QTY-RESERVED
           MOVE SV-LINE-CNT TO WS-INT-ED
           MOVE SPACES TO WS-TEXT
           STRING FUNCTION TRIM(WS-INT-ED)
                  " reservation(s) are recorded for this demand."
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       8000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT.
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE "Y" TO WS-BAD.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE "SRV0008" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO WS-TEXT
           STRING "SORESV does not support the function "
                  FUNCTION TRIM(CA-FUNCTION OF DFHCOMMAREA)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE "SQL0001" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       9300-EXIT.
           EXIT.
