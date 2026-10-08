      *>================================================================
      *> PROGRAM-ID : LOTSER
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *> PURPOSE    : LOT, SERIAL AND SHELF LIFE CONTROL.  ENFORCES
      *>              MANDATORY TRACEABILITY ON EVERY RECEIPT,
      *>              TRANSFER, ISSUE, DESPATCH AND RETURN, KEEPS A
      *>              SERIAL NUMBER UNIQUE WHILE IT REPRESENTS A UNIT
      *>              IN STOCK, AND REFUSES MATERIAL THAT HAS EXPIRED
      *>              OR NO LONGER HAS THE MINIMUM REMAINING SHELF
      *>              LIFE.  ALSO REPORTS LOT AND SERIAL GENEALOGY.
      *> CALLED BY  : STKMOVE, PO RECEIPT, SO DESPATCH, KIT ISSUE,
      *>              THE .NET CLIENT
      *> INTERFACE  : CALL "LOTSER" USING WS-LOTSER-CA
      *>              COPYBOOK LOTSERCA
      *> CALLS      : SQLIO (inv.Item, inv.Lot, inv.Serial,
      *>              inv.StockBalance, inv.StockMovement)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-09-30 DJM  CR0462 ORIGINAL - LOT TRACEABILITY
      *> 1997-03-24 DJM  CR0694 SERIAL REUSE BLOCKED (QUALITY AUDIT)
      *> 2000-05-08 KMP  CR1003 SERIAL GENEALOGY FOR WARRANTY CLAIMS
      *> 2012-02-20 ANW  CR2740 MINIMUM REMAINING SHELF LIFE ON ISSUE
      *> 2026-10-05 STERP ITM-011 ACROSS EVERY MOVEMENT TYPE
      *>================================================================
      *> REQ: ITM-011, ITM-006
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOTSER.
       AUTHOR. D J MARSH.
       DATE-WRITTEN. 30/09/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ITMWORK.
       COPY ITMDATEW.
       01  WS-CONTROL.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-ITEM-READ            PIC X VALUE "N".
           05  WS-LOT-READ             PIC X VALUE "N".
           05  WS-STOCK-QTY            PIC S9(13)V9(6) VALUE 0.
           05  WS-SER-SITE             PIC X(10) VALUE SPACES.
           05  WS-SER-LOC              PIC X(20) VALUE SPACES.
           05  WS-LIMIT-DATE           PIC 9(8) VALUE 0.
       LINKAGE SECTION.
       COPY LOTSERCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "CHKMOVE"  PERFORM 1000-CHECK-MOVEMENT
               WHEN "NEWLOT"   PERFORM 2000-NEW-LOT
               WHEN "NEWSER"   PERFORM 3000-NEW-SERIAL
               WHEN "SETSTAT"  PERFORM 4000-SET-SERIAL-STATUS
               WHEN "EXPCHK"   PERFORM 5000-EXPIRY-CHECK
               WHEN "TRACE"    PERFORM 6000-TRACE
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-ITEM-READ WS-LOT-READ
           MOVE "LOTSER" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "ITM-011" TO CA-REQ-REF
           MOVE "N" TO LS-USABLE LS-CREATED
           MOVE 0 TO LS-TRACE-CNT LS-DAYS-REMAINING
           MOVE SPACES TO LS-TRACEABILITY LS-SERIAL-STATUS
           MOVE FUNCTION UPPER-CASE(LS-ITEM-CODE) TO LS-ITEM-CODE
           MOVE FUNCTION UPPER-CASE(LS-LOT-CODE) TO LS-LOT-CODE
           MOVE FUNCTION UPPER-CASE(LS-SERIAL-NO) TO LS-SERIAL-NO
           IF LS-AS-AT = 0
               MOVE CA-BUSINESS-DATE TO LS-AS-AT
           END-IF
           IF LS-AS-AT = 0
               MOVE 20261005 TO LS-AS-AT
           END-IF
           IF LS-DIRECTION NOT = "O"
               MOVE "I" TO LS-DIRECTION
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES OR LS-ITEM-CODE = SPACES
               MOVE "LOT0006" TO CA-REASON-CODE
               MOVE "A company and an item are required" TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0300-READ-ITEM SECTION.
       0300-START.
           IF WS-ITEM-READ = "Y"
               GO TO 0300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TraceabilityType, ISNULL(ShelfLifeDays,0),"
                  " ISNULL(MinRemainingShelfLifeDays,0), QuantumQty "
                  "FROM inv.Item WHERE CompanyCode = ? AND "
                  "ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE LS-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "LOT0006" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(LS-ITEM-CODE)
                      " is not held in company "
                      FUNCTION TRIM(CA-COMPANY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               MOVE "Y" TO WS-ERROR
               ADD 1 TO CA-MSG-COUNT
               GO TO 0300-EXIT
           END-IF
           MOVE ITM-COL(1) TO LS-TRACEABILITY
           COMPUTE LS-SHELF-LIFE-DAYS = FUNCTION NUMVAL(ITM-COL(2))
           COMPUTE LS-MIN-REMAIN-DAYS = FUNCTION NUMVAL(ITM-COL(3))
           MOVE "Y" TO WS-ITEM-READ.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0400  READ THE LOT AND WORK OUT HOW LONG IT HAS LEFT.
      *>----------------------------------------------------------------
       0400-READ-LOT SECTION.
       0400-START.
           MOVE 0 TO LS-EXPIRY-DATE LS-MANUFACTURED LS-RECEIVED-DATE
           MOVE SPACES TO LS-CONDITION LS-SUPPLIER-LOT
           MOVE "N" TO WS-LOT-READ
           IF LS-LOT-CODE = SPACES
               GO TO 0400-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(CONVERT(char(8), ExpiryDate, 112),"
                  "''), QualityStatus, ISNULL(CONVERT(char(8), "
                  "ManufacturedDate, 112),''), ISNULL(CONVERT(char(8)"
                  ", ReceivedDate, 112),''), ISNULL(SupplierLotRef,''"
                  "), ISNULL(CertificateRef,''), ISNULL(SupplierCode,"
                  "''), ISNULL(OriginCountry,'') FROM inv.Lot WHERE "
                  "CompanyCode = ? AND ItemCode = ? AND LotCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE LS-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE LS-LOT-CODE TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 8 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0400-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 0400-EXIT
           END-IF
           MOVE "Y" TO WS-LOT-READ
           IF ITM-COL(1) NOT = SPACES
               COMPUTE LS-EXPIRY-DATE = FUNCTION NUMVAL(ITM-COL(1))
           END-IF
           MOVE ITM-COL(2) TO LS-CONDITION
           IF ITM-COL(3) NOT = SPACES
               COMPUTE LS-MANUFACTURED = FUNCTION NUMVAL(ITM-COL(3))
           END-IF
           IF ITM-COL(4) NOT = SPACES
               COMPUTE LS-RECEIVED-DATE = FUNCTION NUMVAL(ITM-COL(4))
           END-IF
           MOVE ITM-COL(5) TO LS-SUPPLIER-LOT
           MOVE ITM-COL(6) TO LS-CERTIFICATE-REF
           MOVE ITM-COL(7) TO LS-SUPPLIER-CODE
           MOVE ITM-COL(8)(1:2) TO LS-ORIGIN-COUNTRY
           IF LS-EXPIRY-DATE > 0
               PERFORM 0450-DAYS-REMAINING
           END-IF.
       0400-EXIT.
           EXIT.

       0450-DAYS-REMAINING SECTION.
       0450-START.
           MOVE "DAYSBTWN" TO ITD-FUNC
           MOVE LS-AS-AT TO ITD-DATE-1
           MOVE LS-EXPIRY-DATE TO ITD-DATE-2
           CALL "ITMDATE" USING ITD-BLOCK
           IF ITD-RC > 4
               MOVE 0 TO LS-DAYS-REMAINING
           ELSE
               MOVE ITD-NUM-R TO LS-DAYS-REMAINING
           END-IF.
       0450-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  MAY THIS MOVEMENT USE THIS LOT OR SERIAL? (ITM-011)
      *>----------------------------------------------------------------
       1000-CHECK-MOVEMENT SECTION.
       1000-START.
           PERFORM 0300-READ-ITEM
           IF CA-FAILED
               GO TO 1000-EXIT
           END-IF
           EVALUATE LS-TRACEABILITY
               WHEN "LOT"    PERFORM 1100-CHECK-LOT
               WHEN "SERIAL" PERFORM 1200-CHECK-SERIAL
               WHEN OTHER    PERFORM 1300-CHECK-NONE
           END-EVALUATE
           IF CA-FAILED
               GO TO 1000-EXIT
           END-IF
           MOVE "Y" TO LS-USABLE
           IF CA-MESSAGE = SPACES
               MOVE SPACES TO CA-MESSAGE
               STRING "Traceability accepted for "
                      FUNCTION TRIM(LS-ITEM-CODE)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       1000-EXIT.
           EXIT.

       1100-CHECK-LOT SECTION.
       1100-START.
           IF LS-LOT-CODE = SPACES
               MOVE "LOT0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(LS-ITEM-CODE)
                      " is lot controlled, so every movement must "
                      "name its lot"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1100-EXIT
           END-IF
           PERFORM 0400-READ-LOT
           IF CA-FAILED
               GO TO 1100-EXIT
           END-IF
           IF WS-LOT-READ NOT = "Y"
               IF LS-DIRECTION = "O"
                   MOVE "LOT0006" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Lot " FUNCTION TRIM(LS-LOT-CODE)
                          " is not recorded for item "
                          FUNCTION TRIM(LS-ITEM-CODE)
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   PERFORM 9000-REJECT
               END-IF
               GO TO 1100-EXIT
           END-IF
           PERFORM 1150-SHELF-LIFE
           IF CA-FAILED
               GO TO 1100-EXIT
           END-IF
           IF LS-DIRECTION = "O" AND LS-CONDITION NOT = "ACCEPTED"
               MOVE "LOT0010" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Lot " FUNCTION TRIM(LS-LOT-CODE)
                      " is in condition "
                      FUNCTION TRIM(LS-CONDITION)
                      " and may not be issued without a release"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 6 TO CA-RETURN-CODE
               MOVE "Y" TO WS-ERROR
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       1100-EXIT.
           EXIT.

       1150-SHELF-LIFE SECTION.
       1150-START.
      *>   CR2740.  MISSING EXPIRY ON AN EXPIRY CONTROLLED ITEM IS AN
      *>   EXCEPTION, NOT AN ASSUMPTION OF UNLIMITED LIFE.
           IF LS-SHELF-LIFE-DAYS <= 0
               GO TO 1150-EXIT
           END-IF
           IF LS-EXPIRY-DATE = 0
               MOVE "LOT0007" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Lot " FUNCTION TRIM(LS-LOT-CODE)
                      " has no expiry date and item "
                      FUNCTION TRIM(LS-ITEM-CODE)
                      " is expiry controlled"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1150-EXIT
           END-IF
           IF LS-DIRECTION NOT = "O"
               GO TO 1150-EXIT
           END-IF
           IF LS-EXPIRY-DATE < LS-AS-AT
               MOVE "LOT0008" TO CA-REASON-CODE
               MOVE LS-EXPIRY-DATE TO WS-DATE-N
               MOVE SPACES TO CA-MESSAGE
               STRING "Lot " FUNCTION TRIM(LS-LOT-CODE)
                      " expired on " WS-DATE-DD "/" WS-DATE-MM "/"
                      WS-DATE-YYYY " and is not eligible stock"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1150-EXIT
           END-IF
           IF LS-MIN-REMAIN-DAYS > 0
              AND LS-DAYS-REMAINING < LS-MIN-REMAIN-DAYS
               MOVE "LOT0009" TO CA-REASON-CODE
               MOVE LS-DAYS-REMAINING TO WS-INT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Lot " FUNCTION TRIM(LS-LOT-CODE) " has only "
                      FUNCTION TRIM(WS-INT-ED)
                      " day(s) of shelf life left"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 6 TO CA-RETURN-CODE
               MOVE "Y" TO WS-ERROR
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       1150-EXIT.
           EXIT.

       1200-CHECK-SERIAL SECTION.
       1200-START.
           IF LS-SERIAL-NO = SPACES
               MOVE "LOT0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(LS-ITEM-CODE)
                      " is serial controlled, so every movement "
                      "must name its serial number"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1200-EXIT
           END-IF
           IF LS-QUANTITY NOT = 1 AND LS-QUANTITY NOT = 0
               MOVE "LOT0003" TO CA-REASON-CODE
               MOVE LS-QUANTITY TO WS-QTY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "A serialised unit moves one at a time; "
                      FUNCTION TRIM(WS-QTY-ED) " was entered"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1200-EXIT
           END-IF
           PERFORM 1250-SERIAL-IN-STOCK
           IF CA-FAILED
               GO TO 1200-EXIT
           END-IF
      *>   CR0694.  A SERIAL NUMBER IS UNIQUE WITHIN COMPANY AND ITEM
      *>   WHILE IT REPRESENTS A UNIT WE HOLD.
           IF LS-DIRECTION = "I" AND WS-STOCK-QTY > 0
               MOVE "LOT0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Serial " FUNCTION TRIM(LS-SERIAL-NO)
                      " of item " FUNCTION TRIM(LS-ITEM-CODE)
                      " is already in stock at "
                      FUNCTION TRIM(WS-SER-SITE) " "
                      FUNCTION TRIM(WS-SER-LOC)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1200-EXIT
           END-IF
           IF LS-DIRECTION = "O" AND WS-STOCK-QTY <= 0
               MOVE "LOT0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Serial " FUNCTION TRIM(LS-SERIAL-NO)
                      " of item " FUNCTION TRIM(LS-ITEM-CODE)
                      " is not in stock, so it cannot be issued"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1200-EXIT
           END-IF
           IF LS-LOT-CODE NOT = SPACES
               PERFORM 0400-READ-LOT
               PERFORM 1150-SHELF-LIFE
           END-IF.
       1200-EXIT.
           EXIT.

       1250-SERIAL-IN-STOCK SECTION.
       1250-START.
           MOVE 0 TO WS-STOCK-QTY
           MOVE SPACES TO WS-SER-SITE WS-SER-LOC
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 SUM(QtyOnHand) OVER (), SiteCode, "
                  "LocationCode FROM inv.StockBalance WHERE "
                  "CompanyCode = ? AND ItemCode = ? AND SerialNo = ?"
                  " AND QtyOnHand > 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE LS-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE LS-SERIAL-NO TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1250-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 1250-EXIT
           END-IF
           COMPUTE WS-STOCK-QTY = FUNCTION NUMVAL(ITM-COL(1))
           MOVE ITM-COL(2) TO WS-SER-SITE
           MOVE ITM-COL(3) TO WS-SER-LOC.
       1250-EXIT.
           EXIT.

       1300-CHECK-NONE SECTION.
       1300-START.
           IF LS-LOT-CODE NOT = SPACES OR LS-SERIAL-NO NOT = SPACES
               MOVE "LOT0012" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(LS-ITEM-CODE)
                      " is not traceability controlled; the lot or "
                      "serial supplied will be recorded but not "
                      "enforced"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 4 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       1300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  CREATE OR REFRESH A LOT.  WHEN THE SUPPLIER HAS NOT
      *>       GIVEN AN EXPIRY, THE ITEM SHELF LIFE DERIVES ONE FROM
      *>       THE MANUFACTURED (OR RECEIVED) DATE.
      *>----------------------------------------------------------------
       2000-NEW-LOT SECTION.
       2000-START.
           PERFORM 0300-READ-ITEM
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
           IF LS-LOT-CODE = SPACES
               MOVE "LOT0001" TO CA-REASON-CODE
               MOVE "A lot reference is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           IF LS-RECEIVED-DATE = 0
               MOVE LS-AS-AT TO LS-RECEIVED-DATE
           END-IF
           IF LS-EXPIRY-DATE = 0 AND LS-SHELF-LIFE-DAYS > 0
               PERFORM 2100-DERIVE-EXPIRY
           END-IF
           IF LS-CONDITION = SPACES
               MOVE "ACCEPTED" TO LS-CONDITION
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "MERGE inv.Lot AS t USING (SELECT ? AS C, ? AS I, "
                  "? AS L) AS s ON t.CompanyCode = s.C AND "
                  "t.ItemCode = s.I AND t.LotCode = s.L WHEN MATCHED"
                  " THEN UPDATE SET ExpiryDate = NULLIF(CONVERT(date,"
                  " ?, 112),'19000101'), QualityStatus = ?, "
                  "SupplierLotRef = ?, SupplierCode = ?, "
                  "CertificateRef = ?, ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHEN NOT MATCHED THEN INSERT "
                  "(CompanyCode, ItemCode, LotCode, Revision, "
                  "SupplierLotRef, SupplierCode, ReceivedDate, "
                  "ExpiryDate, QualityStatus, CertificateRef, "
                  "OriginCountry, CreatedBy) VALUES (s.C, s.I, s.L, "
                  "NULLIF(?,''), ?, ?, CONVERT(date, ?, 112), "
                  "NULLIF(CONVERT(date, ?, 112),'19000101'), ?, ?, "
                  "NULLIF(?,''), ?);"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 2200-BIND-LOT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2000-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           MOVE "Y" TO LS-CREATED LS-USABLE
           MOVE SPACES TO CA-MESSAGE
           STRING "Lot " FUNCTION TRIM(LS-LOT-CODE) " recorded for "
                  FUNCTION TRIM(LS-ITEM-CODE)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

       2100-DERIVE-EXPIRY SECTION.
       2100-START.
           MOVE "ADDDAYS" TO ITD-FUNC
           IF LS-MANUFACTURED > 0
               MOVE LS-MANUFACTURED TO ITD-DATE-1
           ELSE
               MOVE LS-RECEIVED-DATE TO ITD-DATE-1
           END-IF
           MOVE LS-SHELF-LIFE-DAYS TO ITD-NUMBER
           CALL "ITMDATE" USING ITD-BLOCK
           IF ITD-RC < 8
               MOVE ITD-DATE-R TO LS-EXPIRY-DATE
           END-IF.
       2100-EXIT.
           EXIT.

       2200-BIND-LOT SECTION.
       2200-START.
           MOVE 18 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 18
               MOVE SPACES TO SQLIO-PARM-VAL(WS-JX)
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE LS-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE LS-LOT-CODE TO SQLIO-PARM-VAL(3)
           IF LS-EXPIRY-DATE = 0
               MOVE "19000101" TO WS-TEXT
           ELSE
               MOVE LS-EXPIRY-DATE TO WS-DATE-N
               MOVE WS-DATE-N TO WS-TEXT
           END-IF
           MOVE WS-TEXT TO SQLIO-PARM-VAL(4)
           MOVE LS-CONDITION TO SQLIO-PARM-VAL(5)
           MOVE LS-SUPPLIER-LOT TO SQLIO-PARM-VAL(6)
           MOVE LS-SUPPLIER-CODE TO SQLIO-PARM-VAL(7)
           MOVE LS-CERTIFICATE-REF TO SQLIO-PARM-VAL(8)
           MOVE CA-USER TO SQLIO-PARM-VAL(9)
           MOVE LS-REVISION TO SQLIO-PARM-VAL(10)
           MOVE LS-SUPPLIER-LOT TO SQLIO-PARM-VAL(11)
           MOVE LS-SUPPLIER-CODE TO SQLIO-PARM-VAL(12)
           MOVE LS-RECEIVED-DATE TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(13)
           MOVE WS-TEXT TO SQLIO-PARM-VAL(14)
           MOVE LS-CONDITION TO SQLIO-PARM-VAL(15)
           MOVE LS-CERTIFICATE-REF TO SQLIO-PARM-VAL(16)
           MOVE LS-ORIGIN-COUNTRY TO SQLIO-PARM-VAL(17)
           MOVE CA-USER TO SQLIO-PARM-VAL(18).
       2200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       3000-NEW-SERIAL SECTION.
       3000-START.
           PERFORM 0300-READ-ITEM
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           IF LS-SERIAL-NO = SPACES
               MOVE "LOT0002" TO CA-REASON-CODE
               MOVE "A serial number is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           MOVE "I" TO LS-DIRECTION
           PERFORM 1250-SERIAL-IN-STOCK
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           IF WS-STOCK-QTY > 0
               MOVE "LOT0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Serial " FUNCTION TRIM(LS-SERIAL-NO)
                      " already represents a unit in stock"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           IF LS-SERIAL-STATUS = SPACES
               MOVE "INSTOCK" TO LS-SERIAL-STATUS
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "MERGE inv.Serial AS t USING (SELECT ? AS C, ? AS "
                  "I, ? AS S) AS s ON t.CompanyCode = s.C AND "
                  "t.ItemCode = s.I AND t.SerialNo = s.S WHEN "
                  "MATCHED THEN UPDATE SET Status = ?, "
                  "CurrentSiteCode = ?, CurrentLocationCode = ?, "
                  "LotCode = NULLIF(?,''), ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHEN NOT MATCHED THEN "
                  "INSERT (CompanyCode, ItemCode, SerialNo, LotCode, "
                  "Revision, Status, CurrentSiteCode, "
                  "CurrentLocationCode, QualityStatus, CreatedBy) "
                  "VALUES (s.C, s.I, s.S, NULLIF(?,''), NULLIF(?,'')"
                  ", ?, ?, ?, 'ACCEPTED', ?);"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 14 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 14
               MOVE SPACES TO SQLIO-PARM-VAL(WS-JX)
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE LS-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE LS-SERIAL-NO TO SQLIO-PARM-VAL(3)
           MOVE LS-SERIAL-STATUS TO SQLIO-PARM-VAL(4)
           MOVE LS-SITE TO SQLIO-PARM-VAL(5)
           MOVE LS-LOCATION TO SQLIO-PARM-VAL(6)
           MOVE LS-LOT-CODE TO SQLIO-PARM-VAL(7)
           MOVE CA-USER TO SQLIO-PARM-VAL(8)
           MOVE LS-LOT-CODE TO SQLIO-PARM-VAL(9)
           MOVE LS-REVISION TO SQLIO-PARM-VAL(10)
           MOVE LS-SERIAL-STATUS TO SQLIO-PARM-VAL(11)
           MOVE LS-SITE TO SQLIO-PARM-VAL(12)
           MOVE LS-LOCATION TO SQLIO-PARM-VAL(13)
           MOVE CA-USER TO SQLIO-PARM-VAL(14)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3000-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           MOVE "Y" TO LS-CREATED LS-USABLE
           MOVE SPACES TO CA-MESSAGE
           STRING "Serial " FUNCTION TRIM(LS-SERIAL-NO)
                  " registered for " FUNCTION TRIM(LS-ITEM-CODE)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       4000-SET-SERIAL-STATUS SECTION.
       4000-START.
           IF LS-SERIAL-NO = SPACES
               MOVE "LOT0002" TO CA-REASON-CODE
               MOVE "A serial number is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 4000-EXIT
           END-IF
           IF LS-SERIAL-STATUS = SPACES
               MOVE "INSTOCK" TO LS-SERIAL-STATUS
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE inv.Serial SET Status = ?, "
                  "CurrentSiteCode = NULLIF(?,''), "
                  "CurrentLocationCode = NULLIF(?,''), ModifiedBy = "
                  "?, ModifiedAt = SYSDATETIME() WHERE CompanyCode ="
                  " ? AND ItemCode = ? AND SerialNo = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 7
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE LS-SERIAL-STATUS TO SQLIO-PARM-VAL(1)
           MOVE LS-SITE TO SQLIO-PARM-VAL(2)
           MOVE LS-LOCATION TO SQLIO-PARM-VAL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(5)
           MOVE LS-ITEM-CODE TO SQLIO-PARM-VAL(6)
           MOVE LS-SERIAL-NO TO SQLIO-PARM-VAL(7)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 4000-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           MOVE "Y" TO LS-USABLE
           MOVE SPACES TO CA-MESSAGE
           STRING "Serial " FUNCTION TRIM(LS-SERIAL-NO) " is now "
                  FUNCTION TRIM(LS-SERIAL-STATUS)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       5000-EXPIRY-CHECK SECTION.
       5000-START.
           PERFORM 0300-READ-ITEM
           IF CA-FAILED
               GO TO 5000-EXIT
           END-IF
           PERFORM 0400-READ-LOT
           IF CA-FAILED
               GO TO 5000-EXIT
           END-IF
           IF WS-LOT-READ NOT = "Y"
               MOVE "LOT0006" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Lot " FUNCTION TRIM(LS-LOT-CODE)
                      " is not recorded"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 5000-EXIT
           END-IF
           MOVE "O" TO LS-DIRECTION
           PERFORM 1150-SHELF-LIFE
           IF NOT CA-FAILED
               MOVE "Y" TO LS-USABLE
               MOVE LS-DAYS-REMAINING TO WS-INT-ED
               MOVE SPACES TO CA-MESSAGE
               IF LS-EXPIRY-DATE = 0
                   STRING "Lot " FUNCTION TRIM(LS-LOT-CODE)
                          " has no expiry policy"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               ELSE
                   STRING "Lot " FUNCTION TRIM(LS-LOT-CODE) " has "
                          FUNCTION TRIM(WS-INT-ED)
                          " day(s) of shelf life remaining"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               END-IF
           END-IF.
       5000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 6000  GENEALOGY.  EVERY MOVEMENT OF THE LOT OR THE SERIAL IN
      *>       MOVEMENT ORDER, WHICH IS WHAT A RECALL NEEDS.
      *>----------------------------------------------------------------
       6000-TRACE SECTION.
       6000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT MovementId, MovementType, CONVERT(char(8),"
                  " MovementDate, 112), Quantity, SiteCode, "
                  "LocationCode, ISNULL(SourceDocType,''), "
                  "ISNULL(SourceDocRef,''), ISNULL(CreatedBy,'') "
                  "FROM inv.StockMovement WHERE CompanyCode = ? AND "
                  "ItemCode = ? AND ((? <> '' AND LotCode = ?) OR "
                  "(? <> '' AND SerialNo = ?)) ORDER BY MovementId"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE LS-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE LS-LOT-CODE TO SQLIO-PARM-VAL(3)
           MOVE LS-LOT-CODE TO SQLIO-PARM-VAL(4)
           MOVE LS-SERIAL-NO TO SQLIO-PARM-VAL(5)
           MOVE LS-SERIAL-NO TO SQLIO-PARM-VAL(6)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 9 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 6000-EXIT
           END-IF
           MOVE 0 TO LS-TRACE-CNT
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA ITM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   IF LS-TRACE-CNT < 25
                       ADD 1 TO LS-TRACE-CNT
                       COMPUTE LS-T-MOVE-ID(LS-TRACE-CNT) =
                           FUNCTION NUMVAL(ITM-COL(1))
                       MOVE ITM-COL(2) TO LS-T-TYPE(LS-TRACE-CNT)
                       COMPUTE LS-T-DATE(LS-TRACE-CNT) =
                           FUNCTION NUMVAL(ITM-COL(3))
                       COMPUTE LS-T-QTY(LS-TRACE-CNT) =
                           FUNCTION NUMVAL(ITM-COL(4))
                       MOVE ITM-COL(5) TO LS-T-SITE(LS-TRACE-CNT)
                       MOVE ITM-COL(6) TO LS-T-LOCATION(LS-TRACE-CNT)
                       MOVE ITM-COL(7) TO LS-T-DOC-TYPE(LS-TRACE-CNT)
                       MOVE ITM-COL(8) TO LS-T-DOC-REF(LS-TRACE-CNT)
                       MOVE ITM-COL(9) TO LS-T-USER(LS-TRACE-CNT)
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF LS-TRACE-CNT = 0
               MOVE 16 TO CA-RETURN-CODE
               MOVE "LOT0006" TO CA-REASON-CODE
               MOVE "No movements are recorded for that identity"
                   TO CA-MESSAGE
           ELSE
               MOVE LS-TRACE-CNT TO WS-INT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-INT-ED)
                      " traced movement(s)"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       6000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE "N" TO LS-USABLE
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "LOT0011" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "LOTSER does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE "Y" TO WS-ERROR WS-SQL-FAILED
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in LOTSER" TO CA-MESSAGE
           END-IF
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE 12 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
