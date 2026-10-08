      *>================================================================
      *> PROGRAM-ID : SOACCEPT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT
      *> PURPOSE    : COMPLETENESS, COMMERCIAL ACCEPTANCE, RELEASE AND
      *>              DEPOSITS FOR ONE SALES ORDER.  AN INCOMPLETE
      *>              DRAFT IS NEVER TREATED AS ACCEPTED DEMAND, SO
      *>              VALIDATE RETURNS THE WHOLE BLOCKER LIST IN THE
      *>              COMMAREA RATHER THAN THE FIRST FAULT IT FINDS.
      *>              ACCEPTANCE TAKES THE CREDIT POSITION FROM
      *>              CREDCHK AND SETS EITHER RELEASED OR AN EXPLICIT
      *>              HOLD WITH ITS REASON.  RELEASE RE-RUNS THE
      *>              CURRENT CHECKS: PRINTING AN ACKNOWLEDGEMENT OR A
      *>              PICK DOCUMENT IS EVIDENCE, NOT A RELEASE, AND
      *>              RELEASE NEVER RESERVES STOCK BY ITSELF.
      *> CALLED BY  : ORDER ENTRY, DESPATCH RELEASE, .NET CLIENT
      *> INTERFACE  : CALL "SOACCEPT" USING WS-SOACCEPT-CA
      *>              COPYBOOK SOACCEPTCA
      *> CALLS      : SQLIO, CREDCHK, UOMCONV, AUDITLOG, ARDEPOS
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-09-12 RJH  CR0301 ORDER ACCEPTANCE AND CREDIT RELEASE
      *> 1999-11-30 KMP  Y2K - ORDER DATES HOLD FOUR DIGIT YEARS
      *> 2011-05-19 DLS  CR2416 SCOPED CREDIT OVERRIDES WITH EXPIRY
      *> 2021-01-04 DLS  CR3004 BREXIT - INCOTERMS AND NAMED PLACE
      *> 2026-10-06 STERP SO-002, SO-003, SO-007, SO-011, SO-019
      *>================================================================
      *> REQ: SO-002, SO-003, SO-007, SO-011, SO-019
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOACCEPT.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 12/09/1994.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY CREDCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-CRED-CA==.
       COPY UOMCONVCA REPLACING ==DFHCOMMAREA== BY ==WS-UOM-CA==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUD-CA==.
       COPY ARDEPOSCA REPLACING ==DFHCOMMAREA== BY ==WS-DEP-CA==.
       01  WS-CTL.
           05  WS-FUNC                 PIC X(8).
           05  WS-CO                   PIC X(10).
           05  WS-SITE                 PIC X(10).
           05  WS-USER                 PIC X(20).
           05  WS-BUSDATE              PIC 9(8).
           05  WS-CORREL               PIC X(36).
           05  WS-RC                   PIC 9(2).
           05  WS-REASON               PIC X(8).
           05  WS-MSG                  PIC X(200).
           05  WS-MSGCNT               PIC 9(2).
           05  WS-TRAN-OPEN            PIC X.
           05  WS-I                    PIC S9(4).
           05  WS-P                    PIC S9(4).
           05  WS-SEV                  PIC X(8).
           05  WS-NEW-REL              PIC X(14).
           05  WS-HOLD-TEXT            PIC X(200).
           05  WS-SUPPLY-GAPS          PIC S9(9).
           05  WS-APPROVAL-GAPS        PIC S9(9).
           05  WS-FIELD                PIC X(40).
           05  WS-TEXT                 PIC X(160).
           05  WS-LINE-NO              PIC S9(9).
           05  WS-SLOT                 PIC S9(4).
           05  WS-REQ-IN               PIC S9(11)V99.
       01  WS-EDITS.
           05  WS-MONEY-ED             PIC -(13)9.99.
           05  WS-QTY-ED               PIC -(13)9.999999.
           05  WS-RATE-ED              PIC -(9)9.99999999.
           05  WS-INT-ED               PIC -(9)9.
           05  WS-UK-MONEY             PIC Z,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DATE-ED              PIC 9(8).
       01  WS-AMTS.
           05  WS-AMT1                 PIC S9(13)V99.
           05  WS-AMT2                 PIC S9(13)V99.
      *>   so.SalesOrder as read for this instruction
       01  WS-OHDR.
           05  WS-H-ID                 PIC X(24).
           05  WS-H-CUST               PIC X(20).
           05  WS-H-INVCUST            PIC X(20).
           05  WS-H-TYPE               PIC X(12).
           05  WS-H-DATE               PIC X(8).
           05  WS-H-CURR               PIC X(3).
           05  WS-H-FX                 PIC X(24).
           05  WS-H-TERMS              PIC X(12).
           05  WS-H-SALESP             PIC X(20).
           05  WS-H-ADDRREF            PIC X(10).
           05  WS-H-ADDRSNAP           PIC X(300).
           05  WS-H-INCO               PIC X(3).
           05  WS-H-INCOPL             PIC X(60).
           05  WS-H-TCVER              PIC X(20).
           05  WS-H-ROT                PIC X(1).
           05  WS-H-VATBASIS           PIC X(20).
           05  WS-H-VATREG             PIC X(15).
           05  WS-H-VATLOC             PIC X(3).
           05  WS-H-NET                PIC X(24).
           05  WS-H-VAT                PIC X(24).
           05  WS-H-GROSS              PIC X(24).
           05  WS-H-GROSSGBP           PIC X(24).
           05  WS-H-COMM               PIC X(12).
           05  WS-H-REL                PIC X(14).
           05  WS-H-FULFIL             PIC X(14).
           05  WS-H-BILLING            PIC X(14).
           05  WS-H-HOLD               PIC X(200).
           05  WS-H-DEPREQ             PIC X(24).
           05  WS-H-DEPRCV             PIC X(24).
           05  WS-H-SITE               PIC X(10).
           05  WS-H-LINECNT            PIC X(10).
      *>   one order line as fetched
       01  WS-LROW.
           05  WS-R-NUM                PIC X(10).
           05  WS-R-ITEM               PIC X(40).
           05  WS-R-REV                PIC X(4).
           05  WS-R-UOM                PIC X(10).
           05  WS-R-QTY                PIC X(24).
           05  WS-R-PSRC               PIC X(20).
           05  WS-R-VAT                PIC X(4).
           05  WS-R-TYPE               PIC X(10).
           05  WS-R-STATUS             PIC X(10).
           05  WS-R-PRICE              PIC X(24).
      *>   the order's lines.  OCCURS 200 since CR0301 - an order
      *>   longer than that is entered as a schedule contract.
       01  WS-LINE-TAB.
           05  WS-LT-CNT               PIC S9(4).
           05  WS-LT OCCURS 200 TIMES.
               10  WS-LT-NUM           PIC S9(9).
               10  WS-LT-ITEM          PIC X(40).
               10  WS-LT-REV           PIC X(4).
               10  WS-LT-UOM           PIC X(10).
               10  WS-LT-QTY           PIC S9(13)V9(6).
               10  WS-LT-PSRC          PIC X(20).
               10  WS-LT-VATCODE       PIC X(4).
               10  WS-LT-TYPE          PIC X(10).
               10  WS-LT-STATUS        PIC X(10).
      *>   item eligibility answer
       01  WS-ITM.
           05  WS-IT-LIFECYCLE         PIC X(10).
           05  WS-IT-SALESUOM          PIC X(10).
           05  WS-IT-SALEABLE          PIC X(1).
           05  WS-IT-REVSTATUS         PIC X(10).
       01  WS-TWO.
           05  WS-T1                   PIC X(30).
           05  WS-T2                   PIC X(30).
       LINKAGE SECTION.
       COPY SOACCEPTCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE-INPUT
           IF WS-RC > 4
               GO TO 0000-EXIT
           END-IF
           EVALUATE WS-FUNC
               WHEN "VALIDATE" PERFORM 1000-VALIDATE-ORDER
               WHEN "ACCEPT"   PERFORM 2000-ACCEPT-ORDER
               WHEN "RELEASE"  PERFORM 3000-RELEASE-ORDER
               WHEN "DEPOSIT"  PERFORM 4000-TAKE-DEPOSIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE
           IF WS-RC > 4
               PERFORM 9500-ROLLBACK
           END-IF.
       0000-EXIT.
           PERFORM 9900-SET-RESULT
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE WS-CTL
           INITIALIZE WS-EDITS
           INITIALIZE WS-AMTS
           INITIALIZE WS-OHDR
           INITIALIZE WS-LROW
           INITIALIZE WS-ITM
           INITIALIZE WS-TWO
           MOVE 0 TO WS-LT-CNT
           MOVE "N" TO WS-TRAN-OPEN
           MOVE 0 TO WS-RC WS-MSGCNT
           MOVE SPACES TO WS-REASON WS-MSG WS-HOLD-TEXT
           MOVE 0 TO WS-SUPPLY-GAPS WS-APPROVAL-GAPS
           MOVE CA-FUNCTION OF DFHCOMMAREA TO WS-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO WS-CO
           MOVE CA-SITE OF DFHCOMMAREA TO WS-SITE
           MOVE CA-USER OF DFHCOMMAREA TO WS-USER
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO WS-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO WS-CORREL
           MOVE "SOACCEPT" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE "SO-003" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE 0 TO SA-BLOCKER-CNT SA-HARD-CNT
           MOVE SPACES TO SA-HOLD-REASON
           MOVE 0 TO SA-CREDIT-EXPOSURE SA-CREDIT-LIMIT
           MOVE 0 TO SA-CREDIT-HEADROOM SA-CREDIT-EXCESS
           MOVE 0 TO SA-OVERDUE-GBP SA-OVERDUE-DAYS
           MOVE SPACES TO SA-CREDIT-DECISION
           IF SA-DEPOSIT-CLEARED = SPACE OR
              SA-DEPOSIT-CLEARED = LOW-VALUE
               MOVE "N" TO SA-DEPOSIT-CLEARED
           END-IF
           IF SA-PRINT-ONLY = SPACE OR SA-PRINT-ONLY = LOW-VALUE
               MOVE "N" TO SA-PRINT-ONLY
           END-IF.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0200-VALIDATE-INPUT SECTION.
       0200-START.
           IF WS-CO = SPACES OR SA-ORDER-NUMBER = SPACES
               MOVE "SOA0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A company code and an order number are "
                      "required" DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0300-READ-ORDER SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), o.SalesOrderId), "
                  "o.CustomerCode, ISNULL(o.InvoiceCustomerCode,''), "
                  "o.OrderType, CONVERT(char(8), o.OrderDate, 112), "
                  "o.CurrencyCode, CONVERT(varchar(24), o.FxRate), "
                  "ISNULL(o.TermsCode,''), "
                  "ISNULL(o.SalespersonCode,''), "
                  "ISNULL(o.DeliveryAddressRef,''), "
                  "ISNULL(o.DeliveryAddressSnapshot,''), "
                  "ISNULL(o.IncotermsRule,''), "
                  "ISNULL(o.IncotermsNamedPlace,''), "
                  "ISNULL(o.TermsAndConditionsVersion,''), "
                  "CONVERT(varchar(1), o.RetentionOfTitle), "
                  "ISNULL(o.VatTreatmentBasis,''), "
                  "ISNULL(o.CustomerVatRegNo,''), "
                  "ISNULL(o.VatLocation,''), "
                  "CONVERT(varchar(24), o.NetAmount), "
                  "CONVERT(varchar(24), o.VatAmount), "
                  "CONVERT(varchar(24), o.GrossAmount), "
                  "CONVERT(varchar(24), o.GrossAmountGBP), "
                  "o.CommercialStatus, o.ReleaseStatus, "
                  "o.FulfilmentStatus, o.BillingStatus, "
                  "ISNULL(o.HoldReason,''), CONVERT(varchar(24), "
                  "ISNULL(o.DepositRequiredGBP,0)), "
                  "CONVERT(varchar(24), o.DepositReceivedGBP), "
                  "o.SiteCode, CONVERT(varchar(10), (SELECT COUNT(*) "
                  "FROM so.SalesOrderLine l WHERE l.SalesOrderId = "
                  "o.SalesOrderId AND l.Status <> 'CANCELLED')) FROM "
                  "so.SalesOrder o WHERE o.CompanyCode = ? AND "
                  "o.OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SA-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 31 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 20 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 3 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 12 TO SQLIO-COL-WIDTH(8)
           MOVE 20 TO SQLIO-COL-WIDTH(9)
           MOVE 10 TO SQLIO-COL-WIDTH(10)
           MOVE 300 TO SQLIO-COL-WIDTH(11)
           MOVE 3 TO SQLIO-COL-WIDTH(12)
           MOVE 60 TO SQLIO-COL-WIDTH(13)
           MOVE 20 TO SQLIO-COL-WIDTH(14)
           MOVE 1 TO SQLIO-COL-WIDTH(15)
           MOVE 20 TO SQLIO-COL-WIDTH(16)
           MOVE 15 TO SQLIO-COL-WIDTH(17)
           MOVE 3 TO SQLIO-COL-WIDTH(18)
           MOVE 24 TO SQLIO-COL-WIDTH(19)
           MOVE 24 TO SQLIO-COL-WIDTH(20)
           MOVE 24 TO SQLIO-COL-WIDTH(21)
           MOVE 24 TO SQLIO-COL-WIDTH(22)
           MOVE 12 TO SQLIO-COL-WIDTH(23)
           MOVE 14 TO SQLIO-COL-WIDTH(24)
           MOVE 14 TO SQLIO-COL-WIDTH(25)
           MOVE 14 TO SQLIO-COL-WIDTH(26)
           MOVE 200 TO SQLIO-COL-WIDTH(27)
           MOVE 24 TO SQLIO-COL-WIDTH(28)
           MOVE 24 TO SQLIO-COL-WIDTH(29)
           MOVE 10 TO SQLIO-COL-WIDTH(30)
           MOVE 10 TO SQLIO-COL-WIDTH(31)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-OHDR
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO WS-RC
               MOVE "SOA0006" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Order " FUNCTION TRIM(SA-ORDER-NUMBER)
                      " is not held for company "
                      FUNCTION TRIM(WS-CO)
                   DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
               GO TO 0300-EXIT
           END-IF
           COMPUTE SA-SALES-ORDER-ID = FUNCTION NUMVAL(WS-H-ID)
           MOVE WS-H-CUST TO SA-CUSTOMER
           MOVE WS-H-INVCUST TO SA-INVOICE-CUST
           MOVE WS-H-TYPE TO SA-ORDER-TYPE
           COMPUTE SA-ORDER-DATE = FUNCTION NUMVAL(WS-H-DATE)
           MOVE WS-H-CURR TO SA-CURRENCY
           COMPUTE SA-FX-RATE = FUNCTION NUMVAL(WS-H-FX)
           MOVE WS-H-TERMS TO SA-TERMS-CODE
           MOVE WS-H-SALESP TO SA-SALESPERSON
           MOVE WS-H-ADDRREF TO SA-DELIVERY-REF
           MOVE WS-H-ADDRSNAP TO SA-DELIVERY-SNAP
           MOVE WS-H-INCO TO SA-INCOTERMS-RULE
           MOVE WS-H-INCOPL TO SA-INCOTERMS-PLACE
           MOVE WS-H-TCVER TO SA-TC-VERSION
           MOVE WS-H-ROT TO SA-RETENTION-TITLE
           MOVE WS-H-VATBASIS TO SA-VAT-BASIS
           MOVE WS-H-VATREG TO SA-CUST-VAT-REG
           MOVE WS-H-VATLOC TO SA-VAT-LOCATION
           COMPUTE SA-NET-AMOUNT = FUNCTION NUMVAL(WS-H-NET)
           COMPUTE SA-VAT-AMOUNT = FUNCTION NUMVAL(WS-H-VAT)
           COMPUTE SA-GROSS-AMOUNT = FUNCTION NUMVAL(WS-H-GROSS)
           COMPUTE SA-GROSS-GBP = FUNCTION NUMVAL(WS-H-GROSSGBP)
           MOVE WS-H-COMM TO SA-COMM-STATUS
           MOVE WS-H-COMM TO SA-PREV-COMM-STATUS
           MOVE WS-H-REL TO SA-RELEASE-STATUS
           MOVE WS-H-REL TO SA-PREV-REL-STATUS
           MOVE WS-H-FULFIL TO SA-FULFIL-STATUS
           MOVE WS-H-BILLING TO SA-BILLING-STATUS
           MOVE WS-H-HOLD TO SA-HOLD-REASON
           COMPUTE SA-DEPOSIT-REQ-GBP = FUNCTION NUMVAL(WS-H-DEPREQ)
           COMPUTE SA-DEPOSIT-RCVD-GBP = FUNCTION NUMVAL(WS-H-DEPRCV)
           COMPUTE SA-LINE-COUNT = FUNCTION NUMVAL(WS-H-LINECNT)
           IF SA-VAT-LOCATION NOT = SPACES AND
              SA-VAT-LOCATION NOT = "GB"
               MOVE "Y" TO SA-INTERNATIONAL
           END-IF.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-READ-ORDER-LINES SECTION.
       0400-START.
           MOVE 0 TO WS-LT-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(10), l.LineNum), "
                  "ISNULL(l.ItemCode,''), ISNULL(l.Revision,''), "
                  "ISNULL(l.UomCode,''), CONVERT(varchar(24), "
                  "l.AcceptedQty), ISNULL(l.PriceSource,''), "
                  "ISNULL(l.VatCode,''), l.LineType, l.Status, "
                  "CONVERT(varchar(24), l.UnitPrice) FROM "
                  "so.SalesOrderLine l WHERE l.CompanyCode = ? AND "
                  "l.OrderNumber = ? AND l.Status <> 'CANCELLED' "
                  "ORDER BY l.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SA-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 4 TO SQLIO-COL-WIDTH(3)
           MOVE 10 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 20 TO SQLIO-COL-WIDTH(6)
           MOVE 4 TO SQLIO-COL-WIDTH(7)
           MOVE 10 TO SQLIO-COL-WIDTH(8)
           MOVE 10 TO SQLIO-COL-WIDTH(9)
           MOVE 24 TO SQLIO-COL-WIDTH(10)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0400-EXIT
           END-IF
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LROW
           PERFORM UNTIL SQLIO-SQLCODE NOT = 0 OR WS-LT-CNT >= 200
               ADD 1 TO WS-LT-CNT
               MOVE WS-LT-CNT TO WS-I
               COMPUTE WS-LT-NUM(WS-I) = FUNCTION NUMVAL(WS-R-NUM)
               MOVE WS-R-ITEM TO WS-LT-ITEM(WS-I)
               MOVE WS-R-REV TO WS-LT-REV(WS-I)
               MOVE WS-R-UOM TO WS-LT-UOM(WS-I)
               COMPUTE WS-LT-QTY(WS-I) = FUNCTION NUMVAL(WS-R-QTY)
               MOVE WS-R-PSRC TO WS-LT-PSRC(WS-I)
               MOVE WS-R-VAT TO WS-LT-VATCODE(WS-I)
               MOVE WS-R-TYPE TO WS-LT-TYPE(WS-I)
               MOVE WS-R-STATUS TO WS-LT-STATUS(WS-I)
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-LROW
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LROW.
       0400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   VALIDATE - SO-002, SO-003.  Every fault is recorded, then
      *>   the worst severity decides the return code.  An incomplete
      *>   draft can never be treated as accepted demand.
       1000-VALIDATE-ORDER SECTION.
       1000-START.
           PERFORM 0300-READ-ORDER
           IF WS-RC > 4
               GO TO 1000-EXIT
           END-IF
           IF SA-COMM-STATUS = "CANCELLED" OR
              SA-COMM-STATUS = "COMPLETED"
               MOVE "SOA0010" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Order " FUNCTION TRIM(SA-ORDER-NUMBER)
                      " is " FUNCTION TRIM(SA-COMM-STATUS)
                      " and cannot be validated for acceptance"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 1000-EXIT
           END-IF
           PERFORM 0400-READ-ORDER-LINES
           IF WS-RC > 4
               GO TO 1000-EXIT
           END-IF
           PERFORM 1100-CHECK-HEADER
           PERFORM 1200-CHECK-LINES
           PERFORM 1900-SCORE-THE-FINDINGS.
       1000-EXIT.
           EXIT.

      *>   the commercial fields an order must carry before it binds
       1100-CHECK-HEADER SECTION.
       1100-START.
           MOVE "SOA0002" TO WS-REASON
           MOVE 0 TO WS-LINE-NO
           IF SA-CUSTOMER = SPACES
               PERFORM 1150-MISSING-CUSTOMER
           END-IF
           IF SA-INVOICE-CUST = SPACES
               MOVE "BLOCKER" TO WS-SEV
               MOVE "InvoiceCustomerCode" TO WS-FIELD
               MOVE SPACES TO WS-TEXT
               STRING "The invoiced party is not stated - name it "
                      "even when it is the ordering party"
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9800-ADD-FINDING
           END-IF
           IF SA-DELIVERY-REF = SPACES AND SA-DELIVERY-SNAP = SPACES
               MOVE "BLOCKER" TO WS-SEV
               MOVE "DeliveryAddress" TO WS-FIELD
               MOVE SPACES TO WS-TEXT
               STRING "A delivery address is required before the "
                      "order can be accepted"
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9800-ADD-FINDING
           END-IF
           IF SA-CURRENCY = SPACES
               MOVE "BLOCKER" TO WS-SEV
               MOVE "CurrencyCode" TO WS-FIELD
               MOVE "A currency is required" TO WS-TEXT
               PERFORM 9800-ADD-FINDING
           END-IF
           IF SA-TERMS-CODE = SPACES
               MOVE "BLOCKER" TO WS-SEV
               MOVE "TermsCode" TO WS-FIELD
               MOVE "Payment terms are required" TO WS-TEXT
               PERFORM 9800-ADD-FINDING
           END-IF
           IF SA-SALESPERSON = SPACES
               MOVE "BLOCKER" TO WS-SEV
               MOVE "SalespersonCode" TO WS-FIELD
               MOVE "A salesperson is required" TO WS-TEXT
               PERFORM 9800-ADD-FINDING
           END-IF
           IF SA-ORDER-DATE = 0
               MOVE "BLOCKER" TO WS-SEV
               MOVE "OrderDate" TO WS-FIELD
               MOVE "An order date is required" TO WS-TEXT
               PERFORM 9800-ADD-FINDING
           END-IF
           IF SA-TC-VERSION = SPACES
               MOVE "BLOCKER" TO WS-SEV
               MOVE "TermsAndConditionsVersion" TO WS-FIELD
               MOVE SPACES TO WS-TEXT
               STRING "The terms and conditions version, including "
                      "retention of title, must be recorded"
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9800-ADD-FINDING
           END-IF
           IF SA-VAT-BASIS = SPACES
               MOVE "BLOCKER" TO WS-SEV
               MOVE "VatTreatmentBasis" TO WS-FIELD
               MOVE SPACES TO WS-TEXT
               STRING "The basis of the VAT treatment must be stated"
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9800-ADD-FINDING
           END-IF
           IF SA-IS-INTERNATIONAL
               PERFORM 1160-CHECK-INCOTERMS
           END-IF
           IF SA-LINE-COUNT = 0
               MOVE "BLOCKER" TO WS-SEV
               MOVE "SalesOrderLine" TO WS-FIELD
               MOVE SPACES TO WS-TEXT
               STRING "The order has no open lines - there is no "
                      "demand to accept" DELIMITED BY SIZE INTO
                      WS-TEXT
               MOVE "SOA0007" TO WS-REASON
               PERFORM 9800-ADD-FINDING
           END-IF.
       1100-EXIT.
           EXIT.

       1150-MISSING-CUSTOMER SECTION.
       1150-START.
           MOVE "BLOCKER" TO WS-SEV
           MOVE "CustomerCode" TO WS-FIELD
           MOVE SPACES TO WS-TEXT
           STRING "The ordering customer is not stated"
               DELIMITED BY SIZE INTO WS-TEXT
           PERFORM 9800-ADD-FINDING.
       1150-EXIT.
           EXIT.

      *>   SO-002 / CR3004 - an international sale states the rule and
      *>   the named place, otherwise the delivery obligation and the
      *>   point of risk are not agreed
       1160-CHECK-INCOTERMS SECTION.
       1160-START.
           IF SA-INCOTERMS-RULE NOT = SPACES AND
              SA-INCOTERMS-PLACE NOT = SPACES
               GO TO 1160-EXIT
           END-IF
           MOVE "BLOCKER" TO WS-SEV
           MOVE "IncotermsRule" TO WS-FIELD
           MOVE SPACES TO WS-TEXT
           STRING "An international sale needs an Incoterms rule and "
                  "a named place" DELIMITED BY SIZE INTO WS-TEXT
           MOVE "SOA0011" TO WS-REASON
           PERFORM 9800-ADD-FINDING.
       1160-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1200-CHECK-LINES SECTION.
       1200-START.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LT-CNT
               IF WS-LT-TYPE(WS-I) NOT = "TEXT"
                   PERFORM 1250-CHECK-ONE-LINE
               END-IF
           END-PERFORM.
       1200-EXIT.
           EXIT.

       1250-CHECK-ONE-LINE SECTION.
       1250-START.
           MOVE WS-LT-NUM(WS-I) TO WS-LINE-NO
           IF WS-LT-PSRC(WS-I) = SPACES
               MOVE "BLOCKER" TO WS-SEV
               MOVE "PriceSource" TO WS-FIELD
               MOVE SPACES TO WS-TEXT
               STRING "No price source is recorded - the selected "
                      "price cannot be explained"
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE "SOA0004" TO WS-REASON
               PERFORM 9800-ADD-FINDING
           END-IF
           IF WS-LT-VATCODE(WS-I) = SPACES
               MOVE "BLOCKER" TO WS-SEV
               MOVE "VatCode" TO WS-FIELD
               MOVE SPACES TO WS-TEXT
               STRING "No VAT code is recorded for this line"
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE "SOA0004" TO WS-REASON
               PERFORM 9800-ADD-FINDING
           END-IF
           IF WS-LT-ITEM(WS-I) = SPACES
               GO TO 1250-EXIT
           END-IF
           PERFORM 1300-ITEM-ELIGIBILITY
           PERFORM 1400-UNIT-COMPATIBILITY.
       1250-EXIT.
           EXIT.

      *>   SO-003 - an obsolete or restricted item is an explicit
      *>   blocker.  Only a policy-governed authority, carried in
      *>   SA-ITEM-AUTHORITY, turns it into a reviewed exception.
       1300-ITEM-ELIGIBILITY SECTION.
       1300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT i.LifecycleStatus, ISNULL(i.SalesUomCode, "
                  "i.StockUomCode), CONVERT(varchar(1), "
                  "i.IsSaleable), ISNULL((SELECT TOP (1) r.Status "
                  "FROM inv.ItemRevision r WHERE r.CompanyCode = "
                  "i.CompanyCode AND r.ItemCode = i.ItemCode AND "
                  "r.Revision = ?),'') FROM inv.Item i WHERE "
                  "i.CompanyCode = ? AND i.ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-LT-REV(WS-I) TO SQLIO-PARM-VAL(1)
           MOVE WS-CO TO SQLIO-PARM-VAL(2)
           MOVE WS-LT-ITEM(WS-I) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-COL-WIDTH(3)
           MOVE 10 TO SQLIO-COL-WIDTH(4)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ITM
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1300-EXIT
           END-IF
           MOVE "SOA0003" TO WS-REASON
           IF SQLIO-NOTFOUND
               MOVE "BLOCKER" TO WS-SEV
               MOVE "ItemCode" TO WS-FIELD
               MOVE SPACES TO WS-TEXT
               STRING "Item " FUNCTION TRIM(WS-LT-ITEM(WS-I))
                      " is not in this company's catalogue"
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9800-ADD-FINDING
               GO TO 1300-EXIT
           END-IF
           IF WS-IT-LIFECYCLE = "OBSOLETE" OR
              WS-IT-LIFECYCLE = "RETIRED" OR
              WS-IT-LIFECYCLE = "BLOCKED" OR
              WS-IT-SALEABLE = "0"
               PERFORM 1350-ITEM-NOT-ELIGIBLE
           END-IF
           IF WS-IT-REVSTATUS = "SUPERSEDED" OR
              WS-IT-REVSTATUS = "RETIRED"
               MOVE "WARNING" TO WS-SEV
               MOVE "SOA0003" TO WS-REASON
               MOVE "Revision" TO WS-FIELD
               MOVE SPACES TO WS-TEXT
               STRING "Revision " FUNCTION TRIM(WS-LT-REV(WS-I))
                      " of " FUNCTION TRIM(WS-LT-ITEM(WS-I))
                      " is " FUNCTION TRIM(WS-IT-REVSTATUS)
                      " - confirm the customer accepts it"
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9800-ADD-FINDING
           END-IF.
       1300-EXIT.
           EXIT.

       1350-ITEM-NOT-ELIGIBLE SECTION.
       1350-START.
           MOVE SPACES TO WS-TEXT
           IF SA-ITEM-AUTHORITY = SPACES
               MOVE "BLOCKER" TO WS-SEV
               STRING "Item " FUNCTION TRIM(WS-LT-ITEM(WS-I))
                      " is " FUNCTION TRIM(WS-IT-LIFECYCLE)
                      " and is not eligible for sale"
                   DELIMITED BY SIZE INTO WS-TEXT
           ELSE
               MOVE "WARNING" TO WS-SEV
               STRING "Item " FUNCTION TRIM(WS-LT-ITEM(WS-I))
                      " is " FUNCTION TRIM(WS-IT-LIFECYCLE)
                      " - sold under authority "
                      FUNCTION TRIM(SA-ITEM-AUTHORITY)
                   DELIMITED BY SIZE INTO WS-TEXT
           END-IF
           MOVE "ItemCode" TO WS-FIELD
           PERFORM 9800-ADD-FINDING.
       1350-EXIT.
           EXIT.

      *>   ITM-005 - a missing conversion is an error, never an
      *>   implied one for one.  SOACCEPT only asks the question.
       1400-UNIT-COMPATIBILITY SECTION.
       1400-START.
           IF WS-LT-UOM(WS-I) = SPACES
               MOVE "BLOCKER" TO WS-SEV
               MOVE "UomCode" TO WS-FIELD
               MOVE "SOA0005" TO WS-REASON
               MOVE SPACES TO WS-TEXT
               STRING "No unit of measure is recorded for line item "
                      FUNCTION TRIM(WS-LT-ITEM(WS-I))
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9800-ADD-FINDING
               GO TO 1400-EXIT
           END-IF
           INITIALIZE WS-UOM-CA
           MOVE "TOSTOCK" TO CA-FUNCTION OF WS-UOM-CA
           MOVE WS-CO TO CA-COMPANY OF WS-UOM-CA
           MOVE WS-SITE TO CA-SITE OF WS-UOM-CA
           MOVE WS-USER TO CA-USER OF WS-UOM-CA
           MOVE WS-BUSDATE TO CA-BUSINESS-DATE OF WS-UOM-CA
           MOVE WS-LT-ITEM(WS-I) TO UC-ITEM-CODE
           MOVE WS-LT-UOM(WS-I) TO UC-FROM-UOM
           MOVE "SALES" TO UC-USAGE-TYPE
           MOVE SA-ORDER-DATE TO UC-EFF-DATE
           MOVE WS-LT-QTY(WS-I) TO UC-ENTERED-QTY
           CALL "UOMCONV" USING WS-UOM-CA
           IF CA-RETURN-CODE OF WS-UOM-CA < 8
               GO TO 1400-EXIT
           END-IF
           MOVE "BLOCKER" TO WS-SEV
           MOVE "UomCode" TO WS-FIELD
           MOVE "SOA0005" TO WS-REASON
           MOVE SPACES TO WS-TEXT
           STRING "Unit " FUNCTION TRIM(WS-LT-UOM(WS-I))
                  " is not compatible with item "
                  FUNCTION TRIM(WS-LT-ITEM(WS-I)) " - "
                  FUNCTION TRIM(CA-MESSAGE OF WS-UOM-CA)
               DELIMITED BY SIZE INTO WS-TEXT
           PERFORM 9800-ADD-FINDING.
       1400-EXIT.
           EXIT.

      *>   one blocker is enough to refuse; warnings alone are 04
       1900-SCORE-THE-FINDINGS SECTION.
       1900-START.
           IF WS-RC > 4
               GO TO 1900-EXIT
           END-IF
           IF SA-HARD-CNT > 0
               MOVE 8 TO WS-RC
               MOVE SA-HARD-CNT TO WS-INT-ED
               MOVE SPACES TO WS-MSG
               STRING "Order " FUNCTION TRIM(SA-ORDER-NUMBER)
                      " is not complete: "
                      FUNCTION TRIM(WS-INT-ED)
                      " blocker(s) must be cleared before acceptance"
                   DELIMITED BY SIZE INTO WS-MSG
               MOVE SA-BLOCKER-CNT TO WS-MSGCNT
               GO TO 1900-EXIT
           END-IF
           IF SA-BLOCKER-CNT > 0
               MOVE 4 TO WS-RC
               MOVE SA-BLOCKER-CNT TO WS-MSGCNT
               MOVE SPACES TO WS-MSG
               STRING "Order " FUNCTION TRIM(SA-ORDER-NUMBER)
                      " is complete but carries findings for review"
                   DELIMITED BY SIZE INTO WS-MSG
           ELSE
               MOVE SPACES TO WS-MSG
               STRING "Order " FUNCTION TRIM(SA-ORDER-NUMBER)
                      " is complete and eligible for acceptance"
                   DELIMITED BY SIZE INTO WS-MSG
           END-IF.
       1900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   ACCEPT - SO-003, SO-007.  Validate, take the current credit
      *>   position, then write the commercial result inside one
      *>   BEGN..COMT and leave an audit trail.
       2000-ACCEPT-ORDER SECTION.
       2000-START.
           PERFORM 1000-VALIDATE-ORDER
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-CREDIT-POSITION
           IF WS-RC > 6
               GO TO 2000-EXIT
           END-IF
           PERFORM 2150-DECIDE-RELEASE
           PERFORM 8000-BEGIN-TRAN
           PERFORM 2200-APPLY-ACCEPTANCE
           IF WS-RC > 6
               GO TO 2000-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           MOVE "ACCEPTED" TO SA-COMM-STATUS
           MOVE WS-NEW-REL TO SA-RELEASE-STATUS
           PERFORM 7500-AUDIT-THE-CHANGE
           PERFORM 2300-REPORT-ACCEPTANCE.
       2000-EXIT.
           EXIT.

      *>   AR-007 / D-06.  CREDCHK owns the exposure arithmetic; this
      *>   program only applies the answer to the order.
       2100-CREDIT-POSITION SECTION.
       2100-START.
           INITIALIZE WS-CRED-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-CRED-CA
           MOVE WS-CO TO CA-COMPANY OF WS-CRED-CA
           MOVE WS-SITE TO CA-SITE OF WS-CRED-CA
           MOVE WS-USER TO CA-USER OF WS-CRED-CA
           MOVE WS-BUSDATE TO CA-BUSINESS-DATE OF WS-CRED-CA
           MOVE WS-CORREL TO CA-CORRELATION-ID OF WS-CRED-CA
           MOVE SA-CUSTOMER TO CK-CUSTOMER
           IF WS-FUNC = "RELEASE"
               MOVE "DESPATCH" TO CK-CHECK-POINT
           ELSE
               MOVE "ORDER" TO CK-CHECK-POINT
           END-IF
           MOVE SA-ORDER-NUMBER TO CK-DOCUMENT-REF
           MOVE SA-ORDER-NUMBER TO CK-EXCLUDE-REF
           MOVE WS-BUSDATE TO CK-EVAL-DATE
           MOVE SA-CURRENCY TO CK-CURRENCY
           MOVE SA-FX-RATE TO CK-FX-RATE
           MOVE SA-ORDER-DATE TO CK-RATE-DATE
           MOVE SA-GROSS-AMOUNT TO CK-ORDER-VALUE
           MOVE SA-GROSS-GBP TO CK-ORDER-VALUE-GBP
           MOVE SA-OVERRIDE-ID TO CK-OVERRIDE-ID
           CALL "CREDCHK" USING WS-CRED-CA
           MOVE CK-DECISION TO SA-CREDIT-DECISION
           MOVE CK-EXPOSURE TO SA-CREDIT-EXPOSURE
           MOVE CK-LIMIT TO SA-CREDIT-LIMIT
           MOVE CK-HEADROOM TO SA-CREDIT-HEADROOM
           MOVE CK-EXCESS TO SA-CREDIT-EXCESS
           MOVE CK-OVERDUE-GBP TO SA-OVERDUE-GBP
           MOVE CK-OVERDUE-DAYS TO SA-OVERDUE-DAYS
           MOVE CK-OVERRIDE-ID TO SA-OVERRIDE-ID
           MOVE CK-OVERRIDE-EXPIRY TO SA-OVERRIDE-EXPIRY
           MOVE CK-OVERRIDE-ACTIVE TO SA-OVERRIDE-ACTIVE
           MOVE CK-ACCOUNT-STATUS TO SA-ACCOUNT-STATUS
           IF CA-RETURN-CODE OF WS-CRED-CA > 11
               MOVE 12 TO WS-RC
               MOVE "SOA0008" TO WS-REASON
               MOVE CA-MESSAGE OF WS-CRED-CA TO WS-MSG
               ADD 1 TO WS-MSGCNT
               GO TO 2100-EXIT
           END-IF
           MOVE CK-DECISION-TEXT TO WS-HOLD-TEXT
           IF WS-HOLD-TEXT = SPACES
               MOVE CK-HOLD-REASON TO WS-HOLD-TEXT
           END-IF.
       2100-EXIT.
           EXIT.

      *>   D-06 - an invoice more than thirty days past due holds the
      *>   new release unless a scoped override is still inside its
      *>   five working day life.
       2150-DECIDE-RELEASE SECTION.
       2150-START.
           MOVE "RELEASED" TO WS-NEW-REL
           MOVE SPACES TO SA-HOLD-REASON
           IF SA-OVERDUE-DAYS > 30
               IF SA-OVERRIDE-ACTIVE NOT = "Y" OR
                  SA-OVERRIDE-EXPIRY < WS-BUSDATE
                   MOVE "CREDITHOLD" TO WS-NEW-REL
                   MOVE SA-OVERDUE-DAYS TO WS-INT-ED
                   MOVE SA-OVERDUE-GBP TO WS-UK-MONEY
                   MOVE SPACES TO SA-HOLD-REASON
                   STRING "Invoices " FUNCTION TRIM(WS-INT-ED)
                          " days past due totalling "
                          FUNCTION TRIM(WS-UK-MONEY)
                          " hold new release (D-06)"
                       DELIMITED BY SIZE INTO SA-HOLD-REASON
               END-IF
           END-IF
           IF CK-HELD OR CK-BLOCKED
               MOVE "CREDITHOLD" TO WS-NEW-REL
               IF SA-HOLD-REASON = SPACES
                   MOVE WS-HOLD-TEXT TO SA-HOLD-REASON
               END-IF
           END-IF
           IF WS-NEW-REL = "RELEASED" AND SA-BLOCKER-CNT > 0
               MOVE "COMMERCIALHOLD" TO WS-NEW-REL
               MOVE SPACES TO SA-HOLD-REASON
               STRING "Commercial findings on the order require "
                      "review before release"
                   DELIMITED BY SIZE INTO SA-HOLD-REASON
           END-IF
           IF WS-NEW-REL = "RELEASED" AND SA-DEPOSIT-REQ-GBP > 0 AND
              SA-DEPOSIT-RCVD-GBP < SA-DEPOSIT-REQ-GBP
               MOVE "COMMERCIALHOLD" TO WS-NEW-REL
               COMPUTE SA-DEPOSIT-OUTSTAND =
                       SA-DEPOSIT-REQ-GBP - SA-DEPOSIT-RCVD-GBP
               MOVE SA-DEPOSIT-OUTSTAND TO WS-UK-MONEY
               MOVE SPACES TO SA-HOLD-REASON
               STRING "Cleared deposit of "
                      FUNCTION TRIM(WS-UK-MONEY)
                      " is still outstanding (SO-019)"
                   DELIMITED BY SIZE INTO SA-HOLD-REASON
           END-IF
           IF WS-NEW-REL NOT = "RELEASED" AND WS-RC < 6
               MOVE 6 TO WS-RC
               MOVE "SOA0008" TO WS-REASON
           END-IF.
       2150-EXIT.
           EXIT.

       2200-APPLY-ACCEPTANCE SECTION.
       2200-START.
           IF WS-RC > 6
               GO TO 2200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrder SET CommercialStatus = "
                  "'ACCEPTED', ReleaseStatus = ?, HoldReason = "
                  "NULLIF(?,''), CreditOverrideId = NULLIF(CONVERT("
                  "int, ?),0), ApprovalId = NULLIF(CONVERT(bigint, "
                  "?),0), ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE CompanyCode = ? AND OrderNumber = ? AND "
                  "CommercialStatus IN ('DRAFT','ACCEPTED','AMENDED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE WS-NEW-REL TO SQLIO-PARM-VAL(1)
           MOVE SA-HOLD-REASON TO SQLIO-PARM-VAL(2)
           MOVE SA-OVERRIDE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE SA-APPROVAL-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE WS-USER TO SQLIO-PARM-VAL(5)
           MOVE WS-CO TO SQLIO-PARM-VAL(6)
           MOVE SA-ORDER-NUMBER TO SQLIO-PARM-VAL(7)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 7
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS.
       2200-EXIT.
           EXIT.

       2300-REPORT-ACCEPTANCE SECTION.
       2300-START.
           MOVE SPACES TO WS-MSG
           IF WS-NEW-REL = "RELEASED"
               STRING "Order " FUNCTION TRIM(SA-ORDER-NUMBER)
                      " accepted and released for fulfilment"
                   DELIMITED BY SIZE INTO WS-MSG
           ELSE
               STRING "Order " FUNCTION TRIM(SA-ORDER-NUMBER)
                      " accepted but held - "
                      FUNCTION TRIM(SA-HOLD-REASON)
                   DELIMITED BY SIZE INTO WS-MSG
           END-IF.
       2300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   RELEASE - SO-011.  The whole point of this entry is that it
      *>   re-asks the CURRENT questions.  An order accepted last week
      *>   is held today when the customer's position has worsened.
      *>   Printing does not release, and release does not reserve.
       3000-RELEASE-ORDER SECTION.
       3000-START.
           PERFORM 0300-READ-ORDER
           IF WS-RC > 4
               GO TO 3000-EXIT
           END-IF
           IF SA-PRINT-REQUEST
               PERFORM 3050-PRINT-IS-NOT-RELEASE
               GO TO 3000-EXIT
           END-IF
           IF SA-COMM-STATUS NOT = "ACCEPTED" AND
              SA-COMM-STATUS NOT = "AMENDED"
               MOVE "SOA0014" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Order " FUNCTION TRIM(SA-ORDER-NUMBER)
                      " is " FUNCTION TRIM(SA-COMM-STATUS)
                      " - it must be accepted before it is released"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           PERFORM 0400-READ-ORDER-LINES
           PERFORM 1100-CHECK-HEADER
           PERFORM 1200-CHECK-LINES
           IF SA-HARD-CNT > 0
               PERFORM 1900-SCORE-THE-FINDINGS
               GO TO 3000-EXIT
           END-IF
           PERFORM 2100-CREDIT-POSITION
           IF WS-RC > 6
               GO TO 3000-EXIT
           END-IF
           PERFORM 3100-SUPPLY-AND-APPROVAL
           IF WS-RC > 6
               GO TO 3000-EXIT
           END-IF
           PERFORM 2150-DECIDE-RELEASE
           IF WS-SUPPLY-GAPS > 0 AND WS-NEW-REL = "RELEASED"
               MOVE "SUPPLYHOLD" TO WS-NEW-REL
               MOVE WS-SUPPLY-GAPS TO WS-INT-ED
               MOVE SPACES TO SA-HOLD-REASON
               STRING FUNCTION TRIM(WS-INT-ED)
                      " linked supply line(s) have no purchase "
                      "commitment yet"
                   DELIMITED BY SIZE INTO SA-HOLD-REASON
               MOVE 6 TO WS-RC
               MOVE "SOA0008" TO WS-REASON
           END-IF
           IF WS-APPROVAL-GAPS > 0 AND WS-NEW-REL = "RELEASED"
               MOVE "AWAITAPPROVAL" TO WS-NEW-REL
               MOVE SPACES TO SA-HOLD-REASON
               STRING "A required approval is still outstanding on "
                      "at least one line"
                   DELIMITED BY SIZE INTO SA-HOLD-REASON
               MOVE 6 TO WS-RC
               MOVE "SOA0008" TO WS-REASON
           END-IF
           PERFORM 8000-BEGIN-TRAN
           PERFORM 3200-APPLY-RELEASE
           IF WS-RC > 6
               GO TO 3000-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           MOVE WS-NEW-REL TO SA-RELEASE-STATUS
           PERFORM 7500-AUDIT-THE-CHANGE
           MOVE SPACES TO WS-MSG
           IF WS-NEW-REL = "RELEASED"
               STRING "Order " FUNCTION TRIM(SA-ORDER-NUMBER)
                      " released; no stock is reserved by this "
                      "instruction" DELIMITED BY SIZE INTO WS-MSG
           ELSE
               STRING "Order " FUNCTION TRIM(SA-ORDER-NUMBER)
                      " remains held - "
                      FUNCTION TRIM(SA-HOLD-REASON)
                   DELIMITED BY SIZE INTO WS-MSG
           END-IF.
       3000-EXIT.
           EXIT.

      *>   AC-042 - a reprint is evidence of what was sent, nothing
      *>   more.  The release status is deliberately left alone.
       3050-PRINT-IS-NOT-RELEASE SECTION.
       3050-START.
           PERFORM 8000-BEGIN-TRAN
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrder SET AcknowledgementIssuedAt "
                  "= SYSDATETIME(), ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE CompanyCode = ? AND "
                  "OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-USER TO SQLIO-PARM-VAL(1)
           MOVE WS-CO TO SQLIO-PARM-VAL(2)
           MOVE SA-ORDER-NUMBER TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC > 4
               GO TO 3050-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           ADD 1 TO SA-ACK-PRINT-COUNT
           MOVE 4 TO WS-RC
           MOVE "SOA0013" TO WS-REASON
           MOVE SPACES TO WS-MSG
           STRING "Acknowledgement issued for order "
                  FUNCTION TRIM(SA-ORDER-NUMBER)
                  "; printing does not release a held order and is "
                  "not an override"
               DELIMITED BY SIZE INTO WS-MSG
           ADD 1 TO WS-MSGCNT.
       3050-EXIT.
           EXIT.

       3100-SUPPLY-AND-APPROVAL SECTION.
       3100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(10), SUM(CASE WHEN "
                  "l.SupplyType IN ('BACKTOBACK','DROPSHIP') AND "
                  "l.LinkedPoNumber IS NULL THEN 1 ELSE 0 END)), "
                  "CONVERT(varchar(10), SUM(CASE WHEN "
                  "l.MarginApprovalId IS NULL AND l.MarginPct IS NOT "
                  "NULL AND l.MarginPct < 0 THEN 1 ELSE 0 END)) FROM "
                  "so.SalesOrderLine l WHERE l.CompanyCode = ? AND "
                  "l.OrderNumber = ? AND l.Status = 'OPEN'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SA-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 0 TO WS-SUPPLY-GAPS WS-APPROVAL-GAPS
           ELSE
               COMPUTE WS-SUPPLY-GAPS = FUNCTION NUMVAL(WS-T1)
               COMPUTE WS-APPROVAL-GAPS = FUNCTION NUMVAL(WS-T2)
           END-IF.
       3100-EXIT.
           EXIT.

       3200-APPLY-RELEASE SECTION.
       3200-START.
           IF WS-RC > 6
               GO TO 3200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrder SET ReleaseStatus = ?, "
                  "HoldReason = NULLIF(?,''), ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE CompanyCode = ? "
                  "AND OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE WS-NEW-REL TO SQLIO-PARM-VAL(1)
           MOVE SA-HOLD-REASON TO SQLIO-PARM-VAL(2)
           MOVE WS-USER TO SQLIO-PARM-VAL(3)
           MOVE WS-CO TO SQLIO-PARM-VAL(4)
           MOVE SA-ORDER-NUMBER TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS.
       3200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   DEPOSIT - SO-019.  The order records what is required and
      *>   what has cleared; ARDEPOS owns the receivable and the tax
      *>   point.  A deposit is never ordinary sales revenue, and
      *>   uncleared funds do not count towards the requirement.
       4000-TAKE-DEPOSIT SECTION.
       4000-START.
      *>       the caller's required figure must survive the read of
      *>       whatever the order currently holds
           MOVE SA-DEPOSIT-REQ-GBP TO WS-REQ-IN
           PERFORM 0300-READ-ORDER
           IF WS-RC > 4
               GO TO 4000-EXIT
           END-IF
           IF WS-REQ-IN > 0
               MOVE WS-REQ-IN TO SA-DEPOSIT-REQ-GBP
           END-IF
           IF SA-DEPOSIT-AMOUNT <= 0 AND SA-DEPOSIT-REQ-GBP <= 0
               MOVE "SOA0012" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A deposit needs a positive required or "
                      "received amount" DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 4000-EXIT
           END-IF
           IF SA-DEPOSIT-AMOUNT > 0
               PERFORM 4100-HAND-TO-ARDEPOS
               IF WS-RC > 4
                   GO TO 4000-EXIT
               END-IF
           END-IF
           IF SA-DEPOSIT-CLEARED = "Y"
               ADD SA-DEPOSIT-AMOUNT TO SA-DEPOSIT-RCVD-GBP
           END-IF
           COMPUTE SA-DEPOSIT-OUTSTAND =
                   SA-DEPOSIT-REQ-GBP - SA-DEPOSIT-RCVD-GBP
           IF SA-DEPOSIT-OUTSTAND < 0
               MOVE 0 TO SA-DEPOSIT-OUTSTAND
           END-IF
           PERFORM 8000-BEGIN-TRAN
           PERFORM 4200-UPDATE-ORDER-DEPOSIT
           IF WS-RC > 4
               GO TO 4000-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           PERFORM 7500-AUDIT-THE-CHANGE
           MOVE SA-DEPOSIT-RCVD-GBP TO WS-UK-MONEY
           MOVE SPACES TO WS-MSG
           IF SA-DEPOSIT-CLEARED = "Y"
               STRING "Cleared deposit recorded on order "
                      FUNCTION TRIM(SA-ORDER-NUMBER)
                      "; held as a liability, not as sales revenue"
                   DELIMITED BY SIZE INTO WS-MSG
           ELSE
               MOVE 4 TO WS-RC
               MOVE "SOA0012" TO WS-REASON
               ADD 1 TO WS-MSGCNT
               STRING "Deposit recorded but not yet cleared - only "
                      "cleared funds count towards the requirement"
                   DELIMITED BY SIZE INTO WS-MSG
           END-IF.
       4000-EXIT.
           EXIT.

       4100-HAND-TO-ARDEPOS SECTION.
       4100-START.
           INITIALIZE WS-DEP-CA
           MOVE "DEPOSIT" TO CA-FUNCTION OF WS-DEP-CA
           MOVE WS-CO TO CA-COMPANY OF WS-DEP-CA
           MOVE WS-SITE TO CA-SITE OF WS-DEP-CA
           MOVE WS-USER TO CA-USER OF WS-DEP-CA
           MOVE WS-BUSDATE TO CA-BUSINESS-DATE OF WS-DEP-CA
           MOVE WS-CORREL TO CA-CORRELATION-ID OF WS-DEP-CA
           MOVE SA-CUSTOMER TO DP-CUSTOMER
           MOVE SA-DEPOSIT-REF TO DP-DEPOSIT-REF
           MOVE "ADVANCE" TO DP-PURPOSE
           MOVE SA-DEPOSIT-METHOD TO DP-METHOD
           MOVE WS-BUSDATE TO DP-DOC-DATE
           MOVE WS-BUSDATE TO DP-POSTING-DATE
           MOVE SA-CURRENCY TO DP-CURRENCY
           MOVE SA-FX-RATE TO DP-FX-RATE
           MOVE SA-DEPOSIT-AMOUNT TO DP-AMOUNT
           MOVE SA-DEPOSIT-VAT-CODE TO DP-VAT-CODE
           MOVE SA-DEPOSIT-CLEARED TO DP-CLEARED
           MOVE SPACES TO DP-REASON
           STRING "Deposit against sales order "
                  FUNCTION TRIM(SA-ORDER-NUMBER)
               DELIMITED BY SIZE INTO DP-REASON
           CALL "ARDEPOS" USING WS-DEP-CA
           MOVE DP-DEPOSIT-ID TO SA-DEPOSIT-ID
           MOVE DP-RESIDUAL TO SA-DEPOSIT-RESIDUAL
           IF DP-DEPOSIT-REF NOT = SPACES
               MOVE DP-DEPOSIT-REF TO SA-DEPOSIT-REF
           END-IF
           IF CA-RETURN-CODE OF WS-DEP-CA > 4
               MOVE CA-RETURN-CODE OF WS-DEP-CA TO WS-RC
               MOVE "SOA0012" TO WS-REASON
               MOVE CA-MESSAGE OF WS-DEP-CA TO WS-MSG
               ADD 1 TO WS-MSGCNT
           END-IF.
       4100-EXIT.
           EXIT.

       4200-UPDATE-ORDER-DEPOSIT SECTION.
       4200-START.
           IF WS-RC > 4
               GO TO 4200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrder SET DepositRequiredGBP = "
                  "CONVERT(decimal(18,2), ?), DepositReceivedGBP = "
                  "CONVERT(decimal(18,2), ?), ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE CompanyCode = ? "
                  "AND OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE SA-DEPOSIT-REQ-GBP TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(1)
           MOVE SA-DEPOSIT-RCVD-GBP TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(2)
           MOVE WS-USER TO SQLIO-PARM-VAL(3)
           MOVE WS-CO TO SQLIO-PARM-VAL(4)
           MOVE SA-ORDER-NUMBER TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS.
       4200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   COM-013 - the attributable history of the decision
       7500-AUDIT-THE-CHANGE SECTION.
       7500-START.
           IF WS-RC > 6
               GO TO 7500-EXIT
           END-IF
           INITIALIZE WS-AUD-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUD-CA
           MOVE WS-CO TO CA-COMPANY OF WS-AUD-CA
           MOVE WS-SITE TO CA-SITE OF WS-AUD-CA
           MOVE WS-USER TO CA-USER OF WS-AUD-CA
           MOVE WS-BUSDATE TO CA-BUSINESS-DATE OF WS-AUD-CA
           MOVE WS-CORREL TO CA-CORRELATION-ID OF WS-AUD-CA
           MOVE "so.SalesOrder" TO AU-ENTITY-TYPE
           MOVE SPACES TO AU-ENTITY-KEY
           STRING FUNCTION TRIM(WS-CO) "/"
                  FUNCTION TRIM(SA-ORDER-NUMBER)
               DELIMITED BY SIZE INTO AU-ENTITY-KEY
           MOVE WS-FUNC TO AU-ACTION
           MOVE "ReleaseStatus" TO AU-FIELD-NAME
           MOVE SA-PREV-REL-STATUS TO AU-OLD-VALUE
           MOVE WS-NEW-REL TO AU-NEW-VALUE
           IF WS-FUNC = "DEPOSIT"
               MOVE "DepositReceivedGBP" TO AU-FIELD-NAME
               MOVE SA-DEPOSIT-RCVD-GBP TO WS-MONEY-ED
               MOVE WS-MONEY-ED TO AU-NEW-VALUE
           END-IF
           MOVE SPACES TO AU-REASON
           IF SA-HOLD-REASON = SPACES
               STRING "SOACCEPT " FUNCTION TRIM(WS-FUNC)
                   DELIMITED BY SIZE INTO AU-REASON
           ELSE
               STRING "SOACCEPT " FUNCTION TRIM(WS-FUNC) " - "
                      FUNCTION TRIM(SA-HOLD-REASON)
                   DELIMITED BY SIZE INTO AU-REASON
           END-IF
           MOVE SPACES TO AU-AUTHORITY-REF
           IF SA-OVERRIDE-ID NOT = 0
               MOVE SA-OVERRIDE-ID TO WS-INT-ED
               STRING "CREDIT OVERRIDE "
                      FUNCTION TRIM(WS-INT-ED)
                   DELIMITED BY SIZE INTO AU-AUTHORITY-REF
           END-IF
           MOVE "SOACCEPT" TO AU-SOURCE
           MOVE "N" TO AU-SENSITIVE
           CALL "AUDITLOG" USING WS-AUD-CA.
       7500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-BEGIN-TRAN SECTION.
       8000-START.
           IF WS-RC > 6 OR WS-TRAN-OPEN = "Y"
               GO TO 8000-EXIT
           END-IF
           MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
           ELSE
               MOVE "Y" TO WS-TRAN-OPEN
           END-IF.
       8000-EXIT.
           EXIT.

       8100-COMMIT-TRAN SECTION.
       8100-START.
           IF WS-TRAN-OPEN NOT = "Y"
               GO TO 8100-EXIT
           END-IF
           MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
           MOVE "COMT" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
           END-IF
           MOVE "N" TO WS-TRAN-OPEN.
       8100-EXIT.
           EXIT.

       8200-EXEC-NO-ROWS SECTION.
       8200-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 8200-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO.
       8200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO WS-RC
           ADD 1 TO WS-MSGCNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE 8 TO WS-RC
           MOVE "SOA0009" TO WS-REASON
           MOVE SPACES TO WS-MSG
           STRING "SOACCEPT does not support function ["
                  FUNCTION TRIM(WS-FUNC) "]"
               DELIMITED BY SIZE INTO WS-MSG
           ADD 1 TO WS-MSGCNT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE 12 TO WS-RC
           MOVE "SQL0001" TO WS-REASON
           MOVE SQLIO-ERRMSG TO WS-MSG
           ADD 1 TO WS-MSGCNT.
       9300-EXIT.
           EXIT.

       9500-ROLLBACK SECTION.
       9500-START.
           IF WS-TRAN-OPEN = "Y"
               MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-TWO
               MOVE "N" TO WS-TRAN-OPEN
           END-IF.
       9500-EXIT.
           EXIT.

      *>   every finding is kept so the user sees the whole list at
      *>   once; only BLOCKER rows stop the acceptance
       9800-ADD-FINDING SECTION.
       9800-START.
           IF SA-BLOCKER-CNT >= 30
               GO TO 9800-EXIT
           END-IF
           ADD 1 TO SA-BLOCKER-CNT
           MOVE SA-BLOCKER-CNT TO WS-SLOT
           MOVE WS-SEV TO SA-B-SEVERITY(WS-SLOT)
           MOVE WS-REASON TO SA-B-REASON(WS-SLOT)
           MOVE WS-LINE-NO TO SA-B-LINE-NUM(WS-SLOT)
           MOVE WS-FIELD TO SA-B-FIELD(WS-SLOT)
           MOVE WS-TEXT TO SA-B-TEXT(WS-SLOT)
           IF WS-SEV = "BLOCKER"
               ADD 1 TO SA-HARD-CNT
           END-IF
           MOVE "SOA0002" TO WS-REASON.
       9800-EXIT.
           EXIT.

       9900-SET-RESULT SECTION.
       9900-START.
           MOVE WS-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE WS-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           IF WS-MSGCNT = 0 AND SA-BLOCKER-CNT > 0
               MOVE SA-BLOCKER-CNT TO WS-MSGCNT
           END-IF
           MOVE WS-MSGCNT TO CA-MSG-COUNT OF DFHCOMMAREA.
       9900-EXIT.
           EXIT.
