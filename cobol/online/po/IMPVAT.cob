      *>================================================================
      *> PROGRAM-ID : IMPVAT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : RECORD A CUSTOMS IMPORT ENTRY, CAPITALISE THE
      *>              DUTY INTO THE COST OF THE GOODS AND ACCOUNT FOR
      *>              POSTPONED IMPORT VAT.  DUTY IS AN IRRECOVERABLE
      *>              TAX AND IS PART OF THE COST OF THE GOODS;
      *>              IMPORT VAT IS RECOVERABLE AND IS NEVER LANDED
      *>              COST.  UNDER POSTPONED VAT ACCOUNTING BOTH LEGS
      *>              ARE POSTED TOGETHER SO THE NET EFFECT ON PROFIT
      *>              AND ON CASH IS NIL.
      *> CALLED BY  : IMPORT ENTRY SCREEN, LANDCOST
      *> INTERFACE  : CALL "IMPVAT" USING DFHCOMMAREA  (IMPVATCA)
      *> CALLS      : POPOST, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-11-05 RJH  CR0841 IMPORT ENTRIES AND DUTY
      *> 2021-01-01 TMW  BREXIT - POSTPONED IMPORT VAT ACCOUNTING
      *> 2026-10-06 STERP PO-025
      *>================================================================
      *> REQ: PO-025, PO-015, PO-017
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IMPVAT.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 05/11/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY POACCNTS.
       COPY POPOSTWK.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(60).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-N                    PIC S9(3) VALUE 0.
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-LINE-ED              PIC -(8)9.
           05  WS-CORR                 PIC X(64).
       LINKAGE SECTION.
       COPY IMPVATCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "CALC"
                   PERFORM 2000-CALC THRU 2000-EXIT
               WHEN "DECLARE"
                   PERFORM 2000-CALC THRU 2000-EXIT
                   IF NOT CA-FAILED
                       PERFORM 3000-DECLARE THRU 3000-EXIT
                   END-IF
               WHEN "POST"
                   PERFORM 2000-CALC THRU 2000-EXIT
                   IF NOT CA-FAILED
                       PERFORM 3000-DECLARE THRU 3000-EXIT
                       PERFORM 4000-POST-DUTY THRU 4000-EXIT
                       PERFORM 4500-POST-VAT THRU 4500-EXIT
                   END-IF
      *> CR2614 CA-FUNCTION is X(8) - "RECONCILE" never matched
               WHEN "RECONCIL"
                   PERFORM 5000-RECONCILE THRU 5000-EXIT
               WHEN OTHER
                   MOVE "IMP0005" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                          " is not supported by IMPVAT"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 8 TO CA-RETURN-CODE
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "IMPVAT" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE 0 TO IM-ENTRY-ID IM-CUSTOMS-VAL-GBP
           MOVE 0 TO IM-DUTY-GBP IM-VAT-GBP
           MOVE 0 TO IM-DUTY-TO-STOCK IM-DUTY-TO-COGS
           MOVE 0 TO IM-VAT-BOX1-GBP IM-VAT-BOX4-GBP
           MOVE 0 TO IM-NET-PROFIT-GBP IM-STMT-DIFF-GBP
           MOVE SPACES TO IM-JOURNAL-NUMBER IM-VAT-JOURNAL
           MOVE "PENDING" TO IM-STATEMENT-STATUS
           MOVE "OK" TO IM-OUTCOME
           IF IM-VAT-METHOD = SPACES
               MOVE "PVA" TO IM-VAT-METHOD
           END-IF
           IF IM-VAT-RATE-PCT = 0
               MOVE 20 TO IM-VAT-RATE-PCT
           END-IF
           IF IM-ENTRY-DATE = 0
               MOVE CA-BUSINESS-DATE TO IM-ENTRY-DATE
           END-IF
           IF IM-POSTING-DATE = 0
               MOVE IM-ENTRY-DATE TO IM-POSTING-DATE
           END-IF
           MOVE SPACES TO WS-CORR
           STRING "IMP-" FUNCTION TRIM(CA-COMPANY) "-"
                  FUNCTION TRIM(IM-ENTRY-REF)
               DELIMITED BY SIZE INTO WS-CORR
           IF CA-CORRELATION-ID NOT = SPACES
               MOVE CA-CORRELATION-ID TO WS-CORR
           END-IF
           IF IM-MRN = SPACES AND CA-FUNCTION NOT = "CALC"
               MOVE "IMP0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The movement reference number of the customs "
                      "declaration is the evidence for the duty and "
                      "the import VAT; it is not optional"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> DUTY AND VAT ARITHMETIC.  DUTY IS CHARGED ON THE CUSTOMS
      *> VALUE AT THE COMMODITY RATE; IMPORT VAT IS CHARGED ON THE
      *> CUSTOMS VALUE PLUS THE DUTY, BECAUSE DUTY IS PART OF THE
      *> VALUE OF THE IMPORTED GOODS FOR VAT PURPOSES.
      *>----------------------------------------------------------------
       2000-CALC SECTION.
       2000-START.
           IF IM-LINE-CNT <= 0
               MOVE "IMP0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "An import entry must declare at least one "
                      "commodity line"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF IM-LINE-CNT > 15
               MOVE 15 TO IM-LINE-CNT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > IM-LINE-CNT
               MOVE "OK" TO IM-L-OUTCOME(WS-I)
               MOVE 0 TO IM-L-DUTY-GBP(WS-I)
               MOVE 0 TO IM-L-VAT-GBP(WS-I)
               MOVE 0 TO IM-L-LANDED-GBP(WS-I)
               IF IM-L-COMMODITY(WS-I) = SPACES
                  OR IM-L-ORIGIN(WS-I) = SPACES
                   MOVE "REJECT" TO IM-L-OUTCOME(WS-I)
                   MOVE "IMP0002" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Every import line needs its commodity "
                          "code and country of origin; the duty "
                          "rate and any preference claim depend "
                          "on them"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 8 TO CA-RETURN-CODE
               ELSE
                   IF IM-L-CUST-VALUE(WS-I) <= 0
                       MOVE "REJECT" TO IM-L-OUTCOME(WS-I)
                       MOVE "IMP0003" TO CA-REASON-CODE
                       MOVE 8 TO CA-RETURN-CODE
                   ELSE
                       IF IM-L-DUTY-PCT(WS-I) < 0
                          OR IM-L-DUTY-PCT(WS-I) > 100
                           MOVE "REJECT" TO IM-L-OUTCOME(WS-I)
                           MOVE "IMP0004" TO CA-REASON-CODE
                           MOVE 8 TO CA-RETURN-CODE
                       ELSE
                           IF IM-L-PREFERENCE(WS-I) = "Y"
                              AND IM-L-PROOF-REF(WS-I) = SPACES
                               MOVE "REJECT"
                                 TO IM-L-OUTCOME(WS-I)
                               MOVE "IMP0009" TO CA-REASON-CODE
                               MOVE SPACES TO CA-MESSAGE
                               STRING "A preferential duty rate "
                                      "must be supported by its "
                                      "proof of origin; an "
                                      "unsupported claim is a "
                                      "customs debt waiting to "
                                      "happen"
                                   DELIMITED BY SIZE
                                   INTO CA-MESSAGE
                               MOVE 8 TO CA-RETURN-CODE
                           ELSE
                               PERFORM 2200-LINE THRU 2200-EXIT
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
      *>     The split between stock and cost of sales follows the
      *>     units already consumed: duty on goods that have gone
      *>     cannot be capitalised into stock that is no longer there.
           IF IM-CONSUMED-PCT > 0
               COMPUTE IM-DUTY-TO-COGS ROUNDED =
                       IM-DUTY-GBP * IM-CONSUMED-PCT / 100
               COMPUTE IM-DUTY-TO-STOCK =
                       IM-DUTY-GBP - IM-DUTY-TO-COGS
           ELSE
               MOVE IM-DUTY-GBP TO IM-DUTY-TO-STOCK
           END-IF
           MOVE IM-VAT-GBP TO IM-VAT-BOX1-GBP
           IF IM-PVA
               MOVE IM-VAT-GBP TO IM-VAT-BOX4-GBP
               COMPUTE IM-NET-PROFIT-GBP =
                       IM-VAT-BOX4-GBP - IM-VAT-BOX1-GBP
           ELSE
               MOVE IM-VAT-GBP TO IM-VAT-BOX4-GBP
               MOVE 0 TO IM-NET-PROFIT-GBP
           END-IF.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2200-LINE SECTION.
       2200-START.
           COMPUTE IM-L-DUTY-GBP(WS-I) ROUNDED =
                   IM-L-CUST-VALUE(WS-I) * IM-L-DUTY-PCT(WS-I) / 100
           COMPUTE IM-L-VAT-GBP(WS-I) ROUNDED =
                   (IM-L-CUST-VALUE(WS-I) + IM-L-DUTY-GBP(WS-I))
                   * IM-VAT-RATE-PCT / 100
           MOVE IM-L-DUTY-GBP(WS-I) TO IM-L-LANDED-GBP(WS-I)
           ADD IM-L-CUST-VALUE(WS-I) TO IM-CUSTOMS-VAL-GBP
           ADD IM-L-DUTY-GBP(WS-I) TO IM-DUTY-GBP
           ADD IM-L-VAT-GBP(WS-I) TO IM-VAT-GBP.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3000-DECLARE SECTION.
       3000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.ImportEntry (CompanyCode, "
                  "ImportEntryRef, MovementReferenceNumber, "
                  "EntryDate, CustomsAgentCode, SupplierCode, "
                  "PurchaseOrderNumber, ReceiptRef, "
                  "CustomsValueGBP, DutyAmountGBP, ImportVatGBP, "
                  "ImportVatMethod, DefermentAccount, "
                  "VatPeriodKey, StatementStatus, CreatedBy) "
                  "OUTPUT INSERTED.ImportEntryId VALUES "
                  "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,'PENDING',?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE IM-ENTRY-REF TO SQLIO-PARM-VAL(2)
           MOVE IM-MRN TO SQLIO-PARM-VAL(3)
           MOVE IM-ENTRY-DATE TO SQLIO-PARM-VAL(4)
           MOVE IM-AGENT-CODE TO SQLIO-PARM-VAL(5)
           MOVE IM-SUPPLIER TO SQLIO-PARM-VAL(6)
           MOVE IM-ORDER-NUMBER TO SQLIO-PARM-VAL(7)
           MOVE IM-RECEIPT-REF TO SQLIO-PARM-VAL(8)
           MOVE IM-CUSTOMS-VAL-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE IM-DUTY-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE IM-VAT-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE IM-VAT-METHOD TO SQLIO-PARM-VAL(12)
           MOVE IM-DEFERMENT-ACC TO SQLIO-PARM-VAL(13)
           MOVE IM-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(14)
           MOVE CA-USER TO SQLIO-PARM-VAL(15)
           PERFORM VARYING WS-N FROM 5 BY 1 UNTIL WS-N > 8
               IF SQLIO-PARM-VAL(WS-N) = SPACES
                   MOVE "Y" TO SQLIO-PARM-NULL(WS-N)
               END-IF
           END-PERFORM
           IF IM-DEFERMENT-ACC = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(13)
           END-IF
           IF IM-VAT-PERIOD-KEY = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(14)
           END-IF
           MOVE 15 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-SQLCODE NOT = 0
               MOVE "IMP0006" TO CA-REASON-CODE
               MOVE SQLIO-ERRMSG TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE FUNCTION NUMVAL(WS-C-VALUE) TO IM-ENTRY-ID
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > IM-LINE-CNT
               IF IM-L-OUTCOME(WS-I) = "OK"
                   PERFORM 3200-WRITE-LINE THRU 3200-EXIT
               END-IF
           END-PERFORM.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3200-WRITE-LINE SECTION.
       3200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.ImportEntryLine (ImportEntryId, "
                  "LineNum, ItemCode, ReceiptRef, ReceiptLine, "
                  "CommodityCode, CountryOfOrigin, "
                  "PreferenceClaimed, Quantity, NetWeightKg, "
                  "CustomsValueGBP, DutyRatePct, DutyAmountGBP, "
                  "ImportVatGBP) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,"
                  "?,?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE IM-ENTRY-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE IM-L-NUM(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE IM-L-ITEM(WS-I) TO SQLIO-PARM-VAL(3)
           MOVE IM-RECEIPT-REF TO SQLIO-PARM-VAL(4)
           MOVE IM-L-RCPT-LINE(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE IM-L-COMMODITY(WS-I) TO SQLIO-PARM-VAL(6)
           MOVE IM-L-ORIGIN(WS-I) TO SQLIO-PARM-VAL(7)
           IF IM-L-PREFERENCE(WS-I) = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(8)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(8)
           END-IF
           MOVE IM-L-QUANTITY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE IM-L-WEIGHT-KG(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE IM-L-CUST-VALUE(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE IM-L-DUTY-PCT(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE IM-L-DUTY-GBP(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE IM-L-VAT-GBP(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(14)
           IF IM-L-ITEM(WS-I) = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(3)
           END-IF
           IF IM-RECEIPT-REF = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(4)
           END-IF
           MOVE 14 TO SQLIO-PARM-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       3200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> DUTY.  CAPITALISED INTO THE COST OF THE GOODS STILL HELD AND
      *> CHARGED TO COST OF SALES FOR THE UNITS ALREADY SOLD, AND
      *> CREDITED TO GOODS RECEIVED NOT INVOICED UNTIL THE CUSTOMS
      *> AGENT INVOICES FOR IT.
      *>----------------------------------------------------------------
       4000-POST-DUTY SECTION.
       4000-START.
           IF IM-DUTY-GBP = 0
               GO TO 4000-EXIT
           END-IF
           INITIALIZE WS-POPOST
           MOVE "POST" TO PP-FUNC
           MOVE CA-COMPANY TO PP-COMPANY
           MOVE CA-SITE TO PP-SITE
           MOVE CA-USER TO PP-USER
           MOVE CA-BUSINESS-DATE TO PP-BUSINESS-DATE
           MOVE SPACES TO PP-CORRELATION
           STRING FUNCTION TRIM(WS-CORR) "-DUTY"
               DELIMITED BY SIZE INTO PP-CORRELATION
           MOVE IM-POSTING-DATE TO PP-POSTING-DATE
           MOVE IM-ENTRY-DATE TO PP-DOCUMENT-DATE
           MOVE "GBP" TO PP-CURRENCY
           MOVE 1 TO PP-FX-RATE
           MOVE PO-EV-DUTY TO PP-EVENT-CODE
           MOVE IM-ENTRY-REF TO PP-SOURCE-REF
           MOVE "PO" TO PP-SUBLEDGER-TYPE
           MOVE IM-AGENT-CODE TO PP-SUBLEDGER-REF
           MOVE IM-MRN TO PP-EVIDENCE-REF
           MOVE SPACES TO PP-DESCRIPTION
           STRING "Customs duty on import entry "
                  FUNCTION TRIM(IM-ENTRY-REF) " MRN "
                  FUNCTION TRIM(IM-MRN)
               DELIMITED BY SIZE INTO PP-DESCRIPTION
           MOVE 0 TO WS-N
           IF IM-DUTY-TO-STOCK NOT = 0
               ADD 1 TO WS-N
               MOVE PO-ACC-STOCK TO PP-L-ACCOUNT(WS-N)
               MOVE IM-DUTY-TO-STOCK TO PP-L-DR(WS-N)
               MOVE 0 TO PP-L-CR(WS-N)
               MOVE "Customs duty capitalised into stock"
                 TO PP-L-NARRATIVE(WS-N)
           END-IF
           IF IM-DUTY-TO-COGS NOT = 0
               ADD 1 TO WS-N
               MOVE PO-ACC-SCRAP TO PP-L-ACCOUNT(WS-N)
               MOVE IM-DUTY-TO-COGS TO PP-L-DR(WS-N)
               MOVE 0 TO PP-L-CR(WS-N)
               MOVE "PURCH" TO PP-L-COST-CTR(WS-N)
               MOVE "Customs duty on units already consumed"
                 TO PP-L-NARRATIVE(WS-N)
           END-IF
           ADD 1 TO WS-N
           MOVE PO-ACC-GRNI TO PP-L-ACCOUNT(WS-N)
           MOVE 0 TO PP-L-DR(WS-N)
           MOVE IM-DUTY-GBP TO PP-L-CR(WS-N)
           MOVE "Customs agent, duty not yet invoiced"
             TO PP-L-NARRATIVE(WS-N)
           MOVE WS-N TO PP-LINE-CNT
           CALL "POPOST" USING WS-POPOST
           MOVE PP-JOURNAL-NUMBER TO IM-JOURNAL-NUMBER
           IF PP-RC > 4
               MOVE PP-RC TO CA-RETURN-CODE
               MOVE "IMP0006" TO CA-REASON-CODE
               MOVE PP-MESSAGE TO CA-MESSAGE
           END-IF.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> POSTPONED IMPORT VAT.  BOTH LEGS GO ON ONE JOURNAL: THE
      *> OUTPUT SIDE IS DECLARED IN BOX 1 AND THE INPUT SIDE
      *> RECOVERED IN BOX 4 OF THE SAME RETURN.  NOTHING REACHES
      *> PROFIT AND NOTHING REACHES THE BANK.  IMPORT VAT IS NEVER
      *> LANDED COST.
      *>----------------------------------------------------------------
       4500-POST-VAT SECTION.
       4500-START.
           IF IM-VAT-GBP = 0 OR CA-FAILED
               GO TO 4500-EXIT
           END-IF
           IF NOT IM-PVA
               MOVE 4 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Import VAT of this entry was paid at the "
                      "border or deferred; it is recovered on the "
                      "C79 evidence by the AP suite and is not "
                      "posted here"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4500-EXIT
           END-IF
           INITIALIZE WS-POPOST
           MOVE "POST" TO PP-FUNC
           MOVE CA-COMPANY TO PP-COMPANY
           MOVE CA-SITE TO PP-SITE
           MOVE CA-USER TO PP-USER
           MOVE CA-BUSINESS-DATE TO PP-BUSINESS-DATE
           MOVE SPACES TO PP-CORRELATION
           STRING FUNCTION TRIM(WS-CORR) "-PVA"
               DELIMITED BY SIZE INTO PP-CORRELATION
           MOVE IM-POSTING-DATE TO PP-POSTING-DATE
           MOVE IM-ENTRY-DATE TO PP-DOCUMENT-DATE
           MOVE "GBP" TO PP-CURRENCY
           MOVE 1 TO PP-FX-RATE
           MOVE PO-EV-IMPORT-VAT TO PP-EVENT-CODE
           MOVE IM-ENTRY-REF TO PP-SOURCE-REF
           MOVE "PO" TO PP-SUBLEDGER-TYPE
           MOVE IM-AGENT-CODE TO PP-SUBLEDGER-REF
           MOVE IM-MRN TO PP-EVIDENCE-REF
           MOVE SPACES TO PP-DESCRIPTION
           STRING "Postponed import VAT, entry "
                  FUNCTION TRIM(IM-ENTRY-REF) ", period "
                  FUNCTION TRIM(IM-VAT-PERIOD-KEY)
               DELIMITED BY SIZE INTO PP-DESCRIPTION
           MOVE PO-ACC-VAT-IN TO PP-L-ACCOUNT(1)
           MOVE IM-VAT-BOX4-GBP TO PP-L-DR(1)
           MOVE 0 TO PP-L-CR(1)
           MOVE "PVAIN" TO PP-L-VATCODE(1)
           MOVE "Postponed import VAT recovered, box 4"
             TO PP-L-NARRATIVE(1)
           MOVE PO-ACC-VAT-OUT TO PP-L-ACCOUNT(2)
           MOVE 0 TO PP-L-DR(2)
           MOVE IM-VAT-BOX1-GBP TO PP-L-CR(2)
           MOVE "PVAOUT" TO PP-L-VATCODE(2)
           MOVE "Postponed import VAT declared, box 1"
             TO PP-L-NARRATIVE(2)
           MOVE 2 TO PP-LINE-CNT
           CALL "POPOST" USING WS-POPOST
           MOVE PP-JOURNAL-NUMBER TO IM-VAT-JOURNAL
           IF PP-RC > 4
               MOVE PP-RC TO CA-RETURN-CODE
               MOVE "IMP0006" TO CA-REASON-CODE
               MOVE PP-MESSAGE TO CA-MESSAGE
           END-IF.
       4500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> STATEMENT RECONCILIATION.  THE MONTHLY POSTPONED IMPORT VAT
      *> STATEMENT IS THE EVIDENCE FOR THE RECOVERY.  WHERE IT DOES
      *> NOT AGREE WITH THE ENTRIES DECLARED, THE ENTRY IS MARKED
      *> DIFFERENCE AND INVESTIGATED - IT IS NEVER ADJUSTED TO AGREE.
      *>----------------------------------------------------------------
       5000-RECONCILE SECTION.
       5000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(ImportVatGBP),0) AS "
                  "varchar(40)) FROM po.ImportEntry WHERE "
                  "CompanyCode = ? AND VatPeriodKey = ? AND "
                  "ImportVatMethod = 'PVA'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE IM-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-SQLCODE = 0 AND WS-C-VALUE NOT = SPACES
               MOVE FUNCTION NUMVAL(WS-C-VALUE) TO IM-VAT-GBP
           END-IF
           COMPUTE IM-STMT-DIFF-GBP =
                   IM-STATEMENT-VAT - IM-VAT-GBP
           IF IM-STMT-DIFF-GBP = 0 AND IM-STATEMENT-REF NOT = SPACES
               MOVE "RECONCILED" TO IM-STATEMENT-STATUS
           ELSE
               MOVE "DIFFERENCE" TO IM-STATEMENT-STATUS
               MOVE 4 TO CA-RETURN-CODE
               MOVE "IMP0008" TO CA-REASON-CODE
               MOVE IM-STMT-DIFF-GBP TO WS-AMT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "The postponed import VAT statement differs "
                      "from the declared entries by "
                      FUNCTION TRIM(WS-AMT-ED)
                      "; the difference is investigated, never "
                      "adjusted away"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.ImportEntry SET StatementStatus = ?, "
                  "StatementRef = ?, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE "
                  "CompanyCode = ? AND VatPeriodKey = ? AND "
                  "ImportVatMethod = 'PVA'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE IM-STATEMENT-STATUS TO SQLIO-PARM-VAL(1)
           MOVE IM-STATEMENT-REF TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(4)
           MOVE IM-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(5)
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO CA-RETURN-CODE
           MOVE "Y" TO WS-FATAL-SW
           MOVE "REJECTED" TO IM-OUTCOME.
       9000-EXIT.
           EXIT.
       END PROGRAM IMPVAT.
