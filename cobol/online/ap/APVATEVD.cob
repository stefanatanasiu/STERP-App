      *>================================================================
      *> PROGRAM-ID : APVATEVD
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : INPUT VAT EVIDENCE AND THE SIX MONTH UNPAID
      *>              INPUT VAT REPAYMENT.
      *>              INPUT TAX IS RELEASED ONLY WITH VALID EVIDENCE:
      *>              A VAT INVOICE CONTAINING THE REQUIRED CONTENT
      *>              FROM A VAT REGISTERED SUPPLIER, A C79 OR
      *>              POSTPONED IMPORT VAT STATEMENT FOR IMPORTS, OR
      *>              SELF ACCOUNTING FOR REVERSE CHARGES.  A PRO
      *>              FORMA, A STATEMENT OR AN INVOICE WITHOUT A
      *>              SUPPLIER VAT NUMBER DOES NOT RELEASE IT: THE
      *>              PAYABLE STILL STANDS AND MAY BE PAID, BUT THE
      *>              TAX IS HELD AS A RECLAIM EXCEPTION (AP-025).
      *>              INPUT TAX RECLAIMED ON AN INVOICE STILL UNPAID
      *>              SIX MONTHS AFTER THE LATER OF SUPPLY DATE AND
      *>              DUE DATE IS REPAID THROUGH THE RETURN, AND
      *>              RECLAIMED EXACTLY ONCE WHEN IT IS PAID (AP-029).
      *> CALLED BY  : .NET REGISTER FORM, APVAT6M, APINVREG CALLERS
      *> INTERFACE  : CALL "APVATEVD" USING WS-APVATEVD-CA
      *>              COPYBOOK APVATEVDCA
      *> CALLS      : SQLIO, APSVC
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-04-02 RJH  ORIGINAL VAT ANALYSIS
      *> 1997-04-06 DJM  CR0730 SIX MONTH BAD DEBT RULES
      *> 2021-01-01 AJS  BREXIT - C79 AND POSTPONED IMPORT VAT
      *> 2026-10-05 STERP ap.VatEvidence, fin.VatTransaction
      *>================================================================
      *> REQ: AP-021, AP-025, AP-029, D-02
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APVATEVD.
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
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-INV-ROW.
           05  WS-R-ID                 PIC X(24).
           05  WS-R-REGISTER           PIC X(30).
           05  WS-R-SUPPLIER           PIC X(20).
           05  WS-R-VATNO              PIC X(15).
           05  WS-R-VATLOC             PIC X(3).
           05  WS-R-TAXPOINT           PIC X(8).
           05  WS-R-DUEDATE            PIC X(8).
           05  WS-R-RECOVER            PIC X(24).
           05  WS-R-NONRECOVER         PIC X(24).
           05  WS-R-NETGBP             PIC X(24).
           05  WS-R-SETTLE             PIC X(12).
           05  WS-R-REPAID             PIC X(8).
           05  WS-R-RECLAIMED          PIC X(8).
           05  WS-R-PERIODKEY          PIC X(10).
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(40).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-DAYS                 PIC S9(7) VALUE 0.
           05  WS-MONEY-ED             PIC -(11)9.99.
           05  WS-DATE-ED              PIC 9(8).
       LINKAGE SECTION.
       COPY APVATEVDCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "EVIDENCE" PERFORM 2000-EVIDENCE THRU 2000-EXIT
               WHEN "SPLIT"    PERFORM 3000-SPLIT THRU 3000-EXIT
               WHEN "RCHARGE"  PERFORM 3500-RCHARGE THRU 3500-EXIT
               WHEN "SIXMONTH" PERFORM 4000-SIXMONTH THRU 4000-EXIT
               WHEN "REPAY"    PERFORM 5000-REPAY THRU 5000-EXIT
               WHEN "RECLAIM"  PERFORM 6000-RECLAIM THRU 6000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNC THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "APVATEVD" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "AP-025" TO CA-REQ-REF
           MOVE SPACES TO VE-NOTE VE-FAILURE-REASON
           MOVE "N" TO VE-REPAYABLE
           MOVE "N" TO VE-ALREADY-REPAID
           MOVE "N" TO VE-ALREADY-RECLAIM
           MOVE 0 TO VE-AFFECTED-CNT
           MOVE FUNCTION UPPER-CASE(CA-COMPANY) TO CA-COMPANY
           IF VE-AS-AT-DATE = 0
               MOVE CA-BUSINESS-DATE TO VE-AS-AT-DATE
           END-IF
           IF VE-REPAY-MONTHS = 0
               INITIALIZE WS-APSVC
               MOVE "PARMNUM" TO SVC-FUNC
               PERFORM 8900-SVC-HEADER THRU 8900-EXIT
               MOVE "VAT.INPUT.REPAY.MONTHS" TO SVC-TEXT-1
               MOVE 6 TO SVC-NUM-1
               CALL "APSVC" USING WS-APSVC
               MOVE SVC-NUM-OUT TO VE-REPAY-MONTHS
           END-IF
           IF CA-COMPANY = SPACES
               MOVE "APV0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AP-025.  THE EVIDENCE TEST IS SEPARATE FROM THE PAYABLE.  A
      *> FAILED TEST RETURNS 06 (HELD) - THE DOCUMENT IS STILL A VALID
      *> LIABILITY, ONLY THE TAX IS HELD.
      *>----------------------------------------------------------------
       2000-EVIDENCE SECTION.
       2000-START.
           MOVE "Y" TO VE-EVIDENCE-VALID
           MOVE "VALID" TO VE-OUTCOME
           MOVE 0 TO VE-HELD-VAT-GBP
           MOVE VE-CLAIMED-VAT-GBP TO VE-ALLOWED-VAT-GBP
           EVALUATE TRUE
               WHEN VE-PROFORMA
                   MOVE "N" TO VE-EVIDENCE-VALID
                   MOVE SPACES TO VE-FAILURE-REASON
                   STRING "A pro forma is a request for payment, not "
                          "a VAT invoice; it cannot release input tax"
                       DELIMITED BY SIZE INTO VE-FAILURE-REASON
               WHEN VE-STATEMENT
                   MOVE "N" TO VE-EVIDENCE-VALID
                   MOVE SPACES TO VE-FAILURE-REASON
                   STRING "A supplier statement is not evidence for "
                          "an input tax claim"
                       DELIMITED BY SIZE INTO VE-FAILURE-REASON
               WHEN VE-NONE
                   MOVE "N" TO VE-EVIDENCE-VALID
                   MOVE SPACES TO VE-FAILURE-REASON
                   STRING "No VAT evidence is held for this document"
                       DELIMITED BY SIZE INTO VE-FAILURE-REASON
               WHEN VE-REVERSECHG
                   MOVE "Y" TO VE-SELF-ACCOUNTS
               WHEN VE-C79 OR VE-PVA
                   IF VE-EVIDENCE-REF = SPACES
                       MOVE "N" TO VE-EVIDENCE-VALID
                       MOVE SPACES TO VE-FAILURE-REASON
                       STRING "Import VAT needs the C79 or postponed "
                              "import VAT statement reference"
                           DELIMITED BY SIZE INTO VE-FAILURE-REASON
                   END-IF
               WHEN OTHER
                   IF VE-SUPPLIER-VATNO = SPACES
                      OR VE-SUPPLIER-VATNO = LOW-VALUES
                       MOVE "N" TO VE-EVIDENCE-VALID
                       MOVE SPACES TO VE-FAILURE-REASON
                       STRING "The invoice carries no supplier VAT "
                              "registration number, so input tax "
                              "cannot be released"
                           DELIMITED BY SIZE INTO VE-FAILURE-REASON
                   END-IF
           END-EVALUATE
           IF VE-EVIDENCE-VALID = "N"
               MOVE "HELD" TO VE-OUTCOME
               MOVE VE-CLAIMED-VAT-GBP TO VE-HELD-VAT-GBP
               MOVE 0 TO VE-ALLOWED-VAT-GBP
               MOVE VE-CLAIMED-VAT-GBP TO VE-NONRECOVER-GBP
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APV0010" TO CA-REASON-CODE
               MOVE VE-FAILURE-REASON TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           ELSE
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Input VAT evidence is valid"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           IF VE-INVOICE-ID NOT = 0
               PERFORM 2100-WRITE-EVIDENCE THRU 2100-EXIT
           END-IF.
       2000-EXIT.
           EXIT.

       2100-WRITE-EVIDENCE SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; "
                  "INSERT ap.VatEvidence (CompanyCode, "
                  "PurchaseInvoiceId, RegisterNumber, SupplierCode, "
                  "EvidenceType, EvidenceRef, SupplierVatRegNo, "
                  "TaxPointDate, VatPeriodKey, ClaimedVatGBP, "
                  "AllowedVatGBP, HeldVatGBP, Outcome, "
                  "FailureReason, RecordedBy) VALUES (?, ?, "
                  "NULLIF(?,''), ?, ?, NULLIF(?,''), NULLIF(?,''), "
                  "CAST(? AS date), NULLIF(?,''), ?, ?, ?, ?, "
                  "NULLIF(?,''), ?); "
                  "SELECT CAST(SCOPE_IDENTITY() AS varchar(24))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE VE-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE VE-REGISTER-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE VE-SUPPLIER TO SQLIO-PARM-VAL(4)
           MOVE VE-EVIDENCE-TYPE TO SQLIO-PARM-VAL(5)
           MOVE VE-EVIDENCE-REF TO SQLIO-PARM-VAL(6)
           MOVE VE-SUPPLIER-VATNO TO SQLIO-PARM-VAL(7)
           MOVE VE-TAX-POINT TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE VE-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(9)
           MOVE VE-CLAIMED-VAT-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE VE-ALLOWED-VAT-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE VE-HELD-VAT-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE VE-OUTCOME TO SQLIO-PARM-VAL(13)
           MOVE VE-FAILURE-REASON TO SQLIO-PARM-VAL(14)
           MOVE CA-USER TO SQLIO-PARM-VAL(15)
           MOVE 15 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 15
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2100-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-C-VALUE)
           MOVE WS-NUM TO VE-EVIDENCE-ID
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PurchaseInvoice SET VatEvidenceValid = "
                  "?, VatEvidenceNote = NULLIF(?,''), "
                  "RecoverableVatGBP = ?, NonRecoverableVatGBP = ?, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE PurchaseInvoiceId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           IF VE-EVIDENCE-VALID = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(1)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(1)
           END-IF
           MOVE VE-FAILURE-REASON TO SQLIO-PARM-VAL(2)
           MOVE VE-ALLOWED-VAT-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE VE-NONRECOVER-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE CA-USER TO SQLIO-PARM-VAL(5)
           MOVE VE-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE 6 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> NON RECOVERABLE VAT IS NOT A SEPARATE TAX ASSET; IT FOLLOWS
      *> THE COST OR EXPENSE UNDER THE APPROVED POLICY.
      *>----------------------------------------------------------------
       3000-SPLIT SECTION.
       3000-START.
           IF VE-RECOVER-PCT = 0 AND VE-IS-RECOVERABLE = "Y"
               MOVE 100 TO VE-RECOVER-PCT
           END-IF
           COMPUTE VE-ALLOWED-VAT-GBP ROUNDED =
               (VE-CLAIMED-VAT-GBP * VE-RECOVER-PCT) / 100
           COMPUTE VE-NONRECOVER-GBP =
               VE-CLAIMED-VAT-GBP - VE-ALLOWED-VAT-GBP
           MOVE VE-ALLOWED-VAT-GBP TO VE-INPUT-VAT-GBP
           MOVE 0 TO VE-OUTPUT-VAT-GBP
           MOVE VE-ALLOWED-VAT-GBP TO WS-MONEY-ED
           MOVE SPACES TO CA-MESSAGE
           STRING "Recoverable input VAT GBP "
                  FUNCTION TRIM(WS-MONEY-ED)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> REVERSE CHARGE AND POSTPONED IMPORT VAT.  THE SAME AMOUNT
      *> GOES INTO BOX 1 AND BOX 4, SO THE NET VAT EFFECT IS NIL AND
      *> THE SUPPLIER IS PAID THE NET AMOUNT ONLY.
      *>----------------------------------------------------------------
       3500-RCHARGE SECTION.
       3500-START.
           COMPUTE VE-OUTPUT-VAT-GBP ROUNDED =
               (VE-NET-GBP * VE-RATE-PCT) / 100
           IF VE-IS-RECOVERABLE = "N"
               MOVE 0 TO VE-INPUT-VAT-GBP
               MOVE VE-OUTPUT-VAT-GBP TO VE-NONRECOVER-GBP
           ELSE
               MOVE VE-OUTPUT-VAT-GBP TO VE-INPUT-VAT-GBP
               MOVE 0 TO VE-NONRECOVER-GBP
           END-IF
           MOVE VE-OUTPUT-VAT-GBP TO VE-ALLOWED-VAT-GBP
           MOVE "Y" TO VE-SELF-ACCOUNTS
           MOVE 1 TO VE-VAT-BOX
           MOVE 7 TO VE-NET-BOX
           MOVE VE-OUTPUT-VAT-GBP TO WS-MONEY-ED
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "GBP " FUNCTION TRIM(WS-MONEY-ED)
                  " self accounted in both the output and the input "
                  "box; the supplier is paid the net amount"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       3500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AP-029.  THE CLOCK STARTS AT THE LATER OF THE SUPPLY DATE AND
      *> THE DUE DATE.  THE TEST IS MADE AS AT A STATED DATE SO THAT
      *> THE ADJUSTMENT LANDS IN THE CORRECT RETURN PERIOD.
      *>----------------------------------------------------------------
       4000-SIXMONTH SECTION.
       4000-START.
           IF VE-INVOICE-ID NOT = 0 OR VE-REGISTER-NUMBER NOT = SPACES
               PERFORM 7000-READ-INVOICE THRU 7000-EXIT
               IF WS-FATAL
                   GO TO 4000-EXIT
               END-IF
           END-IF
           IF VE-SUPPLY-DATE = 0
               MOVE VE-TAX-POINT TO VE-SUPPLY-DATE
           END-IF
           IF VE-DUE-DATE >= VE-SUPPLY-DATE
               MOVE VE-DUE-DATE TO WS-DATE-ED
           ELSE
               MOVE VE-SUPPLY-DATE TO WS-DATE-ED
           END-IF
           INITIALIZE WS-APSVC
           MOVE "ADDMONTH" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE WS-DATE-ED TO SVC-DATE-1
           MOVE VE-REPAY-MONTHS TO SVC-INT-1
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-DATE-OUT TO VE-REPAY-DUE-DATE
           INITIALIZE WS-APSVC
           MOVE "DAYSBTWN" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE WS-DATE-ED TO SVC-DATE-1
           MOVE VE-AS-AT-DATE TO SVC-DATE-2
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-INT-OUT TO WS-DAYS
           COMPUTE VE-MONTHS-UNPAID = WS-DAYS / 30
           MOVE "N" TO VE-REPAYABLE
           IF VE-PAID-DATE NOT = 0
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The invoice has been paid; no repayment of "
                      "input tax is due"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           IF VE-ALREADY-REPAID = "Y"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APV0011" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The input tax on this invoice has already "
                      "been repaid; it is not repaid twice"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 4000-EXIT
           END-IF
           IF VE-AS-AT-DATE >= VE-REPAY-DUE-DATE
             AND VE-ALLOWED-VAT-GBP > 0
               MOVE "Y" TO VE-REPAYABLE
               MOVE VE-ALLOWED-VAT-GBP TO WS-MONEY-ED
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APV0012" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "GBP " FUNCTION TRIM(WS-MONEY-ED)
                      " of input VAT must be repaid: the invoice is "
                      "still unpaid six months after it fell due"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           ELSE
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No input VAT repayment is due yet"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       5000-REPAY SECTION.
       5000-START.
           PERFORM 4000-SIXMONTH THRU 4000-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           IF VE-REPAYABLE NOT = "Y"
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APV0013" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "This invoice does not qualify for an input "
                      "VAT repayment" DELIMITED BY SIZE
                   INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 5000-EXIT
           END-IF
           PERFORM 5100-VAT-TRANSACTION THRU 5100-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PurchaseInvoice SET VatRepaidAt = "
                  "CAST(? AS date), VatRepaidVatPeriodKey = ?, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE PurchaseInvoiceId = ? AND VatRepaidAt IS "
                  "NULL" DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE VE-AS-AT-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE VE-REPAY-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE VE-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           MOVE SQLIO-ROWCOUNT TO VE-AFFECTED-CNT
           MOVE "REPAID" TO VE-OUTCOME
           MOVE "Y" TO VE-ALREADY-REPAID
           MOVE VE-ALLOWED-VAT-GBP TO WS-MONEY-ED
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "GBP " FUNCTION TRIM(WS-MONEY-ED)
                  " of input VAT repaid in VAT period "
                  FUNCTION TRIM(VE-REPAY-PERIOD-KEY)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       5000-EXIT.
           EXIT.

       5100-VAT-TRANSACTION SECTION.
       5100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.VatTransaction (CompanyCode, "
                  "SourceModule, DocumentType, DocumentRef, "
                  "PartyType, PartyCode, PartyVatRegNo, TaxPointDate,"
                  " PostingDate, VatPeriodKey, VatCode, RatePct, "
                  "Mechanism, Direction, CurrencyCode, FxRate, "
                  "NetAmount, VatAmount, NetAmountGBP, VatAmountGBP, "
                  "RecoverableVatGBP, VatBox, NetBox, EvidenceRef, "
                  "AdjustmentType) VALUES (?, 'AP', 'PURCHINVOICE', "
                  "?, 'SUPPLIER', ?, NULLIF(?,''), CAST(? AS date), "
                  "CAST(? AS date), NULLIF(?,''), ?, ?, 'NORMAL', "
                  "'INPUT', 'GBP', 1, 0, ?, 0, ?, ?, 4, NULL, "
                  "NULLIF(?,''), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE VE-REGISTER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE VE-SUPPLIER TO SQLIO-PARM-VAL(3)
           MOVE VE-SUPPLIER-VATNO TO SQLIO-PARM-VAL(4)
           MOVE VE-TAX-POINT TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE VE-AS-AT-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           IF CA-FUNCTION = "RECLAIM"
               MOVE VE-RECLAIM-PERIOD TO SQLIO-PARM-VAL(7)
           ELSE
               MOVE VE-REPAY-PERIOD-KEY TO SQLIO-PARM-VAL(7)
           END-IF
           IF VE-VAT-CODE = SPACES
               MOVE "S" TO VE-VAT-CODE
           END-IF
           MOVE VE-VAT-CODE TO SQLIO-PARM-VAL(8)
           MOVE VE-RATE-PCT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           IF CA-FUNCTION = "RECLAIM"
               MOVE VE-ALLOWED-VAT-GBP TO SQLIO-NUM-EDIT
           ELSE
               COMPUTE WS-NUM = 0 - VE-ALLOWED-VAT-GBP
               MOVE WS-NUM TO SQLIO-NUM-EDIT
           END-IF
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE VE-EVIDENCE-REF TO SQLIO-PARM-VAL(13)
           IF CA-FUNCTION = "RECLAIM"
               MOVE "SIXMONTHRECLAIM" TO SQLIO-PARM-VAL(14)
           ELSE
               MOVE "SIXMONTHREPAY" TO SQLIO-PARM-VAL(14)
           END-IF
           MOVE 14 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 14
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       5100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE RECLAIM HAPPENS EXACTLY ONCE, AND ONLY WHERE THE EARLIER
      *> REPAYMENT ACTUALLY HAPPENED.
      *>----------------------------------------------------------------
       6000-RECLAIM SECTION.
       6000-START.
           PERFORM 7000-READ-INVOICE THRU 7000-EXIT
           IF WS-FATAL
               GO TO 6000-EXIT
           END-IF
           IF VE-ALREADY-REPAID NOT = "Y"
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APV0014" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No input VAT was repaid on this invoice, so "
                      "there is nothing to reclaim"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 6000-EXIT
           END-IF
           IF VE-ALREADY-RECLAIM = "Y"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APV0015" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The input VAT has already been reclaimed; it "
                      "reverses exactly once"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 6000-EXIT
           END-IF
           PERFORM 5100-VAT-TRANSACTION THRU 5100-EXIT
           IF WS-FATAL
               GO TO 6000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PurchaseInvoice SET VatReclaimedAgainAt "
                  "= CAST(? AS date), ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE PurchaseInvoiceId = ? AND "
                  "VatRepaidAt IS NOT NULL AND VatReclaimedAgainAt "
                  "IS NULL" DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE VE-AS-AT-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE VE-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           MOVE SQLIO-ROWCOUNT TO VE-AFFECTED-CNT
           MOVE "RECLAIMED" TO VE-OUTCOME
           MOVE "Y" TO VE-ALREADY-RECLAIM
           MOVE VE-ALLOWED-VAT-GBP TO WS-MONEY-ED
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "GBP " FUNCTION TRIM(WS-MONEY-ED)
                  " of input VAT reclaimed once the invoice was paid"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       6000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7000-READ-INVOICE SECTION.
       7000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(p.PurchaseInvoiceId AS "
                  "varchar(24)), p.RegisterNumber, p.SupplierCode, "
                  "ISNULL(s.VatRegNo,''), s.VatLocation, "
                  "CONVERT(char(8), p.TaxPointDate, 112), "
                  "ISNULL(CONVERT(char(8), p.DueDate, 112),"
                  "'00000000'), CAST(p.RecoverableVatGBP AS "
                  "varchar(24)), CAST(p.NonRecoverableVatGBP AS "
                  "varchar(24)), CAST(p.NetAmountGBP AS varchar(24)),"
                  " p.SettlementState, ISNULL(CONVERT(char(8), "
                  "p.VatRepaidAt, 112),'00000000'), "
                  "ISNULL(CONVERT(char(8), p.VatReclaimedAgainAt, "
                  "112),'00000000'), ISNULL(p.VatPeriodKey,'') "
                  "FROM ap.PurchaseInvoice p JOIN ap.Supplier s ON "
                  "s.CompanyCode = p.CompanyCode AND s.SupplierCode "
                  "= p.SupplierCode WHERE p.CompanyCode = ? AND "
                  "((? <> 0 AND p.PurchaseInvoiceId = ?) OR (? <> '' "
                  "AND p.RegisterNumber = ?))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE VE-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE VE-REGISTER-NUMBER TO SQLIO-PARM-VAL(4)
           MOVE VE-REGISTER-NUMBER TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 14 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 20 TO SQLIO-COL-WIDTH(3)
           MOVE 15 TO SQLIO-COL-WIDTH(4)
           MOVE 3 TO SQLIO-COL-WIDTH(5)
           MOVE 8 TO SQLIO-COL-WIDTH(6)
           MOVE 8 TO SQLIO-COL-WIDTH(7)
           MOVE 24 TO SQLIO-COL-WIDTH(8)
           MOVE 24 TO SQLIO-COL-WIDTH(9)
           MOVE 24 TO SQLIO-COL-WIDTH(10)
           MOVE 12 TO SQLIO-COL-WIDTH(11)
           MOVE 8 TO SQLIO-COL-WIDTH(12)
           MOVE 8 TO SQLIO-COL-WIDTH(13)
           MOVE 10 TO SQLIO-COL-WIDTH(14)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-INV-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 7000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "APV0002" TO CA-REASON-CODE
               MOVE "That registered document was not found"
                   TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 7000-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R-ID)
           MOVE WS-NUM TO VE-INVOICE-ID
           MOVE WS-R-REGISTER TO VE-REGISTER-NUMBER
           MOVE WS-R-SUPPLIER TO VE-SUPPLIER
           MOVE WS-R-VATNO TO VE-SUPPLIER-VATNO
           MOVE WS-R-VATLOC TO VE-VAT-LOCATION
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R-TAXPOINT)
           MOVE WS-NUM TO VE-TAX-POINT
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-R-DUEDATE)
           MOVE WS-NUM TO VE-DUE-DATE
           COMPUTE VE-ALLOWED-VAT-GBP = FUNCTION NUMVAL(WS-R-RECOVER)
           COMPUTE VE-NONRECOVER-GBP =
               FUNCTION NUMVAL(WS-R-NONRECOVER)
           COMPUTE VE-NET-GBP = FUNCTION NUMVAL(WS-R-NETGBP)
           IF WS-R-SETTLE = "SETTLED"
               MOVE CA-BUSINESS-DATE TO VE-PAID-DATE
           END-IF
           IF WS-R-REPAID NOT = "00000000"
               MOVE "Y" TO VE-ALREADY-REPAID
           END-IF
           IF WS-R-RECLAIMED NOT = "00000000"
               MOVE "Y" TO VE-ALREADY-RECLAIM
           END-IF
           IF VE-VAT-PERIOD-KEY = SPACES
               MOVE WS-R-PERIODKEY TO VE-VAT-PERIOD-KEY
           END-IF.
       7000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8500-EXEC-UPDATE SECTION.
       8500-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 8500-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       8500-EXIT.
           EXIT.

       8900-SVC-HEADER SECTION.
       8900-START.
           MOVE CA-COMPANY TO SVC-COMPANY
           MOVE CA-SITE TO SVC-SITE
           MOVE CA-USER TO SVC-USER
           MOVE CA-BUSINESS-DATE TO SVC-DATE
           MOVE CA-CORRELATION-ID TO SVC-CORRELATION.
       8900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-FATAL-SW
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNC SECTION.
       9100-START.
           MOVE "APV0099" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "APVATEVD does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE "Y" TO WS-FATAL-SW
           MOVE 12 TO CA-RETURN-CODE
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in APVATEVD" TO CA-MESSAGE
           END-IF
           ADD 1 TO CA-MSG-COUNT
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       9300-EXIT.
           EXIT.
