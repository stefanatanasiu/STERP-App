      *>================================================================
      *> PROGRAM-ID : APVAT6M
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : SIX MONTH UNPAID INPUT VAT.  INPUT VAT THAT WAS
      *>              RECLAIMED ON A SUPPLIER INVOICE STILL UNPAID SIX
      *>              MONTHS AFTER THE LATER OF ITS SUPPLY DATE AND
      *>              ITS DUE DATE IS IDENTIFIED, LISTED AND - IN
      *>              COMMIT MODE - REPAID THROUGH THE VAT RETURN.
      *>              A SECOND PASS RECLAIMS, ONCE, THE VAT ON ANY
      *>              INVOICE THAT WAS REPAID AND HAS SINCE BEEN PAID.
      *>              THE DATE RULE ITSELF LIVES IN AP6MDATE SO THAT
      *>              IT CAN BE TESTED WITHOUT A DATABASE (TAPVAT6).
      *> RUN AS     : APVAT6M CO=STC ASAT=yyyymmdd USER=APMGR
      *>              [MODE=PREVIEW|COMMIT]
      *> OUTPUT     : run\spool\APVAT6M-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, AP6MDATE, APPOST, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-03-17 DJM  CR0702 ORIGINAL - SIX MONTH BAD DEBT LISTING
      *> 1999-11-02 KMP  Y2K - 4 DIGIT YEARS
      *> 2026-10-06 STERP AP-029 REPAYMENT STAGED, RECLAIM PASS ADDED
      *>================================================================
      *> REQ: AP-029, AP-025
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APVAT6M.
       AUTHOR. D J MERCER.
       DATE-WRITTEN. 17/03/1997.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO DYNAMIC WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-APVAT6M.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APBATWK.
       COPY STRPTWS.
       COPY STBATLOG.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY AP6MDTCA REPLACING ==DFHCOMMAREA== BY ==WS-6M==.
       COPY APPOSTWK.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-ASAT                 PIC 9(8) VALUE 0.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
       01  WS-RUN-STATE.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-PASS                 PIC X(8) VALUE SPACES.
           05  WS-EVID-ID              PIC S9(9) VALUE 0.
           05  WS-INV-ID               PIC S9(9) VALUE 0.
           05  WS-ALREADY              PIC S9(13)V99 VALUE 0.
           05  WS-CORR                 PIC X(64) VALUE SPACES.
           05  WS-ASAT-TEXT            PIC X(10) VALUE SPACES.
       01  WS-DET.
           05  D-SUPPLIER              PIC X(20) VALUE SPACES.
           05  D-SUPP-NAME             PIC X(34) VALUE SPACES.
           05  D-REGISTER              PIC X(20) VALUE SPACES.
           05  D-SUPP-REF              PIC X(20) VALUE SPACES.
           05  D-TAXPOINT              PIC X(10) VALUE SPACES.
           05  D-DUE                   PIC X(10) VALUE SPACES.
           05  D-TRIGGER               PIC X(10) VALUE SPACES.
           05  D-DAYS                  PIC S9(7) VALUE 0.
           05  D-GROSS                 PIC S9(13)V99 VALUE 0.
           05  D-RESIDUAL              PIC S9(13)V99 VALUE 0.
           05  D-CLAIMED               PIC S9(13)V99 VALUE 0.
           05  D-ADJUST                PIC S9(13)V99 VALUE 0.
           05  D-ACTION                PIC X(12) VALUE SPACES.
           05  D-PERIOD                PIC X(10) VALUE SPACES.
       01  WS-TOT.
           05  T-REPAY                 PIC S9(15)V99 VALUE 0.
           05  T-RECLAIM               PIC S9(15)V99 VALUE 0.
           05  T-REPAY-CNT             PIC S9(7) VALUE 0.
           05  T-RECLAIM-CNT           PIC S9(7) VALUE 0.
           05  T-SCANNED               PIC S9(7) VALUE 0.
           05  T-POSTED                PIC S9(7) VALUE 0.
       REPORT SECTION.
       RD  R-APVAT6M
           CONTROLS ARE FINAL D-SUPPLIER
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 54
           FOOTING 56.
       COPY STRPTHDR.
       01  CH-SUPP TYPE IS CONTROL HEADING D-SUPPLIER.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(9) VALUE "Supplier ".
               10  COLUMN 11   PIC X(20) SOURCE D-SUPPLIER.
               10  COLUMN 33   PIC X(34) SOURCE D-SUPP-NAME.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(20) VALUE "Register            ".
               10  COLUMN 24   PIC X(20) VALUE "Supplier reference  ".
               10  COLUMN 45   PIC X(10) VALUE "Tax point ".
               10  COLUMN 56   PIC X(10) VALUE "Due date  ".
               10  COLUMN 67   PIC X(10) VALUE "Six months".
               10  COLUMN 78   PIC X(5) VALUE "Days ".
               10  COLUMN 84   PIC X(13) VALUE "  Outstanding".
               10  COLUMN 98   PIC X(13) VALUE "   Input VAT ".
               10  COLUMN 112  PIC X(13) VALUE "  Adjustment ".
               10  COLUMN 126  PIC X(7) VALUE "Action ".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(66) VALUE ALL "-".
               10  COLUMN 69   PIC X(64) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(20) SOURCE D-REGISTER.
               10  COLUMN 24   PIC X(20) SOURCE D-SUPP-REF.
               10  COLUMN 45   PIC X(10) SOURCE D-TAXPOINT.
               10  COLUMN 56   PIC X(10) SOURCE D-DUE.
               10  COLUMN 67   PIC X(10) SOURCE D-TRIGGER.
               10  COLUMN 78   PIC Z(4)9- SOURCE D-DAYS.
               10  COLUMN 84   PIC Z(9)9.99- SOURCE D-RESIDUAL.
               10  COLUMN 98   PIC Z(9)9.99- SOURCE D-CLAIMED.
               10  COLUMN 112  PIC Z(9)9.99- SOURCE D-ADJUST.
               10  COLUMN 126  PIC X(7) SOURCE D-ACTION.
       01  CF-SUPP TYPE IS CONTROL FOOTING D-SUPPLIER.
           05  LINE PLUS 1.
               10  COLUMN 84   PIC X(27) VALUE
                   "Supplier VAT adjustment    ".
               10  COLUMN 112  PIC Z(9)9.99- SUM D-ADJUST.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 84   PIC X(27) VALUE
                   "Net VAT adjustment         ".
               10  COLUMN 112  PIC Z(9)9.99- SUM D-ADJUST.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(40) VALUE
                   "SIX MONTH UNPAID INPUT VAT (AP-029)".
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(34) VALUE
                   "Invoices scanned                 ".
               10  COLUMN 40   PIC Z(6)9 SOURCE T-SCANNED.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(34) VALUE
                   "Repayments identified            ".
               10  COLUMN 40   PIC Z(6)9 SOURCE T-REPAY-CNT.
               10  COLUMN 50   PIC Z(11)9.99- SOURCE T-REPAY.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(34) VALUE
                   "Reclaims on invoices since paid  ".
               10  COLUMN 40   PIC Z(6)9 SOURCE T-RECLAIM-CNT.
               10  COLUMN 50   PIC Z(11)9.99- SOURCE T-RECLAIM.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(34) VALUE
                   "Journals staged and posted       ".
               10  COLUMN 40   PIC Z(6)9 SOURCE T-POSTED.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(45) VALUE
                   "The adjustment belongs to the return covering".
               10  COLUMN 49   PIC X(20) VALUE "the six month date. ".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(45) VALUE
                   "PREVIEW identifies only; COMMIT repays and   ".
               10  COLUMN 49   PIC X(30) VALUE
                   "reclaims once per invoice.    ".
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           PERFORM 0100-READ-ARGS
           PERFORM 0200-JOB-START
           IF WS-RC < 8
               PERFORM 0300-OPEN-SPOOL
           END-IF
           IF WS-RC < 8
               PERFORM 1000-REPAY-PASS
               PERFORM 2000-RECLAIM-PASS
               PERFORM 0400-CLOSE-SPOOL
           END-IF.
       0000-FINISH.
           PERFORM 0500-JOB-END
           MOVE WS-RC TO RETURN-CODE
           STOP RUN.

      *>----------------------------------------------------------------
       0100-READ-ARGS SECTION.
       0100-START.
           INITIALIZE WS-CMDARGS
           MOVE "CMDLINE" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           MOVE CM-CO TO WS-CO
           MOVE CM-USER TO WS-USER
           MOVE CM-MODE TO WS-MODE
           IF CM-ASAT NOT = SPACES
               COMPUTE WS-ASAT = FUNCTION NUMVAL(CM-ASAT)
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "APMGR" TO WS-USER
           END-IF
           IF WS-MODE NOT = "COMMIT"
               MOVE "PREVIEW" TO WS-MODE
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           IF WS-ASAT = 0
               MOVE WS-TODAY-N TO WS-ASAT
           END-IF
           MOVE WS-ASAT TO WS-DATE-N
           MOVE SPACES TO WS-ASAT-TEXT
           STRING WS-DATE-YYYY "-" WS-DATE-MM "-" WS-DATE-DD
               DELIMITED BY SIZE INTO WS-ASAT-TEXT
           MOVE WS-MODE TO BL-MODE
           DISPLAY "STERP-I-0100 APVAT6M six month input VAT "
                   FUNCTION TRIM(WS-MODE) " for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "APVAT6M" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE "APVAT6M" TO BL-JOB-NAME.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "APVAT6M" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0301 cannot build the spool path"
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           MOVE RP-FULL-PATH TO BL-SPOOL-FILE
           PERFORM 0350-FILL-HEADINGS
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0302 cannot open the spool file"
               GO TO 0300-EXIT
           END-IF
           INITIATE R-APVAT6M.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "APVAT6M" TO RPT-REPORT-ID
           MOVE "Six month unpaid input VAT" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE WS-ASAT TO WS-DATE-N
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "As at " WS-DATE-DD "/" WS-DATE-MM "/"
                  WS-DATE-YYYY
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-MODE = "COMMIT"
               MOVE "Repayment and reclaim committed to the return"
                   TO RPT-SUBTITLE
           ELSE
               MOVE "Preview - nothing is repaid or reclaimed"
                   TO RPT-SUBTITLE
           END-IF
           PERFORM 0360-READ-COMPANY.
       0350-EXIT.
           EXIT.

       0360-READ-COMPANY SECTION.
       0360-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Name, ISNULL(RegisteredNumber,'') FROM "
                  "core.Company WHERE CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 0290-SET-WIDTHS
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-OK
               MOVE AP-COL(1) TO RPT-COMPANY-NAME
               MOVE AP-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF
           MOVE 4 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW.
       0360-EXIT.
           EXIT.

       0290-SET-WIDTHS SECTION.
       0290-START.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0290-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   REQ: AP-029 - pass one, invoices whose input VAT has to go
      *>   back on the return covering the six month date.
       1000-REPAY-PASS SECTION.
       1000-START.
           MOVE "REPAY" TO WS-PASS
           PERFORM 1100-OPEN-UNPAID
           IF WS-SQL-FAILED = "Y"
               MOVE 12 TO WS-RC
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-DONE = "Y"
               PERFORM 1200-FETCH-UNPAID
               IF WS-DONE NOT = "Y"
                   ADD 1 TO T-SCANNED
                   PERFORM 1300-APPLY-RULE
               END-IF
           END-PERFORM
           PERFORM 1900-CLOSE-CURSOR.
       1000-EXIT.
           EXIT.

      *>   only invoices whose input VAT was actually taken: posted,
      *>   evidence held and a recoverable amount
       1100-OPEN-UNPAID SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT i.SupplierCode, s.LegalName, "
                  "i.RegisterNumber, i.SupplierDocRef, "
                  "CONVERT(varchar(8), i.TaxPointDate, 112), "
                  "CONVERT(varchar(8), "
                  "ISNULL(i.DueDate, i.TaxPointDate), 112), "
                  "CONVERT(varchar(30), i.GrossAmountGBP), "
                  "CONVERT(varchar(30), ISNULL(v.AllowedVatGBP, "
                  "i.RecoverableVatGBP)), "
                  "CONVERT(varchar(30), ISNULL(o.ResidualAmountGBP, "
                  "i.GrossAmountGBP)), "
                  "CONVERT(varchar(20), i.PurchaseInvoiceId), "
                  "CONVERT(varchar(20), ISNULL(v.EvidenceId, 0)), "
                  "ISNULL(v.Outcome, 'VALID') "
                  "FROM ap.PurchaseInvoice i "
                  "JOIN ap.Supplier s ON s.CompanyCode = "
                  "i.CompanyCode AND s.SupplierCode = i.SupplierCode "
                  "LEFT JOIN ap.OpenItem o ON o.CompanyCode = "
                  "i.CompanyCode AND o.PurchaseInvoiceId = "
                  "i.PurchaseInvoiceId AND o.ItemType = 'INVOICE' "
                  "OUTER APPLY (SELECT TOP 1 e.EvidenceId, e.Outcome, "
                  "e.AllowedVatGBP FROM ap.VatEvidence e WHERE "
                  "e.CompanyCode = i.CompanyCode AND "
                  "e.PurchaseInvoiceId = i.PurchaseInvoiceId "
                  "ORDER BY e.EvidenceId DESC) v "
                  "WHERE i.CompanyCode = ? "
                  "AND i.DocumentType = 'INVOICE' "
                  "AND i.AccountingState IN ('POSTED','PARTREVERSED') "
                  "AND i.VatEvidenceValid = 1 "
                  "AND i.RecoverableVatGBP > 0 "
                  "AND i.SettlementState IN ('UNPAID','PARTPAID') "
                  "AND ISNULL(v.Outcome,'VALID') IN "
                  "('VALID','RELEASED','RECLAIMED') "
                  "AND i.TaxPointDate <= ? "
                  "ORDER BY i.SupplierCode, i.RegisterNumber"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-ASAT-TEXT TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 12 TO SQLIO-COL-CNT
           PERFORM 0290-SET-WIDTHS
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-ERROR
               MOVE "Y" TO WS-SQL-FAILED
               DISPLAY "STERP-E-1101 " SQLIO-ERRMSG
           END-IF.
       1100-EXIT.
           EXIT.

       1200-FETCH-UNPAID SECTION.
       1200-START.
           MOVE 2 TO SQLIO-CURSOR
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "Y" TO WS-DONE
               GO TO 1200-EXIT
           END-IF
           ADD 1 TO BL-READ-COUNT
           PERFORM 1250-MOVE-ROW.
       1200-EXIT.
           EXIT.

       1250-MOVE-ROW SECTION.
       1250-START.
           MOVE AP-COL(1) TO D-SUPPLIER
           MOVE AP-COL(2) TO D-SUPP-NAME
           MOVE AP-COL(3) TO D-REGISTER
           MOVE AP-COL(4) TO D-SUPP-REF
           COMPUTE WS-DATE-N = FUNCTION NUMVAL(AP-COL(5))
           MOVE WS-DATE-N TO SM-SUPPLY-DATE OF WS-6M
           MOVE SPACES TO D-TAXPOINT
           STRING WS-DATE-DD "/" WS-DATE-MM "/" WS-DATE-YYYY
               DELIMITED BY SIZE INTO D-TAXPOINT
           COMPUTE WS-DATE-N = FUNCTION NUMVAL(AP-COL(6))
           MOVE WS-DATE-N TO SM-DUE-DATE OF WS-6M
           MOVE SPACES TO D-DUE
           STRING WS-DATE-DD "/" WS-DATE-MM "/" WS-DATE-YYYY
               DELIMITED BY SIZE INTO D-DUE
           COMPUTE D-GROSS = FUNCTION NUMVAL(AP-COL(7))
           COMPUTE D-CLAIMED = FUNCTION NUMVAL(AP-COL(8))
           COMPUTE D-RESIDUAL = FUNCTION NUMVAL(AP-COL(9))
           COMPUTE WS-INV-ID = FUNCTION NUMVAL(AP-COL(10))
           COMPUTE WS-EVID-ID = FUNCTION NUMVAL(AP-COL(11)).
       1250-EXIT.
           EXIT.

      *>   REQ: AP-029 - AP6MDATE owns the date rule and the
      *>   proportion; this program owns the evidence and the posting.
       1300-APPLY-RULE SECTION.
       1300-START.
           MOVE "REPAY" TO SM-FUNC OF WS-6M
           MOVE 6 TO SM-MONTHS OF WS-6M
           MOVE WS-ASAT TO SM-AS-AT OF WS-6M
           MOVE D-CLAIMED TO SM-CLAIMED-VAT OF WS-6M
           MOVE D-GROSS TO SM-GROSS OF WS-6M
           MOVE D-RESIDUAL TO SM-RESIDUAL OF WS-6M
           MOVE 0 TO SM-ALREADY-REPAID OF WS-6M
           MOVE 0 TO SM-PAID-DATE OF WS-6M
           CALL "AP6MDATE" USING WS-6M
           MOVE SM-TRIGGER-DATE OF WS-6M TO WS-DATE-N
           MOVE SPACES TO D-TRIGGER
           STRING WS-DATE-DD "/" WS-DATE-MM "/" WS-DATE-YYYY
               DELIMITED BY SIZE INTO D-TRIGGER
           MOVE SM-DAYS-OVER OF WS-6M TO D-DAYS
           MOVE SM-PERIOD-KEY OF WS-6M TO D-PERIOD
           IF SM-DUE-FOR-REPAY OF WS-6M NOT = "Y"
               GO TO 1300-EXIT
           END-IF
           MOVE SM-REPAY-VAT OF WS-6M TO D-ADJUST
           MOVE "REPAY" TO D-ACTION
           ADD 1 TO T-REPAY-CNT
           ADD D-ADJUST TO T-REPAY
           ADD 1 TO BL-SELECTED-COUNT
           IF WS-MODE = "COMMIT"
               PERFORM 1400-COMMIT-REPAY
           END-IF
           ADD 1 TO BL-WRITTEN-COUNT
           GENERATE DE-LINE.
       1300-EXIT.
           EXIT.

       1400-COMMIT-REPAY SECTION.
       1400-START.
           PERFORM 1500-MARK-EVIDENCE
           PERFORM 1600-POST-ADJUSTMENT.
       1400-EXIT.
           EXIT.

      *>   the evidence row carries the audit trail; a missing row is
      *>   created so the reclaim pass has something to reverse
       1500-MARK-EVIDENCE SECTION.
       1500-START.
           MOVE SPACES TO SQLIO-STMT
           IF WS-EVID-ID > 0
               STRING "UPDATE ap.VatEvidence SET Outcome = 'REPAID', "
                      "RepaidVatPeriodKey = ?, RepaidAt = ?, "
                      "FailureReason = ? WHERE EvidenceId = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 4 TO SQLIO-PARM-CNT
               MOVE SM-PERIOD-KEY OF WS-6M TO SQLIO-PARM-VAL(1)
               MOVE "N" TO SQLIO-PARM-NULL(1)
               MOVE WS-ASAT-TEXT TO SQLIO-PARM-VAL(2)
               MOVE "N" TO SQLIO-PARM-NULL(2)
               MOVE SPACES TO SQLIO-PARM-VAL(3)
               STRING "Unpaid six months after "
                      FUNCTION TRIM(D-TRIGGER)
                   DELIMITED BY SIZE INTO SQLIO-PARM-VAL(3)
               MOVE "N" TO SQLIO-PARM-NULL(3)
               MOVE SPACES TO SQLIO-PARM-VAL(4)
               MOVE WS-EVID-ID TO WS-INT-ED
               MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(4)
               MOVE "N" TO SQLIO-PARM-NULL(4)
           ELSE
               STRING "INSERT INTO ap.VatEvidence (CompanyCode, "
                      "PurchaseInvoiceId, RegisterNumber, "
                      "SupplierCode, EvidenceType, ClaimedVatGBP, "
                      "AllowedVatGBP, Outcome, RepaidVatPeriodKey, "
                      "RepaidAt, RecordedBy) VALUES (?, ?, ?, ?, "
                      "'VATINVOICE', ?, ?, 'REPAID', ?, ?, ?)"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 9 TO SQLIO-PARM-CNT
               MOVE WS-CO TO SQLIO-PARM-VAL(1)
               MOVE "N" TO SQLIO-PARM-NULL(1)
               MOVE WS-INV-ID TO WS-INT-ED
               MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(2)
               MOVE "N" TO SQLIO-PARM-NULL(2)
               MOVE D-REGISTER TO SQLIO-PARM-VAL(3)
               MOVE "N" TO SQLIO-PARM-NULL(3)
               MOVE D-SUPPLIER TO SQLIO-PARM-VAL(4)
               MOVE "N" TO SQLIO-PARM-NULL(4)
               MOVE D-CLAIMED TO WS-MONEY-ED
               MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(5)
               MOVE "N" TO SQLIO-PARM-NULL(5)
               MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(6)
               MOVE "N" TO SQLIO-PARM-NULL(6)
               MOVE SM-PERIOD-KEY OF WS-6M TO SQLIO-PARM-VAL(7)
               MOVE "N" TO SQLIO-PARM-NULL(7)
               MOVE WS-ASAT-TEXT TO SQLIO-PARM-VAL(8)
               MOVE "N" TO SQLIO-PARM-NULL(8)
               MOVE WS-USER TO SQLIO-PARM-VAL(9)
               MOVE "N" TO SQLIO-PARM-NULL(9)
           END-IF
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-ERROR
               ADD 1 TO BL-ERROR-COUNT
               MOVE 4 TO WS-RC
               DISPLAY "STERP-E-1501 " SQLIO-ERRMSG
           END-IF.
       1500-EXIT.
           EXIT.

      *>   REQ: AP-029 - the same correlation id posted twice returns
      *>   the original journal, so a restarted run cannot repay the
      *>   same invoice again.
       1600-POST-ADJUSTMENT SECTION.
       1600-START.
           INITIALIZE WS-APPOST
           MOVE "POST" TO PJ-FUNC
           MOVE WS-CO TO PJ-COMPANY
           MOVE WS-USER TO PJ-USER
           MOVE WS-ASAT TO PJ-BUSINESS-DATE
           MOVE WS-ASAT TO PJ-POSTING-DATE
           MOVE WS-ASAT TO PJ-DOCUMENT-DATE
           MOVE "GBP" TO PJ-CURRENCY
           MOVE 1 TO PJ-FX-RATE
           MOVE "JOURNAL" TO PJ-JOURNAL-TYPE
           MOVE "AP" TO PJ-SUBLEDGER-TYPE
           MOVE D-REGISTER TO PJ-SUBLEDGER-REF
           MOVE D-REGISTER TO PJ-SOURCE-REF
           MOVE SPACES TO WS-CORR
           IF WS-PASS = "REPAY"
               MOVE "AP.VATREPAY" TO PJ-EVENT-CODE
               STRING "AP6M-REPAY-" FUNCTION TRIM(WS-CO) "-"
                      FUNCTION TRIM(D-REGISTER)
                   DELIMITED BY SIZE INTO WS-CORR
               MOVE SPACES TO PJ-DESCRIPTION
               STRING "Input VAT repaid, unpaid six months - "
                      FUNCTION TRIM(D-REGISTER)
                   DELIMITED BY SIZE INTO PJ-DESCRIPTION
               MOVE 2 TO PJ-LINE-CNT
               MOVE "2050" TO PJ-L-ACCOUNT(1)
               MOVE D-ADJUST TO PJ-L-DR(1)
               MOVE "VAT control, input tax repaid"
                   TO PJ-L-NARRATIVE(1)
               MOVE "1250" TO PJ-L-ACCOUNT(2)
               MOVE D-ADJUST TO PJ-L-CR(2)
               MOVE "VAT recoverable, claim reversed"
                   TO PJ-L-NARRATIVE(2)
           ELSE
               MOVE "AP.VATRECLAIM" TO PJ-EVENT-CODE
               STRING "AP6M-RECLAIM-" FUNCTION TRIM(WS-CO) "-"
                      FUNCTION TRIM(D-REGISTER)
                   DELIMITED BY SIZE INTO WS-CORR
               MOVE SPACES TO PJ-DESCRIPTION
               STRING "Input VAT reclaimed once paid - "
                      FUNCTION TRIM(D-REGISTER)
                   DELIMITED BY SIZE INTO PJ-DESCRIPTION
               MOVE 2 TO PJ-LINE-CNT
               MOVE "1250" TO PJ-L-ACCOUNT(1)
               MOVE D-ADJUST TO PJ-L-DR(1)
               MOVE "VAT recoverable, claimed again"
                   TO PJ-L-NARRATIVE(1)
               MOVE "2050" TO PJ-L-ACCOUNT(2)
               MOVE D-ADJUST TO PJ-L-CR(2)
               MOVE "VAT control, return in progress"
                   TO PJ-L-NARRATIVE(2)
           END-IF
           MOVE WS-CORR TO PJ-CORRELATION
           MOVE SM-PERIOD-KEY OF WS-6M TO PJ-SERIES-CODE
           CALL "APPOST" USING WS-APPOST
           IF PJ-RC < 8
               ADD 1 TO T-POSTED
           ELSE
               ADD 1 TO BL-ERROR-COUNT
               MOVE 4 TO WS-RC
               DISPLAY "STERP-E-1601 " FUNCTION TRIM(PJ-MESSAGE)
           END-IF.
       1600-EXIT.
           EXIT.

       1900-CLOSE-CURSOR SECTION.
       1900-START.
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW.
       1900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   REQ: AP-029 - pass two, invoices repaid earlier that have
      *>   since been settled.  The reclaim happens exactly once
      *>   because the evidence row moves to RECLAIMED.
       2000-RECLAIM-PASS SECTION.
       2000-START.
           MOVE "RECLAIM" TO WS-PASS
           PERFORM 2100-OPEN-PAID
           IF WS-SQL-FAILED = "Y"
               GO TO 2000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-DONE = "Y"
               PERFORM 2200-FETCH-PAID
               IF WS-DONE NOT = "Y"
                   ADD 1 TO T-SCANNED
                   PERFORM 2300-RECLAIM-ONE
               END-IF
           END-PERFORM
           PERFORM 1900-CLOSE-CURSOR.
       2000-EXIT.
           EXIT.

       2100-OPEN-PAID SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT i.SupplierCode, s.LegalName, "
                  "i.RegisterNumber, i.SupplierDocRef, "
                  "CONVERT(varchar(8), i.TaxPointDate, 112), "
                  "CONVERT(varchar(8), "
                  "ISNULL(i.DueDate, i.TaxPointDate), 112), "
                  "CONVERT(varchar(30), i.GrossAmountGBP), "
                  "CONVERT(varchar(30), v.AllowedVatGBP), "
                  "CONVERT(varchar(30), ISNULL(o.ResidualAmountGBP, "
                  "0)), "
                  "CONVERT(varchar(20), i.PurchaseInvoiceId), "
                  "CONVERT(varchar(20), v.EvidenceId), "
                  "v.Outcome "
                  "FROM ap.PurchaseInvoice i "
                  "JOIN ap.Supplier s ON s.CompanyCode = "
                  "i.CompanyCode AND s.SupplierCode = i.SupplierCode "
                  "JOIN ap.VatEvidence v ON v.CompanyCode = "
                  "i.CompanyCode AND v.PurchaseInvoiceId = "
                  "i.PurchaseInvoiceId AND v.Outcome = 'REPAID' "
                  "LEFT JOIN ap.OpenItem o ON o.CompanyCode = "
                  "i.CompanyCode AND o.PurchaseInvoiceId = "
                  "i.PurchaseInvoiceId AND o.ItemType = 'INVOICE' "
                  "WHERE i.CompanyCode = ? "
                  "AND i.SettlementState IN ('SETTLED','PARTPAID') "
                  "AND ISNULL(o.ResidualAmountGBP, 0) < "
                  "i.GrossAmountGBP "
                  "ORDER BY i.SupplierCode, i.RegisterNumber"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 12 TO SQLIO-COL-CNT
           PERFORM 0290-SET-WIDTHS
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-ERROR
               MOVE "Y" TO WS-SQL-FAILED
               DISPLAY "STERP-E-2101 " SQLIO-ERRMSG
           END-IF.
       2100-EXIT.
           EXIT.

       2200-FETCH-PAID SECTION.
       2200-START.
           MOVE 2 TO SQLIO-CURSOR
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "Y" TO WS-DONE
               GO TO 2200-EXIT
           END-IF
           ADD 1 TO BL-READ-COUNT
           PERFORM 1250-MOVE-ROW
           COMPUTE WS-ALREADY = FUNCTION NUMVAL(AP-COL(8)).
       2200-EXIT.
           EXIT.

       2300-RECLAIM-ONE SECTION.
       2300-START.
           MOVE "RECLAIM" TO SM-FUNC OF WS-6M
           MOVE 6 TO SM-MONTHS OF WS-6M
           MOVE WS-ASAT TO SM-AS-AT OF WS-6M
           MOVE WS-ASAT TO SM-PAID-DATE OF WS-6M
           MOVE D-CLAIMED TO SM-CLAIMED-VAT OF WS-6M
           MOVE D-GROSS TO SM-GROSS OF WS-6M
           MOVE D-RESIDUAL TO SM-RESIDUAL OF WS-6M
           MOVE WS-ALREADY TO SM-ALREADY-REPAID OF WS-6M
           CALL "AP6MDATE" USING WS-6M
           IF SM-RECLAIM-VAT OF WS-6M NOT > 0
               GO TO 2300-EXIT
           END-IF
           MOVE SPACES TO D-TRIGGER
           MOVE 0 TO D-DAYS
           MOVE SM-RECLAIM-VAT OF WS-6M TO D-ADJUST
           COMPUTE D-ADJUST = 0 - D-ADJUST
           MOVE "RECLAIM" TO D-ACTION
           MOVE SM-PERIOD-KEY OF WS-6M TO D-PERIOD
           ADD 1 TO T-RECLAIM-CNT
           ADD SM-RECLAIM-VAT OF WS-6M TO T-RECLAIM
           ADD 1 TO BL-SELECTED-COUNT
           IF WS-MODE = "COMMIT"
               MOVE SM-RECLAIM-VAT OF WS-6M TO D-ADJUST
               PERFORM 2400-MARK-RECLAIMED
               PERFORM 1600-POST-ADJUSTMENT
               COMPUTE D-ADJUST = 0 - SM-RECLAIM-VAT OF WS-6M
           END-IF
           ADD 1 TO BL-WRITTEN-COUNT
           GENERATE DE-LINE.
       2300-EXIT.
           EXIT.

       2400-MARK-RECLAIMED SECTION.
       2400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.VatEvidence SET Outcome = 'RECLAIMED', "
                  "ReclaimedVatPeriodKey = ?, ReclaimedAt = ? "
                  "WHERE EvidenceId = ? AND Outcome = 'REPAID'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE SM-PERIOD-KEY OF WS-6M TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-ASAT-TEXT TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE WS-EVID-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-ERROR
               ADD 1 TO BL-ERROR-COUNT
               MOVE 4 TO WS-RC
               DISPLAY "STERP-E-2401 " SQLIO-ERRMSG
           END-IF.
       2400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-APVAT6M
           CLOSE RPT-FILE.
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           MOVE WS-RC TO BL-RETURN-CODE
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "APVAT6M" TO JL-JOB-NAME
           MOVE BL-WRITTEN-COUNT TO JL-ROWS-PROCESSED
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE BL-SPOOL-FILE TO JL-SPOOL-FILE
           MOVE WS-RC TO CA-RETURN-CODE OF WS-JOBLOG
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE T-REPAY-CNT TO BL-COUNT-ED
           DISPLAY "STERP-I-0500 APVAT6M " BL-COUNT-ED
                   " repayments, " T-RECLAIM-CNT " reclaims"
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0500-EXIT.
           EXIT.
