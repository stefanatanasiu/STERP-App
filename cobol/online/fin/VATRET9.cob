      *>================================================================
      *> PROGRAM-ID : VATRET9
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER (VALUE ADDED TAX)
      *> PURPOSE    : PREPARE, DRILL INTO, FINALISE AND SUBMIT THE NINE
      *>              BOX VAT RETURN.  EVERY FIGURE IS AGGREGATED FROM
      *>              POSTED VAT RECORDS - NOTHING CAN BE TYPED INTO A
      *>              BOX (FIN-025, FIN-026).  BOX 3 = BOX 1 + BOX 2
      *>              AND BOX 5 = BOX 3 - BOX 4.  BOXES 6 TO 9 EXCLUDE
      *>              VAT AND ARE REPORTED IN WHOLE POUNDS.
      *>              FINALISING FREEZES BOTH THE BOX VALUES AND THE
      *>              TRANSACTIONS BEHIND THEM, SO A LATER EDIT TO A
      *>              SOURCE DOCUMENT CANNOT CHANGE WHAT WAS SUBMITTED;
      *>              SUCH AN ITEM IS CARRIED INTO THE NEXT OPEN RETURN
      *>              AS A LATE ENTRY (FIN-018).
      *> CALLED BY  : frmVatReturn, MTDJSON, .NET CLIENT
      *> INTERFACE  : CALL "VATRET9" USING WS-VATRET9-CA
      *>              COPYBOOK VATRET9CA
      *> CALLS      : PERMCHK, AUDITLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-09-02 JMC CR0510 ORIGINAL VAT 100 PRINT
      *> 2012-04-01 KMP CR2602 ONLINE FILING REPLACES THE PAPER FORM
      *> 2019-04-01 DJM CR3001 MAKING TAX DIGITAL - DIGITAL LINKS ONLY
      *> 2026-10-05 STERP BOX DRILL-DOWN AND FROZEN DETAIL
      *>================================================================
      *> REQ: FIN-018, FIN-025, FIN-026, FIN-029
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VATRET9.
       AUTHOR. J M CLEARY.
       DATE-WRITTEN. 02/09/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERMCHK-CA==
                      LEADING ==CA-== BY ==KH-==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDITLOG-CA==
                      LEADING ==CA-== BY ==AH-==.
       01  WS-SWITCHES.
           05  WS-REJECTED             PIC X VALUE "N".
               88  WS-IS-REJECTED                VALUE "Y".
      *>   Set while FINALISE re-computes the boxes for itself.  The
      *>   approver has already proved FIN.VAT.APPROVE and, under the
      *>   SOD-VAT rule (D-07), must not also hold FIN.VAT.PREPARE, so
      *>   the internal recompute does not ask for the preparer's
      *>   authority a second time.
           05  WS-INTERNAL             PIC X VALUE "N".
               88  WS-IS-INTERNAL                VALUE "Y".
      *>   Set only when FINALISE opened the unit of work itself.  A
      *>   raw COMT would commit a caller's transaction as well, which
      *>   is why the depth is read first (as in BANKREC and JNLENTRY).
           05  WS-OWN-TXN              PIC X VALUE "N".
       01  WS-WORK.
           05  WS-IDX                  PIC S9(4) COMP-5 VALUE 0.
           05  WS-BOX                  PIC 9(2) VALUE 0.
           05  WS-AMT                  PIC S9(13)V99 VALUE 0.
           05  WS-MONEY-ED             PIC -(11)9.99.
           05  WS-PERM                 PIC X(40).
       01  WS-BOX-ROW.
           05  WS-B-COMPANY            PIC X(10).
           05  WS-B-KEY                PIC X(10).
           05  WS-B-BASIS              PIC X(10).
           05  WS-B-1                  PIC X(22).
           05  WS-B-2                  PIC X(22).
           05  WS-B-3                  PIC X(22).
           05  WS-B-4                  PIC X(22).
           05  WS-B-5                  PIC X(22).
           05  WS-B-6                  PIC X(22).
           05  WS-B-7                  PIC X(22).
           05  WS-B-8                  PIC X(22).
           05  WS-B-9                  PIC X(22).
           05  WS-B-COUNT              PIC X(10).
           05  WS-B-LATE               PIC X(10).
       01  WS-PER-ROW.
           05  WS-P-REGNO              PIC X(15).
           05  WS-P-START              PIC X(8).
           05  WS-P-END                PIC X(8).
           05  WS-P-FILING             PIC X(8).
           05  WS-P-PAYMENT            PIC X(8).
           05  WS-P-STATUS             PIC X(14).
           05  WS-P-RETURNID           PIC X(18).
           05  WS-P-RETSTATUS          PIC X(14).
           05  WS-P-VERSION            PIC X(8).
       01  WS-DRILL-ROW.
           05  WS-D-BOX                PIC X(6).
           05  WS-D-DOCTYPE            PIC X(20).
           05  WS-D-DOCREF             PIC X(40).
           05  WS-D-PARTY              PIC X(20).
           05  WS-D-TAXPOINT           PIC X(8).
           05  WS-D-POSTDATE           PIC X(8).
           05  WS-D-VATCODE            PIC X(4).
           05  WS-D-MECHANISM          PIC X(14).
           05  WS-D-DIRECTION          PIC X(8).
           05  WS-D-NET                PIC X(22).
           05  WS-D-VAT                PIC X(22).
           05  WS-D-CONTRIB            PIC X(22).
           05  WS-D-LATE               PIC X(4).
       01  WS-CTL-ROW.
           05  WS-X-BALANCE            PIC X(22).
       01  WS-TRAN-ROW.
           05  WS-TRAN-DEPTH           PIC X(8).
       LINKAGE SECTION.
       COPY VATRET9CA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           PERFORM 0200-EDIT-REQUEST THRU 0200-EXIT
           IF WS-IS-REJECTED
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "PREPARE"  PERFORM 1000-PREPARE THRU 1000-EXIT
               WHEN "DRILL"    PERFORM 2000-DRILL THRU 2000-EXIT
               WHEN "FINALISE" PERFORM 3000-FINALISE THRU 3000-EXIT
               WHEN "SUBMIT"   PERFORM 4000-SUBMIT THRU 4000-EXIT
               WHEN "CHECK"    PERFORM 5000-CONTROL-CHECK THRU 5000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.

       0100-INITIALISE.
           MOVE "N" TO WS-REJECTED
           MOVE "N" TO WS-INTERNAL
           MOVE "VATRET9" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "FIN-025" TO CA-REQ-REF
           MOVE 0 TO VR-BOX1
           MOVE 0 TO VR-BOX2
           MOVE 0 TO VR-BOX3
           MOVE 0 TO VR-BOX4
           MOVE 0 TO VR-BOX5
           MOVE 0 TO VR-BOX6
           MOVE 0 TO VR-BOX7
           MOVE 0 TO VR-BOX8
           MOVE 0 TO VR-BOX9
           MOVE 0 TO VR-RECORD-CNT
           MOVE 0 TO VR-LATE-CNT
           MOVE 0 TO VR-DRILL-CNT
           MOVE 0 TO VR-CONTROL-BALANCE
           MOVE 0 TO VR-CONTROL-DIFF
           MOVE "N" TO VR-IS-REPAYMENT
           MOVE SPACES TO VR-RETURN-STATUS
           MOVE SPACES TO VR-PERIOD-STATUS
           MOVE FUNCTION UPPER-CASE(VR-VAT-PERIOD-KEY)
               TO VR-VAT-PERIOD-KEY.
       0100-EXIT.
           EXIT.

       0200-EDIT-REQUEST.
           IF CA-COMPANY = SPACES
               MOVE "VTR0001" TO CA-REASON-CODE
               MOVE "A company is required to prepare a VAT return"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF VR-VAT-PERIOD-KEY = SPACES
               MOVE "VTR0002" TO CA-REASON-CODE
               MOVE "A VAT return period key is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           PERFORM 0300-READ-PERIOD THRU 0300-EXIT.
       0200-EXIT.
           EXIT.

       0300-READ-PERIOD.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(p.VatRegNo, c.VatRegNo), "
                  "CONVERT(char(8), p.StartDate, 112), "
                  "CONVERT(char(8), p.EndDate, 112), "
                  "CONVERT(char(8), p.FilingDueDate, 112), "
                  "CONVERT(char(8), p.PaymentDueDate, 112), p.Status, "
                  "ISNULL(CAST(r.VatReturnId AS varchar(18)),'0'), "
                  "ISNULL(r.Status,''), "
                  "ISNULL(CAST(r.Version AS varchar(8)),'0') "
                  "FROM fin.VatPeriod p JOIN core.Company c "
                  "ON c.CompanyCode = p.CompanyCode "
                  "OUTER APPLY (SELECT TOP (1) x.VatReturnId, "
                  "x.Status, x.Version FROM fin.VatReturn x WHERE "
                  "x.CompanyCode = p.CompanyCode AND x.VatPeriodKey = "
                  "p.VatPeriodKey ORDER BY x.Version DESC) r "
                  "WHERE p.CompanyCode = ? AND p.VatPeriodKey = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE VR-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 9 TO SQLIO-COL-CNT
           MOVE 15 TO SQLIO-COL-WIDTH(1)
           MOVE 8 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-COL-WIDTH(3)
           MOVE 8 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 14 TO SQLIO-COL-WIDTH(6)
           MOVE 18 TO SQLIO-COL-WIDTH(7)
           MOVE 14 TO SQLIO-COL-WIDTH(8)
           MOVE 8 TO SQLIO-COL-WIDTH(9)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PER-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "VTR0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "VAT period " FUNCTION TRIM(VR-VAT-PERIOD-KEY)
                      " is not held for " FUNCTION TRIM(CA-COMPANY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               MOVE "Y" TO WS-REJECTED
               ADD 1 TO CA-MSG-COUNT
               GO TO 0300-EXIT
           END-IF
           MOVE WS-P-REGNO TO VR-VAT-REG-NO
           COMPUTE VR-START-DATE = FUNCTION NUMVAL(WS-P-START)
           COMPUTE VR-END-DATE = FUNCTION NUMVAL(WS-P-END)
           COMPUTE VR-FILING-DUE = FUNCTION NUMVAL(WS-P-FILING)
           COMPUTE VR-PAYMENT-DUE = FUNCTION NUMVAL(WS-P-PAYMENT)
           MOVE WS-P-STATUS TO VR-PERIOD-STATUS
           MOVE WS-P-RETSTATUS TO VR-RETURN-STATUS
           COMPUTE VR-RETURN-ID = FUNCTION NUMVAL(WS-P-RETURNID)
           IF VR-VERSION = 0
               COMPUTE VR-VERSION = FUNCTION NUMVAL(WS-P-VERSION)
           END-IF
           IF VR-VERSION = 0
               MOVE 1 TO VR-VERSION
           END-IF.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000 - PREPARE.  THE BOXES ARE AGGREGATED, NEVER TYPED.
      *>----------------------------------------------------------------
       1000-PREPARE.
           MOVE "FIN.VAT.PREPARE" TO WS-PERM
           PERFORM 8100-AUTHORITY THRU 8100-EXIT
           IF WS-IS-REJECTED
               GO TO 1000-EXIT
           END-IF
           PERFORM 1100-CALL-BOXES THRU 1100-EXIT
           IF CA-FAILED
               GO TO 1000-EXIT
           END-IF
           IF VR-BOX5 < 0
               MOVE "Y" TO VR-IS-REPAYMENT
           END-IF
           IF VR-RETURN-STATUS = "FINALISED"
             OR VR-RETURN-STATUS = "SUBMITTED"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "VTR0010" TO CA-REASON-CODE
               MOVE "That return is already finalised. The figures sho"
                   TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               PERFORM 1300-READ-FROZEN THRU 1300-EXIT
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-SAVE-DRAFT THRU 1200-EXIT
           IF CA-RETURN-CODE < 8
               MOVE SPACES TO CA-MESSAGE
               MOVE VR-BOX5 TO WS-MONEY-ED
               IF VR-IS-REPAYMENT = "Y"
                   STRING "Return prepared. Box 5 is "
                          FUNCTION TRIM(WS-MONEY-ED)
                          " GBP, repayable by HMRC"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               ELSE
                   STRING "Return prepared. Box 5 is "
                          FUNCTION TRIM(WS-MONEY-ED)
                          " GBP, payable to HMRC"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               END-IF
           END-IF.
       1000-EXIT.
           EXIT.

       1100-CALL-BOXES.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC fin.usp_VatBoxes ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE VR-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           IF VR-BASIS = SPACES
               MOVE SPACES TO SQLIO-PARM-VAL(3)
               MOVE "Y" TO SQLIO-PARM-NULL(3)
           ELSE
               MOVE VR-BASIS TO SQLIO-PARM-VAL(3)
               MOVE "N" TO SQLIO-PARM-NULL(3)
           END-IF
           MOVE 14 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           PERFORM VARYING WS-IDX FROM 4 BY 1 UNTIL WS-IDX > 12
               MOVE 22 TO SQLIO-COL-WIDTH(WS-IDX)
           END-PERFORM
           MOVE 10 TO SQLIO-COL-WIDTH(13)
           MOVE 10 TO SQLIO-COL-WIDTH(14)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BOX-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "VTR0004" TO CA-REASON-CODE
               MOVE "The box aggregation returned no row"
                   TO CA-MESSAGE
               MOVE 12 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 1100-EXIT
           END-IF
           MOVE WS-B-BASIS TO VR-BASIS
           COMPUTE VR-BOX1 = FUNCTION NUMVAL(WS-B-1)
           COMPUTE VR-BOX2 = FUNCTION NUMVAL(WS-B-2)
           COMPUTE VR-BOX3 = FUNCTION NUMVAL(WS-B-3)
           COMPUTE VR-BOX4 = FUNCTION NUMVAL(WS-B-4)
           COMPUTE VR-BOX5 = FUNCTION NUMVAL(WS-B-5)
           COMPUTE VR-BOX6 = FUNCTION NUMVAL(WS-B-6)
           COMPUTE VR-BOX7 = FUNCTION NUMVAL(WS-B-7)
           COMPUTE VR-BOX8 = FUNCTION NUMVAL(WS-B-8)
           COMPUTE VR-BOX9 = FUNCTION NUMVAL(WS-B-9)
           COMPUTE VR-RECORD-CNT = FUNCTION NUMVAL(WS-B-COUNT)
           COMPUTE VR-LATE-CNT = FUNCTION NUMVAL(WS-B-LATE)
      *>   Belt and braces: the two box identities are re-proved here
      *>   as well as in the procedure (FIN-025).
           COMPUTE WS-AMT = VR-BOX1 + VR-BOX2
           IF WS-AMT NOT = VR-BOX3
               MOVE WS-AMT TO VR-BOX3
           END-IF
           COMPUTE WS-AMT = VR-BOX3 - VR-BOX4
           IF WS-AMT NOT = VR-BOX5
               MOVE WS-AMT TO VR-BOX5
           END-IF.
       1100-EXIT.
           EXIT.

       1200-SAVE-DRAFT.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.VatReturn SET Box1 = ?, Box2 = ?, "
                  "Box3 = ?, Box4 = ?, Box5 = ?, Box6 = ?, Box7 = ?, "
                  "Box8 = ?, Box9 = ?, PreparedBy = ?, PreparedAt = "
                  "SYSDATETIME(), VatRegNo = ? WHERE CompanyCode = ? "
                  "AND VatPeriodKey = ? AND Status = 'DRAFT'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 1250-BIND-BOXES THRU 1250-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BOX-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1200-EXIT
           END-IF
           IF SQLIO-ROWCOUNT > 0
               MOVE "DRAFT" TO VR-RETURN-STATUS
               GO TO 1200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.VatReturn (CompanyCode, VatPeriodKey, "
                  "Version, VatRegNo, Box1, Box2, Box3, Box4, Box5, "
                  "Box6, Box7, Box8, Box9, Status, PreparedBy, "
                  "PreparedAt) SELECT ?, ?, 1, ?, ?, ?, ?, ?, ?, ?, "
                  "?, ?, ?, 'DRAFT', ?, SYSDATETIME() WHERE NOT "
                  "EXISTS (SELECT 1 FROM fin.VatReturn WHERE "
                  "CompanyCode = ? AND VatPeriodKey = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 15 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE VR-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           MOVE VR-VAT-REG-NO TO SQLIO-PARM-VAL(3)
           MOVE VR-BOX1 TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE VR-BOX2 TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE VR-BOX3 TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE VR-BOX4 TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE VR-BOX5 TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE VR-BOX6 TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE VR-BOX7 TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE "0" TO SQLIO-PARM-VAL(11)
           MOVE "0" TO SQLIO-PARM-VAL(12)
           MOVE CA-USER TO SQLIO-PARM-VAL(13)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(14)
           MOVE VR-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(15)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BOX-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1200-EXIT
           END-IF
           MOVE "DRAFT" TO VR-RETURN-STATUS.
       1200-EXIT.
           EXIT.

       1250-BIND-BOXES.
           MOVE 13 TO SQLIO-PARM-CNT
           MOVE VR-BOX1 TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE VR-BOX2 TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE VR-BOX3 TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE VR-BOX4 TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE VR-BOX5 TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE VR-BOX6 TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE VR-BOX7 TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE "0" TO SQLIO-PARM-VAL(8)
           MOVE "0" TO SQLIO-PARM-VAL(9)
           MOVE CA-USER TO SQLIO-PARM-VAL(10)
           MOVE VR-VAT-REG-NO TO SQLIO-PARM-VAL(11)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(12)
           MOVE VR-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(13)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT.
       1250-EXIT.
           EXIT.

       1300-READ-FROZEN.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT r.CompanyCode, r.VatPeriodKey, 'FROZEN', "
                  "r.Box1, r.Box2, r.Box3, r.Box4, r.Box5, r.Box6, "
                  "r.Box7, r.Box8, r.Box9, '0', '0' "
                  "FROM fin.VatReturn r WHERE r.CompanyCode = ? "
                  "AND r.VatPeriodKey = ? AND r.VatReturnId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE VR-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           MOVE VR-RETURN-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 14 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           PERFORM VARYING WS-IDX FROM 4 BY 1 UNTIL WS-IDX > 12
               MOVE 22 TO SQLIO-COL-WIDTH(WS-IDX)
           END-PERFORM
           MOVE 10 TO SQLIO-COL-WIDTH(13)
           MOVE 10 TO SQLIO-COL-WIDTH(14)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BOX-ROW
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               GO TO 1300-EXIT
           END-IF
           COMPUTE VR-BOX1 = FUNCTION NUMVAL(WS-B-1)
           COMPUTE VR-BOX2 = FUNCTION NUMVAL(WS-B-2)
           COMPUTE VR-BOX3 = FUNCTION NUMVAL(WS-B-3)
           COMPUTE VR-BOX4 = FUNCTION NUMVAL(WS-B-4)
           COMPUTE VR-BOX5 = FUNCTION NUMVAL(WS-B-5)
           COMPUTE VR-BOX6 = FUNCTION NUMVAL(WS-B-6)
           COMPUTE VR-BOX7 = FUNCTION NUMVAL(WS-B-7)
           COMPUTE VR-BOX8 = FUNCTION NUMVAL(WS-B-8)
           COMPUTE VR-BOX9 = FUNCTION NUMVAL(WS-B-9).
       1300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000 - DRILL INTO ONE BOX
      *>----------------------------------------------------------------
       2000-DRILL.
           IF VR-BOX-NO < 1 OR VR-BOX-NO > 9
               MOVE "VTR0020" TO CA-REASON-CODE
               MOVE "A box number between 1 and 9 is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           MOVE 0 TO VR-DRILL-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC fin.usp_VatBoxDrill ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE VR-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           MOVE VR-BOX-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
      *>   fin.usp_VatBoxDrill returns the party, the posting date,
      *>   the mechanism and the direction between the columns this
      *>   program reports, so all thirteen are bound; binding only
      *>   nine of them used to read the mechanism as the VAT amount
      *>   and left every money column at zero (FIN-029).
           MOVE 13 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 40 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 8 TO SQLIO-COL-WIDTH(6)
           MOVE 4 TO SQLIO-COL-WIDTH(7)
           MOVE 14 TO SQLIO-COL-WIDTH(8)
           MOVE 8 TO SQLIO-COL-WIDTH(9)
           MOVE 22 TO SQLIO-COL-WIDTH(10)
           MOVE 22 TO SQLIO-COL-WIDTH(11)
           MOVE 22 TO SQLIO-COL-WIDTH(12)
           MOVE 4 TO SQLIO-COL-WIDTH(13)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DRILL-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-FETCH-DRILL THRU 2100-EXIT
               UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
                  OR VR-DRILL-CNT NOT < 20
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DRILL-ROW
           MOVE SPACES TO CA-MESSAGE
           MOVE VR-BOX-NO TO SQLIO-INT-EDIT
           STRING "Box " FUNCTION TRIM(SQLIO-INT-EDIT)
                  " drills down to the listed source documents"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

       2100-FETCH-DRILL.
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DRILL-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               GO TO 2100-EXIT
           END-IF
           ADD 1 TO VR-DRILL-CNT
           MOVE VR-DRILL-CNT TO WS-IDX
           MOVE WS-D-DOCTYPE TO VR-D-DOC-TYPE(WS-IDX)
           MOVE WS-D-DOCREF TO VR-D-DOC-REF(WS-IDX)
           COMPUTE VR-D-TAX-POINT(WS-IDX) =
               FUNCTION NUMVAL(WS-D-TAXPOINT)
           MOVE WS-D-VATCODE TO VR-D-VAT-CODE(WS-IDX)
           COMPUTE VR-D-NET(WS-IDX) = FUNCTION NUMVAL(WS-D-NET)
           COMPUTE VR-D-VAT(WS-IDX) = FUNCTION NUMVAL(WS-D-VAT)
           COMPUTE VR-D-AMOUNT(WS-IDX) = FUNCTION NUMVAL(WS-D-CONTRIB)
           IF WS-D-LATE(1:1) = "1"
               MOVE "Y" TO VR-D-LATE(WS-IDX)
           ELSE
               MOVE "N" TO VR-D-LATE(WS-IDX)
           END-IF.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000 - FINALISE.  FREEZE THE BOXES AND THE DETAIL BEHIND THEM.
      *>----------------------------------------------------------------
       3000-FINALISE.
           MOVE "FIN.VAT.APPROVE" TO WS-PERM
           PERFORM 8100-AUTHORITY THRU 8100-EXIT
           IF WS-IS-REJECTED
               GO TO 3000-EXIT
           END-IF
           IF VR-RETURN-STATUS = "FINALISED"
             OR VR-RETURN-STATUS = "SUBMITTED"
               MOVE "VTR0030" TO CA-REASON-CODE
               MOVE "That return has already been finalised. A correct"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE "Y" TO WS-INTERNAL
           PERFORM 1000-PREPARE THRU 1000-EXIT
           MOVE "N" TO WS-INTERNAL
           IF CA-RETURN-CODE > 4
               GO TO 3000-EXIT
           END-IF
           PERFORM 9500-BEGIN-WORK THRU 9500-EXIT
           PERFORM 3100-SET-FINAL THRU 3100-EXIT
           PERFORM 3200-FREEZE-BOXES THRU 3200-EXIT
           PERFORM 3300-FREEZE-DETAIL THRU 3300-EXIT
           PERFORM 3400-CLOSE-PERIOD THRU 3400-EXIT
           IF CA-RETURN-CODE > 8
               PERFORM 9520-ROLLB-WORK THRU 9520-EXIT
               GO TO 3000-EXIT
           END-IF
           PERFORM 9510-COMMIT-WORK THRU 9510-EXIT
           MOVE "FINALISED" TO VR-RETURN-STATUS
           MOVE "FINALISED" TO VR-PERIOD-STATUS
           PERFORM 8200-AUDIT THRU 8200-EXIT
           MOVE SPACES TO CA-MESSAGE
           STRING "VAT return " FUNCTION TRIM(VR-VAT-PERIOD-KEY)
                  " finalised. The figures and their supporting "
                  "records are now frozen"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       3000-EXIT.
           EXIT.

       3100-SET-FINAL.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.VatReturn SET Status = 'FINALISED', "
                  "ApprovedBy = ?, ApprovedAt = SYSDATETIME() "
                  "WHERE CompanyCode = ? AND VatPeriodKey = ? "
                  "AND Status = 'DRAFT'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE VR-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BOX-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       3100-EXIT.
           EXIT.

       3200-FREEZE-BOXES.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.VatReturnBox (VatReturnId, CompanyCode, "
                  "BoxNo, BoxValue, TransactionCount, IsWholePounds, "
                  "Caption) SELECT r.VatReturnId, r.CompanyCode, "
                  "b.BoxNo, b.Val, 0, CASE WHEN b.BoxNo >= 6 THEN 1 "
                  "ELSE 0 END, b.Cap FROM fin.VatReturn r CROSS APPLY "
                  "(VALUES (1,r.Box1,'VAT due on sales'),(2,r.Box2,"
                  "'VAT due on EU acquisitions'),(3,r.Box3,"
                  "'Total VAT due'),(4,r.Box4,"
                  "'VAT reclaimed on purchases'),(5,r.Box5,"
                  "'Net VAT to pay or reclaim'),(6,r.Box6,"
                  "'Total sales excluding VAT'),(7,r.Box7,"
                  "'Total purchases excluding VAT'),(8,r.Box8,"
                  "'Goods supplied to the EU'),(9,r.Box9,"
                  "'Goods acquired from the EU')) b (BoxNo, Val, Cap) "
                  "WHERE r.CompanyCode = ? AND r.VatPeriodKey = ? "
                  "AND NOT EXISTS (SELECT 1 FROM fin.VatReturnBox x "
                  "WHERE x.VatReturnId = r.VatReturnId)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE VR-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BOX-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       3200-EXIT.
           EXIT.

       3300-FREEZE-DETAIL.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.VatReturnDetail (VatReturnId, VatTxnId, "
                  "BoxNo, ContributionGBP, IsLateEntry) SELECT "
                  "r.VatReturnId, d.VatTxnId, d.BoxNo, d.Contribution,"
                  " d.IsLateEntry FROM fin.VatReturn r JOIN "
                  "fin.vw_VatBoxDetail d ON d.CompanyCode = "
                  "r.CompanyCode AND d.VatPeriodKey = r.VatPeriodKey "
                  "WHERE r.CompanyCode = ? AND r.VatPeriodKey = ? "
                  "AND NOT EXISTS (SELECT 1 FROM fin.VatReturnDetail x"
                  " WHERE x.VatReturnId = r.VatReturnId)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE VR-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(2)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BOX-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       3300-EXIT.
           EXIT.

       3400-CLOSE-PERIOD.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.VatPeriod SET Status = 'FINALISED', "
                  "FinalisedBy = ?, FinalisedAt = SYSDATETIME() "
                  "WHERE CompanyCode = ? AND VatPeriodKey = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE VR-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BOX-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       3400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000 - RECORD THE MTD SUBMISSION AND ITS RECEIPT
      *>----------------------------------------------------------------
       4000-SUBMIT.
           MOVE "FIN.VAT.SUBMIT" TO WS-PERM
           PERFORM 8100-AUTHORITY THRU 8100-EXIT
           IF WS-IS-REJECTED
               GO TO 4000-EXIT
           END-IF
           IF VR-RETURN-STATUS NOT = "FINALISED"
               MOVE "VTR0040" TO CA-REASON-CODE
               MOVE "Only a finalised return may be submitted to HMRC"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF VR-SUBMISSION-REF = SPACES
               MOVE "VTR0041" TO CA-REASON-CODE
               MOVE "The submission receipt reference must be retained"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.VatReturn SET Status = 'SUBMITTED', "
                  "SubmittedAt = SYSDATETIME(), SubmissionRef = ?, "
                  "HmrcResponse = ? WHERE CompanyCode = ? "
                  "AND VatPeriodKey = ? AND Status = 'FINALISED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE VR-SUBMISSION-REF TO SQLIO-PARM-VAL(1)
           MOVE VR-HMRC-RESPONSE TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE VR-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(4)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BOX-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.VatPeriod SET Status = 'SUBMITTED', "
                  "SubmissionRef = ? WHERE CompanyCode = ? "
                  "AND VatPeriodKey = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE VR-SUBMISSION-REF TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE VR-VAT-PERIOD-KEY TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BOX-ROW
           MOVE "SUBMITTED" TO VR-RETURN-STATUS
           PERFORM 8200-AUDIT THRU 8200-EXIT
           MOVE SPACES TO CA-MESSAGE
           STRING "Return submitted. Receipt "
                  FUNCTION TRIM(VR-SUBMISSION-REF) " retained"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000 - VAT CONTROL ACCOUNT RECONCILIATION (FIN-025)
      *>        BOX 5 MUST AGREE WITH THE MOVEMENT ON THE VAT CONTROL
      *>        ACCOUNTS FOR THE SAME DATES.
      *>----------------------------------------------------------------
       5000-CONTROL-CHECK.
           PERFORM 1100-CALL-BOXES THRU 1100-EXIT
           IF CA-FAILED
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SUM(l.CreditAmountGBP) - "
                  "SUM(l.DebitAmountGBP), 0) FROM fin.JournalLine l "
                  "JOIN fin.Journal j ON j.JournalId = l.JournalId "
                  "AND j.Status = 'POSTED' JOIN fin.Account a ON "
                  "a.CompanyCode = l.CompanyCode AND a.AccountCode = "
                  "l.AccountCode WHERE l.CompanyCode = ? "
                  "AND a.ControlType = 'VAT' AND j.PostingDate "
                  "BETWEEN ? AND ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE VR-START-DATE TO SQLIO-PARM-VAL(2)
           MOVE VR-END-DATE TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 22 TO SQLIO-COL-WIDTH(1)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CTL-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 5000-EXIT
           END-IF
           COMPUTE VR-CONTROL-BALANCE = FUNCTION NUMVAL(WS-X-BALANCE)
           COMPUTE VR-CONTROL-DIFF = VR-BOX5 - VR-CONTROL-BALANCE
           IF VR-CONTROL-DIFF = 0
               MOVE SPACES TO CA-MESSAGE
               STRING "Box 5 agrees with the VAT control account "
                      "movement for the period"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE 4 TO CA-RETURN-CODE
               MOVE "VTR0050" TO CA-REASON-CODE
               MOVE VR-CONTROL-DIFF TO WS-MONEY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Box 5 and the VAT control account differ by "
                      FUNCTION TRIM(WS-MONEY-ED)
                      " GBP. The difference must be explained before "
                      "the return is finalised"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       5000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-NOT-NULL.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-IDX)
           END-PERFORM.
       8000-EXIT.
           EXIT.

       8100-AUTHORITY.
           IF WS-IS-INTERNAL
               GO TO 8100-EXIT
           END-IF
           INITIALIZE WS-PERMCHK-CA
           MOVE "CHECK" TO KH-FUNCTION
           MOVE CA-COMPANY TO KH-COMPANY
           MOVE CA-SITE TO KH-SITE
           MOVE CA-USER TO KH-USER
           MOVE CA-BUSINESS-DATE TO KH-BUSINESS-DATE
           MOVE WS-PERM TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERMCHK-CA
           IF KH-RETURN-CODE > 4
               MOVE KH-REASON-CODE TO CA-REASON-CODE
               MOVE KH-MESSAGE TO CA-MESSAGE
               MOVE KH-RETURN-CODE TO CA-RETURN-CODE
               MOVE "Y" TO WS-REJECTED
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       8100-EXIT.
           EXIT.

       8200-AUDIT.
           INITIALIZE WS-AUDITLOG-CA
           MOVE "WRITE" TO AH-FUNCTION
           MOVE CA-COMPANY TO AH-COMPANY
           MOVE CA-USER TO AH-USER
           MOVE CA-BUSINESS-DATE TO AH-BUSINESS-DATE
           MOVE "fin.VatReturn" TO AU-ENTITY-TYPE
           MOVE VR-VAT-PERIOD-KEY TO AU-ENTITY-KEY
           MOVE CA-FUNCTION TO AU-ACTION
           MOVE VR-BOX5 TO WS-MONEY-ED
           MOVE WS-MONEY-ED TO AU-NEW-VALUE
           MOVE "Box 5 net VAT" TO AU-FIELD-NAME
           MOVE "COBOL" TO AU-SOURCE
           CALL "AUDITLOG" USING WS-AUDITLOG-CA.
       8200-EXIT.
           EXIT.

       9000-REJECT.
           MOVE "Y" TO WS-REJECTED
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE "VTR0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "VATRET9 does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

      *>   A unit of work is only opened, and only ever ended, when
      *>   this program started it.  An online caller or a test suite
      *>   may already hold one, and committing that one here would
      *>   harden work the caller had not finished with.
       9500-BEGIN-WORK.
           MOVE "N" TO WS-OWN-TXN
           MOVE "SELECT CAST(@@TRANCOUNT AS varchar(8))"
               TO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-COL-WIDTH(1)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TRAN-ROW
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "0" TO WS-TRAN-DEPTH
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TRAN-ROW
           IF FUNCTION NUMVAL(WS-TRAN-DEPTH) = 0
               MOVE "Y" TO WS-OWN-TXN
               MOVE "BEGN" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-TRAN-ROW
           END-IF.
       9500-EXIT.
           EXIT.

       9510-COMMIT-WORK.
           IF WS-OWN-TXN = "Y"
               MOVE "COMT" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-TRAN-ROW
               MOVE "N" TO WS-OWN-TXN
           END-IF.
       9510-EXIT.
           EXIT.

       9520-ROLLB-WORK.
           IF WS-OWN-TXN = "Y"
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-TRAN-ROW
               MOVE "N" TO WS-OWN-TXN
           END-IF.
       9520-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in VATRET9" TO CA-MESSAGE
           END-IF
           IF CA-MESSAGE(1:8) = "STERP-E-"
               MOVE CA-MESSAGE(7:6) TO CA-REASON-CODE
               MOVE 8 TO CA-RETURN-CODE
           ELSE
               MOVE "SQL0001" TO CA-REASON-CODE
               MOVE 12 TO CA-RETURN-CODE
           END-IF
           MOVE "Y" TO WS-REJECTED
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
