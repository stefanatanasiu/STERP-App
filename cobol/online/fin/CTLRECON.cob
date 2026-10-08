      *>================================================================
      *> PROGRAM-ID : CTLRECON
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER
      *> PURPOSE    : CONTROL ACCOUNT RECONCILIATION (FIN-021).
      *>              EVERY CONTROL ACCOUNT - RECEIVABLES, PAYABLES,
      *>              INVENTORY, WIP, BANK, TAX AND ACCRUALS - IS
      *>              COMPARED WITH ITS SUPPORTING DETAIL AS AT A
      *>              COMMON CUTOFF.  A DIFFERENCE IS REPORTED WITH THE
      *>              CONTRIBUTING TRANSACTIONS AND AN OWNER; IT IS
      *>              NEVER HIDDEN BY AN AUTOMATIC BALANCING ENTRY.
      *> CALLED BY  : PERCLOSE, frmPeriodClose, .NET CLIENT
      *> INTERFACE  : CALL "CTLRECON" USING WS-CTLRECON-CA
      *>              COPYBOOK CTLRECONCA
      *> CALLS      : EXCPRAIS, PARMGET, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-05-14 JMC CR0860 ORIGINAL - MICRO FOCUS COBOL/2
      *> 2026-10-05 STERP OWNERSHIP AND MATERIALITY (COM-022)
      *>================================================================
      *> REQ: FIN-021
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CTLRECON.
       AUTHOR. J M CLEARY.
       DATE-WRITTEN. 14/05/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCPRAIS-CA==
                      LEADING ==CA-== BY ==XH-==.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARMGET-CA==
                      LEADING ==CA-== BY ==MH-==.
       01  WS-SWITCHES.
           05  WS-REJECTED             PIC X VALUE "N".
               88  WS-IS-REJECTED                VALUE "Y".
       01  WS-WORK.
           05  WS-IDX                  PIC S9(4) COMP-5 VALUE 0.
           05  WS-MONEY-ED             PIC -(11)9.99.
       01  WS-REC-ROW.
           05  WS-R-ACCOUNT            PIC X(12).
           05  WS-R-NAME               PIC X(60).
           05  WS-R-TYPE               PIC X(10).
           05  WS-R-SUBLEDGER          PIC X(6).
           05  WS-R-CONTROL            PIC X(22).
           05  WS-R-DETAIL             PIC X(22).
           05  WS-R-DIFF               PIC X(22).
           05  WS-R-CTLCNT             PIC X(12).
           05  WS-R-DETCNT             PIC X(12).
           05  WS-R-STATE              PIC X(12).
           05  WS-R-LASTSTAT           PIC X(12).
           05  WS-R-OWNER              PIC X(20).
           05  WS-R-NOTE               PIC X(60).
           05  WS-R-RUNAT              PIC X(23).
       01  WS-ID-ROW.
           05  WS-I-ID                 PIC X(18).
       LINKAGE SECTION.
       COPY CTLRECONCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           IF CA-COMPANY = SPACES
               MOVE "CTL0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "RUN"      PERFORM 1000-RUN THRU 1000-EXIT
               WHEN "RECORD"   PERFORM 2000-RECORD THRU 2000-EXIT
               WHEN "ACCEPT"   PERFORM 3000-ACCEPT THRU 3000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.

       0100-INITIALISE.
           MOVE "N" TO WS-REJECTED
           MOVE "CTLRECON" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "FIN-021" TO CA-REQ-REF
           MOVE 0 TO CR-CONTROL-GBP
           MOVE 0 TO CR-DETAIL-GBP
           MOVE 0 TO CR-DIFFERENCE-GBP
           MOVE 0 TO CR-ROW-CNT
           MOVE 0 TO CR-DIFF-COUNT
           MOVE 0 TO CR-EXCEPTION-ID
           MOVE SPACES TO CR-STATE
           MOVE "N" TO CR-MATERIAL
           IF CR-AS-AT-DATE = 0
               MOVE CA-BUSINESS-DATE TO CR-AS-AT-DATE
           END-IF
           IF CR-MATERIALITY = 0
               PERFORM 0150-MATERIALITY THRU 0150-EXIT
           END-IF.
       0100-EXIT.
           EXIT.

       0150-MATERIALITY.
           MOVE 1000 TO CR-MATERIALITY
           INITIALIZE WS-PARMGET-CA
           MOVE "GETNUM" TO MH-FUNCTION
           MOVE CA-COMPANY TO MH-COMPANY
           MOVE CA-USER TO MH-USER
           MOVE CA-BUSINESS-DATE TO MH-BUSINESS-DATE
           MOVE "FIN.CLOSE.MATERIALITY" TO PM-PARAM-CODE
           MOVE "N" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARMGET-CA
           IF PM-FOUND = "Y" AND PM-VALUE-NUM > 0
               MOVE PM-VALUE-NUM TO CR-MATERIALITY
           END-IF.
       0150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000 - RECONCILE ONE OR EVERY CONTROL ACCOUNT
      *>----------------------------------------------------------------
       1000-RUN.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC fin.usp_ControlRecon ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CR-AS-AT-DATE TO SQLIO-PARM-VAL(2)
           MOVE CR-CONTROL-TYPE TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           IF CR-CONTROL-TYPE = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(3)
           END-IF
           MOVE 14 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 60 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           MOVE 6 TO SQLIO-COL-WIDTH(4)
           MOVE 22 TO SQLIO-COL-WIDTH(5)
           MOVE 22 TO SQLIO-COL-WIDTH(6)
           MOVE 22 TO SQLIO-COL-WIDTH(7)
           MOVE 12 TO SQLIO-COL-WIDTH(8)
           MOVE 12 TO SQLIO-COL-WIDTH(9)
           MOVE 12 TO SQLIO-COL-WIDTH(10)
           MOVE 12 TO SQLIO-COL-WIDTH(11)
           MOVE 20 TO SQLIO-COL-WIDTH(12)
           MOVE 60 TO SQLIO-COL-WIDTH(13)
           MOVE 23 TO SQLIO-COL-WIDTH(14)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-REC-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1000-EXIT
           END-IF
           PERFORM 1100-TAKE-ROW THRU 1100-EXIT
               UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-REC-ROW
           IF CR-DIFF-COUNT = 0
               MOVE "RECONCILED" TO CR-STATE
               MOVE SPACES TO CA-MESSAGE
               STRING "Every control account agrees with its supportin"
                      "g detail at that cutoff"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE "DIFFERENCE" TO CR-STATE
               MOVE 4 TO CA-RETURN-CODE
               MOVE "CTL0010" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               MOVE CR-DIFFERENCE-GBP TO WS-MONEY-ED
               STRING CR-DIFF-COUNT " control account(s) differ by "
                      FUNCTION TRIM(WS-MONEY-ED)
                      " GBP in total. The difference stays visible "
                      "until it is explained or corrected"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF
           IF FUNCTION ABS(CR-DIFFERENCE-GBP) > CR-MATERIALITY
               MOVE "Y" TO CR-MATERIAL
           END-IF.
       1000-EXIT.
           EXIT.

       1100-TAKE-ROW.
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-REC-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               GO TO 1100-EXIT
           END-IF
           COMPUTE CR-CONTROL-GBP =
               CR-CONTROL-GBP + FUNCTION NUMVAL(WS-R-CONTROL)
           COMPUTE CR-DETAIL-GBP =
               CR-DETAIL-GBP + FUNCTION NUMVAL(WS-R-DETAIL)
           COMPUTE CR-DIFFERENCE-GBP =
               CR-DIFFERENCE-GBP + FUNCTION NUMVAL(WS-R-DIFF)
           IF WS-R-STATE NOT = "RECONCILED"
               ADD 1 TO CR-DIFF-COUNT
           END-IF
           IF CR-ROW-CNT < 16
               ADD 1 TO CR-ROW-CNT
               MOVE CR-ROW-CNT TO WS-IDX
               MOVE WS-R-ACCOUNT TO CR-R-ACCOUNT(WS-IDX)
               MOVE WS-R-NAME TO CR-R-NAME(WS-IDX)
               MOVE WS-R-TYPE TO CR-R-TYPE(WS-IDX)
               COMPUTE CR-R-CONTROL(WS-IDX) =
                   FUNCTION NUMVAL(WS-R-CONTROL)
               COMPUTE CR-R-DETAIL(WS-IDX) =
                   FUNCTION NUMVAL(WS-R-DETAIL)
               COMPUTE CR-R-DIFF(WS-IDX) =
                   FUNCTION NUMVAL(WS-R-DIFF)
               MOVE WS-R-STATE TO CR-R-STATE(WS-IDX)
           END-IF
           IF CR-ACCOUNT NOT = SPACES AND WS-R-ACCOUNT = CR-ACCOUNT
               MOVE WS-R-OWNER TO CR-OWNER-USER
           END-IF.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000 - RECORD THE RUN AND OWN ANY DIFFERENCE
      *>----------------------------------------------------------------
       2000-RECORD.
           PERFORM 1000-RUN THRU 1000-EXIT
           IF CA-RETURN-CODE = 12
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-WRITE-RUN THRU 2100-EXIT
               VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > CR-ROW-CNT OR CA-RETURN-CODE = 12
           IF CR-DIFF-COUNT > 0
               PERFORM 2200-RAISE-EXCEPTION THRU 2200-EXIT
           END-IF.
       2000-EXIT.
           EXIT.

       2100-WRITE-RUN.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT rpt.ReconciliationRun (CompanyCode, "
                  "ControlType, AsOfDate, FiscalYear, PeriodNo, "
                  "AccountCode, ControlBalanceGBP, DetailBalanceGBP, "
                  "DifferenceGBP, OwnerUser, Status, Note, RunBy) "
                  "VALUES (?, ?, ?, NULLIF(CAST(? AS smallint),0), "
                  "NULLIF(CAST(? AS tinyint),0), ?, ?, ?, ?, "
                  "NULLIF(?,''), ?, NULLIF(?,''), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 13 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CR-R-TYPE(WS-IDX) TO SQLIO-PARM-VAL(2)
           MOVE CR-AS-AT-DATE TO SQLIO-PARM-VAL(3)
           MOVE CR-FISCAL-YEAR TO SQLIO-PARM-VAL(4)
           MOVE CR-PERIOD-NO TO SQLIO-PARM-VAL(5)
           MOVE CR-R-ACCOUNT(WS-IDX) TO SQLIO-PARM-VAL(6)
           MOVE CR-R-CONTROL(WS-IDX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE CR-R-DETAIL(WS-IDX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE CR-R-DIFF(WS-IDX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE CR-OWNER-USER TO SQLIO-PARM-VAL(10)
           IF CR-R-STATE(WS-IDX) = "RECONCILED"
               MOVE "CLEARED" TO SQLIO-PARM-VAL(11)
           ELSE
               MOVE "OPEN" TO SQLIO-PARM-VAL(11)
           END-IF
           MOVE CR-NOTE TO SQLIO-PARM-VAL(12)
           MOVE CA-USER TO SQLIO-PARM-VAL(13)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       2100-EXIT.
           EXIT.

       2200-RAISE-EXCEPTION.
           INITIALIZE WS-EXCPRAIS-CA
           MOVE "RAISE" TO XH-FUNCTION
           MOVE CA-COMPANY TO XH-COMPANY
           MOVE CA-USER TO XH-USER
           MOVE CA-BUSINESS-DATE TO XH-BUSINESS-DATE
           MOVE "FIN" TO EX-MODULE
           IF CR-MATERIAL = "Y"
               MOVE "HOLD" TO EX-SEVERITY
           ELSE
               MOVE "WARNING" TO EX-SEVERITY
           END-IF
           MOVE "CONTROL ACCOUNT DIFFERENCE" TO EX-CATEGORY
           MOVE CA-MESSAGE TO EX-MESSAGE-TEXT
           MOVE "CONTROLREC" TO EX-DOCUMENT-TYPE
           IF CR-ACCOUNT = SPACES
               MOVE CR-CONTROL-TYPE TO EX-DOCUMENT-REF
           ELSE
               MOVE CR-ACCOUNT TO EX-DOCUMENT-REF
           END-IF
           IF CR-OWNER-USER = SPACES
               MOVE "FINCTRL" TO EX-OWNER-ROLE
           ELSE
               MOVE CR-OWNER-USER TO EX-OWNER-USER
           END-IF
           MOVE 2 TO EX-PRIORITY
           MOVE CR-DIFFERENCE-GBP TO EX-AMOUNT-GBP
           CALL "EXCPRAIS" USING WS-EXCPRAIS-CA
           MOVE EX-EXCEPTION-ID TO CR-EXCEPTION-ID
           IF XH-RETURN-CODE > CA-RETURN-CODE
               MOVE XH-RETURN-CODE TO CA-RETURN-CODE
           END-IF.
       2200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000 - RECORD THAT A DIFFERENCE IS EXPLAINED OR CORRECTED.
      *>        NO BALANCING JOURNAL IS CREATED HERE (FIN-021).
      *>----------------------------------------------------------------
       3000-ACCEPT.
           IF CR-NOTE = SPACES OR CR-NOTE = LOW-VALUES
               MOVE "CTL0020" TO CA-REASON-CODE
               MOVE "An explanation is required before a control diffe"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           IF CR-RUN-ID = 0
               MOVE "CTL0021" TO CA-REASON-CODE
               MOVE "The reconciliation run to accept must be identifi"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE rpt.ReconciliationRun SET Status = "
                  "'EXPLAINED', ReviewedBy = ?, ReviewedAt = "
                  "SYSDATETIME(), Note = ? WHERE ReconRunId = ? "
                  "AND CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE CR-NOTE TO SQLIO-PARM-VAL(2)
           MOVE CR-RUN-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(4)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3000-EXIT
           END-IF
           IF SQLIO-ROWCOUNT = 0
               MOVE "CTL0022" TO CA-REASON-CODE
               MOVE "That reconciliation run is not held" TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 3000-EXIT
           END-IF
           MOVE "EXPLAINED" TO CR-STATE
           MOVE "The difference is recorded as explained, with its own"
               TO CA-MESSAGE.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-NOT-NULL.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-IDX)
           END-PERFORM.
       8000-EXIT.
           EXIT.

       9000-REJECT.
           MOVE "Y" TO WS-REJECTED
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE "CTL0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "CTLRECON does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in CTLRECON" TO CA-MESSAGE
           END-IF
           MOVE 12 TO CA-RETURN-CODE
           MOVE "Y" TO WS-REJECTED
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
