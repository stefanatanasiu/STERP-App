      *>================================================================
      *> PROGRAM-ID : PERCLOSE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER
      *> PURPOSE    : PERIOD CONTROLS AND THE CLOSE CHECKLIST.
      *>              OPERATIONAL READINESS, SOFT CLOSE AND HARD CLOSE
      *>              ARE DISTINCT STATES.  SOFT CLOSE STILL ADMITS AN
      *>              EXPLICITLY AUTHORISED CLOSE ADJUSTMENT; HARD
      *>              CLOSE BLOCKS POSTING ALTOGETHER.  AN ORDINARY
      *>              USER CANNOT REACH A CLOSED PERIOD BY CHANGING THE
      *>              DOCUMENT DATE (FIN-010).
      *>              HARD CLOSE REQUIRES THE BLOCKING CHECKLIST STEPS
      *>              TO BE COMPLETE: AN UNRESOLVED MATERIAL DIFFERENCE
      *>              BLOCKS AN UNQUALIFIED CLOSE UNLESS THE CONTROLLER
      *>              ACCEPTS IT EXPLICITLY, WHICH IS RECORDED AS
      *>              EVIDENCE RATHER THAN SILENTLY CLEARED (FIN-011).
      *>              REOPENING RECORDS PERIOD, REASON AND APPROVER.
      *> CALLED BY  : frmPeriodClose, YEAREND, .NET CLIENT
      *> INTERFACE  : CALL "PERCLOSE" USING WS-PERCLOSE-CA
      *>              COPYBOOK PERCLOSECA
      *> CALLS      : PERMCHK, AUDITLOG, PARMGET, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-11-08 APW CR0145 ORIGINAL - MICRO FOCUS COBOL/2
      *> 1998-07-21 RJH CR0890 SOFT CLOSE INTRODUCED FOR THE AUDIT
      *> 2026-10-05 STERP CHECKLIST, MATERIALITY AND SIGN-OFF
      *>================================================================
      *> REQ: FIN-010, FIN-011
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERCLOSE.
       AUTHOR. A P WHITTAKER.
       DATE-WRITTEN. 08/11/1994.
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
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARMGET-CA==
                      LEADING ==CA-== BY ==MH-==.
       01  WS-SWITCHES.
           05  WS-REJECTED             PIC X VALUE "N".
               88  WS-IS-REJECTED                VALUE "Y".
       01  WS-WORK.
           05  WS-IDX                  PIC S9(4) COMP-5 VALUE 0.
           05  WS-NEW-STATUS           PIC X(8).
           05  WS-PERM                 PIC X(40).
           05  WS-MONEY-ED             PIC -(11)9.99.
      *>   The standard close checklist.  Each entry is 79 bytes:
      *>   step code, blocking flag, owning role and the step name.
       01  WS-STD-STEPS.
           05  FILLER PIC X(19) VALUE "ARAP      YFINCTRL ".
           05  FILLER PIC X(60) VALUE
               "AR and AP detail reconciles to the control accounts".
           05  FILLER PIC X(19) VALUE "ACCRUALS  YFINCTRL ".
           05  FILLER PIC X(60) VALUE
               "Owned receipts, accruals and supplier returns reviewed".
           05  FILLER PIC X(19) VALUE "CUTOFF    YFINCTRL ".
           05  FILLER PIC X(60) VALUE
               "Despatched not invoiced and credited not returned seen".
           05  FILLER PIC X(19) VALUE "STOCK     YCOSTACC ".
           05  FILLER PIC X(60) VALUE
               "Inventory quantity, value and quarantine reconciled".
           05  FILLER PIC X(19) VALUE "WIP       YCOSTACC ".
           05  FILLER PIC X(60) VALUE
               "WIP balances, completions and variance disposal seen".
           05  FILLER PIC X(19) VALUE "BANK      YTREASURY".
           05  FILLER PIC X(60) VALUE
               "Bank reconciliation, FX valuation and transfers done".
           05  FILLER PIC X(19) VALUE "VAT       YVATOWNER".
           05  FILLER PIC X(60) VALUE
               "VAT control reconciles to the VAT records held".
           05  FILLER PIC X(19) VALUE "SCHEDULE  YFINCTRL ".
           05  FILLER PIC X(60) VALUE
               "Accrual and prepayment schedules and journals complete".
           05  FILLER PIC X(19) VALUE "STATUTORY NFINCTRL ".
           05  FILLER PIC X(60) VALUE
               "Statutory deadlines due in the next period reviewed".
           05  FILLER PIC X(19) VALUE "SIGNOFF   YFINCTRL ".
           05  FILLER PIC X(60) VALUE
               "Trial balance balances and the reviewers sign off".
       01  FILLER REDEFINES WS-STD-STEPS.
           05  WS-STD OCCURS 10 TIMES.
               10  WS-STD-CODE         PIC X(10).
               10  WS-STD-BLOCK        PIC X.
               10  WS-STD-OWNER        PIC X(8).
               10  WS-STD-NAME         PIC X(60).
      *>   8000-NOT-NULL uses WS-IDX, so the checklist build keeps an
      *>   index of its own or it would stop after the first step.
       01  WS-STEP-IDX                 PIC 9(4) VALUE 0.
       01  WS-HDR-ROW.
           05  WS-H-COMPANY            PIC X(10).
           05  WS-H-YEAR               PIC X(8).
           05  WS-H-PERIOD             PIC X(8).
           05  WS-H-NAME               PIC X(20).
           05  WS-H-START              PIC X(10).
           05  WS-H-END                PIC X(10).
           05  WS-H-STATUS             PIC X(8).
           05  WS-H-SOFT-AT            PIC X(30).
           05  WS-H-HARD-AT            PIC X(30).
           05  WS-H-CLOSEDBY           PIC X(20).
           05  WS-H-REOPEN-BY          PIC X(20).
           05  WS-H-REOPEN-WHY         PIC X(200).
           05  WS-H-STEPS              PIC X(10).
           05  WS-H-DONE               PIC X(10).
           05  WS-H-BLOCK              PIC X(10).
           05  WS-H-DIFF               PIC X(22).
           05  WS-H-READY              PIC X(10).
       01  WS-STEP-ROW.
           05  WS-T-ID                 PIC X(10).
           05  WS-T-CODE               PIC X(20).
           05  WS-T-NAME               PIC X(60).
           05  WS-T-OWNER              PIC X(20).
           05  WS-T-OWNER-USER         PIC X(20).
           05  WS-T-STATUS             PIC X(12).
           05  WS-T-BLOCK              PIC X(4).
           05  WS-T-DIFF               PIC X(22).
           05  WS-T-EVIDENCE           PIC X(60).
           05  WS-T-COMPLETED-BY       PIC X(20).
           05  WS-T-COMPLETED-AT       PIC X(30).
           05  WS-T-REVIEWED-BY        PIC X(20).
           05  WS-T-REVIEWED-AT        PIC X(30).
           05  WS-T-NOTE               PIC X(300).
       LINKAGE SECTION.
       COPY PERCLOSECA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           PERFORM 0200-EDIT-REQUEST THRU 0200-EXIT
           IF WS-IS-REJECTED
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "STATUS"   PERFORM 1000-STATUS THRU 1000-EXIT
               WHEN "BUILD"    PERFORM 2000-BUILD THRU 2000-EXIT
               WHEN "STEP"     PERFORM 3000-STEP THRU 3000-EXIT
               WHEN "SOFT"     PERFORM 4000-CLOSE-PERIOD THRU 4000-EXIT
               WHEN "HARD"     PERFORM 4000-CLOSE-PERIOD THRU 4000-EXIT
               WHEN "REOPEN"   PERFORM 5000-REOPEN THRU 5000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.

       0100-INITIALISE.
           MOVE "N" TO WS-REJECTED
           MOVE "PERCLOSE" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "FIN-010" TO CA-REQ-REF
           MOVE 0 TO CL-STEP-CNT
           MOVE 0 TO CL-DONE-CNT
           MOVE 0 TO CL-BLOCKING-OPEN
           MOVE 0 TO CL-UNRESOLVED-DIFF
           MOVE 0 TO CL-STEPS-LISTED
           MOVE SPACES TO CL-READINESS
           MOVE SPACES TO CL-PERIOD-STATUS
           MOVE "N" TO CL-SIGNED-OFF
           MOVE 1000 TO CL-MATERIALITY
           PERFORM 0150-MATERIALITY THRU 0150-EXIT.
       0100-EXIT.
           EXIT.

       0150-MATERIALITY.
           INITIALIZE WS-PARMGET-CA
           MOVE "GETNUM" TO MH-FUNCTION
           MOVE CA-COMPANY TO MH-COMPANY
           MOVE CA-USER TO MH-USER
           MOVE CA-BUSINESS-DATE TO MH-BUSINESS-DATE
           MOVE "FIN.CLOSE.MATERIALITY" TO PM-PARAM-CODE
           MOVE "N" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARMGET-CA
           IF PM-FOUND = "Y" AND PM-VALUE-NUM > 0
               MOVE PM-VALUE-NUM TO CL-MATERIALITY
           END-IF.
       0150-EXIT.
           EXIT.

       0200-EDIT-REQUEST.
           IF CA-COMPANY = SPACES
               MOVE "CLS0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF CL-FISCAL-YEAR = 0 OR CL-PERIOD-NO = 0
               MOVE "CLS0002" TO CA-REASON-CODE
               MOVE "A fiscal year and period number are required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000 - PERIOD STATE AND CHECKLIST READINESS
      *>----------------------------------------------------------------
       1000-STATUS.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC fin.usp_CloseStatus ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CL-FISCAL-YEAR TO SQLIO-PARM-VAL(2)
           MOVE CL-PERIOD-NO TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
      *>   The seventeen columns of the first result set of
      *>   fin.usp_CloseStatus, in the order the procedure returns
      *>   them.  Dates arrive as ISO text, so NUMVAL yields yyyymmdd.
           MOVE 17 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 8 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 10 TO SQLIO-COL-WIDTH(5)
           MOVE 10 TO SQLIO-COL-WIDTH(6)
           MOVE 8 TO SQLIO-COL-WIDTH(7)
           MOVE 30 TO SQLIO-COL-WIDTH(8)
           MOVE 30 TO SQLIO-COL-WIDTH(9)
           MOVE 20 TO SQLIO-COL-WIDTH(10)
           MOVE 20 TO SQLIO-COL-WIDTH(11)
           MOVE 200 TO SQLIO-COL-WIDTH(12)
           MOVE 10 TO SQLIO-COL-WIDTH(13)
           MOVE 10 TO SQLIO-COL-WIDTH(14)
           MOVE 10 TO SQLIO-COL-WIDTH(15)
           MOVE 22 TO SQLIO-COL-WIDTH(16)
           MOVE 10 TO SQLIO-COL-WIDTH(17)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-HDR-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1000-EXIT
           END-IF
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-HDR-ROW
           IF SQLIO-NOTFOUND
               MOVE "CLS0003" TO CA-REASON-CODE
               MOVE "That accounting period is not held for the compan"
                   TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               MOVE "CLOS" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-HDR-ROW
               GO TO 1000-EXIT
           END-IF
           MOVE WS-H-NAME TO CL-PERIOD-NAME
           MOVE WS-H-STATUS TO CL-PERIOD-STATUS
           COMPUTE CL-START-DATE = FUNCTION NUMVAL(WS-H-START)
           COMPUTE CL-END-DATE = FUNCTION NUMVAL(WS-H-END)
           COMPUTE CL-STEP-CNT = FUNCTION NUMVAL(WS-H-STEPS)
           COMPUTE CL-DONE-CNT = FUNCTION NUMVAL(WS-H-DONE)
           COMPUTE CL-BLOCKING-OPEN = FUNCTION NUMVAL(WS-H-BLOCK)
           COMPUTE CL-UNRESOLVED-DIFF = FUNCTION NUMVAL(WS-H-DIFF)
           MOVE WS-H-READY TO CL-READINESS
           MOVE WS-H-CLOSEDBY TO CL-CLOSED-BY
           IF CL-STEP-CNT > 0 AND CL-DONE-CNT = CL-STEP-CNT
               MOVE "Y" TO CL-SIGNED-OFF
           END-IF
      *>   Second result set: the steps themselves.
           MOVE "MORE" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-STEP-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1000-EXIT
           END-IF
      *>   The fourteen columns of the second result set.
           MOVE 14 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 60 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 12 TO SQLIO-COL-WIDTH(6)
           MOVE 4 TO SQLIO-COL-WIDTH(7)
           MOVE 22 TO SQLIO-COL-WIDTH(8)
           MOVE 60 TO SQLIO-COL-WIDTH(9)
           MOVE 20 TO SQLIO-COL-WIDTH(10)
           MOVE 30 TO SQLIO-COL-WIDTH(11)
           MOVE 20 TO SQLIO-COL-WIDTH(12)
           MOVE 30 TO SQLIO-COL-WIDTH(13)
           MOVE 300 TO SQLIO-COL-WIDTH(14)
           PERFORM 1100-FETCH-STEP THRU 1100-EXIT
               UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
                  OR CL-STEPS-LISTED NOT < 20
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-STEP-ROW
           MOVE SPACES TO CA-MESSAGE
           IF CL-BLOCKING-OPEN > 0
               STRING "Period " FUNCTION TRIM(CL-PERIOD-NAME)
                      " is not ready to close: blocking checklist "
                      "steps remain open"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               STRING "Period " FUNCTION TRIM(CL-PERIOD-NAME)
                      " is " FUNCTION TRIM(CL-PERIOD-STATUS)
                      " and the checklist is complete"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       1000-EXIT.
           EXIT.

       1100-FETCH-STEP.
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-STEP-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               GO TO 1100-EXIT
           END-IF
           ADD 1 TO CL-STEPS-LISTED
           MOVE CL-STEPS-LISTED TO WS-IDX
           MOVE WS-T-CODE TO CL-S-CODE(WS-IDX)
           MOVE WS-T-NAME TO CL-S-NAME(WS-IDX)
           MOVE WS-T-STATUS TO CL-S-STATUS(WS-IDX)
           MOVE WS-T-OWNER TO CL-S-OWNER(WS-IDX)
           MOVE WS-T-EVIDENCE TO CL-S-EVIDENCE(WS-IDX)
           COMPUTE CL-S-DIFF(WS-IDX) = FUNCTION NUMVAL(WS-T-DIFF)
           IF WS-T-BLOCK(1:1) = "1"
               MOVE "Y" TO CL-S-BLOCKING(WS-IDX)
           ELSE
               MOVE "N" TO CL-S-BLOCKING(WS-IDX)
           END-IF.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000 - BUILD THE STANDARD CHECKLIST FOR A PERIOD
      *>----------------------------------------------------------------
       2000-BUILD.
           MOVE "FIN.PERIOD.CLOSE" TO WS-PERM
           PERFORM 8100-AUTHORITY THRU 8100-EXIT
           IF WS-IS-REJECTED
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-ADD-STEP THRU 2100-EXIT
               VARYING WS-STEP-IDX FROM 1 BY 1 UNTIL WS-STEP-IDX > 10
           PERFORM 1000-STATUS THRU 1000-EXIT
           MOVE SPACES TO CA-MESSAGE
           STRING "Close checklist prepared for "
                  FUNCTION TRIM(CL-PERIOD-NAME)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

       2100-ADD-STEP.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.CloseChecklist (CompanyCode, FiscalYear,"
                  " PeriodNo, StepCode, StepName, OwnerRole, "
                  "IsBlocking, Status) SELECT ?, ?, ?, ?, ?, ?, ?, "
                  "'OPEN' WHERE NOT EXISTS (SELECT 1 FROM "
                  "fin.CloseChecklist c WHERE c.CompanyCode = ? "
                  "AND c.FiscalYear = ? AND c.PeriodNo = ? "
                  "AND c.StepCode = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 11 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CL-FISCAL-YEAR TO SQLIO-PARM-VAL(2)
           MOVE CL-PERIOD-NO TO SQLIO-PARM-VAL(3)
           MOVE WS-STD-CODE(WS-STEP-IDX) TO SQLIO-PARM-VAL(4)
           MOVE WS-STD-NAME(WS-STEP-IDX) TO SQLIO-PARM-VAL(5)
           MOVE WS-STD-OWNER(WS-STEP-IDX) TO SQLIO-PARM-VAL(6)
           IF WS-STD-BLOCK(WS-STEP-IDX) = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(7)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(7)
           END-IF
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(8)
           MOVE CL-FISCAL-YEAR TO SQLIO-PARM-VAL(9)
           MOVE CL-PERIOD-NO TO SQLIO-PARM-VAL(10)
           MOVE WS-STD-CODE(WS-STEP-IDX) TO SQLIO-PARM-VAL(11)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-HDR-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000 - COMPLETE OR ACCEPT ONE CHECKLIST STEP
      *>        ACCEPTING A DIFFERENCE IS A CONTROLLER DECISION AND IS
      *>        RECORDED AS SUCH - IT IS NOT A SILENT TIDY UP.
      *>----------------------------------------------------------------
       3000-STEP.
           MOVE "FIN.PERIOD.CLOSE" TO WS-PERM
           PERFORM 8100-AUTHORITY THRU 8100-EXIT
           IF WS-IS-REJECTED
               GO TO 3000-EXIT
           END-IF
           IF CL-STEP-CODE = SPACES
               MOVE "CLS0010" TO CA-REASON-CODE
               MOVE "A checklist step code is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           IF CL-STEP-ACTION = SPACES
               MOVE "DONE" TO CL-STEP-ACTION
           END-IF
           IF CL-STEP-ACTION = "DONE" AND CL-EVIDENCE-REF = SPACES
               MOVE "CLS0011" TO CA-REASON-CODE
               MOVE "Each completed close step must reference its evid"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           IF CL-STEP-ACTION = "ACCEPT" AND CL-REASON = SPACES
               MOVE "CLS0012" TO CA-REASON-CODE
               MOVE "Accepting an unresolved difference needs a permit"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           IF CL-STEP-ACTION = "DONE"
             AND FUNCTION ABS(CL-DIFFERENCE) > CL-MATERIALITY
               MOVE "CLS0013" TO CA-REASON-CODE
               MOVE CL-DIFFERENCE TO WS-MONEY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "That step carries an unresolved difference of "
                      FUNCTION TRIM(WS-MONEY-ED)
                      " GBP, which is material. It must be explained "
                      "or formally accepted by the controller"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 6 TO CA-RETURN-CODE
               MOVE "Y" TO WS-REJECTED
               ADD 1 TO CA-MSG-COUNT
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.CloseChecklist SET Status = ?, "
                  "EvidenceRef = NULLIF(?,''), DifferenceGBP = ?, "
                  "Note = NULLIF(?,''), CompletedBy = ?, CompletedAt ="
                  " SYSDATETIME(), ReviewedBy = NULLIF(?,''), "
                  "ReviewedAt = CASE WHEN ? <> '' THEN SYSDATETIME() "
                  "END WHERE CompanyCode = ? AND FiscalYear = ? "
                  "AND PeriodNo = ? AND StepCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 11 TO SQLIO-PARM-CNT
           IF CL-STEP-ACTION = "ACCEPT"
               MOVE "ACCEPTED" TO SQLIO-PARM-VAL(1)
           ELSE
               IF CL-STEP-ACTION = "BLOCK"
                   MOVE "BLOCKED" TO SQLIO-PARM-VAL(1)
               ELSE
                   MOVE "DONE" TO SQLIO-PARM-VAL(1)
               END-IF
           END-IF
           MOVE CL-EVIDENCE-REF TO SQLIO-PARM-VAL(2)
           MOVE CL-DIFFERENCE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE CL-REASON TO SQLIO-PARM-VAL(4)
           MOVE CA-USER TO SQLIO-PARM-VAL(5)
           MOVE CL-REVIEWER TO SQLIO-PARM-VAL(6)
           MOVE CL-REVIEWER TO SQLIO-PARM-VAL(7)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(8)
           MOVE CL-FISCAL-YEAR TO SQLIO-PARM-VAL(9)
           MOVE CL-PERIOD-NO TO SQLIO-PARM-VAL(10)
           MOVE CL-STEP-CODE TO SQLIO-PARM-VAL(11)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-HDR-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3000-EXIT
           END-IF
           IF SQLIO-ROWCOUNT = 0
               MOVE "CLS0014" TO CA-REASON-CODE
               MOVE "That checklist step is not held for the period"
                   TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 3000-EXIT
           END-IF
           PERFORM 8200-AUDIT THRU 8200-EXIT
           PERFORM 1000-STATUS THRU 1000-EXIT.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000 - SOFT AND HARD CLOSE
      *>----------------------------------------------------------------
       4000-CLOSE-PERIOD.
           MOVE "FIN.PERIOD.CLOSE" TO WS-PERM
           PERFORM 8100-AUTHORITY THRU 8100-EXIT
           IF WS-IS-REJECTED
               GO TO 4000-EXIT
           END-IF
           PERFORM 1000-STATUS THRU 1000-EXIT
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           IF CA-FUNCTION = "SOFT"
               MOVE "SOFT" TO WS-NEW-STATUS
           ELSE
               MOVE "HARD" TO WS-NEW-STATUS
           END-IF
           IF CL-PERIOD-STATUS = "HARD"
               MOVE "CLS0020" TO CA-REASON-CODE
               MOVE "That period is already hard closed" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF WS-NEW-STATUS = "HARD" AND CL-BLOCKING-OPEN > 0
             AND CL-FORCE NOT = "Y"
               MOVE "CLS0021" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               MOVE CL-UNRESOLVED-DIFF TO WS-MONEY-ED
               STRING "Hard close is blocked: "
                      "blocking checklist steps are still open and "
                      FUNCTION TRIM(WS-MONEY-ED)
                      " GBP of difference is unresolved"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 6 TO CA-RETURN-CODE
               MOVE "Y" TO WS-REJECTED
               ADD 1 TO CA-MSG-COUNT
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.Period SET Status = ?, SoftCloseAt = "
                  "CASE WHEN ? = 'SOFT' THEN SYSDATETIME() ELSE "
                  "SoftCloseAt END, HardCloseAt = CASE WHEN ? = 'HARD'"
                  " THEN SYSDATETIME() ELSE HardCloseAt END, "
                  "ClosedBy = ? WHERE CompanyCode = ? "
                  "AND FiscalYear = ? AND PeriodNo = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE WS-NEW-STATUS TO SQLIO-PARM-VAL(1)
           MOVE WS-NEW-STATUS TO SQLIO-PARM-VAL(2)
           MOVE WS-NEW-STATUS TO SQLIO-PARM-VAL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(5)
           MOVE CL-FISCAL-YEAR TO SQLIO-PARM-VAL(6)
           MOVE CL-PERIOD-NO TO SQLIO-PARM-VAL(7)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-HDR-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 4000-EXIT
           END-IF
           MOVE WS-NEW-STATUS TO CL-PERIOD-STATUS
           MOVE CA-USER TO CL-CLOSED-BY
           PERFORM 8200-AUDIT THRU 8200-EXIT
           MOVE SPACES TO CA-MESSAGE
           IF WS-NEW-STATUS = "SOFT"
               STRING "Period " FUNCTION TRIM(CL-PERIOD-NAME)
                      " is soft closed. Only authorised close "
                      "adjustments may now be posted into it"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               STRING "Period " FUNCTION TRIM(CL-PERIOD-NAME)
                      " is hard closed. Posting into it is blocked"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           IF CL-FORCE = "Y" AND CL-BLOCKING-OPEN > 0
               MOVE 4 TO CA-RETURN-CODE
               MOVE "CLS0022" TO CA-REASON-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000 - REOPEN.  PERIOD, REASON AND APPROVER ARE ALL RECORDED,
      *>        AND THE REPORTS TO REISSUE ARE IDENTIFIED (FIN-010).
      *>----------------------------------------------------------------
       5000-REOPEN.
           MOVE "FIN.PERIOD.REOPEN" TO WS-PERM
           PERFORM 8100-AUTHORITY THRU 8100-EXIT
           IF WS-IS-REJECTED
               GO TO 5000-EXIT
           END-IF
           IF CL-REASON = SPACES OR CL-REASON = LOW-VALUES
               MOVE "CLS0030" TO CA-REASON-CODE
               MOVE "Reopening a closed period requires a recorded rea"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           IF CL-REVIEWER = SPACES
               MOVE "CLS0031" TO CA-REASON-CODE
               MOVE "Reopening requires a named approver" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.Period SET Status = 'OPEN', ReopenedBy ="
                  " ?, ReopenedAt = SYSDATETIME(), ReopenReason = ? "
                  "WHERE CompanyCode = ? AND FiscalYear = ? "
                  "AND PeriodNo = ? AND Status IN ('SOFT','HARD')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CL-REVIEWER TO SQLIO-PARM-VAL(1)
           MOVE CL-REASON TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE CL-FISCAL-YEAR TO SQLIO-PARM-VAL(4)
           MOVE CL-PERIOD-NO TO SQLIO-PARM-VAL(5)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-HDR-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 5000-EXIT
           END-IF
           IF SQLIO-ROWCOUNT = 0
               MOVE "CLS0032" TO CA-REASON-CODE
               MOVE "That period is not closed, so there is nothing to"
                   TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 5000-EXIT
           END-IF
           MOVE "OPEN" TO CL-PERIOD-STATUS
           PERFORM 8200-AUDIT THRU 8200-EXIT
           MOVE 4 TO CA-RETURN-CODE
           MOVE "CLS0033" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Period reopened under the approved calendar policy."
                  " The trial balance, statements and any VAT return "
                  "covering it must be reissued"
               DELIMITED BY SIZE INTO CA-MESSAGE
           ADD 1 TO CA-MSG-COUNT.
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
           MOVE "fin.Period" TO AU-ENTITY-TYPE
           MOVE SPACES TO AU-ENTITY-KEY
           STRING FUNCTION TRIM(CA-COMPANY) "/" CL-FISCAL-YEAR "/"
                  CL-PERIOD-NO
               DELIMITED BY SIZE INTO AU-ENTITY-KEY
           MOVE CA-FUNCTION TO AU-ACTION
           MOVE CL-PERIOD-STATUS TO AU-NEW-VALUE
           MOVE CL-REASON TO AU-REASON
           MOVE CL-EVIDENCE-REF(1:60) TO AU-AUTHORITY-REF
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
           MOVE "CLS0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "PERCLOSE does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in PERCLOSE" TO CA-MESSAGE
           END-IF
           MOVE 12 TO CA-RETURN-CODE
           MOVE "Y" TO WS-REJECTED
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
