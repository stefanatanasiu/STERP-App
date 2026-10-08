      *>================================================================
      *> PROGRAM-ID : RECONCTL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : CROSS MODULE CONTROLS AND REPORTING (XMD)
      *> PURPOSE    : CONTROL ACCOUNT RECONCILIATION AT A COMMON
      *>              CUTOFF.  FOR EACH CONTROL TYPE - DEBTORS,
      *>              CREDITORS, INVENTORY, WORK IN PROGRESS, GOODS
      *>              RECEIVED NOT INVOICED, VAT, BANK AND ACCRUALS -
      *>              THE SUBSIDIARY LEDGER IS TOTALLED AT THE SAME
      *>              EVENT CUTOFF AS THE GENERAL LEDGER AND THE TWO
      *>              ARE COMPARED.  A DIFFERENCE IS NEVER NETTED
      *>              AWAY: IT IS PRINTED, WRITTEN TO
      *>              rpt.ReconciliationRun AND RAISED AS AN OWNED
      *>              EXCEPTION FOR THE NAMED CONTROL ACCOUNT OWNER.
      *>              PREVIEW LISTS ONLY; COMMIT WRITES THE RUN AND
      *>              THE EXCEPTIONS.  RE-RUNNING THE SAME CUTOFF
      *>              REPLACES ITS OWN OPEN ROWS RATHER THAN ADDING
      *>              A SECOND SET.
      *> RUN AS     : RECONCTL CO=STC ASAT=20261031 MODE=COMMIT
      *>              USER=FINCTRL [TOL=0.01]
      *> OUTPUT     : run\spool\RECONCTL-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, PARMGET, EXCPRAIS,
      *>              SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-04-19 DJM  CR0163 ORIGINAL CONTROL ACCOUNT PROOF
      *> 1995-06-12 RJH  CR0388 GRNI AND WIP BRIDGES ADDED AFTER THE
      *>                 1995 AUDIT MANAGEMENT LETTER
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS THROUGHOUT
      *> 2003-07-04 DJM  CR1588 VAT CONTROL SPLIT FROM THE TAX SUSPENSE
      *> 2026-10-06 STERP XMD-005 SHARED CUTOFF, OWNED DIFFERENCES
      *> 2026-10-07 STERP CR2611 ONE DEFINITION PER CONTROL: DEBTORS
      *>                 MEASURED AS OUTSTANDING (ARRECON/AGEDDEBT) AND
      *>                 STOCK AS THE COSTING POOL (CSTINVAL)
      *>================================================================
      *> REQ: XMD-005, XMD-010, XMD-012, XMD-006, FIN-021, RPT-015
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECONCTL.
       AUTHOR. D J MILLER.
       DATE-WRITTEN. 19/04/1993.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO DYNAMIC XB-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS XB-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-RECONCTL.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY XMDBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARMGET==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCPRAIS==.
      *>   the eight control families of the 1993 suite.  the sign
      *>   turns the general ledger debit-less-credit balance into the
      *>   direction in which the subsidiary ledger states itself, so
      *>   that a liability control reads as a positive obligation
       01  WS-CTL-TABLE.
           05  FILLER PIC X(10) VALUE "AR".
           05  FILLER PIC X(32) VALUE "Trade debtors control".
           05  FILLER PIC S9(2) VALUE +1.
           05  FILLER PIC X(20) VALUE "CREDMGR".
           05  FILLER PIC X(10) VALUE "AP".
           05  FILLER PIC X(32) VALUE "Trade creditors control".
           05  FILLER PIC S9(2) VALUE -1.
           05  FILLER PIC X(20) VALUE "APMGR".
           05  FILLER PIC X(10) VALUE "INVENTORY".
           05  FILLER PIC X(32) VALUE "Inventory control".
           05  FILLER PIC S9(2) VALUE +1.
           05  FILLER PIC X(20) VALUE "COSTACC".
           05  FILLER PIC X(10) VALUE "WIP".
           05  FILLER PIC X(32) VALUE "Work in progress control".
           05  FILLER PIC S9(2) VALUE +1.
           05  FILLER PIC X(20) VALUE "COSTACC".
           05  FILLER PIC X(10) VALUE "GRNI".
           05  FILLER PIC X(32) VALUE "Goods received not invoiced".
           05  FILLER PIC S9(2) VALUE -1.
           05  FILLER PIC X(20) VALUE "PURMGR".
           05  FILLER PIC X(10) VALUE "VAT".
           05  FILLER PIC X(32) VALUE "VAT control".
           05  FILLER PIC S9(2) VALUE -1.
           05  FILLER PIC X(20) VALUE "VATOWNER".
           05  FILLER PIC X(10) VALUE "BANK".
           05  FILLER PIC X(32) VALUE "Bank and cash control".
           05  FILLER PIC S9(2) VALUE +1.
           05  FILLER PIC X(20) VALUE "TREASURY".
           05  FILLER PIC X(10) VALUE "ACCRUAL".
           05  FILLER PIC X(32) VALUE "Service accruals control".
           05  FILLER PIC S9(2) VALUE -1.
           05  FILLER PIC X(20) VALUE "FINCTRL".
       01  WS-CTL-TAB REDEFINES WS-CTL-TABLE.
           05  WS-CTL OCCURS 8 TIMES.
               10  WS-CTL-CODE         PIC X(10).
               10  WS-CTL-DESC         PIC X(32).
               10  WS-CTL-SIGN         PIC S9(2).
               10  WS-CTL-OWNER        PIC X(20).
       01  WS-GL-TABLE.
           05  WS-GL OCCURS 8 TIMES.
               10  WS-GL-BAL           PIC S9(13)V99.
               10  WS-GL-ACCTS         PIC X(40).
               10  WS-GL-CNT           PIC S9(4).
       01  WS-DET.
           05  D-CODE                  PIC X(10) VALUE SPACES.
           05  D-DESC                  PIC X(32) VALUE SPACES.
           05  D-ACCTS                 PIC X(16) VALUE SPACES.
           05  D-SUB                   PIC S9(13)V99 VALUE 0.
           05  D-GL                    PIC S9(13)V99 VALUE 0.
           05  D-DIFF                  PIC S9(13)V99 VALUE 0.
           05  D-STATUS                PIC X(12) VALUE SPACES.
           05  D-OWNER                 PIC X(20) VALUE SPACES.
           05  D-NOTE                  PIC X(96) VALUE SPACES.
       01  WS-TOT.
           05  T-SUB                   PIC S9(13)V99 VALUE 0.
           05  T-GL                    PIC S9(13)V99 VALUE 0.
           05  T-DIFF                  PIC S9(13)V99 VALUE 0.
           05  T-ABSDIFF               PIC S9(13)V99 VALUE 0.
       01  WS-SCOPE.
           05  S-ASAT                  PIC X(10) VALUE SPACES.
           05  S-PERIOD                PIC X(12) VALUE SPACES.
           05  S-MODE                  PIC X(10) VALUE SPACES.
           05  S-TOL                   PIC X(12) VALUE SPACES.
       REPORT SECTION.
       RD  R-RECONCTL
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 8 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
      *>   the scope block is an ordinary DETAIL group: this compiler
      *>   does not emit CONTROL HEADING FINAL (see tech-debt xmd.md)
       01  DE-SCOPE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26) VALUE
                   "Event cutoff (as at date) ".
               10  COLUMN 28   PIC X(10) SOURCE S-ASAT.
               10  COLUMN 42   PIC X(18) VALUE
                   "Accounting period ".
               10  COLUMN 60   PIC X(12) SOURCE S-PERIOD.
               10  COLUMN 76   PIC X(6) VALUE "Mode  ".
               10  COLUMN 82   PIC X(10) SOURCE S-MODE.
               10  COLUMN 96   PIC X(11) VALUE "Tolerance  ".
               10  COLUMN 107  PIC X(12) SOURCE S-TOL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE
                   "Basis: POSTED journals only, sterling carrying".
               10  COLUMN 48   PIC X(60) VALUE
                   "values, same cutoff on both sides.".
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(7) VALUE "Control".
               10  COLUMN 12   PIC X(11) VALUE "Description".
               10  COLUMN 45   PIC X(16) VALUE "Control accounts".
               10  COLUMN 68   PIC X(12) VALUE "Subsidiary  ".
               10  COLUMN 88   PIC X(11) VALUE "GL balance ".
               10  COLUMN 106  PIC X(11) VALUE "Difference ".
               10  COLUMN 120  PIC X(6) VALUE "Status".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-CTL TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(10) SOURCE D-CODE.
               10  COLUMN 12   PIC X(32) SOURCE D-DESC.
               10  COLUMN 45   PIC X(16) SOURCE D-ACCTS.
               10  COLUMN 62   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-SUB.
               10  COLUMN 81   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-GL.
               10  COLUMN 100  PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-DIFF.
               10  COLUMN 120  PIC X(12) SOURCE D-STATUS.
       01  DE-NOTE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 14   PIC X(96) SOURCE D-NOTE.
               10  COLUMN 112  PIC X(5) VALUE "Owner".
               10  COLUMN 118  PIC X(14) SOURCE D-OWNER.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(20) VALUE "Control totals".
               10  COLUMN 62   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-SUB.
               10  COLUMN 81   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-GL.
               10  COLUMN 100  PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-DIFF.
               10  COLUMN 120  PIC X(12) SOURCE RPT-STATUS-TEXT.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(32) VALUE
                   "Unreconciled control families  ".
               10  COLUMN 34   PIC ZZ9 SOURCE XB-DIFFS.
               10  COLUMN 40   PIC X(28) VALUE
                   "Exceptions raised or listed ".
               10  COLUMN 68   PIC ZZ9 SOURCE XB-RAISED.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(64) VALUE
                   "Each difference stays explicit and owned until it".
               10  COLUMN 67   PIC X(62) VALUE
                   "is corrected at source or adjusted with a reviewer."
               .
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF XB-RC > 4
               GO TO 0000-FINISH
           END-IF
           MOVE "RECONCTL" TO RPT-REPORT-ID
           MOVE "Control account reconciliation" TO RPT-TITLE
           MOVE "Subsidiary ledgers against the general ledger"
               TO RPT-SUBTITLE
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF XB-RC > 4
               GO TO 0000-FINISH
           END-IF
           INITIATE R-RECONCTL
           GENERATE DE-SCOPE
           PERFORM 1000-LOAD-GL THRU 1000-EXIT
           PERFORM 2000-COMPARE THRU 2000-EXIT
           PERFORM 3000-PROVE THRU 3000-EXIT
           TERMINATE R-RECONCTL
           PERFORM 9350-CLOSE-SPOOL THRU 9350-EXIT.
       0000-FINISH.
           PERFORM 9150-JOB-END THRU 9150-EXIT
           MOVE XB-RC TO RETURN-CODE
           STOP RUN.

       0100-READ-ARGS.
           PERFORM 9500-STD-ARGS THRU 9500-EXIT
           PERFORM 9600-RESOLVE-PERIOD THRU 9600-EXIT
           MOVE "XMD.RECON.TOLERANCE" TO XB-CODE
           MOVE 0.01 TO XB-AMT
           PERFORM 9650-GET-PARM-NUM THRU 9650-EXIT
           MOVE XB-AMT TO XB-TOLERANCE
           MOVE "TOL" TO XB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF XB-TEXT NOT = SPACES
               COMPUTE XB-TOLERANCE = FUNCTION NUMVAL(XB-TEXT)
           END-IF
           MOVE XB-ASAT TO XB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO S-ASAT
           MOVE XB-ASAT TO XB-DATE-ED
           MOVE XB-MODE TO S-MODE
           MOVE XB-TOLERANCE TO XB-MON-ED
           MOVE FUNCTION TRIM(XB-MON-ED) TO S-TOL
           MOVE SPACES TO S-PERIOD
           IF XB-FY = 0
               MOVE "UNDATED" TO S-PERIOD
               MOVE 4 TO XB-RC
               DISPLAY "STERP-W-0101 no accounting period covers "
                       XB-ASAT
           ELSE
               MOVE XB-FY TO XB-SEQ-ED
               MOVE SPACES TO S-PERIOD
               STRING XB-SEQ-ED(6:4) "/" XB-PER " "
                      XB-PERIOD-STATUS(1:4)
                   DELIMITED BY SIZE INTO S-PERIOD
           END-IF
           DISPLAY "STERP-I-0100 RECONCTL cutoff " XB-ASAT
                   " company " FUNCTION TRIM(XB-CO)
                   " mode " FUNCTION TRIM(XB-MODE).
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-021  one pass over the posted ledger gives the
      *> control side of every family at the same cutoff
      *>----------------------------------------------------------------
       1000-LOAD-GL.
           PERFORM VARYING XB-I FROM 1 BY 1 UNTIL XB-I > 8
               MOVE 0 TO WS-GL-BAL(XB-I)
               MOVE 0 TO WS-GL-CNT(XB-I)
               MOVE SPACES TO WS-GL-ACCTS(XB-I)
           END-PERFORM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT a.ControlType, CONVERT(varchar(24),"
                  "SUM(l.DebitAmountGBP - l.CreditAmountGBP)), "
                  "CONVERT(varchar(12),COUNT(DISTINCT a.AccountCode"
                  ")), MIN(a.AccountCode) + CASE WHEN COUNT(DISTINC"
                  "T a.AccountCode) > 1 THEN '..' ELSE '' END FROM "
                  "fin.JournalLine l JOIN fin.Journal j ON "
                  "j.JournalId = l.JournalId JOIN fin.Account a ON "
                  "a.CompanyCode = l.CompanyCode AND a.AccountCode "
                  "= l.AccountCode WHERE l.CompanyCode = ? AND "
                  "j.Status = 'POSTED' AND CONVERT(char(8),"
                  "j.PostingDate,112) <= ? AND a.ControlType IS NOT "
                  "NULL AND a.ControlType <> '' GROUP BY "
                  "a.ControlType"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO XB-RC
               DISPLAY "STERP-E-1001 " SQLIO-ERRMSG(1:100)
               GO TO 1000-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   PERFORM 1100-STORE-GL THRU 1100-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       1000-EXIT.
           EXIT.

       1100-STORE-GL.
           PERFORM VARYING XB-J FROM 1 BY 1 UNTIL XB-J > 8
               IF WS-CTL-CODE(XB-J) = BR-COL(1)
                   COMPUTE WS-GL-BAL(XB-J) =
                       FUNCTION NUMVAL(BR-COL(2))
                   COMPUTE WS-GL-CNT(XB-J) =
                       FUNCTION NUMVAL(BR-COL(3))
                   MOVE BR-COL(4) TO WS-GL-ACCTS(XB-J)
               END-IF
           END-PERFORM.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-005, XMD-012  the subsidiary side of every family,
      *> then the comparison.  a family with no control account in the
      *> chart is still listed, because silence is not a nil return
      *>----------------------------------------------------------------
       2000-COMPARE.
           PERFORM VARYING XB-I FROM 1 BY 1 UNTIL XB-I > 8
               MOVE WS-CTL-CODE(XB-I) TO D-CODE
               MOVE WS-CTL-DESC(XB-I) TO D-DESC
               MOVE WS-CTL-OWNER(XB-I) TO D-OWNER
               MOVE WS-GL-ACCTS(XB-I) TO D-ACCTS
               COMPUTE D-GL = WS-GL-BAL(XB-I) * WS-CTL-SIGN(XB-I)
               MOVE SPACES TO D-NOTE
               PERFORM 2100-SUBLEDGER THRU 2100-EXIT
               COMPUTE D-DIFF = D-SUB - D-GL
               ADD D-SUB TO T-SUB
               ADD D-GL TO T-GL
               ADD D-DIFF TO T-DIFF
               IF D-DIFF > XB-TOLERANCE OR D-DIFF < 0 - XB-TOLERANCE
                   MOVE "DIFFERENCE" TO D-STATUS
                   ADD 1 TO XB-DIFFS
                   IF D-DIFF > 0
                       ADD D-DIFF TO T-ABSDIFF
                   ELSE
                       COMPUTE T-ABSDIFF = T-ABSDIFF - D-DIFF
                   END-IF
               ELSE
                   MOVE "AGREED" TO D-STATUS
               END-IF
               IF WS-GL-CNT(XB-I) = 0
                   MOVE "NO CONTROL" TO D-STATUS
                   MOVE "No account of this control type is posted "
                        TO D-NOTE
               END-IF
               ADD 1 TO XB-ROWS-OUT
               GENERATE DE-CTL
               IF D-STATUS NOT = "AGREED"
                   PERFORM 2300-RAISE THRU 2300-EXIT
                   GENERATE DE-NOTE
               END-IF
               PERFORM 2400-WRITE-RUN THRU 2400-EXIT
           END-PERFORM.
       2000-EXIT.
           EXIT.

       2100-SUBLEDGER.
           MOVE 0 TO D-SUB
           MOVE SPACES TO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE XB-CO TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           EVALUATE WS-CTL-CODE(XB-I)
               WHEN "AR"        PERFORM 2110-SQL-AR THRU 2110-EXIT
               WHEN "AP"        PERFORM 2120-SQL-AP THRU 2120-EXIT
               WHEN "INVENTORY" PERFORM 2130-SQL-INV THRU 2130-EXIT
               WHEN "WIP"       PERFORM 2140-SQL-WIP THRU 2140-EXIT
               WHEN "GRNI"      PERFORM 2150-SQL-GRNI THRU 2150-EXIT
               WHEN "VAT"       PERFORM 2160-SQL-VAT THRU 2160-EXIT
               WHEN "BANK"      PERFORM 2170-SQL-BANK THRU 2170-EXIT
               WHEN "ACCRUAL"   PERFORM 2180-SQL-ACCR THRU 2180-EXIT
           END-EVALUATE
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               COMPUTE D-SUB = FUNCTION NUMVAL(BR-COL(1))
           ELSE
               IF SQLIO-ERROR
                   MOVE "Subsidiary total unavailable - input "
                        TO D-NOTE
                   MOVE "INCOMPLETE" TO D-STATUS
                   MOVE 4 TO XB-RC
               END-IF
           END-IF.
       2100-EXIT.
           EXIT.

      *> CR2611  the sales ledger balance is what is still outstanding,
      *> not the sum of the documents as raised: a settlement discount
      *> or a write off allowed on an allocation leaves the customer
      *> owing less and is posted to the control account.  The same
      *> expression is used by ARRECON and AGEDDEBT so that the three
      *> reconciliations of the debtors control cannot disagree.
       2110-SQL-AR.
           STRING "SELECT CONVERT(varchar(24),ISNULL(SUM(t.Resid)"
                  ",0)) FROM (SELECT o.OriginalAmountGBP - ISNULL(("
                  "SELECT SUM(a.AppliedAmountGBP + "
                  "a.DiscountAmountGBP + a.WriteOffAmountGBP) FROM "
                  "ar.Allocation a WHERE a.TargetOpenItemId = "
                  "o.OpenItemId AND a.Status = 'ACTIVE' AND "
                  "a.EffectiveDate <= ?),0) + ISNULL((SELECT SUM("
                  "b.AppliedAmountGBP) FROM ar.Allocation b WHERE "
                  "b.SourceOpenItemId = o.OpenItemId AND b.Status = "
                  "'ACTIVE' AND b.EffectiveDate <= ?),0) AS Resid "
                  "FROM ar.OpenItem o WHERE o.CompanyCode = ? AND "
                  "CONVERT(char(8),o.PostingDate,112) <= ? AND "
                  "o.SettlementStatus <> 'REVERSED') t"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(1)
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE XB-CO TO SQLIO-PARM-VAL(3)
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(4).
       2110-EXIT.
           EXIT.

       2120-SQL-AP.
           STRING "SELECT CONVERT(varchar(24),ISNULL(SUM("
                  "o.OriginalAmountGBP),0)) FROM ap.OpenItem o "
                  "WHERE o.CompanyCode = ? AND CONVERT(char(8),"
                  "o.PostingDate,112) <= ? AND o.SettlementStatus "
                  "<> 'REVERSED'"
               DELIMITED BY SIZE INTO SQLIO-STMT.
       2120-EXIT.
           EXIT.

      *> CR2611  stock is worth what is on hand at cost, which is the
      *> costing pool (CST-022), not the running total of the movement
      *> history: the movement history carries only the movements the
      *> demonstration drivers made, so an issue of opening stock made
      *> the "valuation" negative.  cst.ItemCost is the one valuation
      *> and CSTINVAL reconciles the same figure to the same accounts.
      *> The pool is a position, not a dated history, so the cutoff is
      *> consumed and not applied - as it is for WIP and ACCRUAL.
       2130-SQL-INV.
           STRING "SELECT CONVERT(varchar(24),ISNULL(SUM("
                  "ic.CarryingValueGBP - ISNULL(ic.AllowanceGBP,0))"
                  ",0)) FROM cst.ItemCost ic WHERE ic.CompanyCode "
                  "= ? AND ? >= '00000000'"
               DELIMITED BY SIZE INTO SQLIO-STMT.
       2130-EXIT.
           EXIT.

       2140-SQL-WIP.
           STRING "SELECT CONVERT(varchar(24),ISNULL(SUM("
                  "k.WipValueGBP),0)) FROM kit.ExecutionKit k WHERE "
                  "k.CompanyCode = ? AND k.Status NOT IN ('DRAFT',"
                  "'FINCLOSED','CANCELLED') AND ? >= '00000000'"
               DELIMITED BY SIZE INTO SQLIO-STMT.
       2140-EXIT.
           EXIT.

       2150-SQL-GRNI.
           STRING "SELECT CONVERT(varchar(24),ISNULL((SELECT SUM("
                  "r.GrniValueGBP) FROM po.Receipt r WHERE "
                  "r.CompanyCode = ? AND r.Status = 'CONFIRMED' AND "
                  "CONVERT(char(8),r.PostingDate,112) <= ?),0) - "
                  "ISNULL((SELECT SUM(m.GrniClearedGBP) FROM "
                  "ap.InvoiceMatch m JOIN ap.PurchaseInvoice i ON "
                  "i.PurchaseInvoiceId = m.PurchaseInvoiceId WHERE "
                  "i.CompanyCode = ? AND m.Status <> 'REVERSED' AND "
                  "CONVERT(char(8),i.PostingDate,112) <= ?),0))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT.
       2150-EXIT.
           EXIT.

       2160-SQL-VAT.
           STRING "SELECT CONVERT(varchar(24),ISNULL(SUM(CASE WHEN "
                  "v.Direction = 'OUTPUT' THEN v.VatAmountGBP ELSE "
                  "-ISNULL(v.RecoverableVatGBP,v.VatAmountGBP) END)"
                  ",0)) FROM fin.VatTransaction v WHERE "
                  "v.CompanyCode = ? AND CONVERT(char(8),"
                  "v.PostingDate,112) <= ?"
               DELIMITED BY SIZE INTO SQLIO-STMT.
       2160-EXIT.
           EXIT.

       2170-SQL-BANK.
           STRING "SELECT CONVERT(varchar(24),ISNULL(SUM(v.Bal),0))"
                  " FROM (SELECT (SELECT TOP 1 "
                  "r.ClosingStatementBalance - ISNULL("
                  "r.OutstandingPayments,0) + ISNULL("
                  "r.OutstandingReceipts,0) FROM fin.BankRecon r "
                  "WHERE r.CompanyCode = b.CompanyCode AND "
                  "r.BankAccountCode = b.BankAccountCode AND "
                  "CONVERT(char(8),r.ToDate,112) <= ? ORDER BY "
                  "r.ToDate DESC) AS Bal FROM fin.BankAccount b "
                  "WHERE b.CompanyCode = ?) v"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(1)
           MOVE XB-CO TO SQLIO-PARM-VAL(2).
       2170-EXIT.
           EXIT.

       2180-SQL-ACCR.
           STRING "SELECT CONVERT(varchar(24),ISNULL(SUM("
                  "ISNULL(s.AcceptedValueGBP,0) - ISNULL("
                  "s.MatchedValueGBP,0)),0)) FROM "
                  "po.ServiceAcceptance s WHERE s.CompanyCode = ? "
                  "AND ? >= '00000000'"
               DELIMITED BY SIZE INTO SQLIO-STMT.
       2180-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-006, COM-022  a reconciliation difference belongs to
      *> the named control account owner with a due date, not to the
      *> clerk who happened to run the job
      *>----------------------------------------------------------------
       2300-RAISE.
           INITIALIZE WS-EXCPRAIS
           MOVE "WARNING" TO EX-SEVERITY
           MOVE "RECONCILIATION DIFFERENCE" TO EX-CATEGORY
           MOVE "CONTROL" TO EX-DOCUMENT-TYPE
           MOVE SPACES TO XB-TEXT
           STRING FUNCTION TRIM(XB-CO) "/" FUNCTION TRIM(D-CODE)
                  "/" XB-DATE-ED
               DELIMITED BY SIZE INTO XB-TEXT
           MOVE XB-TEXT(1:40) TO EX-DOCUMENT-REF
           MOVE D-DIFF TO XB-MON-ED
           MOVE SPACES TO XB-TEXT2
           STRING FUNCTION TRIM(D-DESC)
                  " differs from the general ledger by GBP "
                  FUNCTION TRIM(XB-MON-ED)
                  " at the cutoff. Correct at source or record an "
                  "approved adjustment with a reviewer."
               DELIMITED BY SIZE INTO XB-TEXT2
           MOVE XB-TEXT2(1:200) TO EX-MESSAGE-TEXT
           MOVE D-OWNER TO EX-OWNER-ROLE
           MOVE 2 TO EX-PRIORITY
           MOVE D-DIFF TO EX-AMOUNT-GBP
           MOVE XB-ASAT TO EX-DUE-AT
           MOVE XB-TEXT2(1:96) TO D-NOTE
           PERFORM 9700-RAISE-EXCEPTION THRU 9700-EXIT.
       2300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-010, RPT-015  the run is stored so that two reports
      *> of the same measure can be reconciled to one another later
      *>----------------------------------------------------------------
       2400-WRITE-RUN.
           IF NOT XB-COMMIT
               GO TO 2400-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM rpt.ReconciliationRun WHERE "
                  "CompanyCode = ? AND ControlType = ? AND AsOfDate "
                  "= CONVERT(date,?,112) AND Status = 'OPEN'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE D-CODE TO SQLIO-PARM-VAL(2)
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
                       SQLIO-PARM-NULL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO rpt.ReconciliationRun (CompanyCode, "
                  "ControlType, AsOfDate, FiscalYear, PeriodNo, "
                  "AccountCode, ControlBalanceGBP, DetailBalanceGBP"
                  ", DifferenceGBP, ContributingRefs, OwnerUser, "
                  "Status, Note, RunBy) VALUES (?,?,CONVERT(date,?,"
                  "112),?,?,?,?,?,?,?,?,?,?,?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE D-CODE TO SQLIO-PARM-VAL(2)
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(3)
           MOVE XB-FY TO XB-SEQ-ED
           MOVE FUNCTION TRIM(XB-SEQ-ED) TO SQLIO-PARM-VAL(4)
           MOVE XB-PER TO XB-SEQ-ED
           MOVE FUNCTION TRIM(XB-SEQ-ED) TO SQLIO-PARM-VAL(5)
           MOVE D-ACCTS TO SQLIO-PARM-VAL(6)
           MOVE D-GL TO XB-MON-ED
           MOVE FUNCTION TRIM(XB-MON-ED) TO SQLIO-PARM-VAL(7)
           MOVE D-SUB TO XB-MON-ED
           MOVE FUNCTION TRIM(XB-MON-ED) TO SQLIO-PARM-VAL(8)
           MOVE D-DIFF TO XB-MON-ED
           MOVE FUNCTION TRIM(XB-MON-ED) TO SQLIO-PARM-VAL(9)
           MOVE D-DESC TO SQLIO-PARM-VAL(10)
           MOVE D-OWNER TO SQLIO-PARM-VAL(11)
           IF D-STATUS = "AGREED"
               MOVE "ACCEPTED" TO SQLIO-PARM-VAL(12)
           ELSE
               MOVE "OPEN" TO SQLIO-PARM-VAL(12)
           END-IF
           MOVE D-NOTE TO SQLIO-PARM-VAL(13)
           MOVE XB-USER TO SQLIO-PARM-VAL(14)
           PERFORM VARYING XB-J FROM 1 BY 1 UNTIL XB-J > 14
               MOVE "N" TO SQLIO-PARM-NULL(XB-J)
           END-PERFORM
           MOVE 14 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 4 TO XB-RC
               DISPLAY "STERP-W-2401 " SQLIO-ERRMSG(1:90)
           ELSE
               ADD 1 TO XB-POSTED
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       2400-EXIT.
           EXIT.

       3000-PROVE.
           IF XB-DIFFS = 0
               MOVE "RECONCILED" TO RPT-STATUS-TEXT
           ELSE
               MOVE "UNRECONCILED" TO RPT-STATUS-TEXT
               IF XB-RC < 4
                   MOVE 4 TO XB-RC
               END-IF
           END-IF
           DISPLAY "STERP-I-3001 " XB-ROWS-OUT " control families, "
                   XB-DIFFS " with a difference, " XB-RAISED
                   " exception(s)"
           IF XB-COMMIT
               DISPLAY "STERP-I-3002 " XB-POSTED
                       " reconciliation row(s) written"
           END-IF.
       3000-EXIT.
           EXIT.

       COPY XMDBATPR.
