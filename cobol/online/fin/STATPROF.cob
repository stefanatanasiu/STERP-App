      *>================================================================
      *> PROGRAM-ID : STATPROF
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER
      *> PURPOSE    : THE COMPANY STATUTORY PROFILE, ACCOUNTING
      *>              REFERENCE DATE AND FILING DEADLINES (FIN-030).
      *>              FOR A PRIVATE COMPANY THE OBLIGATIONS DERIVE FROM
      *>              THE PERIOD END: ACCOUNTS NINE MONTHS AFTER THE
      *>              YEAR END, CORPORATION TAX PAYABLE NINE MONTHS AND
      *>              ONE DAY AFTER IT, AND THE CT600 TWELVE MONTHS
      *>              AFTER IT.  AC-119: A 31 MARCH 2027 YEAR END GIVES
      *>              31 DECEMBER 2027, 1 JANUARY 2028 AND 31 MARCH
      *>              2028.
      *>              A CHANGE OF ACCOUNTING REFERENCE DATE MAY CREATE
      *>              A PERIOD OF ACCOUNT OF UP TO EIGHTEEN MONTHS.
      *>              A POSTING ACCOUNT WITH NO STATUTORY FORMAT LINE
      *>              BLOCKS THE STATUTORY STATEMENT (FIN-023).
      *> CALLED BY  : PLRPT, BSRPT, frmPeriodClose, .NET CLIENT
      *> INTERFACE  : CALL "STATPROF" USING WS-STATPROF-CA
      *>              COPYBOOK STATPROFCA
      *> CALLS      : DATECALC, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2001-06-05 RJH CR1420 ORIGINAL - COMPANIES ACT FILING DIARY
      *> 2026-10-05 STERP 18 MONTH ARD CHANGE, MAPPING CHECK
      *>================================================================
      *> REQ: FIN-023, FIN-030
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STATPROF.
       AUTHOR. R J HOLBROOK.
       DATE-WRITTEN. 05/06/2001.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATECALC-CA==
                      LEADING ==CA-== BY ==DH-==.
       01  WS-SWITCHES.
           05  WS-REJECTED             PIC X VALUE "N".
               88  WS-IS-REJECTED                VALUE "Y".
       01  WS-WORK.
           05  WS-IDX                  PIC S9(4) COMP-5 VALUE 0.
           05  WS-DATE                 PIC 9(8) VALUE 0.
           05  WS-MONTHS               PIC S9(4) VALUE 0.
           05  WS-OB-TYPE              PIC X(16).
           05  WS-OB-DESC              PIC X(60).
           05  WS-OB-DUE               PIC 9(8).
           05  WS-OB-ROLE              PIC X(20).
           05  WS-DATE-ED              PIC X(10).
       01  WS-CO-ROW.
           05  WS-C-FRAMEWORK          PIC X(10).
           05  WS-C-ARD                PIC X(5).
           05  WS-C-REGNO              PIC X(10).
           05  WS-C-VATNO              PIC X(15).
           05  WS-C-STAGGER            PIC X(12).
           05  WS-C-SCHEME             PIC X(12).
           05  WS-C-JURIS              PIC X(30).
           05  WS-C-LEGALFORM          PIC X(20).
       01  WS-MAP-ROW.
           05  WS-M-COUNT              PIC X(12).
           05  WS-M-DISALLOW           PIC X(22).
       LINKAGE SECTION.
       COPY STATPROFCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           IF CA-COMPANY = SPACES
               MOVE "STA0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0000-RETURN
           END-IF
           PERFORM 0200-READ-COMPANY THRU 0200-EXIT
           IF WS-IS-REJECTED
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "PROFILE"   PERFORM 1000-PROFILE THRU 1000-EXIT
               WHEN "DEADLINE"  PERFORM 2000-DEADLINES THRU 2000-EXIT
               WHEN "ARDCHANG" PERFORM 3000-ARD-CHANGE THRU 3000-EXIT
               WHEN "MAPCHECK"  PERFORM 4000-MAP-CHECK THRU 4000-EXIT
               WHEN OTHER       PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.

       0100-INITIALISE.
           MOVE "N" TO WS-REJECTED
           MOVE "STATPROF" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "FIN-030" TO CA-REQ-REF
           MOVE 0 TO SP-ACCOUNTS-DUE
           MOVE 0 TO SP-CTPAY-DUE
           MOVE 0 TO SP-CTRETURN-DUE
           MOVE 0 TO SP-CONFIRM-DUE
           MOVE 0 TO SP-UNMAPPED-CNT
           MOVE 0 TO SP-OBLIG-CNT
           MOVE 0 TO SP-LENGTH-MONTHS
           MOVE 0 TO SP-DISALLOWABLE-GBP
           MOVE "Y" TO SP-MAP-COMPLETE
           MOVE "Y" TO SP-ARD-VALID
           IF SP-COMPANY-TYPE = SPACES
               MOVE "PRIVATE" TO SP-COMPANY-TYPE
           END-IF.
       0100-EXIT.
           EXIT.

       0200-READ-COMPANY.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT c.AccountingFramework, c.AccountingRefDate, "
                  "ISNULL(c.RegisteredNumber,''), "
                  "ISNULL(c.VatRegNo,''), ISNULL(c.VatStagger,''), "
                  "ISNULL(c.VatScheme,'INVOICE'), "
                  "ISNULL(c.Jurisdiction,''), ISNULL(c.LegalForm,'') "
                  "FROM core.Company c WHERE c.CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 8 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 5 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           MOVE 15 TO SQLIO-COL-WIDTH(4)
           MOVE 12 TO SQLIO-COL-WIDTH(5)
           MOVE 12 TO SQLIO-COL-WIDTH(6)
           MOVE 30 TO SQLIO-COL-WIDTH(7)
           MOVE 20 TO SQLIO-COL-WIDTH(8)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CO-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 0200-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "STA0002" TO CA-REASON-CODE
               MOVE "That company is not held" TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               MOVE "Y" TO WS-REJECTED
               ADD 1 TO CA-MSG-COUNT
               GO TO 0200-EXIT
           END-IF
           MOVE WS-C-FRAMEWORK TO SP-FRAMEWORK
           MOVE WS-C-ARD TO SP-ARD
           MOVE WS-C-REGNO TO SP-REG-NUMBER
           MOVE WS-C-VATNO TO SP-VAT-REG-NO
           MOVE WS-C-STAGGER TO SP-VAT-STAGGER
           MOVE WS-C-SCHEME TO SP-VAT-SCHEME
           MOVE WS-C-JURIS TO SP-JURISDICTION
           IF WS-C-LEGALFORM NOT = SPACES
               MOVE WS-C-LEGALFORM TO SP-COMPANY-TYPE
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000 - THE PROFILE PLUS ITS DERIVED DEADLINES
      *>----------------------------------------------------------------
       1000-PROFILE.
           PERFORM 2000-DEADLINES THRU 2000-EXIT
           PERFORM 4000-MAP-CHECK THRU 4000-EXIT.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000 - DERIVE THE STATUTORY OBLIGATIONS FROM THE PERIOD END
      *>----------------------------------------------------------------
       2000-DEADLINES.
           IF SP-PERIOD-END = 0
               MOVE "STA0010" TO CA-REASON-CODE
               MOVE "A period end date is required to derive the oblig"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           MOVE 0 TO SP-OBLIG-CNT
      *>   Accounts filing: nine months after the period end for a
      *>   private company, six for a public one.
           IF SP-COMPANY-TYPE = "PUBLIC"
               MOVE 6 TO WS-MONTHS
           ELSE
               MOVE 9 TO WS-MONTHS
           END-IF
           IF SP-FIRST-PERIOD = "Y"
               ADD 12 TO WS-MONTHS
           END-IF
           MOVE SP-PERIOD-END TO WS-DATE
           PERFORM 2100-ADD-MONTHS THRU 2100-EXIT
           MOVE WS-DATE TO SP-ACCOUNTS-DUE
           MOVE "ACCOUNTS" TO WS-OB-TYPE
           MOVE "Annual accounts filed at Companies House"
               TO WS-OB-DESC
           MOVE SP-ACCOUNTS-DUE TO WS-OB-DUE
           MOVE "FINCTRL" TO WS-OB-ROLE
           PERFORM 2200-ADD-OBLIGATION THRU 2200-EXIT
      *>   Corporation tax is payable nine months and one day after the
      *>   end of the accounting period.
           MOVE 9 TO WS-MONTHS
           MOVE SP-PERIOD-END TO WS-DATE
           PERFORM 2100-ADD-MONTHS THRU 2100-EXIT
           PERFORM 2150-ADD-ONE-DAY THRU 2150-EXIT
           MOVE WS-DATE TO SP-CTPAY-DUE
           MOVE "CTPAYMENT" TO WS-OB-TYPE
           MOVE "Corporation tax payment" TO WS-OB-DESC
           MOVE SP-CTPAY-DUE TO WS-OB-DUE
           MOVE "FINCTRL" TO WS-OB-ROLE
           PERFORM 2200-ADD-OBLIGATION THRU 2200-EXIT
      *>   The CT600 return is due twelve months after the period end.
           MOVE 12 TO WS-MONTHS
           MOVE SP-PERIOD-END TO WS-DATE
           PERFORM 2100-ADD-MONTHS THRU 2100-EXIT
           MOVE WS-DATE TO SP-CTRETURN-DUE
           MOVE "CTRETURN" TO WS-OB-TYPE
           MOVE "Corporation tax return CT600" TO WS-OB-DESC
           MOVE SP-CTRETURN-DUE TO WS-OB-DUE
           MOVE "FINCTRL" TO WS-OB-ROLE
           PERFORM 2200-ADD-OBLIGATION THRU 2200-EXIT
      *>   The confirmation statement follows the review period, taken
      *>   here as twelve months from the period end plus fourteen days
           MOVE 12 TO WS-MONTHS
           MOVE SP-PERIOD-END TO WS-DATE
           PERFORM 2100-ADD-MONTHS THRU 2100-EXIT
           MOVE 14 TO WS-MONTHS
           PERFORM 2160-ADD-DAYS THRU 2160-EXIT
           MOVE WS-DATE TO SP-CONFIRM-DUE
           MOVE "CONFIRMATION" TO WS-OB-TYPE
           MOVE "Confirmation statement" TO WS-OB-DESC
           MOVE SP-CONFIRM-DUE TO WS-OB-DUE
           MOVE "SYSADM" TO WS-OB-ROLE
           PERFORM 2200-ADD-OBLIGATION THRU 2200-EXIT
           MOVE SPACES TO CA-MESSAGE
           MOVE SP-ACCOUNTS-DUE TO WS-DATE
           PERFORM 2300-FORMAT-DATE THRU 2300-EXIT
           STRING "Accounts are due by " FUNCTION TRIM(WS-DATE-ED)
                  " with corporation tax and the CT600 following"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

       2100-ADD-MONTHS.
           INITIALIZE WS-DATECALC-CA
           MOVE "ADDMONTH" TO DH-FUNCTION
           MOVE CA-COMPANY TO DH-COMPANY
           MOVE CA-USER TO DH-USER
           MOVE CA-BUSINESS-DATE TO DH-BUSINESS-DATE
           MOVE WS-DATE TO DC-DATE-1
           MOVE WS-MONTHS TO DC-NUMBER
           CALL "DATECALC" USING WS-DATECALC-CA
           IF DH-RETURN-CODE < 8
               MOVE DC-RESULT-DATE TO WS-DATE
           END-IF.
       2100-EXIT.
           EXIT.

       2150-ADD-ONE-DAY.
           MOVE 1 TO WS-MONTHS
           PERFORM 2160-ADD-DAYS THRU 2160-EXIT.
       2150-EXIT.
           EXIT.

       2160-ADD-DAYS.
           INITIALIZE WS-DATECALC-CA
           MOVE "ADDDAYS" TO DH-FUNCTION
           MOVE CA-COMPANY TO DH-COMPANY
           MOVE CA-USER TO DH-USER
           MOVE CA-BUSINESS-DATE TO DH-BUSINESS-DATE
           MOVE WS-DATE TO DC-DATE-1
           MOVE WS-MONTHS TO DC-NUMBER
           CALL "DATECALC" USING WS-DATECALC-CA
           IF DH-RETURN-CODE < 8
               MOVE DC-RESULT-DATE TO WS-DATE
           END-IF.
       2160-EXIT.
           EXIT.

       2300-FORMAT-DATE.
           INITIALIZE WS-DATECALC-CA
           MOVE "FORMAT" TO DH-FUNCTION
           MOVE CA-COMPANY TO DH-COMPANY
           MOVE CA-USER TO DH-USER
           MOVE CA-BUSINESS-DATE TO DH-BUSINESS-DATE
           MOVE WS-DATE TO DC-DATE-1
           MOVE "DD/MM/YYYY" TO DC-FORMAT-OUT
           CALL "DATECALC" USING WS-DATECALC-CA
           MOVE DC-TEXT-OUT(1:10) TO WS-DATE-ED.
       2300-EXIT.
           EXIT.

       2200-ADD-OBLIGATION.
           IF SP-OBLIG-CNT < 8
               ADD 1 TO SP-OBLIG-CNT
               MOVE SP-OBLIG-CNT TO WS-IDX
               MOVE WS-OB-TYPE TO SP-O-TYPE(WS-IDX)
               MOVE WS-OB-DESC TO SP-O-DESC(WS-IDX)
               MOVE WS-OB-DUE TO SP-O-DUE-DATE(WS-IDX)
               MOVE "OPEN" TO SP-O-STATUS(WS-IDX)
               MOVE WS-OB-ROLE TO SP-O-ROLE(WS-IDX)
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.StatutoryObligation (CompanyCode, "
                  "PeriodEndDate, ObligationType, Description, "
                  "DueDate, EscalateFrom, ResponsibleRole, Status) "
                  "SELECT ?, ?, ?, ?, ?, DATEADD(day, -30, "
                  "CAST(? AS date)), ?, 'OPEN' WHERE NOT EXISTS "
                  "(SELECT 1 FROM fin.StatutoryObligation x WHERE "
                  "x.CompanyCode = ? AND x.PeriodEndDate = ? "
                  "AND x.ObligationType = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 10 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SP-PERIOD-END TO SQLIO-PARM-VAL(2)
           MOVE WS-OB-TYPE TO SQLIO-PARM-VAL(3)
           MOVE WS-OB-DESC TO SQLIO-PARM-VAL(4)
           MOVE WS-OB-DUE TO SQLIO-PARM-VAL(5)
           MOVE WS-OB-DUE TO SQLIO-PARM-VAL(6)
           MOVE WS-OB-ROLE TO SQLIO-PARM-VAL(7)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(8)
           MOVE SP-PERIOD-END TO SQLIO-PARM-VAL(9)
           MOVE WS-OB-TYPE TO SQLIO-PARM-VAL(10)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-MAP-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       2200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000 - A CHANGE OF ACCOUNTING REFERENCE DATE
      *>        A PERIOD OF ACCOUNT MAY NOT EXCEED EIGHTEEN MONTHS.
      *>----------------------------------------------------------------
       3000-ARD-CHANGE.
           IF SP-PERIOD-START = 0 OR SP-NEW-PERIOD-END = 0
               MOVE "STA0020" TO CA-REASON-CODE
               MOVE "The period start and the new reference date are b"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           INITIALIZE WS-DATECALC-CA
           MOVE "DAYSBTWN" TO DH-FUNCTION
           MOVE CA-COMPANY TO DH-COMPANY
           MOVE CA-USER TO DH-USER
           MOVE CA-BUSINESS-DATE TO DH-BUSINESS-DATE
           MOVE SP-PERIOD-START TO DC-DATE-1
           MOVE SP-NEW-PERIOD-END TO DC-DATE-2
           CALL "DATECALC" USING WS-DATECALC-CA
           IF DH-RETURN-CODE > 4
               MOVE DH-REASON-CODE TO CA-REASON-CODE
               MOVE DH-MESSAGE TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
      *>   Months are approximated from whole days; eighteen months is
      *>   the statutory maximum for a period of account.
           COMPUTE SP-LENGTH-MONTHS ROUNDED =
               (DC-RESULT-NUM + 1) / 30.4375
           IF SP-LENGTH-MONTHS > 18
               MOVE "N" TO SP-ARD-VALID
               MOVE "STA0021" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A period of account of " SP-LENGTH-MONTHS
                      " months exceeds the eighteen month maximum"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           IF SP-REASON = SPACES
               MOVE "STA0022" TO CA-REASON-CODE
               MOVE "A change of accounting reference date needs an ap"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE "Y" TO SP-ARD-VALID
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.PeriodOfAccount (CompanyCode, StartDate,"
                  " EndDate, LengthMonths, Framework, IsArdChange, "
                  "ApprovedBy, ApprovedAt, Reason, Status) SELECT ?, "
                  "?, ?, ?, ?, 1, ?, SYSDATETIME(), ?, 'OPEN' "
                  "WHERE NOT EXISTS (SELECT 1 FROM fin.PeriodOfAccount"
                  " x WHERE x.CompanyCode = ? AND x.EndDate = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 9 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SP-PERIOD-START TO SQLIO-PARM-VAL(2)
           MOVE SP-NEW-PERIOD-END TO SQLIO-PARM-VAL(3)
           MOVE SP-LENGTH-MONTHS TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE SP-FRAMEWORK TO SQLIO-PARM-VAL(5)
           MOVE CA-USER TO SQLIO-PARM-VAL(6)
           MOVE SP-REASON TO SQLIO-PARM-VAL(7)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(8)
           MOVE SP-NEW-PERIOD-END TO SQLIO-PARM-VAL(9)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-MAP-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE SP-NEW-PERIOD-END TO SP-PERIOD-END
           PERFORM 2000-DEADLINES THRU 2000-EXIT
           MOVE SPACES TO CA-MESSAGE
           STRING "A period of account of " SP-LENGTH-MONTHS
                  " months is accepted and its obligations are "
                  "rederived"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000 - EVERY POSTING ACCOUNT MAPS TO EXACTLY ONE STATUTORY
      *>        FORMAT LINE, OR THE STATEMENT IS BLOCKED (FIN-023).
      *>----------------------------------------------------------------
       4000-MAP-CHECK.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(12)), "
                  "ISNULL((SELECT SUM(v.MovementGBP) FROM "
                  "fin.vw_AccountBalance v WHERE v.CompanyCode = ? "
                  "AND v.TaxAnalysisFlag IS NOT NULL),0) "
                  "FROM fin.Account a LEFT JOIN fin.StatutoryLine s "
                  "ON s.Framework = ? AND s.StatutoryLineCode = "
                  "a.StatutoryLineCode WHERE a.CompanyCode = ? "
                  "AND a.AccountKind = 'POSTING' "
                  "AND a.Status = 'ACTIVE' AND s.StatutoryLineCode "
                  "IS NULL"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SP-FRAMEWORK TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 22 TO SQLIO-COL-WIDTH(2)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-MAP-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 4000-EXIT
           END-IF
           COMPUTE SP-UNMAPPED-CNT = FUNCTION NUMVAL(WS-M-COUNT)
           COMPUTE SP-DISALLOWABLE-GBP =
               FUNCTION NUMVAL(WS-M-DISALLOW)
           IF SP-UNMAPPED-CNT = 0
               MOVE "Y" TO SP-MAP-COMPLETE
           ELSE
               MOVE "N" TO SP-MAP-COMPLETE
               MOVE 8 TO CA-RETURN-CODE
               MOVE "STA0030" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING SP-UNMAPPED-CNT " posting account(s) have no "
                      FUNCTION TRIM(SP-FRAMEWORK)
                      " statutory format line. The statutory statement"
                      " is blocked until every account is mapped"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       4000-EXIT.
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
           MOVE "STA0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "STATPROF does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in STATPROF" TO CA-MESSAGE
           END-IF
           MOVE 12 TO CA-RETURN-CODE
           MOVE "Y" TO WS-REJECTED
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
