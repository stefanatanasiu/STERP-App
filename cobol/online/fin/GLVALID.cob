      *>================================================================
      *> PROGRAM-ID : GLVALID
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER
      *> PURPOSE    : GOVERNED CHART OF ACCOUNTS AND ANALYSIS DIMENSION
      *>              VALIDATION.  EVERY POSTING INSTRUCTION ASKS HERE
      *>              BEFORE IT TOUCHES THE LEDGER.  A SUMMARY ONLY OR
      *>              RETIRED ACCOUNT, A MISSING MANDATORY COST CENTRE,
      *>              A FORBIDDEN DIMENSION, AN UNKNOWN DIMENSION VALUE
      *>              OR A MISSING BUSINESS EVENT MAPPING EACH BLOCK
      *>              THE POSTING AND NAME THE OFFENDING FIELD.  NO
      *>              SILENT SUSPENSE ACCOUNT IS EVER SUBSTITUTED.
      *> CALLED BY  : JNLENTRY, JNLPOST, EVERY SUBLEDGER, .NET CLIENT
      *> INTERFACE  : CALL "GLVALID" USING WS-GLVALID-CA
      *>              COPYBOOK GLVALIDCA
      *> CALLS      : SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-06-14 APW CR0088 ORIGINAL - MICRO FOCUS COBOL/2 ON OS/2
      *> 1996-02-11 RJH CR0412 SUMMARY ACCOUNTS MAY NOT BE POSTED TO
      *> 1999-11-30 KMP Y2K - 4 DIGIT YEARS THROUGHOUT
      *> 2004-09-02 APW CR1811 PROJECT DIMENSION ADDED ALONGSIDE COST
      *>                CENTRE.  OLD SINGLE DIMENSION CODE REMOVED.
      *> 2026-10-05 STERP EVENT ACCOUNT MAPPING, RETIREMENT RULES
      *>================================================================
      *> REQ: FIN-001, FIN-002, FIN-003, FIN-005
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GLVALID.
       AUTHOR. A P WHITTAKER.
       DATE-WRITTEN. 14/06/1994.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       01  WS-SWITCHES.
           05  WS-REJECTED             PIC X VALUE "N".
               88  WS-IS-REJECTED                VALUE "Y".
           05  WS-FOUND                PIC X VALUE "N".
       01  WS-WORK.
           05  WS-DATE-TEXT            PIC X(8).
           05  WS-IDX                  PIC S9(4) COMP-5 VALUE 0.
           05  WS-COUNT                PIC S9(9) COMP-5 VALUE 0.
           05  WS-MSG                  PIC X(200).
      *> Account row as it comes back from SQL Server.  Every column
      *> arrives as left justified text - SQLIO does no conversion.
       01  WS-ACC-ROW.
           05  WS-A-NAME               PIC X(80).
           05  WS-A-CLASS              PIC X(10).
           05  WS-A-CLASS-TYPE         PIC X(10).
           05  WS-A-NORMAL             PIC X(2).
           05  WS-A-STATEMENT          PIC X(2).
           05  WS-A-KIND               PIC X(8).
           05  WS-A-STATUS             PIC X(10).
           05  WS-A-CONTROL            PIC X(10).
           05  WS-A-SUBLEDGER          PIC X(6).
           05  WS-A-MANUAL             PIC X(4).
           05  WS-A-VATELIG            PIC X(4).
           05  WS-A-STATLINE           PIC X(20).
           05  WS-A-MGMT               PIC X(30).
           05  WS-A-TAXFLAG            PIC X(20).
           05  WS-A-VALIDFROM          PIC X(8).
           05  WS-A-VALIDTO            PIC X(8).
       01  WS-DIM-ROW.
           05  WS-D-REQUIREMENT        PIC X(10).
           05  WS-D-ALLOWED            PIC X(200).
       01  WS-VAL-ROW.
           05  WS-V-NAME               PIC X(80).
           05  WS-V-ACTIVE             PIC X(4).
       01  WS-LEG-ROW.
           05  WS-G-LEG                PIC X(12).
           05  WS-G-DRCR               PIC X(2).
           05  WS-G-ACCOUNT            PIC X(12).
           05  WS-G-CC                 PIC X(20).
           05  WS-G-PROJ               PIC X(20).
           05  WS-G-DESC               PIC X(60).
       01  WS-CNT-ROW.
           05  WS-C-COUNT              PIC X(18).
       LINKAGE SECTION.
       COPY GLVALIDCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
      *> 0000 - CONTROL
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           PERFORM 0200-EDIT-REQUEST THRU 0200-EXIT
           IF WS-IS-REJECTED
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "ACCOUNT"  PERFORM 1000-CHECK-ACCOUNT THRU 1000-EXIT
               WHEN "LINE"     PERFORM 2000-CHECK-LINE THRU 2000-EXIT
               WHEN "EVENT"    PERFORM 3000-CHECK-EVENT THRU 3000-EXIT
               WHEN "USAGE"    PERFORM 4000-CHECK-USAGE THRU 4000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.

       0100-INITIALISE.
           MOVE "N" TO WS-REJECTED
           MOVE "GLVALID" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "FIN-002" TO CA-REQ-REF
           MOVE SPACES TO GV-ACCOUNT-NAME
           MOVE SPACES TO GV-CLASS-CODE
           MOVE SPACES TO GV-CLASS-TYPE
           MOVE SPACES TO GV-NORMAL-BALANCE
           MOVE SPACES TO GV-STATEMENT
           MOVE SPACES TO GV-ACCOUNT-KIND
           MOVE SPACES TO GV-ACCOUNT-STATUS
           MOVE SPACES TO GV-CONTROL-TYPE
           MOVE SPACES TO GV-CONTROL-SUBLDG
           MOVE SPACES TO GV-STAT-LINE
           MOVE SPACES TO GV-MGMT-GROUP
           MOVE SPACES TO GV-TAX-ANALYSIS
           MOVE SPACES TO GV-CC-REQUIREMENT
           MOVE SPACES TO GV-PROJ-REQUIRED
           MOVE SPACES TO GV-CC-NAME
           MOVE SPACES TO GV-PROJ-NAME
           MOVE SPACES TO GV-FAIL-FIELD
           MOVE "N" TO GV-ALLOW-MANUAL
           MOVE "N" TO GV-VAT-ELIGIBLE
           MOVE "N" TO GV-MAY-RETIRE
           MOVE "N" TO GV-MAY-DELETE
           MOVE "N" TO GV-VALID
           MOVE 0 TO GV-USE-COUNT
           MOVE 0 TO GV-LEG-CNT
           MOVE FUNCTION UPPER-CASE(GV-ACCOUNT) TO GV-ACCOUNT
           MOVE FUNCTION UPPER-CASE(GV-DIM-CC) TO GV-DIM-CC
           MOVE FUNCTION UPPER-CASE(GV-DIM-PROJ) TO GV-DIM-PROJ
           MOVE FUNCTION UPPER-CASE(GV-EVENT-CODE) TO GV-EVENT-CODE
           IF GV-POSTING-DATE = 0
               MOVE CA-BUSINESS-DATE TO GV-POSTING-DATE
           END-IF
           MOVE GV-POSTING-DATE TO WS-DATE-TEXT.
       0100-EXIT.
           EXIT.

       0200-EDIT-REQUEST.
           IF CA-COMPANY = SPACES OR CA-COMPANY = LOW-VALUES
               MOVE "GLV0001" TO CA-REASON-CODE
               MOVE "A company is required to validate an account"
                   TO CA-MESSAGE
               MOVE "CA-COMPANY" TO GV-FAIL-FIELD
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION = "EVENT"
               GO TO 0200-EXIT
           END-IF
           IF GV-ACCOUNT = SPACES OR GV-ACCOUNT = LOW-VALUES
               MOVE "GLV0002" TO CA-REASON-CODE
               MOVE "An account code is required" TO CA-MESSAGE
               MOVE "GV-ACCOUNT" TO GV-FAIL-FIELD
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000 - ACCOUNT EXISTENCE, STATUS AND POSTING ELIGIBILITY
      *>----------------------------------------------------------------
       1000-CHECK-ACCOUNT.
           PERFORM 1100-READ-ACCOUNT THRU 1100-EXIT
           IF CA-FAILED
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-TEST-ELIGIBILITY THRU 1200-EXIT.
       1000-EXIT.
           EXIT.

       1100-READ-ACCOUNT.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT a.Name, a.ClassCode, c.ClassType, "
                  "c.NormalBalance, c.Statement, a.AccountKind, "
                  "a.Status, ISNULL(a.ControlType,''), "
                  "ISNULL(a.ControlSubledger,''), "
                  "CAST(a.AllowManualJournal AS char(1)), "
                  "CAST(a.IsVatEligible AS char(1)), "
                  "ISNULL(a.StatutoryLineCode,''), "
                  "ISNULL(a.ManagementGroup,''), "
                  "ISNULL(a.TaxAnalysisFlag,''), "
                  "CONVERT(char(8), a.ValidFrom, 112), "
                  "ISNULL(CONVERT(char(8), a.ValidTo, 112),'') "
                  "FROM fin.Account a JOIN fin.AccountClass c "
                  "ON c.ClassCode = a.ClassCode "
                  "WHERE a.CompanyCode = ? AND a.AccountCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE GV-ACCOUNT TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 16 TO SQLIO-COL-CNT
           MOVE 80 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           MOVE 2 TO SQLIO-COL-WIDTH(4)
           MOVE 2 TO SQLIO-COL-WIDTH(5)
           MOVE 8 TO SQLIO-COL-WIDTH(6)
           MOVE 10 TO SQLIO-COL-WIDTH(7)
           MOVE 10 TO SQLIO-COL-WIDTH(8)
           MOVE 6 TO SQLIO-COL-WIDTH(9)
           MOVE 4 TO SQLIO-COL-WIDTH(10)
           MOVE 4 TO SQLIO-COL-WIDTH(11)
           MOVE 20 TO SQLIO-COL-WIDTH(12)
           MOVE 30 TO SQLIO-COL-WIDTH(13)
           MOVE 20 TO SQLIO-COL-WIDTH(14)
           MOVE 8 TO SQLIO-COL-WIDTH(15)
           MOVE 8 TO SQLIO-COL-WIDTH(16)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ACC-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "GLV0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Account " FUNCTION TRIM(GV-ACCOUNT)
                      " is not held in the chart of accounts for "
                      FUNCTION TRIM(CA-COMPANY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "GV-ACCOUNT" TO GV-FAIL-FIELD
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 1100-EXIT
           END-IF
           MOVE WS-A-NAME TO GV-ACCOUNT-NAME
           MOVE WS-A-CLASS TO GV-CLASS-CODE
           MOVE WS-A-CLASS-TYPE TO GV-CLASS-TYPE
           MOVE WS-A-NORMAL TO GV-NORMAL-BALANCE
           MOVE WS-A-STATEMENT TO GV-STATEMENT
           MOVE WS-A-KIND TO GV-ACCOUNT-KIND
           MOVE WS-A-STATUS TO GV-ACCOUNT-STATUS
           MOVE WS-A-CONTROL TO GV-CONTROL-TYPE
           MOVE WS-A-SUBLEDGER TO GV-CONTROL-SUBLDG
           MOVE WS-A-STATLINE TO GV-STAT-LINE
           MOVE WS-A-MGMT TO GV-MGMT-GROUP
           MOVE WS-A-TAXFLAG TO GV-TAX-ANALYSIS
           IF WS-A-MANUAL(1:1) = "1"
               MOVE "Y" TO GV-ALLOW-MANUAL
           ELSE
               MOVE "N" TO GV-ALLOW-MANUAL
           END-IF
           IF WS-A-VATELIG(1:1) = "1"
               MOVE "Y" TO GV-VAT-ELIGIBLE
           ELSE
               MOVE "N" TO GV-VAT-ELIGIBLE
           END-IF.
       1100-EXIT.
           EXIT.

       1200-TEST-ELIGIBILITY.
           IF GV-ACCOUNT-KIND NOT = "POSTING"
               MOVE "GLV0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Account " FUNCTION TRIM(GV-ACCOUNT)
                      " is a summary reporting heading, not a posting "
                      "account"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "GV-ACCOUNT" TO GV-FAIL-FIELD
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1200-EXIT
           END-IF
           IF GV-ACCOUNT-STATUS NOT = "ACTIVE"
               MOVE "GLV0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Account " FUNCTION TRIM(GV-ACCOUNT) " is "
                      FUNCTION TRIM(FUNCTION LOWER-CASE(
                      GV-ACCOUNT-STATUS))
                      " and may not be posted to"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "GV-ACCOUNT" TO GV-FAIL-FIELD
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1200-EXIT
           END-IF
           IF WS-A-VALIDFROM > WS-DATE-TEXT
               MOVE "GLV0006" TO CA-REASON-CODE
               MOVE "The account is not yet valid on that posting date"
                   TO CA-MESSAGE
               MOVE "GV-POSTING-DATE" TO GV-FAIL-FIELD
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1200-EXIT
           END-IF
           IF WS-A-VALIDTO NOT = SPACES
               IF WS-A-VALIDTO < WS-DATE-TEXT
                   MOVE "GLV0006" TO CA-REASON-CODE
                   MOVE "The account was retired before that posting d"
                       TO CA-MESSAGE
                   MOVE "GV-POSTING-DATE" TO GV-FAIL-FIELD
                   PERFORM 9000-REJECT THRU 9000-EXIT
                   GO TO 1200-EXIT
               END-IF
           END-IF
      *>   A control account belongs to its own subledger.  A manual
      *>   journal may not reach into it (FIN-005).
           IF GV-MANUAL-ENTRY = "Y" AND GV-ALLOW-MANUAL = "N"
               MOVE "GLV0007" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Account " FUNCTION TRIM(GV-ACCOUNT)
                      " is a control account for "
                      FUNCTION TRIM(GV-CONTROL-SUBLDG)
                      " and only that subledger may post to it"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "GV-ACCOUNT" TO GV-FAIL-FIELD
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1200-EXIT
           END-IF
           MOVE "Y" TO GV-VALID.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000 - A COMPLETE POSTING LINE: ACCOUNT PLUS ITS DIMENSIONS
      *>----------------------------------------------------------------
       2000-CHECK-LINE.
           PERFORM 1000-CHECK-ACCOUNT THRU 1000-EXIT
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
           MOVE "N" TO GV-VALID
           MOVE "CC" TO WS-MSG
           PERFORM 2100-READ-DIM-RULE THRU 2100-EXIT
           MOVE WS-D-REQUIREMENT TO GV-CC-REQUIREMENT
           PERFORM 2200-TEST-COST-CENTRE THRU 2200-EXIT
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
           MOVE "PROJ" TO WS-MSG
           PERFORM 2100-READ-DIM-RULE THRU 2100-EXIT
           MOVE WS-D-REQUIREMENT TO GV-PROJ-REQUIRED
           PERFORM 2300-TEST-PROJECT THRU 2300-EXIT
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
           MOVE "Y" TO GV-VALID.
       2000-EXIT.
           EXIT.

       2100-READ-DIM-RULE.
           MOVE SPACES TO WS-D-REQUIREMENT
           MOVE SPACES TO WS-D-ALLOWED
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT r.Requirement, ISNULL(r.AllowedValues,'') "
                  "FROM fin.AccountDimRule r WHERE r.CompanyCode = ? "
                  "AND r.AccountCode = ? AND r.DimensionCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE GV-ACCOUNT TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE WS-MSG(1:10) TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 200 TO SQLIO-COL-WIDTH(2)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DIM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "OPTIONAL" TO WS-D-REQUIREMENT
           END-IF.
       2100-EXIT.
           EXIT.

       2200-TEST-COST-CENTRE.
           IF GV-CC-REQUIREMENT = "MANDATORY"
             AND (GV-DIM-CC = SPACES OR GV-DIM-CC = LOW-VALUES)
               MOVE "GLV0010" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Line " FUNCTION TRIM(GV-FAIL-FIELD)
                   DELIMITED BY SIZE INTO WS-MSG
               STRING "A cost centre is mandatory on account "
                      FUNCTION TRIM(GV-ACCOUNT)
                      " and none was supplied"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "GV-DIM-CC" TO GV-FAIL-FIELD
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2200-EXIT
           END-IF
           IF GV-CC-REQUIREMENT = "FORBIDDEN"
             AND GV-DIM-CC NOT = SPACES
               MOVE "GLV0011" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Account " FUNCTION TRIM(GV-ACCOUNT)
                      " does not carry a cost centre"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "GV-DIM-CC" TO GV-FAIL-FIELD
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2200-EXIT
           END-IF
           IF GV-DIM-CC = SPACES
               GO TO 2200-EXIT
           END-IF
           MOVE "CC" TO WS-MSG
           MOVE GV-DIM-CC TO GV-CC-NAME
           PERFORM 2400-READ-DIM-VALUE THRU 2400-EXIT
           IF WS-FOUND = "N"
               MOVE "GLV0012" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Cost centre " FUNCTION TRIM(GV-DIM-CC)
                      " is not an active analysis code"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "GV-DIM-CC" TO GV-FAIL-FIELD
               MOVE SPACES TO GV-CC-NAME
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2200-EXIT
           END-IF
           MOVE WS-V-NAME TO GV-CC-NAME.
       2200-EXIT.
           EXIT.

       2300-TEST-PROJECT.
           IF GV-PROJ-REQUIRED = "MANDATORY"
             AND (GV-DIM-PROJ = SPACES OR GV-DIM-PROJ = LOW-VALUES)
               MOVE "GLV0013" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A project code is mandatory on account "
                      FUNCTION TRIM(GV-ACCOUNT)
                      " and none was supplied"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "GV-DIM-PROJ" TO GV-FAIL-FIELD
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2300-EXIT
           END-IF
           IF GV-PROJ-REQUIRED = "FORBIDDEN"
             AND GV-DIM-PROJ NOT = SPACES
               MOVE "GLV0014" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Account " FUNCTION TRIM(GV-ACCOUNT)
                      " does not carry a project code"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "GV-DIM-PROJ" TO GV-FAIL-FIELD
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2300-EXIT
           END-IF
           IF GV-DIM-PROJ = SPACES
               GO TO 2300-EXIT
           END-IF
           MOVE "PROJ" TO WS-MSG
           PERFORM 2400-READ-DIM-VALUE THRU 2400-EXIT
           IF WS-FOUND = "N"
               MOVE "GLV0015" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Project " FUNCTION TRIM(GV-DIM-PROJ)
                      " is not a permitted combination for account "
                      FUNCTION TRIM(GV-ACCOUNT)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "GV-DIM-PROJ" TO GV-FAIL-FIELD
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2300-EXIT
           END-IF
           MOVE WS-V-NAME TO GV-PROJ-NAME.
       2300-EXIT.
           EXIT.

       2400-READ-DIM-VALUE.
           MOVE "N" TO WS-FOUND
           MOVE SPACES TO WS-V-NAME
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT d.Name, CAST(d.IsActive AS char(1)) "
                  "FROM fin.DimensionValue d WHERE d.CompanyCode = ? "
                  "AND d.DimensionCode = ? AND d.DimValue = ? "
                  "AND d.IsActive = 1"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-MSG(1:10) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           IF WS-MSG(1:4) = "PROJ"
               MOVE GV-DIM-PROJ TO SQLIO-PARM-VAL(3)
           ELSE
               MOVE GV-DIM-CC TO SQLIO-PARM-VAL(3)
           END-IF
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 80 TO SQLIO-COL-WIDTH(1)
           MOVE 4 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-VAL-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2400-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               MOVE "Y" TO WS-FOUND
           END-IF.
       2400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000 - BUSINESS EVENT ACCOUNT MAPPING (FIN-003)
      *>        NO MAPPING MEANS THE EVENT IS BLOCKED, NOT DEFAULTED.
      *>----------------------------------------------------------------
       3000-CHECK-EVENT.
           IF GV-EVENT-CODE = SPACES OR GV-EVENT-CODE = LOW-VALUES
               MOVE "GLV0020" TO CA-REASON-CODE
               MOVE "A business event code is required" TO CA-MESSAGE
               MOVE "GV-EVENT-CODE" TO GV-FAIL-FIELD
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE 0 TO GV-LEG-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT m.LegCode, m.DrCr, m.AccountCode, "
                  "ISNULL(m.DimCostCentre,''), "
                  "ISNULL(m.DimProject,''), m.LegDescription "
                  "FROM fin.EventAccountMap m WHERE m.CompanyCode = ? "
                  "AND m.EventCode = ? AND m.IsActive = 1 "
                  "ORDER BY m.SortOrder"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE GV-EVENT-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 2 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 60 TO SQLIO-COL-WIDTH(6)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LEG-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3000-EXIT
           END-IF
           PERFORM 3100-FETCH-LEG THRU 3100-EXIT
               UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
                  OR GV-LEG-CNT NOT < 8
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LEG-ROW
           IF GV-LEG-CNT = 0
               MOVE "GLV0021" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No approved account mapping exists for event "
                      FUNCTION TRIM(GV-EVENT-CODE)
                      ". The posting is blocked rather than sent to a "
                      "suspense account"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "GV-EVENT-CODE" TO GV-FAIL-FIELD
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE "Y" TO GV-VALID.
       3000-EXIT.
           EXIT.

       3100-FETCH-LEG.
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LEG-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               GO TO 3100-EXIT
           END-IF
           ADD 1 TO GV-LEG-CNT
           MOVE GV-LEG-CNT TO WS-IDX
           MOVE WS-G-LEG TO GV-L-LEG-CODE(WS-IDX)
           MOVE WS-G-DRCR TO GV-L-DR-CR(WS-IDX)
           MOVE WS-G-ACCOUNT TO GV-L-ACCOUNT(WS-IDX)
           MOVE WS-G-CC TO GV-L-DIM-CC(WS-IDX)
           MOVE WS-G-PROJ TO GV-L-DIM-PROJ(WS-IDX)
           MOVE WS-G-DESC TO GV-L-DESC(WS-IDX).
       3100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000 - MAY THE ACCOUNT BE RETIRED OR DELETED? (FIN-001)
      *>        A REFERENCED ACCOUNT IS NEVER DELETED, SO PREVIOUSLY
      *>        ISSUED STATEMENTS CANNOT SILENTLY CHANGE.
      *>----------------------------------------------------------------
       4000-CHECK-USAGE.
           PERFORM 1100-READ-ACCOUNT THRU 1100-EXIT
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT_BIG(*) FROM fin.JournalLine l "
                  "WHERE l.CompanyCode = ? AND l.AccountCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE GV-ACCOUNT TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 18 TO SQLIO-COL-WIDTH(1)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CNT-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 4000-EXIT
           END-IF
           COMPUTE WS-COUNT = FUNCTION NUMVAL(WS-C-COUNT)
           MOVE WS-COUNT TO GV-USE-COUNT
           MOVE "Y" TO GV-MAY-RETIRE
           IF WS-COUNT > 0
               MOVE "N" TO GV-MAY-DELETE
               MOVE 4 TO CA-RETURN-CODE
               MOVE "GLV0022" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Account " FUNCTION TRIM(GV-ACCOUNT)
                      " carries posted history and may be retired but "
                      "never deleted or reclassified"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           ELSE
               MOVE "Y" TO GV-MAY-DELETE
           END-IF
           MOVE "Y" TO GV-VALID.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 9000 - REFUSALS
      *>----------------------------------------------------------------
       9000-REJECT.
           MOVE "Y" TO WS-REJECTED
           MOVE "N" TO GV-VALID
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE "GLV0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "GLVALID does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE "N" TO GV-VALID
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in GLVALID" TO CA-MESSAGE
           END-IF
           MOVE 12 TO CA-RETURN-CODE
           MOVE "Y" TO WS-REJECTED
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
