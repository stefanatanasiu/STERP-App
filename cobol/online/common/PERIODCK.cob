      *>================================================================
      *> PROGRAM-ID : PERIODCK
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : POSTING DATE AND PERIOD POLICY (COM-009).
      *>              DATE VALIDATION MUST DISTINGUISH THE BUSINESS
      *>              EVENT DATE (WHEN SOMETHING HAPPENED), THE TAX
      *>              POINT (WHEN VAT IS DUE) AND THE POSTING DATE
      *>              (WHICH LEDGER PERIOD IT LANDS IN).  BACKDATING A
      *>              DOCUMENT DOES NOT AUTHORISE POSTING INTO A
      *>              CLOSED PERIOD.
      *>              THE PERIOD STATUS COMES FROM usp_ValidatePeriod
      *>              WHICH RAISES A BUSINESS ERROR FOR HARD, FUTURE,
      *>              UNKNOWN AND UNAUTHORISED SOFT PERIODS.  THIS
      *>              PROGRAM TURNS THAT INTO A CLEAN RETURN CODE:
      *>                00  post it
      *>                06  soft closed - an authorised close
      *>                    adjustment is possible (D-15)
      *>                08  hard closed, future or no period at all
      *> CALLED BY  : FIN, AR, AP, CST, KIT AND THE .NET CLIENT
      *> INTERFACE  : CALL "PERIODCK" USING WS-PERIODCK-CA
      *>              COPYBOOK PERIODCKCA
      *> CALLS      : SQLIO -> fin.usp_ValidatePeriod
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-01-24 RJH  CR0210 PERIOD CONTROL FOR THE NOMINAL LEDGER
      *> 1999-11-30 KMP  Y2K - PERIOD KEY WIDENED TO YYYYPP
      *> 2026-10-05 STERP fin.Period AND usp_ValidatePeriod, SOFT CLOSE
      *>================================================================
      *> REQ: COM-009
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERIODCK.
       AUTHOR. R J HARGREAVES.
       DATE-WRITTEN. 24/01/1994.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       01  WS-FLAGS.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
       01  WS-WORK.
           05  WS-FY                   PIC S9(9) VALUE 0.
           05  WS-PN                   PIC S9(9) VALUE 0.
       01  WS-PERIOD-KEY.
           05  WS-PK-YEAR              PIC 9(4).
           05  WS-PK-PERIOD            PIC 9(2).
       01  WS-ROW.
           05  WS-V-COMPANY            PIC X(10).
           05  WS-V-FY                 PIC X(8).
           05  WS-V-PN                 PIC X(8).
           05  WS-V-STATUS             PIC X(10).
           05  WS-V-ALLOWED            PIC X(4).
           05  WS-V-ACTION             PIC X(12).
       LINKAGE SECTION.
       COPY PERIODCKCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE-DATES
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "CHECK"    PERFORM 1000-CHECK-PERIOD
               WHEN "DATES"    PERFORM 1000-CHECK-PERIOD
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE WS-WORK
           MOVE "N" TO WS-ERROR
           MOVE "PERIODCK" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "COM-009" TO CA-REQ-REF
           MOVE 0 TO CA-MSG-COUNT
           MOVE 0 TO PC-FISCAL-YEAR PC-PERIOD-NO
           MOVE SPACES TO PC-PERIOD-STATUS PC-PERIOD-YYYYPP
           MOVE SPACES TO PC-REASON-TEXT
           MOVE "N" TO PC-ALLOWED
           MOVE "FIN.PERIOD.SOFTPOST" TO PC-OVERRIDE-PERM
           IF PC-ACTION = SPACES OR PC-ACTION = LOW-VALUES
               MOVE "POST" TO PC-ACTION
           END-IF
           MOVE FUNCTION UPPER-CASE(PC-ACTION) TO PC-ACTION
           IF PC-POSTING-DATE = 0
               MOVE CA-BUSINESS-DATE TO PC-POSTING-DATE
           END-IF.
       0100-EXIT.
           EXIT.

      *>   COM-009 - the three dates are separate and each must be a
      *>   real date before any period question is asked
       0200-VALIDATE-DATES SECTION.
       0200-START.
           IF CA-COMPANY = SPACES OR CA-COMPANY = LOW-VALUES
               MOVE "PER0001" TO CA-REASON-CODE
               MOVE "A company code is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF FUNCTION TEST-DATE-YYYYMMDD(PC-POSTING-DATE) NOT = 0
               MOVE "PER0002" TO CA-REASON-CODE
               MOVE "The posting date is not a valid calendar date"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF PC-DOCUMENT-DATE NOT = 0
              AND FUNCTION TEST-DATE-YYYYMMDD(PC-DOCUMENT-DATE) NOT = 0
               MOVE "PER0003" TO CA-REASON-CODE
               MOVE "The document date is not a valid calendar date"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF PC-TAX-POINT NOT = 0
              AND FUNCTION TEST-DATE-YYYYMMDD(PC-TAX-POINT) NOT = 0
               MOVE "PER0004" TO CA-REASON-CODE
               MOVE "The tax point is not a valid calendar date"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-CHECK-PERIOD SECTION.
       1000-START.
      *>     fin.usp_ValidatePeriod answers only "yes"; it reports every
      *>     refusal with THROW, so it cannot tell a hard close (08)
      *>     from a soft close that an authorised user may still post
      *>     into (06), and because it runs with SET XACT_ABORT ON the
      *>     raise would also doom the caller's posting transaction.
      *>     COM-009 needs the distinction, so the period status is
      *>     read here and the policy applied below.  The rule is kept
      *>     the same as the procedure, including the soft close
      *>     permission.  See docs\tech-debt\common.md.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) p.CompanyCode, "
                  "CAST(p.FiscalYear AS varchar(8)), "
                  "CAST(p.PeriodNo AS varchar(8)), p.Status, "
                  "CAST(core.fn_HasPermission(?, "
                  "'FIN.PERIOD.SOFTPOST', p.CompanyCode, NULL) "
                  "AS varchar(4)), ? FROM fin.Period p "
                  "WHERE p.CompanyCode = ? "
                  "AND ? BETWEEN p.StartDate AND p.EndDate"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           IF CA-USER = SPACES
               MOVE SPACES TO SQLIO-PARM-VAL(1)
               MOVE "Y" TO SQLIO-PARM-NULL(1)
           ELSE
               MOVE CA-USER TO SQLIO-PARM-VAL(1)
               MOVE "N" TO SQLIO-PARM-NULL(1)
           END-IF
           MOVE PC-ACTION TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE PC-POSTING-DATE TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 8 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-COL-WIDTH(3)
           MOVE 10 TO SQLIO-COL-WIDTH(4)
           MOVE 4 TO SQLIO-COL-WIDTH(5)
           MOVE 12 TO SQLIO-COL-WIDTH(6)
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "PER0005" TO CA-REASON-CODE
               MOVE "No financial period covers that posting date"
                   TO CA-MESSAGE
               MOVE CA-MESSAGE TO PC-REASON-TEXT
               PERFORM 9000-REJECT
               GO TO 1000-EXIT
           END-IF
           COMPUTE WS-FY = FUNCTION NUMVAL(WS-V-FY)
           COMPUTE WS-PN = FUNCTION NUMVAL(WS-V-PN)
           MOVE WS-FY TO PC-FISCAL-YEAR
           MOVE WS-PN TO PC-PERIOD-NO
           MOVE WS-FY TO WS-PK-YEAR
           MOVE WS-PN TO WS-PK-PERIOD
           MOVE WS-PERIOD-KEY TO PC-PERIOD-YYYYPP
           MOVE WS-V-STATUS TO PC-PERIOD-STATUS
           PERFORM 1100-APPLY-PERIOD-POLICY.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1100 - COM-009 period policy.  Backdating a document does not
      *> authorise posting into a closed period, so the answer carries
      *> the status and, where one exists, the authorised route (D-15 -
      *> only the financial controller reopens a period).
      *>----------------------------------------------------------------
      *> REQ: COM-009
       1100-APPLY-PERIOD-POLICY SECTION.
       1100-START.
           MOVE SPACES TO CA-MESSAGE
           EVALUATE PC-PERIOD-STATUS
               WHEN "OPEN"
                   MOVE "Y" TO PC-ALLOWED
                   MOVE 0 TO CA-RETURN-CODE
                   STRING "Posting date " PC-POSTING-DATE
                          " is in period " PC-PERIOD-YYYYPP " (OPEN)"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               WHEN "SOFT"
      *>             a soft closed period is still postable, but only
      *>             by somebody holding the close adjustment authority
                   IF WS-V-ALLOWED = "1"
                       MOVE "Y" TO PC-ALLOWED
                       MOVE 0 TO CA-RETURN-CODE
                       STRING "Posting date " PC-POSTING-DATE
                              " is in period " PC-PERIOD-YYYYPP
                              " (SOFT) - posted as a close adjustment"
                           DELIMITED BY SIZE INTO CA-MESSAGE
                   ELSE
                       MOVE "N" TO PC-ALLOWED
                       MOVE 6 TO CA-RETURN-CODE
                       MOVE "PER0006" TO CA-REASON-CODE
                       MOVE "FIN.PERIOD.SOFTPOST" TO PC-OVERRIDE-PERM
                       ADD 1 TO CA-MSG-COUNT
                       STRING "Period " PC-PERIOD-YYYYPP
                              " is soft closed - only an authorised "
                              "close adjustment may be posted into it"
                           DELIMITED BY SIZE INTO CA-MESSAGE
                   END-IF
               WHEN "HARD"
                   MOVE "N" TO PC-ALLOWED
                   MOVE "PER0003" TO CA-REASON-CODE
                   STRING "Period " PC-PERIOD-YYYYPP " is hard closed"
                          " - post the correction in the current open"
                          " period or ask the financial controller to"
                          " reopen it"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   PERFORM 9000-REJECT
               WHEN "FUTURE"
                   MOVE "N" TO PC-ALLOWED
                   MOVE "PER0004" TO CA-REASON-CODE
                   STRING "Period " PC-PERIOD-YYYYPP
                          " is not yet open for posting"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   PERFORM 9000-REJECT
               WHEN OTHER
                   MOVE "N" TO PC-ALLOWED
                   MOVE "PER0007" TO CA-REASON-CODE
                   STRING "Period " PC-PERIOD-YYYYPP
                          " has an unknown status "
                          FUNCTION TRIM(PC-PERIOD-STATUS)
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   PERFORM 9000-REJECT
           END-EVALUATE
           MOVE CA-MESSAGE TO PC-REASON-TEXT.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE "N" TO PC-ALLOWED
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "PER0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "PERIODCK does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

      *>   fin.usp_ValidatePeriod refuses with THROW 50009 and a
      *>   STERP-E-0009 message.  A soft close is the one refusal with
      *>   an authorised route, so it answers 06 rather than 08.
       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE "N" TO PC-ALLOWED
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in PERIODCK" TO CA-MESSAGE
           END-IF
           MOVE CA-MESSAGE TO PC-REASON-TEXT
           EVALUATE TRUE
               WHEN CA-MESSAGE(1:8) NOT = "STERP-E-"
                   MOVE "SQL0001" TO CA-REASON-CODE
                   MOVE 12 TO CA-RETURN-CODE
               WHEN CA-MESSAGE(1:200) = SPACES
                   MOVE "PER0006" TO CA-REASON-CODE
                   MOVE 8 TO CA-RETURN-CODE
               WHEN OTHER
                   MOVE "PER0006" TO CA-REASON-CODE
                   MOVE "CLOSED" TO PC-PERIOD-STATUS
                   MOVE 8 TO CA-RETURN-CODE
                   PERFORM 9400-SOFT-CLOSE-TEST
           END-EVALUATE
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.

       9400-SOFT-CLOSE-TEST SECTION.
       9400-START.
           IF CA-MESSAGE(1:200) NOT = SPACES
               INSPECT CA-MESSAGE TALLYING CA-MSG-COUNT
                   FOR ALL "soft closed"
           END-IF
           IF CA-MSG-COUNT > 0
               MOVE "SOFT" TO PC-PERIOD-STATUS
               MOVE "PER0007" TO CA-REASON-CODE
               MOVE 6 TO CA-RETURN-CODE
           END-IF
           MOVE 0 TO CA-MSG-COUNT.
       9400-EXIT.
           EXIT.
