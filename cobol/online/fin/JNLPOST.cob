      *>================================================================
      *> PROGRAM-ID : JNLPOST
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER
      *> PURPOSE    : POST AND REVERSE JOURNALS.  THE POSTING ITSELF IS
      *>              OWNED BY fin.usp_PostJournal, WHICH CONSUMES THE
      *>              LINES STAGED UNDER THE CORRELATION ID AND IS
      *>              ALL OR NOTHING.  REPEATING THE SAME INSTRUCTION
      *>              RETURNS THE ORIGINAL JOURNAL INSTEAD OF POSTING A
      *>              SECOND ONE (COM-015).
      *>              A CORRECTION IS NEVER AN AMENDMENT: IT IS A NEW
      *>              LINKED REVERSAL, FULL OR PARTIAL, THAT CANNOT
      *>              EXCEED THE UNREVERSED ORIGINAL AND MUST USE A
      *>              PERMITTED PERIOD (FIN-006).
      *> CALLED BY  : frmJournalEntry, RECURJNL, YEAREND, REVALRUN
      *> INTERFACE  : CALL "JNLPOST" USING WS-JNLPOST-CA
      *>              COPYBOOK JNLPOSTCA
      *> CALLS      : PERMCHK, PERIODCK, AUDITLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-07-02 APW CR0093 ORIGINAL - MICRO FOCUS COBOL/2
      *> 1996-02-11 RJH CR0412 PARTIAL REVERSALS REQUIRE AUTHORITY
      *> 1999-11-30 KMP Y2K - 4 DIGIT YEARS
      *> 2026-10-05 STERP IDEMPOTENT POSTING, REVERSIBLE BALANCE
      *>================================================================
      *> REQ: FIN-004, FIN-005, FIN-006
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JNLPOST.
       AUTHOR. A P WHITTAKER.
       DATE-WRITTEN. 02/07/1994.
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
       01  WS-WORK.
           05  WS-IDX                  PIC S9(4) COMP-5 VALUE 0.
           05  WS-AMT                  PIC S9(13)V99 VALUE 0.
           05  WS-PCT                  PIC S9(5)V9(4) VALUE 0.
           05  WS-POST-DATE            PIC 9(8) VALUE 0.
           05  WS-MONEY-ED             PIC -(11)9.99.
       01  WS-PER-ROW.
           05  WS-R-STATUS             PIC X(10).
           05  WS-R-ALLOWED            PIC X(4).
           05  WS-R-FY                 PIC X(8).
           05  WS-R-PERIOD             PIC X(8).
       01  WS-POST-ROW.
           05  WS-P-JOURNAL-ID         PIC X(18).
           05  WS-P-JOURNAL-NO         PIC X(30).
           05  WS-P-COMPANY            PIC X(10).
           05  WS-P-FY                 PIC X(8).
           05  WS-P-PERIOD             PIC X(8).
           05  WS-P-DR                 PIC X(22).
           05  WS-P-CR                 PIC X(22).
           05  WS-P-STATUS             PIC X(12).
           05  WS-P-DUPLICATE          PIC X(4).
       01  WS-ENQ-ROW.
           05  WS-E-JOURNAL-ID         PIC X(18).
           05  WS-E-TYPE               PIC X(12).
           05  WS-E-DESC               PIC X(200).
           05  WS-E-POSTDATE           PIC X(8).
           05  WS-E-FY                 PIC X(8).
           05  WS-E-PERIOD             PIC X(8).
           05  WS-E-DR                 PIC X(22).
           05  WS-E-CR                 PIC X(22).
           05  WS-E-REVERSED           PIC X(22).
           05  WS-E-STATUS             PIC X(12).
           05  WS-E-ISREV              PIC X(4).
           05  WS-E-LINES              PIC X(10).
       LINKAGE SECTION.
       COPY JNLPOSTCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           PERFORM 0200-EDIT-REQUEST THRU 0200-EXIT
           IF WS-IS-REJECTED
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "POST"     PERFORM 1000-POST THRU 1000-EXIT
               WHEN "REVERSE"  PERFORM 2000-REVERSE THRU 2000-EXIT
               WHEN "ENQUIRE"  PERFORM 3000-ENQUIRE THRU 3000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.

       0100-INITIALISE.
           MOVE "N" TO WS-REJECTED
           MOVE "JNLPOST" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "FIN-005" TO CA-REQ-REF
           MOVE 0 TO JP-JOURNAL-ID
           MOVE 0 TO JP-FISCAL-YEAR
           MOVE 0 TO JP-PERIOD-NO
           MOVE 0 TO JP-TOTAL-DR-GBP
           MOVE 0 TO JP-TOTAL-CR-GBP
           MOVE 0 TO JP-LINE-COUNT
           MOVE SPACES TO JP-NEW-JOURNAL-NO
           MOVE SPACES TO JP-STATUS
           MOVE "N" TO JP-WAS-DUPLICATE
           MOVE "N" TO JP-IS-REVERSAL
           IF JP-CURRENCY = SPACES OR JP-CURRENCY = LOW-VALUES
               MOVE "GBP" TO JP-CURRENCY
           END-IF
           IF JP-JOURNAL-TYPE = SPACES
               MOVE "GL" TO JP-JOURNAL-TYPE
           END-IF
           IF JP-SOURCE-MODULE = SPACES
               MOVE "FIN" TO JP-SOURCE-MODULE
           END-IF
           IF JP-SERIES-CODE = SPACES
               IF CA-FUNCTION = "REVERSE"
                   MOVE "JNLR" TO JP-SERIES-CODE
               ELSE
                   MOVE "JNL" TO JP-SERIES-CODE
               END-IF
           END-IF
           IF JP-POSTING-DATE = 0
               MOVE CA-BUSINESS-DATE TO JP-POSTING-DATE
           END-IF
           IF JP-DOCUMENT-DATE = 0
               MOVE JP-POSTING-DATE TO JP-DOCUMENT-DATE
           END-IF.
       0100-EXIT.
           EXIT.

       0200-EDIT-REQUEST.
           IF CA-COMPANY = SPACES
               MOVE "JNP0001" TO CA-REASON-CODE
               MOVE "A company is required to post a journal"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF CA-CORRELATION-ID = SPACES
             AND CA-FUNCTION NOT = "ENQUIRE"
               MOVE "JNP0002" TO CA-REASON-CODE
               MOVE "A correlation id is required so a repeated postin"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000 - POST THE STAGED LINES
      *>----------------------------------------------------------------
       1000-POST.
           MOVE "FIN.JOURNAL.POST" TO SQLIO-STMT(1:30)
           PERFORM 1100-AUTHORITY THRU 1100-EXIT
           IF WS-IS-REJECTED
               GO TO 1000-EXIT
           END-IF
           PERFORM 2050-PERIOD-DECISION THRU 2050-EXIT
           IF WS-IS-REJECTED OR CA-RETURN-CODE = 12
               GO TO 1000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC fin.usp_PostJournal ?, ?, ?, ?, ?, ?, ?, ?, "
                  "?, ?, ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 14 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE JP-DESCRIPTION TO SQLIO-PARM-VAL(4)
           MOVE JP-POSTING-DATE TO SQLIO-PARM-VAL(5)
           MOVE JP-JOURNAL-TYPE TO SQLIO-PARM-VAL(6)
           MOVE JP-DOCUMENT-DATE TO SQLIO-PARM-VAL(7)
           MOVE JP-CURRENCY TO SQLIO-PARM-VAL(8)
           MOVE JP-SOURCE-MODULE TO SQLIO-PARM-VAL(9)
           MOVE JP-SOURCE-REF TO SQLIO-PARM-VAL(10)
           MOVE JP-EVENT-CODE TO SQLIO-PARM-VAL(11)
           MOVE JP-SERIES-CODE TO SQLIO-PARM-VAL(12)
           IF JP-FX-RATE = 0
               MOVE SPACES TO SQLIO-PARM-VAL(13)
           ELSE
               MOVE JP-FX-RATE TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           END-IF
           MOVE JP-EVIDENCE-REF TO SQLIO-PARM-VAL(14)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           IF JP-FX-RATE = 0
               MOVE "Y" TO SQLIO-PARM-NULL(13)
           END-IF
           IF JP-SOURCE-REF = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(10)
           END-IF
           IF JP-EVENT-CODE = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(11)
           END-IF
           IF JP-EVIDENCE-REF = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(14)
           END-IF
           MOVE 9 TO SQLIO-COL-CNT
           MOVE 18 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           MOVE 8 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 22 TO SQLIO-COL-WIDTH(6)
           MOVE 22 TO SQLIO-COL-WIDTH(7)
           MOVE 12 TO SQLIO-COL-WIDTH(8)
           MOVE 4 TO SQLIO-COL-WIDTH(9)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-POST-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "JNP0003" TO CA-REASON-CODE
               MOVE "The posting returned no result row" TO CA-MESSAGE
               MOVE 12 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 1000-EXIT
           END-IF
           PERFORM 1500-TAKE-RESULT THRU 1500-EXIT
           PERFORM 1600-MARK-DRAFT THRU 1600-EXIT
           PERFORM 1700-AUDIT THRU 1700-EXIT.
       1000-EXIT.
           EXIT.

       1100-AUTHORITY.
           INITIALIZE WS-PERMCHK-CA
           MOVE "CHECK" TO KH-FUNCTION
           MOVE CA-COMPANY TO KH-COMPANY
           MOVE CA-SITE TO KH-SITE
           MOVE CA-USER TO KH-USER
           MOVE CA-BUSINESS-DATE TO KH-BUSINESS-DATE
           MOVE SQLIO-STMT(1:30) TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERMCHK-CA
           IF KH-RETURN-CODE > 4
               MOVE KH-REASON-CODE TO CA-REASON-CODE
               MOVE KH-MESSAGE TO CA-MESSAGE
               MOVE KH-RETURN-CODE TO CA-RETURN-CODE
               MOVE "Y" TO WS-REJECTED
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       1100-EXIT.
           EXIT.

       1500-TAKE-RESULT.
           COMPUTE JP-JOURNAL-ID = FUNCTION NUMVAL(WS-P-JOURNAL-ID)
           MOVE WS-P-JOURNAL-NO TO JP-NEW-JOURNAL-NO
           MOVE WS-P-JOURNAL-NO TO JP-JOURNAL-NUMBER
           COMPUTE JP-FISCAL-YEAR = FUNCTION NUMVAL(WS-P-FY)
           COMPUTE JP-PERIOD-NO = FUNCTION NUMVAL(WS-P-PERIOD)
           COMPUTE JP-TOTAL-DR-GBP = FUNCTION NUMVAL(WS-P-DR)
           COMPUTE JP-TOTAL-CR-GBP = FUNCTION NUMVAL(WS-P-CR)
           MOVE WS-P-STATUS TO JP-STATUS
           IF WS-P-DUPLICATE(1:1) = "1"
               MOVE "Y" TO JP-WAS-DUPLICATE
               MOVE 4 TO CA-RETURN-CODE
               MOVE "JNP0007" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "That instruction had already been carried out. "
                      "Journal " FUNCTION TRIM(JP-NEW-JOURNAL-NO)
                      " is the original result, and no second entry "
                      "has been made"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           ELSE
               MOVE "N" TO JP-WAS-DUPLICATE
               MOVE SPACES TO CA-MESSAGE
               MOVE JP-TOTAL-DR-GBP TO WS-MONEY-ED
               STRING "Journal " FUNCTION TRIM(JP-NEW-JOURNAL-NO)
                      " posted, " FUNCTION TRIM(WS-MONEY-ED)
                      " GBP each side"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       1500-EXIT.
           EXIT.

       1600-MARK-DRAFT.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.JournalDraft SET Status = 'POSTED', "
                  "PostedJournalNo = ?, ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE CorrelationId = ? "
                  "AND Status <> 'POSTED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE JP-NEW-JOURNAL-NO TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-POST-ROW.
       1600-EXIT.
           EXIT.

       1700-AUDIT.
           INITIALIZE WS-AUDITLOG-CA
           MOVE "WRITE" TO AH-FUNCTION
           MOVE CA-COMPANY TO AH-COMPANY
           MOVE CA-USER TO AH-USER
           MOVE CA-BUSINESS-DATE TO AH-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO AH-CORRELATION-ID
           MOVE "fin.Journal" TO AU-ENTITY-TYPE
           MOVE JP-NEW-JOURNAL-NO TO AU-ENTITY-KEY
           IF CA-FUNCTION = "REVERSE"
               MOVE "REVERSE" TO AU-ACTION
               MOVE JP-REASON TO AU-REASON
           ELSE
               MOVE "POST" TO AU-ACTION
               MOVE JP-DESCRIPTION TO AU-REASON
           END-IF
           MOVE JP-TOTAL-DR-GBP TO WS-MONEY-ED
           MOVE WS-MONEY-ED TO AU-NEW-VALUE
           MOVE "COBOL" TO AU-SOURCE
           CALL "AUDITLOG" USING WS-AUDITLOG-CA.
       1700-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000 - LINKED REVERSAL (FULL OR PARTIAL)
      *>----------------------------------------------------------------
       2000-REVERSE.
           MOVE "FIN.JOURNAL.REVERSE" TO SQLIO-STMT(1:30)
           PERFORM 1100-AUTHORITY THRU 1100-EXIT
           IF WS-IS-REJECTED
               GO TO 2000-EXIT
           END-IF
           IF JP-JOURNAL-NUMBER = SPACES
               MOVE "JNP0010" TO CA-REASON-CODE
               MOVE "The journal number to reverse is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF JP-REASON = SPACES OR JP-REASON = LOW-VALUES
               MOVE "JNP0011" TO CA-REASON-CODE
               MOVE "A reversal must carry the reason for the correcti"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 3000-ENQUIRE THRU 3000-EXIT
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
           PERFORM 2050-PERIOD-DECISION THRU 2050-EXIT
           IF WS-IS-REJECTED OR CA-RETURN-CODE = 12
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-EDIT-AMOUNT THRU 2100-EXIT
           IF WS-IS-REJECTED
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC fin.usp_ReverseJournal ?, ?, ?, ?, ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 8 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE JP-JOURNAL-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE JP-REASON TO SQLIO-PARM-VAL(4)
           MOVE JP-POSTING-DATE TO SQLIO-PARM-VAL(5)
           MOVE WS-PCT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(7)
           MOVE JP-SERIES-CODE TO SQLIO-PARM-VAL(8)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 9 TO SQLIO-COL-CNT
           MOVE 18 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           MOVE 8 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 22 TO SQLIO-COL-WIDTH(6)
           MOVE 22 TO SQLIO-COL-WIDTH(7)
           MOVE 12 TO SQLIO-COL-WIDTH(8)
           MOVE 4 TO SQLIO-COL-WIDTH(9)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-POST-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "JNP0012" TO CA-REASON-CODE
               MOVE "The reversal returned no result row" TO CA-MESSAGE
               MOVE 12 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 2000-EXIT
           END-IF
           MOVE "Y" TO JP-IS-REVERSAL
           PERFORM 1500-TAKE-RESULT THRU 1500-EXIT
           MOVE SPACES TO CA-MESSAGE
           MOVE JP-TOTAL-DR-GBP TO WS-MONEY-ED
           STRING "Reversal " FUNCTION TRIM(JP-NEW-JOURNAL-NO)
                  " posted for " FUNCTION TRIM(WS-MONEY-ED)
                  " GBP. The original entry is unchanged"
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 1700-AUDIT THRU 1700-EXIT.
       2000-EXIT.
           EXIT.

      *> COM-009 / FIN-010: decide the period with a query, never by
      *> letting fin.usp_PostJournal raise.  A THROW under XACT_ABORT
      *> dooms whatever unit of work the caller has open, so every
      *> later statement on the same connection fails as well.  The
      *> period policy is therefore read first and the request is
      *> refused here, leaving the transaction clean (D-15).
       2050-PERIOD-DECISION.
           MOVE JP-POSTING-DATE TO WS-POST-DATE
           IF WS-POST-DATE = 0
               MOVE CA-BUSINESS-DATE TO WS-POST-DATE
               MOVE WS-POST-DATE TO JP-POSTING-DATE
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT p.Status, CAST(CASE WHEN p.Status = 'OPEN' "
                  "THEN 1 WHEN p.Status = 'SOFT' AND "
                  "core.fn_HasPermission(?, 'FIN.PERIOD.SOFTPOST', ?, "
                  "NULL) = 1 THEN 1 ELSE 0 END AS char(1)), "
                  "CAST(p.FiscalYear AS varchar(8)), "
                  "CAST(p.PeriodNo AS varchar(8)) FROM fin.Period p "
                  "WHERE p.CompanyCode = ? AND ? BETWEEN p.StartDate "
                  "AND p.EndDate"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE WS-POST-DATE TO SQLIO-PARM-VAL(4)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 4 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-COL-WIDTH(3)
           MOVE 8 TO SQLIO-COL-WIDTH(4)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PER-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2050-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "JNP0016" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No financial period covers posting date "
                      WS-POST-DATE(7:2) "/" WS-POST-DATE(5:2) "/"
                      WS-POST-DATE(1:4) " for this company"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2050-EXIT
           END-IF
           COMPUTE JP-FISCAL-YEAR = FUNCTION NUMVAL(WS-R-FY)
           COMPUTE JP-PERIOD-NO = FUNCTION NUMVAL(WS-R-PERIOD)
           IF WS-R-ALLOWED(1:1) = "1"
               GO TO 2050-EXIT
           END-IF
           MOVE SPACES TO CA-MESSAGE
           IF WS-R-STATUS = "SOFT"
               MOVE "JNP0017" TO CA-REASON-CODE
               STRING "Period " FUNCTION TRIM(WS-R-PERIOD)
                      " is soft closed. Only an authorised close "
                      "adjustment may be posted into it"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 6 TO CA-RETURN-CODE
               MOVE "Y" TO WS-REJECTED
               ADD 1 TO CA-MSG-COUNT
           ELSE
               MOVE "JNP0018" TO CA-REASON-CODE
               STRING "Period " FUNCTION TRIM(WS-R-PERIOD)
                      " is " FUNCTION TRIM(WS-R-STATUS)
                      " closed for posting. Post the entry in the "
                      "current open period"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       2050-EXIT.
           EXIT.

      *> The reversal may be expressed as a percentage or as an amount.
      *> Either way it cannot exceed what is left of the original
      *> (FIN-006): the remaining reversible balance rules.
       2100-EDIT-AMOUNT.
           IF JP-REVERSE-AMOUNT NOT = 0
               IF JP-TOTAL-DR-GBP = 0
                   MOVE 0 TO WS-PCT
               ELSE
                   COMPUTE WS-PCT ROUNDED =
                       JP-REVERSE-AMOUNT * 100 / JP-TOTAL-DR-GBP
               END-IF
           ELSE
               IF JP-REVERSE-PCT = 0
                   MOVE 100 TO WS-PCT
               ELSE
                   MOVE JP-REVERSE-PCT TO WS-PCT
               END-IF
               COMPUTE JP-REVERSE-AMOUNT ROUNDED =
                   JP-TOTAL-DR-GBP * WS-PCT / 100
           END-IF
           IF WS-PCT NOT > 0
               MOVE "JNP0013" TO CA-REASON-CODE
               MOVE "A reversal must have a positive value"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2100-EXIT
           END-IF
           IF JP-REVERSE-AMOUNT > JP-REVERSIBLE-GBP
               MOVE "JNP0014" TO CA-REASON-CODE
               MOVE JP-REVERSIBLE-GBP TO WS-MONEY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "The reversal exceeds the unreversed value of "
                      "the original. Only "
                      FUNCTION TRIM(WS-MONEY-ED)
                      " GBP remains reversible"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000 - HEADER, TOTALS AND REMAINING REVERSIBLE BALANCE
      *>----------------------------------------------------------------
       3000-ENQUIRE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(j.JournalId AS varchar(18)), "
                  "j.JournalType, j.Description, "
                  "CONVERT(char(8), j.PostingDate, 112), "
                  "CAST(j.FiscalYear AS varchar(8)), "
                  "CAST(j.PeriodNo AS varchar(8)), j.TotalDebitGBP, "
                  "j.TotalCreditGBP, j.ReversedAmountGBP, j.Status, "
                  "CAST(j.IsReversal AS char(1)), "
                  "CAST((SELECT COUNT(*) FROM fin.JournalLine l WHERE "
                  "l.JournalId = j.JournalId) AS varchar(10)) "
                  "FROM fin.Journal j WHERE j.CompanyCode = ? "
                  "AND j.JournalNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE JP-JOURNAL-NUMBER TO SQLIO-PARM-VAL(2)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 12 TO SQLIO-COL-CNT
           MOVE 18 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 200 TO SQLIO-COL-WIDTH(3)
           MOVE 8 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 8 TO SQLIO-COL-WIDTH(6)
           MOVE 22 TO SQLIO-COL-WIDTH(7)
           MOVE 22 TO SQLIO-COL-WIDTH(8)
           MOVE 22 TO SQLIO-COL-WIDTH(9)
           MOVE 12 TO SQLIO-COL-WIDTH(10)
           MOVE 4 TO SQLIO-COL-WIDTH(11)
           MOVE 10 TO SQLIO-COL-WIDTH(12)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ENQ-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "JNP0015" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Journal " FUNCTION TRIM(JP-JOURNAL-NUMBER)
                      " is not held for " FUNCTION TRIM(CA-COMPANY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 3000-EXIT
           END-IF
           COMPUTE JP-JOURNAL-ID = FUNCTION NUMVAL(WS-E-JOURNAL-ID)
           MOVE WS-E-TYPE TO JP-JOURNAL-TYPE
           MOVE WS-E-DESC TO JP-DESCRIPTION
           COMPUTE JP-FISCAL-YEAR = FUNCTION NUMVAL(WS-E-FY)
           COMPUTE JP-PERIOD-NO = FUNCTION NUMVAL(WS-E-PERIOD)
           COMPUTE JP-TOTAL-DR-GBP = FUNCTION NUMVAL(WS-E-DR)
           COMPUTE JP-TOTAL-CR-GBP = FUNCTION NUMVAL(WS-E-CR)
           COMPUTE JP-REVERSED-GBP = FUNCTION NUMVAL(WS-E-REVERSED)
           COMPUTE JP-REVERSIBLE-GBP =
               JP-TOTAL-DR-GBP - JP-REVERSED-GBP
           MOVE WS-E-STATUS TO JP-STATUS
           COMPUTE JP-LINE-COUNT = FUNCTION NUMVAL(WS-E-LINES)
           IF WS-E-ISREV(1:1) = "1"
               MOVE "Y" TO JP-IS-REVERSAL
           END-IF.
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
           MOVE "JNP0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "JNLPOST does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

      *> A business refusal raised by the posting procedure arrives as
      *> a THROW, so STERP-E- messages are reported as 08 (no business
      *> effect) rather than 12 (system failure) - COM-017.
       9300-SQL-FAILED.
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in JNLPOST" TO CA-MESSAGE
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
