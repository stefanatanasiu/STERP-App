      *>================================================================
      *> PROGRAM-ID : JNLENTRY
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER
      *> PURPOSE    : PREPARE, VALIDATE, SUBMIT, APPROVE AND REJECT A
      *>              MANUAL JOURNAL.  THE HEADER LIVES IN
      *>              fin.JournalDraft AND THE LINES IN fin.JournalStage
      *>              UNDER THE SAME CORRELATION ID, SO JNLPOST ONLY HAS
      *>              TO CALL fin.usp_PostJournal WITH THAT ID.
      *>              A DRAFT HAS NO LEDGER EFFECT.  A JOURNAL REMAINS
      *>              NON POSTED UNTIL IT BALANCES IN THE DOCUMENT
      *>              CURRENCY AND IN STERLING, EVERY LINE PASSES
      *>              GLVALID, THE PERIOD PERMITS POSTING AND, WHERE
      *>              REQUIRED, AN INDEPENDENT APPROVER HAS SIGNED.
      *> CALLED BY  : frmJournalEntry, RECURJNL, .NET CLIENT
      *> INTERFACE  : CALL "JNLENTRY" USING WS-JNLENTRY-CA
      *>              COPYBOOK JNLENTRYCA
      *> CALLS      : GLVALID, PERIODCK, PERMCHK, AUDITLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-06-30 APW CR0092 ORIGINAL - MICRO FOCUS COBOL/2
      *> 1997-04-15 RJH CR0640 SELF APPROVAL REFUSED (SEGREGATION)
      *> 1999-11-30 KMP Y2K - 4 DIGIT YEARS
      *> 2026-10-05 STERP CORRELATION ID STAGING, EVIDENCE REFERENCE
      *>================================================================
      *> REQ: FIN-004, FIN-005
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JNLENTRY.
       AUTHOR. A P WHITTAKER.
       DATE-WRITTEN. 30/06/1994.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY GLVALIDCA REPLACING ==DFHCOMMAREA== BY ==WS-GLVALID-CA==
                      LEADING ==CA-== BY ==GH-==.
       COPY PERIODCKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERIODCK-CA==
                      LEADING ==CA-== BY ==PH-==.
       01  WS-SWITCHES.
           05  WS-REJECTED             PIC X VALUE "N".
               88  WS-IS-REJECTED                VALUE "Y".
           05  WS-LINE-BAD             PIC X VALUE "N".
           05  WS-OWN-TXN              PIC X VALUE "N".
               88  WS-I-OWN-TXN                  VALUE "Y".
       01  WS-WORK.
           05  WS-IDX                  PIC S9(4) COMP-5 VALUE 0.
           05  WS-LINE-NO              PIC 9(4) VALUE 0.
           05  WS-DR                   PIC S9(13)V99 VALUE 0.
           05  WS-CR                   PIC S9(13)V99 VALUE 0.
           05  WS-RATE                 PIC S9(10)V9(8) VALUE 0.
           05  WS-AMT                  PIC S9(13)V99 VALUE 0.
           05  WS-HASH                 PIC 9(16) VALUE 0.
           05  WS-HASH-X               PIC X(40).
           05  WS-STATUS               PIC X(12).
           05  WS-PREPARER             PIC X(20).
           05  WS-MONEY-ED             PIC -(11)9.99.
       01  WS-TXN-ROW.
           05  WS-T-DEPTH              PIC X(8).
       01  WS-DRAFT-ROW.
           05  WS-K-STATUS             PIC X(12).
           05  WS-K-PREPARED           PIC X(20).
           05  WS-K-APPROVED           PIC X(20).
           05  WS-K-HASH               PIC X(40).
           05  WS-K-DESC               PIC X(200).
           05  WS-K-POSTDATE           PIC X(8).
           05  WS-K-DOCDATE            PIC X(8).
           05  WS-K-CURRENCY           PIC X(4).
           05  WS-K-TYPE               PIC X(12).
           05  WS-K-EVIDENCE           PIC X(120).
           05  WS-K-LINES              PIC X(10).
       01  WS-STAGE-ROW.
           05  WS-S-LINE               PIC X(10).
           05  WS-S-ACCOUNT            PIC X(12).
           05  WS-S-DEBIT              PIC X(22).
           05  WS-S-CREDIT             PIC X(22).
           05  WS-S-CC                 PIC X(20).
           05  WS-S-PROJ               PIC X(20).
           05  WS-S-NARRATIVE          PIC X(80).
           05  WS-S-VATCODE            PIC X(4).
           05  WS-S-SUBTYPE            PIC X(6).
           05  WS-S-SUBREF             PIC X(40).
       LINKAGE SECTION.
       COPY JNLENTRYCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           PERFORM 0200-EDIT-HEADER THRU 0200-EXIT
           IF WS-IS-REJECTED
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "VALIDAT"  PERFORM 1000-VALIDATE THRU 1000-EXIT
               WHEN "SAVE"     PERFORM 2000-SAVE THRU 2000-EXIT
               WHEN "SUBMIT"   PERFORM 3000-SUBMIT THRU 3000-EXIT
               WHEN "APPROVE"  PERFORM 4000-APPROVE THRU 4000-EXIT
               WHEN "REJECT"   PERFORM 5000-REJECT-JNL THRU 5000-EXIT
               WHEN "VOID"     PERFORM 6000-VOID THRU 6000-EXIT
               WHEN "LOAD"     PERFORM 7000-LOAD THRU 7000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.

       0100-INITIALISE.
           MOVE "N" TO WS-REJECTED
           MOVE "JNLENTRY" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "FIN-004" TO CA-REQ-REF
           MOVE 0 TO JE-ERROR-CNT
           MOVE 0 TO JE-FIRST-BAD-LINE
           MOVE 0 TO JE-STAGED-CNT
           MOVE 0 TO JE-TOTAL-DEBIT
           MOVE 0 TO JE-TOTAL-CREDIT
           MOVE 0 TO JE-TOTAL-DR-GBP
           MOVE 0 TO JE-TOTAL-CR-GBP
           MOVE 0 TO JE-OUT-OF-BALANCE
           MOVE "N" TO JE-SELF-APPROVAL
           MOVE "Y" TO JE-APPROVAL-REQD
           MOVE SPACES TO JE-PERIOD-STATUS
           IF JE-CURRENCY = SPACES OR JE-CURRENCY = LOW-VALUES
               MOVE "GBP" TO JE-CURRENCY
           END-IF
           IF JE-JOURNAL-TYPE = SPACES
               MOVE "GL" TO JE-JOURNAL-TYPE
           END-IF
           IF JE-SOURCE-MODULE = SPACES
               MOVE "FIN" TO JE-SOURCE-MODULE
           END-IF
           IF JE-POSTING-DATE = 0
               MOVE CA-BUSINESS-DATE TO JE-POSTING-DATE
           END-IF
           IF JE-DOCUMENT-DATE = 0
               MOVE JE-POSTING-DATE TO JE-DOCUMENT-DATE
           END-IF
           IF JE-PREPARED-BY = SPACES
               MOVE CA-USER TO JE-PREPARED-BY
           END-IF
           IF JE-LINE-CNT > 50
               MOVE 50 TO JE-LINE-CNT
           END-IF.
       0100-EXIT.
           EXIT.

       0200-EDIT-HEADER.
           IF CA-COMPANY = SPACES
               MOVE "JNL0001" TO CA-REASON-CODE
               MOVE "A company is required on a journal" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF CA-CORRELATION-ID = SPACES
             OR CA-CORRELATION-ID = LOW-VALUES
               MOVE "JNL0002" TO CA-REASON-CODE
               MOVE "A correlation id identifies the journal being pre"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION = "LOAD" OR CA-FUNCTION = "VOID"
             OR CA-FUNCTION = "APPROVE" OR CA-FUNCTION = "REJECT"
               GO TO 0200-EXIT
           END-IF
           IF JE-DESCRIPTION = SPACES OR JE-DESCRIPTION = LOW-VALUES
               MOVE "JNL0003" TO CA-REASON-CODE
               MOVE "A journal needs a description that explains it"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF JE-LINE-CNT = 0
               MOVE "JNL0004" TO CA-REASON-CODE
               MOVE "A journal must have at least two lines"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000 - VALIDATE EVERY LINE AND THE BALANCE.  NOTHING IS WRITTEN
      *>----------------------------------------------------------------
       1000-VALIDATE.
           MOVE 0 TO WS-DR
           MOVE 0 TO WS-CR
           MOVE 0 TO JE-ERROR-CNT
           PERFORM 1100-VALIDATE-ONE THRU 1100-EXIT
               VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > JE-LINE-CNT
           MOVE WS-DR TO JE-TOTAL-DEBIT
           MOVE WS-CR TO JE-TOTAL-CREDIT
           COMPUTE JE-OUT-OF-BALANCE = WS-DR - WS-CR
           PERFORM 1500-CHECK-PERIOD THRU 1500-EXIT
           PERFORM 1600-CHECK-BALANCE THRU 1600-EXIT
           IF JE-ERROR-CNT = 0 AND CA-RETURN-CODE < 8
               MOVE SPACES TO CA-MESSAGE
               MOVE JE-TOTAL-DEBIT TO WS-MONEY-ED
               STRING "Journal validated, " FUNCTION TRIM(WS-MONEY-ED)
                      " each side, period "
                      FUNCTION TRIM(JE-PERIOD-STATUS)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       1000-EXIT.
           EXIT.

       1100-VALIDATE-ONE.
           MOVE "N" TO WS-LINE-BAD
           MOVE SPACES TO JE-L-MESSAGE(WS-IDX)
           MOVE "OK" TO JE-L-STATUS(WS-IDX)
           IF JE-L-NUM(WS-IDX) = 0
               MOVE WS-IDX TO JE-L-NUM(WS-IDX)
           END-IF
      *>   An account line carries a debit or a credit, never both.
           IF JE-L-DEBIT(WS-IDX) NOT = 0
             AND JE-L-CREDIT(WS-IDX) NOT = 0
               MOVE "JNL0010" TO CA-REASON-CODE
               MOVE "A line carries a debit or a credit, not both"
                   TO JE-L-MESSAGE(WS-IDX)
               PERFORM 1900-LINE-ERROR THRU 1900-EXIT
               GO TO 1100-EXIT
           END-IF
           IF JE-L-DEBIT(WS-IDX) = 0 AND JE-L-CREDIT(WS-IDX) = 0
               MOVE "JNL0011" TO CA-REASON-CODE
               MOVE "A line with no value cannot be posted"
                   TO JE-L-MESSAGE(WS-IDX)
               PERFORM 1900-LINE-ERROR THRU 1900-EXIT
               GO TO 1100-EXIT
           END-IF
           IF JE-L-DEBIT(WS-IDX) < 0 OR JE-L-CREDIT(WS-IDX) < 0
               MOVE "JNL0012" TO CA-REASON-CODE
               MOVE "Use the opposite side rather than a negative amou"
                   TO JE-L-MESSAGE(WS-IDX)
               PERFORM 1900-LINE-ERROR THRU 1900-EXIT
               GO TO 1100-EXIT
           END-IF
           PERFORM 1200-CALL-GLVALID THRU 1200-EXIT
           IF WS-LINE-BAD = "Y"
               GO TO 1100-EXIT
           END-IF
           ADD JE-L-DEBIT(WS-IDX) TO WS-DR
           ADD JE-L-CREDIT(WS-IDX) TO WS-CR.
       1100-EXIT.
           EXIT.

       1200-CALL-GLVALID.
           INITIALIZE WS-GLVALID-CA
           MOVE "LINE" TO GH-FUNCTION
           MOVE CA-COMPANY TO GH-COMPANY
           MOVE CA-SITE TO GH-SITE
           MOVE CA-USER TO GH-USER
           MOVE CA-BUSINESS-DATE TO GH-BUSINESS-DATE
           MOVE JE-L-ACCOUNT(WS-IDX) TO GV-ACCOUNT
           MOVE JE-POSTING-DATE TO GV-POSTING-DATE
           MOVE JE-L-DIM-CC(WS-IDX) TO GV-DIM-CC
           MOVE JE-L-DIM-PROJ(WS-IDX) TO GV-DIM-PROJ
           MOVE JE-L-VAT-CODE(WS-IDX) TO GV-VAT-CODE
           MOVE JE-SOURCE-MODULE TO GV-SOURCE-MODULE
           MOVE JE-L-NUM(WS-IDX) TO GV-LINE-NUM
           IF JE-SOURCE-MODULE = "FIN" AND JE-JOURNAL-TYPE = "GL"
               MOVE "Y" TO GV-MANUAL-ENTRY
           ELSE
               MOVE "N" TO GV-MANUAL-ENTRY
           END-IF
           CALL "GLVALID" USING WS-GLVALID-CA
           IF GH-RETURN-CODE > 4
               MOVE GH-REASON-CODE TO CA-REASON-CODE
               MOVE GH-MESSAGE
                   TO JE-L-MESSAGE(WS-IDX)
               PERFORM 1900-LINE-ERROR THRU 1900-EXIT
           END-IF.
       1200-EXIT.
           EXIT.

       1500-CHECK-PERIOD.
           INITIALIZE WS-PERIODCK-CA
           MOVE "CHECK" TO PH-FUNCTION
           MOVE CA-COMPANY TO PH-COMPANY
           MOVE CA-USER TO PH-USER
           MOVE CA-BUSINESS-DATE TO PH-BUSINESS-DATE
           MOVE JE-POSTING-DATE TO PC-POSTING-DATE
           MOVE JE-DOCUMENT-DATE TO PC-DOCUMENT-DATE
           MOVE "POST" TO PC-ACTION
           CALL "PERIODCK" USING WS-PERIODCK-CA
           MOVE PC-PERIOD-STATUS TO JE-PERIOD-STATUS
           MOVE PC-FISCAL-YEAR TO JE-FISCAL-YEAR
           MOVE PC-PERIOD-NO TO JE-PERIOD-NO
           IF PH-RETURN-CODE > 4
               MOVE PH-REASON-CODE TO CA-REASON-CODE
               MOVE PH-MESSAGE TO CA-MESSAGE
               MOVE PH-RETURN-CODE TO CA-RETURN-CODE
               ADD 1 TO JE-ERROR-CNT
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-REJECTED
           END-IF.
       1500-EXIT.
           EXIT.

       1600-CHECK-BALANCE.
           IF JE-OUT-OF-BALANCE NOT = 0
               MOVE "JNL0020" TO CA-REASON-CODE
               MOVE JE-OUT-OF-BALANCE TO WS-MONEY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "The journal does not balance. Debits and credit"
                      "s differ by " FUNCTION TRIM(WS-MONEY-ED)
                      " " FUNCTION TRIM(JE-CURRENCY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO JE-ERROR-CNT
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1600-EXIT.
           EXIT.

       1900-LINE-ERROR.
           MOVE "Y" TO WS-LINE-BAD
           MOVE "ER" TO JE-L-STATUS(WS-IDX)
           ADD 1 TO JE-ERROR-CNT
           IF JE-FIRST-BAD-LINE = 0
               MOVE JE-L-NUM(WS-IDX) TO JE-FIRST-BAD-LINE
               MOVE JE-L-MESSAGE(WS-IDX) TO CA-MESSAGE
           END-IF
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       1900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000 - SAVE THE DRAFT HEADER AND STAGE ITS LINES
      *>----------------------------------------------------------------
       2000-SAVE.
           PERFORM 1000-VALIDATE THRU 1000-EXIT
           IF JE-ERROR-CNT > 0 AND CA-FUNCTION NOT = "SAVE"
               GO TO 2000-EXIT
           END-IF
           IF CA-RETURN-CODE = 12
               GO TO 2000-EXIT
           END-IF
           PERFORM 2050-CONTENT-HASH THRU 2050-EXIT
           PERFORM 2400-BEGIN-WORK THRU 2400-EXIT
           PERFORM 2100-WRITE-DRAFT THRU 2100-EXIT
           IF CA-RETURN-CODE = 12
               PERFORM 2600-ROLLBACK-WORK THRU 2600-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-CLEAR-STAGE THRU 2200-EXIT
           PERFORM 2300-STAGE-LINE THRU 2300-EXIT
               VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > JE-LINE-CNT OR CA-RETURN-CODE = 12
           IF CA-RETURN-CODE = 12
               PERFORM 2600-ROLLBACK-WORK THRU 2600-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 2500-COMMIT-WORK THRU 2500-EXIT
           MOVE JE-LINE-CNT TO JE-STAGED-CNT
           IF JE-STATUS = SPACES
               MOVE "DRAFT" TO JE-STATUS
           END-IF
           IF CA-RETURN-CODE < 8
               MOVE SPACES TO CA-MESSAGE
               STRING "Draft journal saved with "
                      FUNCTION TRIM(JE-PERIOD-STATUS)
                      " period. No ledger effect until it is posted"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       2000-EXIT.
           EXIT.

      *> COM-015: the caller may already own the unit of work (a batch
      *> run, another online program or a test harness).  Only the
      *> program that opened the transaction may close it - committing
      *> a caller's transaction would make its work permanent behind
      *> its back and leave its ROLB with nothing to undo.
       2400-BEGIN-WORK.
           MOVE "N" TO WS-OWN-TXN
           MOVE SPACES TO SQLIO-STMT
           MOVE "SELECT CAST(@@TRANCOUNT AS varchar(8))" TO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-COL-WIDTH(1)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TXN-ROW
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "0" TO WS-T-DEPTH
           END-IF
           IF FUNCTION NUMVAL(WS-T-DEPTH) = 0
               MOVE "Y" TO WS-OWN-TXN
               MOVE "BEGN" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-TXN-ROW
           END-IF.
       2400-EXIT.
           EXIT.

       2500-COMMIT-WORK.
           IF WS-I-OWN-TXN
               MOVE "COMT" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-TXN-ROW
               MOVE "N" TO WS-OWN-TXN
           END-IF.
       2500-EXIT.
           EXIT.

       2600-ROLLBACK-WORK.
           IF WS-I-OWN-TXN
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-TXN-ROW
               MOVE "N" TO WS-OWN-TXN
           END-IF.
       2600-EXIT.
           EXIT.

      *> A very old trick: the content hash is a cheap checksum of the
      *> coding that matters, so that a material change after approval
      *> can be detected without keeping a second copy (COM-008).
       2050-CONTENT-HASH.
           MOVE 0 TO WS-HASH
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > JE-LINE-CNT
               COMPUTE WS-AMT = JE-L-DEBIT(WS-IDX)
                              - JE-L-CREDIT(WS-IDX)
               COMPUTE WS-HASH = FUNCTION MOD(
                   WS-HASH * 31 + WS-IDX * 7
                   + FUNCTION NUMVAL(JE-L-ACCOUNT(WS-IDX))
                   + WS-AMT * 100, 9999999999999999)
           END-PERFORM
           MOVE WS-HASH TO WS-HASH-X.
       2050-EXIT.
           EXIT.

       2100-WRITE-DRAFT.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.JournalDraft SET JournalType = ?, "
                  "Description = ?, SourceModule = ?, SourceRef = ?, "
                  "EventCode = NULLIF(?,''), DocumentDate = ?, "
                  "PostingDate = ?, CurrencyCode = ?, "
                  "TotalDebit = ?, TotalCredit = ?, "
                  "EvidenceRef = NULLIF(?,''), LineCount = ?, "
                  "ContentHash = ?, Status = CASE WHEN Status IN "
                  "('SUBMITTED','APPROVED') AND ContentHash <> ? "
                  "THEN 'DRAFT' ELSE Status END, "
                  "ApprovedBy = CASE WHEN ContentHash <> ? THEN NULL "
                  "ELSE ApprovedBy END, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE CorrelationId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 2150-BIND-DRAFT THRU 2150-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DRAFT-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2100-EXIT
           END-IF
           IF SQLIO-ROWCOUNT > 0
               GO TO 2100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalDraft (CompanyCode, "
                  "CorrelationId, JournalType, Description, "
                  "SourceModule, SourceRef, EventCode, DocumentDate, "
                  "PostingDate, CurrencyCode, TotalDebit, TotalCredit,"
                  " EvidenceRef, LineCount, ContentHash, Status, "
                  "PreparedBy, CreatedBy) VALUES (?, ?, ?, ?, ?, ?, "
                  "NULLIF(?,''), ?, ?, ?, ?, ?, NULLIF(?,''), ?, ?, "
                  "'DRAFT', ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 18 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(2)
           MOVE JE-JOURNAL-TYPE TO SQLIO-PARM-VAL(3)
           MOVE JE-DESCRIPTION TO SQLIO-PARM-VAL(4)
           MOVE JE-SOURCE-MODULE TO SQLIO-PARM-VAL(5)
           MOVE JE-SOURCE-REF TO SQLIO-PARM-VAL(6)
           MOVE JE-EVENT-CODE TO SQLIO-PARM-VAL(7)
           MOVE JE-DOCUMENT-DATE TO SQLIO-PARM-VAL(8)
           MOVE JE-POSTING-DATE TO SQLIO-PARM-VAL(9)
           MOVE JE-CURRENCY TO SQLIO-PARM-VAL(10)
           MOVE JE-TOTAL-DEBIT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE JE-TOTAL-CREDIT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE JE-EVIDENCE-REF TO SQLIO-PARM-VAL(13)
           MOVE JE-LINE-CNT TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(14)
           MOVE WS-HASH-X TO SQLIO-PARM-VAL(15)
           MOVE JE-PREPARED-BY TO SQLIO-PARM-VAL(16)
           MOVE CA-USER TO SQLIO-PARM-VAL(17)
           MOVE CA-USER TO SQLIO-PARM-VAL(18)
           PERFORM 2160-ALL-NOT-NULL THRU 2160-EXIT
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DRAFT-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       2100-EXIT.
           EXIT.

       2150-BIND-DRAFT.
           MOVE 17 TO SQLIO-PARM-CNT
           MOVE JE-JOURNAL-TYPE TO SQLIO-PARM-VAL(1)
           MOVE JE-DESCRIPTION TO SQLIO-PARM-VAL(2)
           MOVE JE-SOURCE-MODULE TO SQLIO-PARM-VAL(3)
           MOVE JE-SOURCE-REF TO SQLIO-PARM-VAL(4)
           MOVE JE-EVENT-CODE TO SQLIO-PARM-VAL(5)
           MOVE JE-DOCUMENT-DATE TO SQLIO-PARM-VAL(6)
           MOVE JE-POSTING-DATE TO SQLIO-PARM-VAL(7)
           MOVE JE-CURRENCY TO SQLIO-PARM-VAL(8)
           MOVE JE-TOTAL-DEBIT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE JE-TOTAL-CREDIT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE JE-EVIDENCE-REF TO SQLIO-PARM-VAL(11)
           MOVE JE-LINE-CNT TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE WS-HASH-X TO SQLIO-PARM-VAL(13)
           MOVE WS-HASH-X TO SQLIO-PARM-VAL(14)
           MOVE WS-HASH-X TO SQLIO-PARM-VAL(15)
           MOVE CA-USER TO SQLIO-PARM-VAL(16)
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(17)
           PERFORM 2160-ALL-NOT-NULL THRU 2160-EXIT.
       2150-EXIT.
           EXIT.

       2160-ALL-NOT-NULL.
           PERFORM VARYING WS-LINE-NO FROM 1 BY 1
               UNTIL WS-LINE-NO > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-LINE-NO)
           END-PERFORM.
       2160-EXIT.
           EXIT.

       2200-CLEAR-STAGE.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE fin.JournalStage WHERE CorrelationId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DRAFT-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       2200-EXIT.
           EXIT.

       2300-STAGE-LINE.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, CurrencyCode, DimCostCentre, "
                  "DimProject, Narrative, VatCode, SubledgerType, "
                  "SubledgerRef, BankAccountCode, CreatedBy) VALUES "
                  "(?, ?, ?, ?, ?, ?, ?, NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 15 TO SQLIO-PARM-CNT
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(1)
           MOVE JE-L-NUM(WS-IDX) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE JE-L-ACCOUNT(WS-IDX) TO SQLIO-PARM-VAL(4)
           MOVE JE-L-DEBIT(WS-IDX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE JE-L-CREDIT(WS-IDX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE JE-CURRENCY TO SQLIO-PARM-VAL(7)
           MOVE JE-L-DIM-CC(WS-IDX) TO SQLIO-PARM-VAL(8)
           MOVE JE-L-DIM-PROJ(WS-IDX) TO SQLIO-PARM-VAL(9)
           MOVE JE-L-NARRATIVE(WS-IDX) TO SQLIO-PARM-VAL(10)
           MOVE JE-L-VAT-CODE(WS-IDX) TO SQLIO-PARM-VAL(11)
           MOVE JE-L-SUBLDG-TYP(WS-IDX) TO SQLIO-PARM-VAL(12)
           MOVE JE-L-SUBLDG-REF(WS-IDX) TO SQLIO-PARM-VAL(13)
           MOVE JE-L-BANK-ACCT(WS-IDX) TO SQLIO-PARM-VAL(14)
           MOVE CA-USER TO SQLIO-PARM-VAL(15)
           PERFORM 2160-ALL-NOT-NULL THRU 2160-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DRAFT-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       2300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000 - SUBMIT FOR REVIEW.  BALANCED, CODED AND EVIDENCED.
      *>----------------------------------------------------------------
       3000-SUBMIT.
           PERFORM 2000-SAVE THRU 2000-EXIT
           IF CA-RETURN-CODE > 4
               GO TO 3000-EXIT
           END-IF
           IF JE-ERROR-CNT > 0
               MOVE "JNL0030" TO CA-REASON-CODE
               MOVE "The journal still has errors and cannot be submit"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           IF JE-EVIDENCE-REF = SPACES
             OR JE-EVIDENCE-REF = LOW-VALUES
               MOVE "JNL0031" TO CA-REASON-CODE
               MOVE "Supporting evidence must be referenced before a j"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE "SUBMITTED" TO WS-STATUS
           PERFORM 3500-SET-STATUS THRU 3500-EXIT
           IF CA-RETURN-CODE < 8
               MOVE "SUBMITTED" TO JE-STATUS
               MOVE "Journal submitted for independent approval"
                   TO CA-MESSAGE
           END-IF.
       3000-EXIT.
           EXIT.

       3500-SET-STATUS.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.JournalDraft SET Status = ?, "
                  "SubmittedBy = CASE WHEN ? = 'SUBMITTED' THEN ? "
                  "ELSE SubmittedBy END, SubmittedAt = CASE WHEN ? = "
                  "'SUBMITTED' THEN SYSDATETIME() ELSE SubmittedAt "
                  "END, ApprovedBy = CASE WHEN ? = 'APPROVED' THEN ? "
                  "ELSE ApprovedBy END, ApprovedAt = CASE WHEN ? = "
                  "'APPROVED' THEN SYSDATETIME() ELSE ApprovedAt END, "
                  "RejectedBy = CASE WHEN ? IN ('REJECTED','VOID') "
                  "THEN ? ELSE RejectedBy END, RejectReason = CASE "
                  "WHEN ? IN ('REJECTED','VOID') THEN ? ELSE "
                  "RejectReason END, ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE CorrelationId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 13 TO SQLIO-PARM-CNT
           MOVE WS-STATUS TO SQLIO-PARM-VAL(1)
           MOVE WS-STATUS TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE WS-STATUS TO SQLIO-PARM-VAL(4)
           MOVE WS-STATUS TO SQLIO-PARM-VAL(5)
           MOVE CA-USER TO SQLIO-PARM-VAL(6)
           MOVE WS-STATUS TO SQLIO-PARM-VAL(7)
           MOVE WS-STATUS TO SQLIO-PARM-VAL(8)
           MOVE CA-USER TO SQLIO-PARM-VAL(9)
           MOVE WS-STATUS TO SQLIO-PARM-VAL(10)
           MOVE JE-REASON TO SQLIO-PARM-VAL(11)
           MOVE CA-USER TO SQLIO-PARM-VAL(12)
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(13)
           PERFORM 2160-ALL-NOT-NULL THRU 2160-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DRAFT-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3500-EXIT
           END-IF
           IF SQLIO-ROWCOUNT = 0
               MOVE "JNL0032" TO CA-REASON-CODE
               MOVE "No journal is held under that correlation id"
                   TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       3500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000 - APPROVE.  SELF APPROVAL IS REFUSED (COM-003).
      *>----------------------------------------------------------------
       4000-APPROVE.
           PERFORM 7100-READ-DRAFT THRU 7100-EXIT
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           IF WS-K-STATUS NOT = "SUBMITTED"
               MOVE "JNL0040" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Only a submitted journal can be approved. This "
                      "one is " FUNCTION TRIM(WS-K-STATUS)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF FUNCTION TRIM(WS-K-PREPARED) = FUNCTION TRIM(CA-USER)
               MOVE "Y" TO JE-SELF-APPROVAL
               MOVE "JNL0041" TO CA-REASON-CODE
               MOVE "A preparer may not approve their own journal. An "
                   TO CA-MESSAGE
               MOVE 6 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-REJECTED
               GO TO 4000-EXIT
           END-IF
           MOVE "APPROVED" TO WS-STATUS
           PERFORM 3500-SET-STATUS THRU 3500-EXIT
           IF CA-RETURN-CODE < 8
               MOVE "APPROVED" TO JE-STATUS
               MOVE CA-USER TO JE-APPROVED-BY
               MOVE "Journal approved and ready to post" TO CA-MESSAGE
           END-IF.
       4000-EXIT.
           EXIT.

       5000-REJECT-JNL.
           IF JE-REASON = SPACES OR JE-REASON = LOW-VALUES
               MOVE "JNL0050" TO CA-REASON-CODE
               MOVE "A reviewer must supply a reason for a rejection"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE "REJECTED" TO WS-STATUS
           PERFORM 3500-SET-STATUS THRU 3500-EXIT
           IF CA-RETURN-CODE < 8
               MOVE "REJECTED" TO JE-STATUS
               MOVE "Journal returned for correction. No financial eff"
                   TO CA-MESSAGE
           END-IF.
       5000-EXIT.
           EXIT.

       6000-VOID.
           PERFORM 7100-READ-DRAFT THRU 7100-EXIT
           IF CA-FAILED
               GO TO 6000-EXIT
           END-IF
           IF WS-K-STATUS = "POSTED"
               MOVE "JNL0060" TO CA-REASON-CODE
               MOVE "A posted journal is corrected by a reversal, neve"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 6000-EXIT
           END-IF
           MOVE "VOID" TO WS-STATUS
           PERFORM 3500-SET-STATUS THRU 3500-EXIT
           PERFORM 2200-CLEAR-STAGE THRU 2200-EXIT
           IF CA-RETURN-CODE < 8
               MOVE "VOID" TO JE-STATUS
               MOVE "Draft closed. Its reference and history are retai"
                   TO CA-MESSAGE
           END-IF.
       6000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 7000 - LOAD A STORED DRAFT BACK INTO THE COMMAREA
      *>----------------------------------------------------------------
       7000-LOAD.
           PERFORM 7100-READ-DRAFT THRU 7100-EXIT
           IF CA-FAILED
               GO TO 7000-EXIT
           END-IF
           MOVE 0 TO JE-LINE-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT s.LineNum, s.AccountCode, s.DebitAmount, "
                  "s.CreditAmount, ISNULL(s.DimCostCentre,''), "
                  "ISNULL(s.DimProject,''), ISNULL(s.Narrative,''), "
                  "ISNULL(s.VatCode,''), ISNULL(s.SubledgerType,''), "
                  "ISNULL(s.SubledgerRef,'') FROM fin.JournalStage s "
                  "WHERE s.CorrelationId = ? ORDER BY s.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 22 TO SQLIO-COL-WIDTH(3)
           MOVE 22 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 20 TO SQLIO-COL-WIDTH(6)
           MOVE 80 TO SQLIO-COL-WIDTH(7)
           MOVE 4 TO SQLIO-COL-WIDTH(8)
           MOVE 6 TO SQLIO-COL-WIDTH(9)
           MOVE 40 TO SQLIO-COL-WIDTH(10)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-STAGE-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 7000-EXIT
           END-IF
           PERFORM 7200-FETCH-STAGE THRU 7200-EXIT
               UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
                  OR JE-LINE-CNT NOT < 50
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-STAGE-ROW
           MOVE JE-LINE-CNT TO JE-STAGED-CNT.
       7000-EXIT.
           EXIT.

       7100-READ-DRAFT.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT d.Status, ISNULL(d.PreparedBy,''), "
                  "ISNULL(d.ApprovedBy,''), ISNULL(d.ContentHash,''), "
                  "d.Description, CONVERT(char(8), d.PostingDate, 112)"
                  ", CONVERT(char(8), d.DocumentDate, 112), "
                  "d.CurrencyCode, d.JournalType, "
                  "ISNULL(d.EvidenceRef,''), "
                  "CAST(d.LineCount AS varchar(10)) "
                  "FROM fin.JournalDraft d WHERE d.CorrelationId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 11 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 20 TO SQLIO-COL-WIDTH(3)
           MOVE 40 TO SQLIO-COL-WIDTH(4)
           MOVE 200 TO SQLIO-COL-WIDTH(5)
           MOVE 8 TO SQLIO-COL-WIDTH(6)
           MOVE 8 TO SQLIO-COL-WIDTH(7)
           MOVE 4 TO SQLIO-COL-WIDTH(8)
           MOVE 12 TO SQLIO-COL-WIDTH(9)
           MOVE 120 TO SQLIO-COL-WIDTH(10)
           MOVE 10 TO SQLIO-COL-WIDTH(11)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DRAFT-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 7100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "JNL0032" TO CA-REASON-CODE
               MOVE "No journal is held under that correlation id"
                   TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 7100-EXIT
           END-IF
           MOVE WS-K-STATUS TO JE-STATUS
           MOVE WS-K-PREPARED TO JE-PREPARED-BY
           MOVE WS-K-APPROVED TO JE-APPROVED-BY
           MOVE WS-K-DESC TO JE-DESCRIPTION
           MOVE WS-K-TYPE TO JE-JOURNAL-TYPE
           MOVE WS-K-EVIDENCE TO JE-EVIDENCE-REF
           MOVE WS-K-CURRENCY(1:3) TO JE-CURRENCY
           COMPUTE JE-POSTING-DATE = FUNCTION NUMVAL(WS-K-POSTDATE)
           COMPUTE JE-DOCUMENT-DATE = FUNCTION NUMVAL(WS-K-DOCDATE).
       7100-EXIT.
           EXIT.

       7200-FETCH-STAGE.
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-STAGE-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               GO TO 7200-EXIT
           END-IF
           ADD 1 TO JE-LINE-CNT
           MOVE JE-LINE-CNT TO WS-IDX
           COMPUTE JE-L-NUM(WS-IDX) = FUNCTION NUMVAL(WS-S-LINE)
           MOVE WS-S-ACCOUNT TO JE-L-ACCOUNT(WS-IDX)
           COMPUTE JE-L-DEBIT(WS-IDX) = FUNCTION NUMVAL(WS-S-DEBIT)
           COMPUTE JE-L-CREDIT(WS-IDX) = FUNCTION NUMVAL(WS-S-CREDIT)
           MOVE WS-S-CC TO JE-L-DIM-CC(WS-IDX)
           MOVE WS-S-PROJ TO JE-L-DIM-PROJ(WS-IDX)
           MOVE WS-S-NARRATIVE TO JE-L-NARRATIVE(WS-IDX)
           MOVE WS-S-VATCODE TO JE-L-VAT-CODE(WS-IDX)
           MOVE WS-S-SUBTYPE TO JE-L-SUBLDG-TYP(WS-IDX)
           MOVE WS-S-SUBREF TO JE-L-SUBLDG-REF(WS-IDX)
           ADD JE-L-DEBIT(WS-IDX) TO JE-TOTAL-DEBIT
           ADD JE-L-CREDIT(WS-IDX) TO JE-TOTAL-CREDIT.
       7200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT.
           MOVE "Y" TO WS-REJECTED
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE "JNL0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "JNLENTRY does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in JNLENTRY" TO CA-MESSAGE
           END-IF
           MOVE 12 TO CA-RETURN-CODE
           MOVE "Y" TO WS-REJECTED
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
