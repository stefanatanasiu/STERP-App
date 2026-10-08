      *>================================================================
      *> PROGRAM-ID : TGLVALID
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER TEST SUITE
      *> PURPOSE    : ACCOUNT AND MAPPING GOVERNANCE (AC-009) AND THE
      *>              JOURNAL APPROVAL AND REVERSAL CYCLE (AC-010).
      *>              EVERY WRITE RUNS INSIDE ONE BEGN ... ROLB SO THE
      *>              SUITE IS REPEATABLE AND LEAVES NOTHING BEHIND.
      *>----------------------------------------------------------------
      *> 2026-10-05 STERP ORIGINAL
      *>================================================================
      *> REQ: FIN-001, FIN-002, FIN-003, FIN-004, FIN-005, FIN-006
      *> AC: AC-009, AC-010
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TGLVALID.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY SQLIOCA.
       COPY GLVALIDCA REPLACING ==DFHCOMMAREA== BY ==WS-GLVALID-CA==
                      LEADING ==CA-== BY ==GH-==.
       COPY JNLENTRYCA REPLACING ==DFHCOMMAREA== BY ==WS-JNLENTRY-CA==
                      LEADING ==CA-== BY ==JE-H-==.
       COPY JNLPOSTCA REPLACING ==DFHCOMMAREA== BY ==WS-JNLPOST-CA==
                      LEADING ==CA-== BY ==JH-==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "N".
       01  WS-WORK.
           05  WS-CORREL               PIC X(36).
           05  WS-JOURNAL              PIC X(30).
           05  WS-PING                 PIC X(40).
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TGLVALID finance ledger governance" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-CONNECT
           IF WS-DB-UP NOT = "Y"
               MOVE "no database reachable" TO TK-DETAIL
               MOVE "finance suite needs SQL Server" TO TK-NAME
               PERFORM TK-SKIP
               PERFORM TK-END
               STOP RUN
           END-IF
           PERFORM 1000-ACCOUNT-TESTS
           PERFORM 2000-DIMENSION-TESTS
           PERFORM 3000-EVENT-TESTS
           PERFORM 4000-JOURNAL-TESTS
           PERFORM 9000-DISCONNECT
           PERFORM TK-END
           STOP RUN.

       0100-CONNECT.
           MOVE "SELECT 'Y'" TO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING
           IF NOT SQLIO-ERROR
               MOVE "Y" TO WS-DB-UP
           END-IF.

       9000-DISCONNECT.
           MOVE "ROLB" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING.

      *>----------------------------------------------------------------
      *> 1000 - AC-009 ACCOUNT GOVERNANCE
      *>----------------------------------------------------------------
       1000-ACCOUNT-TESTS.
           PERFORM 1010-SETUP-CA
           MOVE "ACCOUNT" TO GH-FUNCTION
           MOVE "6800" TO GV-ACCOUNT
           CALL "GLVALID" USING WS-GLVALID-CA
           MOVE "AC-009 an active posting account validates" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE GH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-009 the account class comes back" TO TK-NAME
           MOVE "ADMIN" TO TK-EXPECTED-X
           MOVE GV-CLASS-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 1010-SETUP-CA
           MOVE "ACCOUNT" TO GH-FUNCTION
           MOVE "9999" TO GV-ACCOUNT
           CALL "GLVALID" USING WS-GLVALID-CA
           MOVE "AC-009 an unknown account is not found" TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE GH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

      *>   A control account may only be posted by its own subledger.
           PERFORM 1010-SETUP-CA
           MOVE "ACCOUNT" TO GH-FUNCTION
           MOVE "1200" TO GV-ACCOUNT
           MOVE "Y" TO GV-MANUAL-ENTRY
           CALL "GLVALID" USING WS-GLVALID-CA
           MOVE "AC-009 a manual journal cannot touch a control acct"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE GH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-009 the refusal names the offending field"
               TO TK-NAME
           MOVE "GV-ACCOUNT" TO TK-EXPECTED-X
           MOVE GV-FAIL-FIELD TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   A referenced account keeps its history and is never deleted.
           PERFORM 1010-SETUP-CA
           MOVE "USAGE" TO GH-FUNCTION
           MOVE "1200" TO GV-ACCOUNT
           CALL "GLVALID" USING WS-GLVALID-CA
           MOVE "AC-009 a used account may be retired, never deleted"
               TO TK-NAME
           IF GV-MAY-RETIRE = "Y"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

       1010-SETUP-CA.
           INITIALIZE WS-GLVALID-CA
           MOVE "STC" TO GH-COMPANY
           MOVE "RDG" TO GH-SITE
           MOVE "GLCLERK" TO GH-USER
           MOVE 20261015 TO GH-BUSINESS-DATE
           MOVE 20261015 TO GV-POSTING-DATE.

      *>----------------------------------------------------------------
      *> 2000 - AC-009 DIMENSION VALIDATION
      *>----------------------------------------------------------------
       2000-DIMENSION-TESTS.
           PERFORM 1010-SETUP-CA
           MOVE "LINE" TO GH-FUNCTION
           MOVE "6800" TO GV-ACCOUNT
           CALL "GLVALID" USING WS-GLVALID-CA
           MOVE "AC-009 a missing mandatory cost centre blocks posting"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE GH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-009 the failing dimension is identified" TO TK-NAME
           MOVE "GV-DIM-CC" TO TK-EXPECTED-X
           MOVE GV-FAIL-FIELD TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 1010-SETUP-CA
           MOVE "LINE" TO GH-FUNCTION
           MOVE "6800" TO GV-ACCOUNT
           MOVE "ADMIN" TO GV-DIM-CC
           CALL "GLVALID" USING WS-GLVALID-CA
           MOVE "AC-009 a complete expense line validates" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE GH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 1010-SETUP-CA
           MOVE "LINE" TO GH-FUNCTION
           MOVE "6800" TO GV-ACCOUNT
           MOVE "NOSUCHCC" TO GV-DIM-CC
           CALL "GLVALID" USING WS-GLVALID-CA
           MOVE "AC-009 an unknown cost centre is refused" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE GH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 1010-SETUP-CA
           MOVE "LINE" TO GH-FUNCTION
           MOVE "2020" TO GV-ACCOUNT
           MOVE "ADMIN" TO GV-DIM-CC
           CALL "GLVALID" USING WS-GLVALID-CA
           MOVE "AC-009 a forbidden dimension on a balance sheet acct"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE GH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 3000 - AC-009 BUSINESS EVENT MAPPING
      *>----------------------------------------------------------------
       3000-EVENT-TESTS.
           PERFORM 1010-SETUP-CA
           MOVE "EVENT" TO GH-FUNCTION
           MOVE "FIN.ACCRUAL" TO GV-EVENT-CODE
           CALL "GLVALID" USING WS-GLVALID-CA
           MOVE "AC-009 an approved event returns its mapped legs"
               TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE GV-LEG-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 1010-SETUP-CA
           MOVE "EVENT" TO GH-FUNCTION
           MOVE "FIN.NOTMAPPED" TO GV-EVENT-CODE
           CALL "GLVALID" USING WS-GLVALID-CA
           MOVE "AC-009 a missing mapping blocks, no suspense account"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE GH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-009 the mapping refusal is reason GLV0021"
               TO TK-NAME
           MOVE "GLV0021" TO TK-EXPECTED-X
           MOVE GH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 4000 - AC-010 PREPARE, VALIDATE, POST AND REVERSE
      *>----------------------------------------------------------------
       4000-JOURNAL-TESTS.
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING
           MOVE "ZZTEST-JNL-AC010-0001" TO WS-CORREL
      *>   An unbalanced journal must fail.
           PERFORM 4010-SETUP-JOURNAL
           MOVE 250 TO JE-L-DEBIT(1)
           MOVE 200 TO JE-L-CREDIT(2)
           MOVE "VALIDAT" TO JE-H-FUNCTION
           CALL "JNLENTRY" USING WS-JNLENTRY-CA
           MOVE "AC-010 an unbalanced journal cannot be posted"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE JE-H-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-010 the out of balance amount is reported"
               TO TK-NAME
           MOVE 50 TO TK-EXPECTED-N
           MOVE JE-OUT-OF-BALANCE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

      *>   Correct it, validate, stage and post.
           PERFORM 4010-SETUP-JOURNAL
           MOVE 250 TO JE-L-DEBIT(1)
           MOVE 250 TO JE-L-CREDIT(2)
           MOVE "VALIDAT" TO JE-H-FUNCTION
           CALL "JNLENTRY" USING WS-JNLENTRY-CA
           MOVE "AC-010 the corrected journal validates" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE JE-H-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 4010-SETUP-JOURNAL
           MOVE 250 TO JE-L-DEBIT(1)
           MOVE 250 TO JE-L-CREDIT(2)
           MOVE "SUBMIT" TO JE-H-FUNCTION
           CALL "JNLENTRY" USING WS-JNLENTRY-CA
           MOVE "AC-010 a balanced evidenced journal submits"
               TO TK-NAME
           MOVE "SUBMITTED" TO TK-EXPECTED-X
           MOVE JE-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   The preparer may not approve their own journal (COM-003).
           INITIALIZE WS-JNLENTRY-CA
           MOVE "STC" TO JE-H-COMPANY
           MOVE "GLCLERK" TO JE-H-USER
           MOVE 20261015 TO JE-H-BUSINESS-DATE
           MOVE WS-CORREL TO JE-H-CORRELATION-ID
           MOVE "APPROVE" TO JE-H-FUNCTION
           CALL "JNLENTRY" USING WS-JNLENTRY-CA
           MOVE "AC-010 self approval is refused" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE JE-H-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           INITIALIZE WS-JNLENTRY-CA
           MOVE "STC" TO JE-H-COMPANY
           MOVE "FINCTRL" TO JE-H-USER
           MOVE 20261015 TO JE-H-BUSINESS-DATE
           MOVE WS-CORREL TO JE-H-CORRELATION-ID
           MOVE "APPROVE" TO JE-H-FUNCTION
           CALL "JNLENTRY" USING WS-JNLENTRY-CA
           MOVE "AC-010 an independent approver may approve"
               TO TK-NAME
           MOVE "APPROVED" TO TK-EXPECTED-X
           MOVE JE-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 4020-POST-IT
           MOVE "AC-010 the approved journal posts" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE JH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-010 the posting is balanced at 250.00" TO TK-NAME
           MOVE 250 TO TK-EXPECTED-N
           MOVE JP-TOTAL-DR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE JP-NEW-JOURNAL-NO TO WS-JOURNAL

      *>   COM-015: repeating the instruction returns the original.
           PERFORM 4020-POST-IT
           MOVE "AC-010 repeating the posting has no second effect"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE JP-WAS-DUPLICATE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   A reversal above the unreversed original must fail.
           PERFORM 4030-SETUP-REVERSE
           MOVE 400 TO JP-REVERSE-AMOUNT
           CALL "JNLPOST" USING WS-JNLPOST-CA
           MOVE "AC-010 a reversal over the original value is refused"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE JH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

      *>   A reversal into a hard closed period must fail.
           PERFORM 4030-SETUP-REVERSE
           MOVE 100 TO JP-REVERSE-AMOUNT
           MOVE 20260515 TO JP-POSTING-DATE
           CALL "JNLPOST" USING WS-JNLPOST-CA
           MOVE "AC-010 a reversal in a closed period is refused"
               TO TK-NAME
           IF JH-RETURN-CODE > 4
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

      *>   A valid partial reversal is a new linked entry.
           PERFORM 4030-SETUP-REVERSE
           MOVE "ZZTEST-JNL-AC010-REV1" TO JH-CORRELATION-ID
           MOVE 100 TO JP-REVERSE-AMOUNT
           CALL "JNLPOST" USING WS-JNLPOST-CA
           MOVE "AC-010 an authorised partial reversal posts"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE JH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-010 the reversal is 100.00 of the 250.00 original"
               TO TK-NAME
           MOVE 100 TO TK-EXPECTED-N
           MOVE JP-TOTAL-DR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

      *>   The original is unchanged and 150.00 remains reversible.
           INITIALIZE WS-JNLPOST-CA
           MOVE "STC" TO JH-COMPANY
           MOVE "FINCTRL" TO JH-USER
           MOVE 20261015 TO JH-BUSINESS-DATE
           MOVE "ENQUIRE" TO JH-FUNCTION
           MOVE WS-JOURNAL TO JP-JOURNAL-NUMBER
           CALL "JNLPOST" USING WS-JNLPOST-CA
           MOVE "AC-010 the original journal lines are unchanged"
               TO TK-NAME
           MOVE 250 TO TK-EXPECTED-N
           MOVE JP-TOTAL-DR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-010 150.00 of the original remains reversible"
               TO TK-NAME
           MOVE 150 TO TK-EXPECTED-N
           MOVE JP-REVERSIBLE-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       4010-SETUP-JOURNAL.
           INITIALIZE WS-JNLENTRY-CA
           MOVE "STC" TO JE-H-COMPANY
           MOVE "RDG" TO JE-H-SITE
           MOVE "GLCLERK" TO JE-H-USER
           MOVE 20261015 TO JE-H-BUSINESS-DATE
           MOVE WS-CORREL TO JE-H-CORRELATION-ID
           MOVE "Test accrual of audit fees" TO JE-DESCRIPTION
           MOVE "AUDIT/2026/Q3" TO JE-EVIDENCE-REF
           MOVE 20261015 TO JE-POSTING-DATE
           MOVE 20261015 TO JE-DOCUMENT-DATE
           MOVE "GBP" TO JE-CURRENCY
           MOVE 2 TO JE-LINE-CNT
           MOVE 1 TO JE-L-NUM(1)
           MOVE "6410" TO JE-L-ACCOUNT(1)
           MOVE "ADMIN" TO JE-L-DIM-CC(1)
           MOVE "Audit fee accrual" TO JE-L-NARRATIVE(1)
           MOVE 2 TO JE-L-NUM(2)
           MOVE "2020" TO JE-L-ACCOUNT(2)
           MOVE "Audit fee accrual" TO JE-L-NARRATIVE(2).

       4020-POST-IT.
           INITIALIZE WS-JNLPOST-CA
           MOVE "STC" TO JH-COMPANY
           MOVE "RDG" TO JH-SITE
           MOVE "FINCTRL" TO JH-USER
           MOVE 20261015 TO JH-BUSINESS-DATE
           MOVE WS-CORREL TO JH-CORRELATION-ID
           MOVE "POST" TO JH-FUNCTION
           MOVE "Test accrual of audit fees" TO JP-DESCRIPTION
           MOVE 20261015 TO JP-POSTING-DATE
           MOVE "GBP" TO JP-CURRENCY
           CALL "JNLPOST" USING WS-JNLPOST-CA.

       4030-SETUP-REVERSE.
           INITIALIZE WS-JNLPOST-CA
           MOVE "STC" TO JH-COMPANY
           MOVE "RDG" TO JH-SITE
           MOVE "FINCTRL" TO JH-USER
           MOVE 20261015 TO JH-BUSINESS-DATE
           MOVE "ZZTEST-JNL-AC010-REVX" TO JH-CORRELATION-ID
           MOVE "REVERSE" TO JH-FUNCTION
           MOVE WS-JOURNAL TO JP-JOURNAL-NUMBER
           MOVE 20261015 TO JP-POSTING-DATE
           MOVE "Correction of a miscoded accrual" TO JP-REASON.

       COPY TESTKITP.
