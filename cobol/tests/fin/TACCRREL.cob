      *>================================================================
      *> PROGRAM-ID : TACCRREL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : RECURRING, ACCRUAL AND PREPAYMENT TESTS
      *> PURPOSE    : AC-011.  THREE RECURRING OCCURRENCES OF 100.00
      *>              AND A PREPAYMENT OF 300.00 RELEASED OVER THREE
      *>              PERIODS.  REPEATING THE SECOND OCCURRENCE AND
      *>              ASKING FOR A FOURTH RELEASE MUST LEAVE THE
      *>              RELEASED TOTAL AT 300.00 AND NOT 400.00, WITH A
      *>              ZERO RESIDUAL AND NO FOURTH RELEASE.  AN
      *>              OCCURRENCE SCHEDULED INTO A CLOSED PERIOD RAISES
      *>              AN EXCEPTION INSTEAD OF BEING QUIETLY REDATED.
      *>              EVERY WRITE RUNS INSIDE ONE BEGN ... ROLB SO THE
      *>              SUITE IS REPEATABLE AND LEAVES NOTHING BEHIND.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: FIN-007, FIN-008, FIN-009
      *> AC: AC-011
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TACCRREL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY SQLIOCA.
       COPY ACCRRELCA REPLACING ==DFHCOMMAREA== BY ==WS-ACCRREL-CA==
                      LEADING ==CA-== BY ==AH-==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "N".
           05  WS-FIXTURE-OK           PIC X VALUE "Y".
       01  WS-WORK.
           05  WS-PING                 PIC X(40).
           05  WS-AMT                  PIC S9(13)V99 VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TACCRREL recurring release" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-CONNECT
           IF WS-DB-UP NOT = "Y"
               MOVE "no database reachable" TO TK-DETAIL
               MOVE "release suite needs SQL Server" TO TK-NAME
               PERFORM TK-SKIP
               PERFORM TK-END
               STOP RUN
           END-IF
           PERFORM 0200-BEGIN-WORK
           IF WS-FIXTURE-OK NOT = "Y"
               MOVE "recurring fixtures were not written" TO TK-DETAIL
               MOVE "AC-011 recurring release tests" TO TK-NAME
               PERFORM TK-SKIP
               PERFORM 9000-ROLLBACK
               PERFORM TK-END
               STOP RUN
           END-IF
           PERFORM 1000-SCHEDULE-TESTS
           PERFORM 2000-NEXT-TESTS
           PERFORM 3000-ONCE-ONLY-TESTS
           PERFORM 4000-CLOSED-PERIOD-TESTS
           PERFORM 9000-ROLLBACK
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

       0200-BEGIN-WORK.
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING
           PERFORM 0300-FIXTURES.

       9000-ROLLBACK.
           MOVE "ROLB" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING.

       0300-FIXTURES.
      *>   ZZTPRE01 is the prepayment of AC-011, already released over
      *>   its three periods, so nothing is left to release.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.RecurringJournal (CompanyCode, "
                  "RecurringCode, Name, JournalKind, Frequency, "
                  "StartDate, OccurrencesPlanned, OccurrencesPosted, "
                  "SourceAmount, ReleasedAmount, RequiresApproval, "
                  "Status) VALUES ('STC','ZZTPRE01',"
                  "'Test insurance prepayment','PREPAY','MONTHLY',"
                  "'2026-10-15',3,3,300.00,300.00,0,'ACTIVE')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.RecurringOccurrence (CompanyCode, "
                  "RecurringCode, OccurrenceNo, ScheduledDate, "
                  "PostingDate, Amount, Status, JournalNumber) VALUES"
                  " ('STC','ZZTPRE01',1,'2026-10-15','2026-10-15',"
                  "100.00,'POSTED','ZZT-JNL-0001'),"
                  "('STC','ZZTPRE01',2,'2026-11-15','2026-11-15',"
                  "100.00,'POSTED','ZZT-JNL-0002'),"
                  "('STC','ZZTPRE01',3,'2026-12-15','2026-12-15',"
                  "100.00,'POSTED','ZZT-JNL-0003')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
      *>   ZZTPRE02 is the same prepayment before any release.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.RecurringJournal (CompanyCode, "
                  "RecurringCode, Name, JournalKind, Frequency, "
                  "StartDate, OccurrencesPlanned, OccurrencesPosted, "
                  "SourceAmount, ReleasedAmount, RequiresApproval, "
                  "Status) VALUES ('STC','ZZTPRE02',"
                  "'Test rates prepayment','PREPAY','MONTHLY',"
                  "'2026-10-15',3,0,300.00,0,0,'ACTIVE')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
      *>   ZZTREC03 is a recurring journal of 100.00 a month.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.RecurringJournal (CompanyCode, "
                  "RecurringCode, Name, JournalKind, Frequency, "
                  "StartDate, OccurrencesPlanned, OccurrencesPosted, "
                  "SourceAmount, ReleasedAmount, RequiresApproval, "
                  "Status) VALUES ('STC','ZZTREC03',"
                  "'Test monthly service charge','RECURRING',"
                  "'MONTHLY','2026-10-15',3,2,0,200.00,0,'ACTIVE')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.RecurringOccurrence (CompanyCode, "
                  "RecurringCode, OccurrenceNo, ScheduledDate, "
                  "PostingDate, Amount, Status, JournalNumber) VALUES"
                  " ('STC','ZZTREC03',1,'2026-10-15','2026-10-15',"
                  "100.00,'POSTED','ZZT-JNL-0011'),"
                  "('STC','ZZTREC03',2,'2026-11-15','2026-11-15',"
                  "100.00,'POSTED','ZZT-JNL-0012')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
      *>   ZZTREC04 falls due in August 2026, which is hard closed.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.RecurringJournal (CompanyCode, "
                  "RecurringCode, Name, JournalKind, Frequency, "
                  "StartDate, OccurrencesPlanned, OccurrencesPosted, "
                  "SourceAmount, ReleasedAmount, RequiresApproval, "
                  "Status) VALUES ('STC','ZZTREC04',"
                  "'Test charge due in a closed period','RECURRING',"
                  "'MONTHLY','2026-08-15',3,0,0,0,0,'ACTIVE')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL.

       0900-RUN-SQL.
           MOVE 8 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING
           IF SQLIO-ERROR
               MOVE "N" TO WS-FIXTURE-OK
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING.

       0950-SETUP-AK.
           INITIALIZE WS-ACCRREL-CA
           MOVE "STC" TO AH-COMPANY
           MOVE "RDG" TO AH-SITE
           MOVE "FINCTRL" TO AH-USER
           MOVE 20261015 TO AH-BUSINESS-DATE.

      *>----------------------------------------------------------------
      *> 1000 - AC-011 THE SCHEDULE.  A PREPAYMENT OF 300.00 OVER
      *>        THREE PERIODS RELEASES 100.00 EACH TIME AND THE THREE
      *>        RELEASES ADD BACK TO EXACTLY 300.00 (COM-011).
      *>----------------------------------------------------------------
       1000-SCHEDULE-TESTS.
           PERFORM 0950-SETUP-AK
           MOVE "SCHEDULE" TO AH-FUNCTION
           MOVE "ZZTPRE02" TO AK-RECUR-CODE
           CALL "ACCRREL" USING WS-ACCRREL-CA
           MOVE "AC-011 the prepayment schedule is built" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE AH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-011 the prepayment runs over three periods"
               TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           MOVE AK-SCHED-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-011 each release of the prepayment is 100.00"
               TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE AK-RELEASE-AMOUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           COMPUTE WS-AMT = AK-S-AMOUNT(1) + AK-S-AMOUNT(2)
                          + AK-S-AMOUNT(3)
           MOVE "AC-011 the three releases add back to 300.00"
               TO TK-NAME
           MOVE 300.00 TO TK-EXPECTED-N
           MOVE WS-AMT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-011 nothing is released yet, so 300.00 remains"
               TO TK-NAME
           MOVE 300.00 TO TK-EXPECTED-N
           MOVE AK-REMAINING TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-AK
           MOVE "SCHEDULE" TO AH-FUNCTION
           MOVE "ZZTREC03" TO AK-RECUR-CODE
           CALL "ACCRREL" USING WS-ACCRREL-CA
           MOVE "AC-011 the recurring journal has three occurrences"
               TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           MOVE AK-SCHED-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-011 the two posted occurrences are known" TO TK-NAME
           MOVE "POSTED" TO TK-EXPECTED-X
           MOVE AK-S-STATUS(2) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-011 the third occurrence is still due" TO TK-NAME
           MOVE "DUE" TO TK-EXPECTED-X
           MOVE AK-S-STATUS(3) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 0950-SETUP-AK
           MOVE "SCHEDULE" TO AH-FUNCTION
           MOVE "NOSUCHCODE" TO AK-RECUR-CODE
           CALL "ACCRREL" USING WS-ACCRREL-CA
           MOVE "FIN-007 an unknown recurring journal is not found"
               TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE AH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-AK
           MOVE "SCHEDULE" TO AH-FUNCTION
           CALL "ACCRREL" USING WS-ACCRREL-CA
           MOVE "FIN-007 a request without a code is refused"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE AH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 2000 - FIN-007 WHICH OCCURRENCE IS NEXT
      *>----------------------------------------------------------------
       2000-NEXT-TESTS.
           PERFORM 0950-SETUP-AK
           MOVE "NEXTOCC" TO AH-FUNCTION
           MOVE "ZZTPRE02" TO AK-RECUR-CODE
           CALL "ACCRREL" USING WS-ACCRREL-CA
           MOVE "FIN-007 the first occurrence is next to release"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE AK-OCCURRENCE-NO TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-AK
           MOVE "NEXTOCC" TO AH-FUNCTION
           MOVE "ZZTREC03" TO AK-RECUR-CODE
           CALL "ACCRREL" USING WS-ACCRREL-CA
           MOVE "FIN-007 a part released schedule resumes at three"
               TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           MOVE AK-OCCURRENCE-NO TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 3000 - AC-011 ONCE AND ONCE ONLY.  REPEATING THE SECOND
      *>        OCCURRENCE AND ASKING FOR A FOURTH LEAVES THE RELEASED
      *>        TOTAL AT 300.00, NOT 400.00, AND NOTHING IS RESIDUAL.
      *>----------------------------------------------------------------
       3000-ONCE-ONLY-TESTS.
           PERFORM 0950-SETUP-AK
           MOVE "STATUS" TO AH-FUNCTION
           MOVE "ZZTPRE01" TO AK-RECUR-CODE
           CALL "ACCRREL" USING WS-ACCRREL-CA
           MOVE "AC-011 the prepayment has released 300.00 in all"
               TO TK-NAME
           MOVE 300.00 TO TK-EXPECTED-N
           MOVE AK-RELEASED-TOTAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-011 the prepayment leaves a zero residual"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE AK-REMAINING TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

      *>   Repeat the second occurrence.  It is reported, never posted
      *>   again, and the released total does not become 400.00.
           PERFORM 0950-SETUP-AK
           MOVE "RELEASE" TO AH-FUNCTION
           MOVE "ZZTPRE01" TO AK-RECUR-CODE
           MOVE 2 TO AK-OCCURRENCE-NO
           CALL "ACCRREL" USING WS-ACCRREL-CA
           MOVE "AC-011 the second occurrence is already posted"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE AK-ALREADY-POSTED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-011 the repeat is reported, not refused" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE AH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-011 the original journal is quoted back" TO TK-NAME
           MOVE "ZZT-JNL-0002" TO TK-EXPECTED-X
           MOVE AK-JOURNAL-NUMBER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-011 the released total is 300.00 and not 400.00"
               TO TK-NAME
           MOVE 300.00 TO TK-EXPECTED-N
           MOVE AK-RELEASED-TOTAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

      *>   There is no fourth release: the schedule is exhausted.
           PERFORM 0950-SETUP-AK
           MOVE "RELEASE" TO AH-FUNCTION
           MOVE "ZZTPRE01" TO AK-RECUR-CODE
           MOVE 4 TO AK-OCCURRENCE-NO
           CALL "ACCRREL" USING WS-ACCRREL-CA
           MOVE "AC-011 a fourth release is beyond the schedule"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE AH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-011 the refusal is reported as ACR0030" TO TK-NAME
           MOVE "ACR0030" TO TK-EXPECTED-X
           MOVE AH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-011 no fourth release is recorded" TO TK-NAME
           MOVE SPACES TO TK-EXPECTED-X
           MOVE AK-JOURNAL-NUMBER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 0950-SETUP-AK
           MOVE "NEXTOCC" TO AH-FUNCTION
           MOVE "ZZTPRE01" TO AK-RECUR-CODE
           CALL "ACCRREL" USING WS-ACCRREL-CA
           MOVE "AC-011 an exhausted schedule offers nothing more"
               TO TK-NAME
           IF AK-OCCURRENCE-NO = 0 OR AH-RETURN-CODE > 0
               PERFORM TK-PASS
           ELSE
               MOVE AH-MESSAGE TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 4000 - AC-011 AN OCCURRENCE SCHEDULED INTO A CLOSED PERIOD IS
      *>        RAISED AS AN EXCEPTION.  IT IS NEVER SILENTLY MOVED TO
      *>        A DATE THAT HAPPENS TO BE OPEN.
      *>----------------------------------------------------------------
       4000-CLOSED-PERIOD-TESTS.
           PERFORM 0950-SETUP-AK
           MOVE "RELEASE" TO AH-FUNCTION
           MOVE "ZZTREC04" TO AK-RECUR-CODE
           MOVE 1 TO AK-OCCURRENCE-NO
           CALL "ACCRREL" USING WS-ACCRREL-CA
           MOVE "AC-011 a release into a closed period is blocked"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE AK-BLOCKED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-011 the closed period is reported as hard closed"
               TO TK-NAME
           MOVE "HARD" TO TK-EXPECTED-X
           MOVE AK-PERIOD-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-011 the scheduled date is not quietly redated"
               TO TK-NAME
           MOVE 20260815 TO TK-EXPECTED-N
           MOVE AK-POSTING-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-011 the block carries a recorded reason" TO TK-NAME
           IF AK-BLOCK-REASON NOT = SPACES
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE "AC-011 the blocked release is raised, not ignored"
               TO TK-NAME
           IF AH-RETURN-CODE > 4
               PERFORM TK-PASS
           ELSE
               MOVE AH-MESSAGE TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "AC-011 the block is kept as an exception" TO TK-NAME
           IF AK-EXCEPTION-ID NOT = 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

       COPY TESTKITP.
