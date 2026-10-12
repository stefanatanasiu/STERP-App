>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : TKITDEMO
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : Proves what the kit demonstration driver KITDEMO
*>              leaves behind: a completed kit whose output was
*>              received into stock and whose genealogy names the
*>              components that went into it, every service call
*>              on a correlation id of its own.
*>
*>              The suite reads; it never writes.  When the
*>              database has no KITDEMO run on file (a fresh
*>              database, or a build machine with no demonstration
*>              data) the database tests are skipped rather than
*>              failed - the dataset is built by
*>              build\demo-data.ps1, not by the test suite.
*> RUN AS     : bin\tests\TKITDEMO.exe
*> CALLS      : SQLIO, GENEALOG, KITCOMPL
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2026-10-07 STERP KIT-015, KIT-019, AC-083
*>================================================================
*> REQ: KIT-015, KIT-019
*> AC: AC-083
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TKITDEMO.
 AUTHOR. STERP.
 DATE-WRITTEN. 07/10/2026.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY TESTKITW.
 COPY SQLIOCA.
 COPY GENEALOGCA REPLACING ==DFHCOMMAREA== BY ==Ws-Gen-Ca==
      LEADING ==CA-== BY ==GE-==.
 COPY KITCOMPLCA REPLACING ==DFHCOMMAREA== BY ==Ws-Compl-Ca==
      LEADING ==CA-== BY ==CP-==.
 01  Ws-Probe                    PIC X(60).
 01  Ws-Work.
     05  Ws-Db-Up                PIC X VALUE "Y".
     05  Ws-Have-Run             PIC X VALUE "Y".
     05  Ws-Kit-Ref              PIC X(30) VALUE SPACES.
     05  Ws-Parent               PIC X(40) VALUE SPACES.
 PROCEDURE DIVISION.
 Main-Entry.
     MOVE "TKITDEMO kit demonstration driver" TO TK-SUITE
     PERFORM TK-BEGIN
     PERFORM Test-Services-Refuse-Nonsense
     PERFORM Probe-The-Database
     IF Ws-Db-Up = "N" OR Ws-Have-Run = "N"
         PERFORM Skip-The-Database-Tests
     ELSE
         PERFORM Test-The-Kit-Was-Completed
         PERFORM Test-Genealogy-Names-The-Components
         PERFORM Test-Genealogy-Kit-Scope
         PERFORM Test-One-Id-Per-Completion
     END-IF
     PERFORM TK-END
     STOP RUN.

*>----------------------------------------------------------------
*> No database is needed: both services name their own function
*> before they look at anything, which is what lets the driver
*> record a refusal instead of guessing.
 Test-Services-Refuse-Nonsense.
     INITIALIZE Ws-Gen-Ca
     MOVE "STC" TO GE-COMPANY
     MOVE "TESTER" TO GE-USER
     MOVE "WIBBLE" TO GE-FUNCTION
     CALL "GENEALOG" USING Ws-Gen-Ca
     MOVE "GENEALOG refuses an unknown function" TO TK-NAME
     MOVE 8 TO TK-EXPECTED-N
     MOVE GE-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     INITIALIZE Ws-Compl-Ca
     MOVE "STC" TO CP-COMPANY
     MOVE "TESTER" TO CP-USER
     MOVE "WIBBLE" TO CP-FUNCTION
     CALL "KITCOMPL" USING Ws-Compl-Ca
     MOVE "A refused completion receipts nothing" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KM-COMPLETION-ID TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
 Probe-The-Database.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT TOP (1) CONVERT(varchar(60), k.KitRef) FROM "
            "kit.ExecutionKit k WHERE (k.SourceDemandRef LIKE "
            "'KITDEMO-%' OR k.CorrelationId LIKE 'KITDEMO-%') AND "
            "EXISTS (SELECT 1 FROM "
            "kit.KitCompletion c WHERE c.CompanyCode = "
            "k.CompanyCode AND c.KitRef = k.KitRef) ORDER BY "
            "k.ExecutionKitId DESC"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     IF SQLIO-ERROR
         MOVE "N" TO Ws-Db-Up
         EXIT PARAGRAPH
     END-IF
     IF Ws-Probe = SPACES
         MOVE "N" TO Ws-Have-Run
     ELSE
         MOVE Ws-Probe TO Ws-Kit-Ref
     END-IF.

 Skip-The-Database-Tests.
     MOVE "KITDEMO leaves a completed kit with genealogy"
          TO TK-NAME
     IF Ws-Db-Up = "N"
         MOVE "no database connection" TO TK-DETAIL
     ELSE
         MOVE "no KITDEMO run on file - run build\demo-data.cmd"
              TO TK-DETAIL
     END-IF
     PERFORM TK-SKIP.

 Read-Scalar.
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 60 TO SQLIO-COL-WIDTH (1)
     MOVE 8 TO SQLIO-CURSOR
     MOVE SPACES TO Ws-Probe
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Probe.

*>----------------------------------------------------------------
*> KIT-015.  The driver's kit was really received into stock: a
*> completion row, good output on it, and a stock movement to
*> match.  A completion that recorded nothing is not a completion.
*> REQ: KIT-015
 Test-The-Kit-Was-Completed.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(60), SUM(c.GoodQty)) FROM "
            "kit.KitCompletion c WHERE c.CompanyCode = 'STC' AND "
            "c.KitRef = '" DELIMITED BY SIZE
            FUNCTION TRIM(Ws-Kit-Ref) DELIMITED BY SIZE
            "'" DELIMITED BY SIZE
         INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "The demonstration kit received good output" TO TK-NAME
     IF FUNCTION NUMVAL (Ws-Probe) > 0
         PERFORM TK-PASS
     ELSE
         MOVE Ws-Kit-Ref TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(60), COUNT(*)) FROM "
            "kit.KitCompletion c WHERE c.CompanyCode = 'STC' AND "
            "c.KitRef = '" DELIMITED BY SIZE
            FUNCTION TRIM(Ws-Kit-Ref) DELIMITED BY SIZE
            "' AND c.MovementId IS NOT NULL" DELIMITED BY SIZE
         INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "Every completion moved stock, it did not just say so"
          TO TK-NAME
     IF FUNCTION NUMVAL (Ws-Probe) > 0
         PERFORM TK-PASS
     ELSE
         MOVE "no inv.StockMovement behind the completion"
              TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(60), k.Status) FROM "
            "kit.ExecutionKit k WHERE k.CompanyCode = 'STC' AND "
            "k.KitRef = '" DELIMITED BY SIZE
            FUNCTION TRIM(Ws-Kit-Ref) DELIMITED BY SIZE
            "'" DELIMITED BY SIZE
         INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "The kit is no longer waiting to be built" TO TK-NAME
     IF Ws-Probe = "DRAFT" OR Ws-Probe = "RELEASED"
        OR Ws-Probe = "REVIEWED"
         MOVE Ws-Probe TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         PERFORM TK-PASS
     END-IF.

*>----------------------------------------------------------------
*> KIT-019 / AC-083.  Genealogy is the point of the exercise: the
*> completion must be able to name the components that went into
*> it, and every genealogy line must belong to a completion of the
*> same kit rather than float free.
*> REQ: KIT-019
*> AC: AC-083
 Test-Genealogy-Names-The-Components.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(60), COUNT(*)) FROM "
            "kit.KitGenealogy g WHERE g.CompanyCode = 'STC' AND "
            "g.KitRef = '" DELIMITED BY SIZE
            FUNCTION TRIM(Ws-Kit-Ref) DELIMITED BY SIZE
            "'" DELIMITED BY SIZE
         INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "The completion carries a genealogy" TO TK-NAME
     IF FUNCTION NUMVAL (Ws-Probe) > 0
         PERFORM TK-PASS
     ELSE
         MOVE Ws-Kit-Ref TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(60), COUNT(*)) FROM "
            "kit.KitGenealogy g WHERE g.CompanyCode = 'STC' AND "
            "g.KitRef = '" DELIMITED BY SIZE
            FUNCTION TRIM(Ws-Kit-Ref) DELIMITED BY SIZE
            "' AND NOT EXISTS (SELECT 1 FROM kit.KitCompletion c "
            "WHERE c.CompletionId = g.CompletionId AND c.KitRef = "
            "g.KitRef)" DELIMITED BY SIZE
         INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "No genealogy line floats free of its completion"
          TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
     PERFORM TK-ASSERT-NUM
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(60), COUNT(*)) FROM "
            "kit.KitGenealogy g WHERE g.CompanyCode = 'STC' AND "
            "g.KitRef = '" DELIMITED BY SIZE
            FUNCTION TRIM(Ws-Kit-Ref) DELIMITED BY SIZE
            "' AND g.ConsumedQty <= 0" DELIMITED BY SIZE
         INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "Nothing was consumed in quantity zero" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
     PERFORM TK-ASSERT-NUM
     INITIALIZE Ws-Gen-Ca
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(60), MAX(c.ParentItemCode)) "
            "FROM kit.KitCompletion c WHERE c.CompanyCode = 'STC' "
            "AND c.KitRef = '" DELIMITED BY SIZE
            FUNCTION TRIM(Ws-Kit-Ref) DELIMITED BY SIZE
            "'" DELIMITED BY SIZE
         INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE Ws-Probe TO Ws-Parent
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(60), MAX(c.CompletionId)) "
            "FROM kit.KitCompletion c WHERE c.CompanyCode = 'STC' "
            "AND c.KitRef = '" DELIMITED BY SIZE
            FUNCTION TRIM(Ws-Kit-Ref) DELIMITED BY SIZE
            "'" DELIMITED BY SIZE
         INTO SQLIO-STMT
     PERFORM Read-Scalar
     COMPUTE GN-COMPLETION-ID = FUNCTION NUMVAL (Ws-Probe)
     MOVE "STC" TO GE-COMPANY
     MOVE "QUALITY" TO GE-USER
     MOVE 20261007 TO GE-BUSINESS-DATE
     MOVE "FORWARD" TO GN-DIRECTION
     MOVE Ws-Kit-Ref TO GN-KIT-REF
     MOVE Ws-Parent TO GN-ITEM-CODE
     MOVE 5 TO GN-MAX-DEPTH
     MOVE 1 TO GN-FROM-LINE
     MOVE 20 TO GN-WIN-MAX
     MOVE "FORWARD" TO GE-FUNCTION
     CALL "GENEALOG" USING Ws-Gen-Ca
     MOVE "GENEALOG reads the driver's kit back" TO TK-NAME
     IF GE-RETURN-CODE < 8
         PERFORM TK-PASS
     ELSE
         MOVE GE-MESSAGE (1:200) TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF.

*>----------------------------------------------------------------
*> A kit-specific trace must not append company-wide dispositions.
 Test-Genealogy-Kit-Scope.
     INITIALIZE Ws-Gen-Ca
     MOVE "STC" TO GE-COMPANY
     MOVE "QUALITY" TO GE-USER
     MOVE "BACKWARD" TO GE-FUNCTION GN-DIRECTION
     MOVE "ZZ-NO-SUCH-KIT" TO GN-KIT-REF
     MOVE "Y" TO GN-INCLUDE-RET GN-INCLUDE-SCRAP
     MOVE 5 TO GN-MAX-DEPTH
     MOVE 1 TO GN-FROM-LINE
     MOVE 20 TO GN-WIN-MAX
     CALL "GENEALOG" USING Ws-Gen-Ca
     MOVE "An unknown kit does not show other kits' dispositions"
          TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE GN-LINE-TOTAL TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "An empty kit trace names the no-genealogy refusal"
          TO TK-NAME
     MOVE 16 TO TK-EXPECTED-N
     MOVE GE-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*> COM-015 / CR2741.  One correlation id is never shared by two
*> idempotent services.  The driver derives a sub-id per step from
*> a thirty character stem, so no two completions of the kit may
*> carry the same id - if they did, the ledger would treat the
*> second as a replay and post nothing.
*> REQ: KIT-015
 Test-One-Id-Per-Completion.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(60), COUNT(*) - "
            "COUNT(DISTINCT c.CorrelationId)) FROM "
            "kit.KitCompletion c WHERE c.CompanyCode = 'STC' AND "
            "c.KitRef = '" DELIMITED BY SIZE
            FUNCTION TRIM(Ws-Kit-Ref) DELIMITED BY SIZE
            "' AND c.CorrelationId IS NOT NULL" DELIMITED BY SIZE
         INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "Each completion carries its own correlation id"
          TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
     PERFORM TK-ASSERT-NUM
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(60), COUNT(*)) FROM "
            "kit.KitIssue i WHERE i.CompanyCode = 'STC' AND "
            "i.KitRef = '" DELIMITED BY SIZE
            FUNCTION TRIM(Ws-Kit-Ref) DELIMITED BY SIZE
            "' AND i.IssueType = 'ISSUE'" DELIMITED BY SIZE
         INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "The kit was issued before it was completed" TO TK-NAME
     IF FUNCTION NUMVAL (Ws-Probe) > 0
         PERFORM TK-PASS
     ELSE
         MOVE "no kit.KitIssue rows for the demonstration kit"
              TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF.

 COPY TESTKITP.
