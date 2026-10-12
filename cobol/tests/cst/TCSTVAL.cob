      *>================================================================
      *> PROGRAM-ID : TCSTVAL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY (UNIT TESTS)
      *> PURPOSE    : THE THREE VALUATION ENGINES - FIFOVAL, MWAVAL AND
      *>              STDCOST - PROVED AGAINST REAL ROWS IN THE cst
      *>              SCHEMA.  WORKED EXAMPLES C1 AND C2 OF SPEC 07.
      *>
      *>                C1  RECEIVE 10 AT 5.00 THEN 10 AT 8.00 AND
      *>                    ISSUE 12.  THE ISSUE IS RELIEVED AT 66.00
      *>                    AND EIGHT UNITS WORTH 64.00 ARE LEFT.
      *>                C2  100 UNITS WORTH 400.00 PLUS 50 WORTH
      *>                    350.00 AVERAGE AT 5.00.  AN ISSUE OF 40
      *>                    COSTS 200.00 AND LEAVES 110 WORTH 550.00.
      *>
      *>              EVERY DATABASE CASE IS BUILT INSIDE ONE
      *>              TRANSACTION THAT IS ALWAYS ROLLED BACK, SO THE
      *>              SUITE LEAVES NOTHING BEHIND AND MAY BE RUN AS
      *>              OFTEN AS ANYONE LIKES.  THE ENGINES ARE CALLED
      *>              WITH COMMIT FLAG "T" - WRITE, BUT DO NOT OWN THE
      *>              TRANSACTION.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: CST-002, CST-005, CST-006, CST-007, CST-009, CST-024
      *> REQ: CST-026
      *> AC : AC-C1, AC-C2
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCSTVAL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY SQLIOCA.
       COPY FIFOVALCA REPLACING ==DFHCOMMAREA== BY ==WS-FV-CA==.
       COPY MWAVALCA  REPLACING ==DFHCOMMAREA== BY ==WS-MW-CA==.
       COPY STDCOSTCA REPLACING ==DFHCOMMAREA== BY ==WS-SC-CA==.
       01  WS-PROBE                    PIC X(60).
       01  WS-WORK.
           05  WS-DB-UP                PIC X VALUE "Y".
           05  WS-TABLES               PIC X VALUE "Y".
           05  WS-SQL-BAD              PIC X VALUE "N".
           05  WS-SQL-MSG              PIC X(200) VALUE SPACES.
           05  WS-ITEM                 PIC X(40)
                                       VALUE "ZZTCSTVAL-NO-ITEM".
           05  WS-SET                  PIC X(20) VALUE "ZZVALSET".
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TCSTVAL valuation engines" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-ENGINES-REFUSE-NONSENSE
           PERFORM 8000-PROBE-DATABASE
           IF WS-DB-UP = "N" OR WS-TABLES = "N"
               PERFORM 8100-SKIP-DATABASE-TESTS
           ELSE
               PERFORM 8200-BEGIN-TRANSACTION
               PERFORM 2000-FIFO-WORKED-EXAMPLE
               PERFORM 3000-MWA-WORKED-EXAMPLE
               PERFORM 4000-STANDARD-FROM-A-SET
               PERFORM 8300-ROLLBACK-TRANSACTION
           END-IF
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *> 1000  NO DATABASE IS NEEDED HERE.  BOTH ENGINES VALIDATE THEIR
      *>       KEYS BEFORE THEY GO ANYWHERE NEAR A TABLE, AND FIFOVAL
      *>       NAMES AN UNKNOWN FUNCTION WITHOUT READING A LAYER.
      *>----------------------------------------------------------------
       1000-ENGINES-REFUSE-NONSENSE.
           PERFORM 1900-RESET-FIFO
           MOVE SPACES TO FV-ITEM-CODE
           CALL "FIFOVAL" USING WS-FV-CA
           MOVE "FIFOVAL refuses an issue with no item code"
               TO TK-NAME
           MOVE "CST0201" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-FV-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A refused FIFO issue values nothing" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE FV-ISSUE-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 1900-RESET-FIFO
           MOVE 0 TO FV-QUANTITY
           CALL "FIFOVAL" USING WS-FV-CA
           MOVE "FIFOVAL refuses an issue of nothing" TO TK-NAME
           MOVE "CST0202" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-FV-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1900-RESET-FIFO
           MOVE "WIBBLE" TO CA-FUNCTION OF WS-FV-CA
           CALL "FIFOVAL" USING WS-FV-CA
           MOVE "FIFOVAL refuses an unknown function" TO TK-NAME
           MOVE "CST0207" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-FV-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1950-RESET-MWA
           MOVE SPACES TO MW-ITEM-CODE
           CALL "MWAVAL" USING WS-MW-CA
           MOVE "MWAVAL refuses a pool with no item code" TO TK-NAME
           MOVE "CST0301" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-MW-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A refused average issue relieves nothing" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE MW-ISSUE-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 1950-RESET-MWA
           MOVE 0 TO MW-QUANTITY
           CALL "MWAVAL" USING WS-MW-CA
           MOVE "MWAVAL refuses an issue of nothing" TO TK-NAME
           MOVE "CST0302" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-MW-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1970-RESET-STD
           MOVE SPACES TO SC-COST-SET
           MOVE SPACES TO CA-COMPANY OF WS-SC-CA
           CALL "STDCOST" USING WS-SC-CA
           MOVE "STDCOST refuses a review with no company" TO TK-NAME
           MOVE "CST0401" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       1900-RESET-FIFO.
           INITIALIZE WS-FV-CA
           MOVE "ISSUE" TO CA-FUNCTION OF WS-FV-CA
           MOVE "STC" TO CA-COMPANY OF WS-FV-CA
           MOVE "RDG" TO CA-SITE OF WS-FV-CA
           MOVE "COSTCLRK" TO CA-USER OF WS-FV-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-FV-CA
           MOVE WS-ITEM TO FV-ITEM-CODE
           MOVE 1 TO FV-QUANTITY
           MOVE "N" TO FV-COMMIT.

       1950-RESET-MWA.
           INITIALIZE WS-MW-CA
           MOVE "ISSUE" TO CA-FUNCTION OF WS-MW-CA
           MOVE "STC" TO CA-COMPANY OF WS-MW-CA
           MOVE "RDG" TO CA-SITE OF WS-MW-CA
           MOVE "COSTCLRK" TO CA-USER OF WS-MW-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-MW-CA
           MOVE WS-ITEM TO MW-ITEM-CODE
           MOVE "RDG" TO MW-POOL-SITE
           MOVE 1 TO MW-QUANTITY
           MOVE "N" TO MW-COMMIT.

       1970-RESET-STD.
           INITIALIZE WS-SC-CA
           MOVE "GETSTD" TO CA-FUNCTION OF WS-SC-CA
           MOVE "STC" TO CA-COMPANY OF WS-SC-CA
           MOVE "RDG" TO CA-SITE OF WS-SC-CA
           MOVE "COSTACC" TO CA-USER OF WS-SC-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-SC-CA
           MOVE WS-ITEM TO SC-ITEM-CODE
           MOVE WS-SET TO SC-COST-SET
           MOVE 20261006 TO SC-EFF-DATE
           MOVE 1 TO SC-LOT-SIZE
           MOVE "N" TO SC-COMMIT.

      *>----------------------------------------------------------------
      *> 2000  WORKED EXAMPLE C1.  TWO LAYERS, ONE ISSUE OF TWELVE.
      *>       THE ANSWER IS 66.00 AND IT IS NOT AN AVERAGE - AN
      *>       AVERAGE WOULD HAVE SAID 12 x 6.50 = 78.00.
      *>       REQ: CST-005, CST-009   AC: AC-C1
      *>----------------------------------------------------------------
       2000-FIFO-WORKED-EXAMPLE.
           PERFORM 2900-BUILD-FIFO-POOL
           IF WS-SQL-BAD = "Y"
               MOVE "C1 FIFO pool could not be built" TO TK-NAME
               MOVE WS-SQL-MSG TO TK-DETAIL
               PERFORM TK-FAIL
               EXIT PARAGRAPH
           END-IF
           PERFORM 1900-RESET-FIFO
           MOVE 12 TO FV-QUANTITY
           MOVE "ISSUE" TO FV-DOC-TYPE
           MOVE "ZZISS1" TO FV-DOC-REF
           MOVE "T" TO FV-COMMIT
           CALL "FIFOVAL" USING WS-FV-CA
           MOVE "C1 the FIFO issue is accepted" TO TK-NAME
           IF CA-RETURN-CODE OF WS-FV-CA = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-FV-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "C1 an issue of twelve is relieved at 66.00"
               TO TK-NAME
           MOVE 66.00 TO TK-EXPECTED-N
           MOVE FV-ISSUE-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C1 the issue took value from both layers" TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE FV-LAYERS-USED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C1 the older layer gave up all ten units" TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE FV-L-QTY-TAKEN (1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C1 the newer layer gave up only two units"
               TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE FV-L-QTY-TAKEN (2) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C1 eight units are left in the pool" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE FV-REMAIN-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C1 what is left is worth 64.00" TO TK-NAME
           MOVE 64.00 TO TK-EXPECTED-N
           MOVE FV-REMAIN-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C1 the average unit cost of the issue is 5.50"
               TO TK-NAME
           MOVE 5.50 TO TK-EXPECTED-N
           MOVE FV-ISSUE-UNIT-AVG TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 2920-QUERY-THE-POOL
           PERFORM 2940-REFUSE-NEGATIVE-STOCK.

      *>  THE QUERY READS THE LAYERS BACK FROM THE TABLE, SO IT PROVES
      *>  THE ISSUE WAS WRITTEN AND NOT ONLY CALCULATED.
       2920-QUERY-THE-POOL.
           PERFORM 1900-RESET-FIFO
           MOVE "QUERY" TO CA-FUNCTION OF WS-FV-CA
           CALL "FIFOVAL" USING WS-FV-CA
           MOVE "C1 one open layer survives the issue" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE FV-OPEN-LAYER-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C1 the stored pool is eight units" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE FV-POOL-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C1 the stored pool is worth 64.00" TO TK-NAME
           MOVE 64.00 TO TK-EXPECTED-N
           MOVE FV-POOL-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C1 the surviving layer is the 8.00 one" TO TK-NAME
           MOVE 8.000000 TO TK-EXPECTED-N
           MOVE FV-L-UNIT-COST (1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>  CST-005.  STOCK CANNOT BE WORTH LESS THAN NOTHING, SO AN
      *>  ISSUE THAT OUTRUNS THE LAYERS IS REFUSED RATHER THAN
      *>  INVENTING A COST FOR THE UNCOVERED UNITS.
       2940-REFUSE-NEGATIVE-STOCK.
           PERFORM 1900-RESET-FIFO
           MOVE 100 TO FV-QUANTITY
           CALL "FIFOVAL" USING WS-FV-CA
           MOVE "C1 an issue beyond the layers is refused" TO TK-NAME
           MOVE "CST0204" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-FV-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "C1 the refusal is an error, not a warning" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-FV-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       2900-BUILD-FIFO-POOL.
           PERFORM 8400-CLEAR-ITEM-COST
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT cst.ItemCost (CompanyCode, ItemCode, "
                  "Revision, SiteCode, CostMethod, QtyOnHand, "
                  "CarryingValueGBP, AverageUnitCost, "
                  "LastUpdatedBy) VALUES ('STC','"
                  FUNCTION TRIM(WS-ITEM)
                  "','','RDG','FIFO',20,130.00,6.500000,'TCSTVAL')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT cst.CostLayer (CompanyCode, ItemCode, "
                  "Revision, LayerSeq, ReceiptDate, SourceDocType, "
                  "SourceDocRef, QtyOriginal, QtyRemaining, "
                  "UnitCost, ValueRemainingGBP, IsClosed) VALUES "
                  "('STC','" FUNCTION TRIM(WS-ITEM)
                  "','',1,'2026-09-01','GRN','ZZGRN1',"
                  "10,10,5.000000,50.00,0),('STC','"
                  FUNCTION TRIM(WS-ITEM)
                  "','',2,'2026-09-14','GRN','ZZGRN2',"
                  "10,10,8.000000,80.00,0)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT.

      *>----------------------------------------------------------------
      *> 3000  WORKED EXAMPLE C2.  A RECEIPT MOVES THE AVERAGE; AN
      *>       ISSUE NEVER DOES.  REQ: CST-006, CST-024   AC: AC-C2
      *>----------------------------------------------------------------
       3000-MWA-WORKED-EXAMPLE.
           PERFORM 3900-BUILD-AVERAGE-POOL
           IF WS-SQL-BAD = "Y"
               MOVE "C2 average pool could not be built" TO TK-NAME
               MOVE WS-SQL-MSG TO TK-DETAIL
               PERFORM TK-FAIL
               EXIT PARAGRAPH
           END-IF
           PERFORM 1950-RESET-MWA
           MOVE "RECEIPT" TO CA-FUNCTION OF WS-MW-CA
           MOVE 50 TO MW-QUANTITY
           MOVE 350.00 TO MW-VALUE-GBP
           MOVE "GRN" TO MW-DOC-TYPE
           MOVE "ZZGRN3" TO MW-DOC-REF
           MOVE "T" TO MW-COMMIT
           CALL "MWAVAL" USING WS-MW-CA
           MOVE "C2 the receipt is accepted" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-MW-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C2 the pool was a hundred units at 4.00 before"
               TO TK-NAME
           MOVE 4.000000 TO TK-EXPECTED-N
           MOVE MW-AVG-BEFORE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C2 the new average is 750.00 over 150 = 5.00"
               TO TK-NAME
           MOVE 5.000000 TO TK-EXPECTED-N
           MOVE MW-AVG-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C2 the pool is 150 units after the receipt"
               TO TK-NAME
           MOVE 150 TO TK-EXPECTED-N
           MOVE MW-QTY-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C2 the pool carries 750.00 after the receipt"
               TO TK-NAME
           MOVE 750.00 TO TK-EXPECTED-N
           MOVE MW-VALUE-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 3920-ISSUE-AT-THE-AVERAGE
           PERFORM 3940-REFUSE-NEGATIVE-STOCK.

       3920-ISSUE-AT-THE-AVERAGE.
           PERFORM 1950-RESET-MWA
           MOVE 40 TO MW-QUANTITY
           MOVE "ISSUE" TO MW-DOC-TYPE
           MOVE "ZZISS2" TO MW-DOC-REF
           MOVE "T" TO MW-COMMIT
           CALL "MWAVAL" USING WS-MW-CA
           MOVE "C2 an issue of forty costs 200.00" TO TK-NAME
           MOVE 200.00 TO TK-EXPECTED-N
           MOVE MW-ISSUE-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C2 a hundred and ten units remain" TO TK-NAME
           MOVE 110 TO TK-EXPECTED-N
           MOVE MW-QTY-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C2 what remains is worth 550.00" TO TK-NAME
           MOVE 550.00 TO TK-EXPECTED-N
           MOVE MW-VALUE-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C2 an issue does not move the average" TO TK-NAME
           MOVE 5.000000 TO TK-EXPECTED-N
           MOVE MW-AVG-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C2 nothing is stranded on a pool that is not empty"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE MW-RESIDUAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 1950-RESET-MWA
           MOVE "QUERY" TO CA-FUNCTION OF WS-MW-CA
           CALL "MWAVAL" USING WS-MW-CA
           MOVE "C2 the stored pool reads back at 550.00" TO TK-NAME
           MOVE 550.00 TO TK-EXPECTED-N
           MOVE MW-VALUE-BEFORE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C2 the stored pool is on the average method"
               TO TK-NAME
           MOVE "MWA" TO TK-EXPECTED-X
           MOVE MW-COST-METHOD TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       3940-REFUSE-NEGATIVE-STOCK.
           PERFORM 1950-RESET-MWA
           MOVE 500 TO MW-QUANTITY
           CALL "MWAVAL" USING WS-MW-CA
           MOVE "C2 an issue beyond the pool is refused" TO TK-NAME
           MOVE "CST0303" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-MW-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1950-RESET-MWA
           MOVE "WIBBLE" TO CA-FUNCTION OF WS-MW-CA
           CALL "MWAVAL" USING WS-MW-CA
           MOVE "MWAVAL refuses an unknown function" TO TK-NAME
           MOVE "CST0307" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-MW-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       3900-BUILD-AVERAGE-POOL.
           PERFORM 8400-CLEAR-ITEM-COST
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT cst.ItemCost (CompanyCode, ItemCode, "
                  "Revision, SiteCode, CostMethod, QtyOnHand, "
                  "CarryingValueGBP, AverageUnitCost, AllowanceGBP, "
                  "LastUpdatedBy) VALUES ('STC','"
                  FUNCTION TRIM(WS-ITEM)
                  "','','RDG','MWA',100,400.00,4.000000,0,"
                  "'TCSTVAL')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT.

      *>----------------------------------------------------------------
      *> 4000  CST-002 AND CST-007.  A STANDARD IS READ FROM A NAMED
      *>       COST SET, AND AN ITEM WITH NO STANDARD IN THAT SET IS
      *>       REPORTED AS NOT FOUND RATHER THAN COSTED AT ZERO.
      *>----------------------------------------------------------------
       4000-STANDARD-FROM-A-SET.
           PERFORM 4900-BUILD-COST-SET
           IF WS-SQL-BAD = "Y"
               MOVE "C3 the cost set could not be built" TO TK-NAME
               MOVE WS-SQL-MSG TO TK-DETAIL
               PERFORM TK-FAIL
               EXIT PARAGRAPH
           END-IF
           PERFORM 1970-RESET-STD
           CALL "STDCOST" USING WS-SC-CA
           MOVE "The named set answers a standard of 12.00"
               TO TK-NAME
           MOVE 12.000000 TO TK-EXPECTED-N
           MOVE SC-STD-UNIT-COST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The previous standard of 10.00 comes with it"
               TO TK-NAME
           MOVE 10.000000 TO TK-EXPECTED-N
           MOVE SC-PREV-UNIT-COST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The set that answered is the active one" TO TK-NAME
           MOVE "ACTIVE" TO TK-EXPECTED-X
           MOVE SC-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The standard is split over its elements" TO TK-NAME
           MOVE 7.000000 TO TK-EXPECTED-N
           MOVE SC-EL-MATERIAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 1970-RESET-STD
           MOVE "IMPACT" TO CA-FUNCTION OF WS-SC-CA
           CALL "STDCOST" USING WS-SC-CA
           MOVE "Impact retains the cost set lifecycle status"
               TO TK-NAME
           MOVE "ACTIVE" TO TK-EXPECTED-X
           MOVE SC-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "Impact names the independent preparer" TO TK-NAME
           MOVE "COSTCLRK" TO TK-EXPECTED-X
           MOVE SC-PREPARED-BY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "Impact names the approving user" TO TK-NAME
           MOVE "FINMGR" TO TK-EXPECTED-X
           MOVE SC-APPROVED-BY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1970-RESET-STD
           MOVE "IMPACT" TO CA-FUNCTION OF WS-SC-CA
           MOVE "ZZNOSUCH" TO SC-COST-SET
           CALL "STDCOST" USING WS-SC-CA
           MOVE "Impact refuses an unknown cost set" TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-SC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 1970-RESET-STD
           MOVE "ZZNOSUCH" TO SC-COST-SET
           CALL "STDCOST" USING WS-SC-CA
           MOVE "An item with no standard in the set is not found"
               TO TK-NAME
           MOVE "CST0406" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A missing standard is never quietly zero" TO TK-NAME
           MOVE "INCOMPLETE" TO TK-EXPECTED-X
           MOVE SC-COMPLETENESS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       4900-BUILD-COST-SET.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE e FROM cst.StandardCostElement e JOIN "
                  "cst.StandardCost s ON s.StandardCostId = "
                  "e.StandardCostId WHERE s.CompanyCode = 'STC' AND "
                  "s.CostSetCode = 'ZZVALSET'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM cst.StandardCost WHERE CompanyCode = "
                  "'STC' AND CostSetCode = 'ZZVALSET'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM cst.CostSet WHERE CompanyCode = "
                  "'STC' AND CostSetCode = 'ZZVALSET'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT cst.CostSet (CompanyCode, CostSetCode, "
                  "Name, Purpose, EffectiveFrom, CurrencyCode, "
                  "FiscalYear, Status, PreparedBy, ApprovedBy, "
                  "ApprovedAt, CreatedBy) VALUES ('STC','ZZVALSET',"
                  "'Valuation test set','STANDARD','2026-09-01',"
                  "'GBP',2027,'ACTIVE','COSTCLRK','FINMGR',"
                  "SYSUTCDATETIME(),'TCSTVAL')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT cst.StandardCost (CompanyCode, "
                  "CostSetCode, ItemCode, Revision, CostingLotSize, "
                  "TotalUnitCost, PreviousUnitCost, VariancePct, "
                  "Completeness) VALUES ('STC','ZZVALSET','"
                  FUNCTION TRIM(WS-ITEM)
                  "','',1,12.000000,10.000000,20.0000,'COMPLETE')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT cst.StandardCostElement (StandardCostId, "
                  "ElementCode, UnitCost, FixedCostPerLot) SELECT "
                  "s.StandardCostId, v.El, v.Amt, 0 FROM "
                  "cst.StandardCost s CROSS JOIN (SELECT TOP 1 "
                  "ElementCode AS El, CAST(7.000000 AS "
                  "decimal(19,6)) AS Amt FROM cst.CostElement WHERE "
                  "ElementType = 'MATERIAL' ORDER BY ElementCode) v "
                  "WHERE s.CompanyCode = 'STC' AND s.CostSetCode = "
                  "'ZZVALSET'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT.

      *>================================================================
      *> 8000  DATABASE PLUMBING.  PROBE, BEGIN, BUILD, ROLL BACK -
      *>       THE SAME SHAPE AS TKITCST IN THE KITTING SUITE.
      *>================================================================
       8000-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM "
                  "sys.objects o JOIN sys.schemas s ON s.schema_id "
                  "= o.schema_id WHERE s.name = 'cst' AND o.name IN "
                  "('CostLayer','ItemCost','CostSet',"
                  "'StandardCost','StandardCostElement')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8500-READ-SCALAR
           IF WS-SQL-BAD = "Y"
               MOVE "N" TO WS-DB-UP
               MOVE "N" TO WS-SQL-BAD
               EXIT PARAGRAPH
           END-IF
           IF FUNCTION NUMVAL (WS-PROBE) < 5
               MOVE "N" TO WS-TABLES
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 ItemCode FROM inv.Item WHERE "
                  "CompanyCode = 'STC' ORDER BY ItemCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8500-READ-SCALAR
           IF WS-SQL-BAD = "Y" OR WS-PROBE = SPACES
               MOVE "N" TO WS-TABLES
               MOVE "N" TO WS-SQL-BAD
               EXIT PARAGRAPH
           END-IF
           MOVE WS-PROBE (1:40) TO WS-ITEM.

       8100-SKIP-DATABASE-TESTS.
           MOVE "Valuation against real cost layers and pools"
               TO TK-NAME
           IF WS-DB-UP = "N"
               MOVE "no database connection" TO TK-DETAIL
           ELSE
               MOVE "the cst tables are not deployed" TO TK-DETAIL
           END-IF
           PERFORM TK-SKIP.

       8200-BEGIN-TRANSACTION.
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 7 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA, WS-PROBE.

       8300-ROLLBACK-TRANSACTION.
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 7 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA, WS-PROBE.

       8400-CLEAR-ITEM-COST.
           MOVE "N" TO WS-SQL-BAD
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM cst.CostLayer WHERE CompanyCode = "
                  "'STC' AND ItemCode = '" FUNCTION TRIM(WS-ITEM)
                  "'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM cst.ItemCost WHERE CompanyCode = "
                  "'STC' AND ItemCode = '" FUNCTION TRIM(WS-ITEM)
                  "'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT.

       8500-READ-SCALAR.
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 60 TO SQLIO-COL-WIDTH (1)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           MOVE SPACES TO WS-PROBE
           CALL "SQLIO" USING SQLIO-CA, WS-PROBE
           IF SQLIO-SQLCODE < 0
               MOVE "Y" TO WS-SQL-BAD
               MOVE SQLIO-ERRMSG (1:200) TO WS-SQL-MSG
           END-IF.

       8600-RUN-STATEMENT.
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA, WS-PROBE
           IF SQLIO-SQLCODE < 0
               MOVE "Y" TO WS-SQL-BAD
               MOVE SQLIO-ERRMSG (1:200) TO WS-SQL-MSG
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA, WS-PROBE.

       COPY TESTKITP.
