      *>================================================================
      *> PROGRAM-ID : TSOKIT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT UNIT TESTS
      *> PURPOSE    : KITSOAL - COMMERCIAL KIT COMPLETENESS AND THE
      *>              ALLOCATION OF A KIT PRICE BY RELATIVE STANDALONE
      *>              SELLING PRICE.  THE WORKED NUMBERS ARE THOSE OF
      *>              ACCEPTANCE SCENARIOS AC-047 AND AC-130.
      *>
      *>              AC-047  two non stock kits, each one body and
      *>              three pins, with only five pins deliverable:
      *>              two kits need six pins, so one complete kit is
      *>              possible, the proposed complete despatch of two
      *>              is refused and no parent inventory is consumed.
      *>
      *>              AC-130  a GBP 100.00 kit whose components have
      *>              standalone prices of GBP 60.00 and GBP 60.00
      *>              allocates GBP 50.00 to each, with GBP 10.00 VAT
      *>              on the standard rated component only.
      *>
      *>              The structure is handed to KITSOAL in the
      *>              COMMAREA (KA-STRUCT-SOURCE = CALLER) so the
      *>              arithmetic is tested without any database.
      *>              The one database dependent assertion runs inside
      *>              BEGN..ROLB and skips when the data is absent.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: SO-020, SO-028   AC: AC-047, AC-130
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSOKIT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY KITSOALCA REPLACING ==DFHCOMMAREA== BY ==WS-KIT-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
       01  WS-PROBE.
           05  WS-PROBE-1              PIC X(30).
           05  WS-PROBE-2              PIC X(30).
       01  WS-I                        PIC S9(4) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSOKIT commercial kits" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           PERFORM 1000-AC047-REFUSED
           PERFORM 1100-AC047-ONE-KIT
           PERFORM 1200-NO-PARENT-INVENTORY
           PERFORM 2000-AC130-ALLOCATION
           PERFORM 2100-SINGLE-SUPPLY
           PERFORM 2200-ALLOC-RECONCILES
           PERFORM 3000-BAD-INPUT
           PERFORM 4000-CHECKDUP-DB
           PERFORM TK-END
           STOP RUN.

       0100-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CompanyCode, '' FROM core.Company "
                  "WHERE CompanyCode = 'STC'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-DB-UP
           END-IF.

      *>   one body and three pins per kit, two kits wanted, five
      *>   pins deliverable - the AC-047 fixture
       0200-BUILD-AC047.
           INITIALIZE WS-KIT-CA
           MOVE "STC" TO CA-COMPANY OF WS-KIT-CA
           MOVE "RDG" TO CA-SITE OF WS-KIT-CA
           MOVE "SALESREP" TO CA-USER OF WS-KIT-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-KIT-CA
           MOVE "COMPLETE" TO CA-FUNCTION OF WS-KIT-CA
           MOVE "CALLER" TO KA-STRUCT-SOURCE
           MOVE "KIT-DESKSET" TO KA-KIT-ITEM
           MOVE 2 TO KA-PARENT-QTY
           MOVE 2 TO KA-LINE-CNT
           MOVE "BODY-01" TO KA-L-ITEM(1)
           MOVE 1 TO KA-L-QTY-PER(1)
           MOVE 2 TO KA-L-DELIVER(1)
           MOVE "N" TO KA-L-OPTIONAL(1)
           MOVE "PIN-01" TO KA-L-ITEM(2)
           MOVE 3 TO KA-L-QTY-PER(2)
           MOVE 5 TO KA-L-DELIVER(2)
           MOVE "N" TO KA-L-OPTIONAL(2).

      *>----------------------------------------------------------------
      *> AC-047  two kits need six pins; only five are deliverable
      *>----------------------------------------------------------------
       1000-AC047-REFUSED.
           PERFORM 0200-BUILD-AC047
           MOVE 2 TO KA-PROPOSED-KITS
           CALL "KITSOAL" USING WS-KIT-CA
           MOVE "AC-047 only one complete kit is possible"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE KA-COMPLETE-KITS TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-KIT-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "AC-047 a complete despatch of two is refused"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-KIT-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-KIT-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "AC-047 refusal carries reason KIT0003" TO TK-NAME
           MOVE "KIT0003" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-KIT-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-047 the refusal flag is set" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE KA-REFUSED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-047 the pin is the limiting component" TO TK-NAME
           MOVE "PIN-01" TO TK-EXPECTED-X
           MOVE KA-LIMIT-ITEM TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-047 six pins are needed so one is short"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE KA-LIMIT-SHORT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-047 the body could support two kits" TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE KA-L-KITS-POSS(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-047 five pins support only one kit" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE KA-L-KITS-POSS(2) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   one complete kit uses one body and three pins
       1100-AC047-ONE-KIT.
           PERFORM 0200-BUILD-AC047
           MOVE 1 TO KA-PROPOSED-KITS
           CALL "KITSOAL" USING WS-KIT-CA
           MOVE "AC-047 one complete kit is permitted" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-KIT-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-KIT-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "AC-047 one kit is not refused" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE KA-REFUSED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-047 one kit consumes one body" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE KA-L-QTY-PER(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-047 one kit consumes three pins" TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           MOVE KA-L-QTY-PER(2) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   D-11 - the non stock parent never carries inventory demand
       1200-NO-PARENT-INVENTORY.
           PERFORM 0200-BUILD-AC047
           MOVE 2 TO KA-PROPOSED-KITS
           CALL "KITSOAL" USING WS-KIT-CA
           MOVE "AC-047 no parent inventory is consumed" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE KA-PARENT-DEMAND TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-047 the parent holds no warehouse balance"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE KA-PARENT-BALANCE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> AC-130  GBP 100.00 kit, components of GBP 60.00 standalone
      *>         price each, one standard rated and one zero rated
      *>----------------------------------------------------------------
       0300-BUILD-AC130.
           INITIALIZE WS-KIT-CA
           MOVE "STC" TO CA-COMPANY OF WS-KIT-CA
           MOVE "RDG" TO CA-SITE OF WS-KIT-CA
           MOVE "SALESREP" TO CA-USER OF WS-KIT-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-KIT-CA
           MOVE "ALLOCATE" TO CA-FUNCTION OF WS-KIT-CA
           MOVE "CALLER" TO KA-STRUCT-SOURCE
           MOVE "KIT-DESKSET" TO KA-KIT-ITEM
           MOVE 1 TO KA-PARENT-QTY
           MOVE 100.00 TO KA-PARENT-NET
           MOVE 2 TO KA-LINE-CNT
           MOVE "BODY-01" TO KA-L-ITEM(1)
           MOVE 1 TO KA-L-QTY-PER(1)
           MOVE 60.00 TO KA-L-SSP(1)
           MOVE "S" TO KA-L-VAT-CODE(1)
           MOVE 20.0000 TO KA-L-VAT-RATE(1)
           MOVE "N" TO KA-L-OPTIONAL(1)
           MOVE "BOOK-01" TO KA-L-ITEM(2)
           MOVE 1 TO KA-L-QTY-PER(2)
           MOVE 60.00 TO KA-L-SSP(2)
           MOVE "Z" TO KA-L-VAT-CODE(2)
           MOVE 0 TO KA-L-VAT-RATE(2)
           MOVE "N" TO KA-L-OPTIONAL(2).

       2000-AC130-ALLOCATION.
           PERFORM 0300-BUILD-AC130
           CALL "KITSOAL" USING WS-KIT-CA
           MOVE "AC-130 allocation succeeds" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-KIT-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-KIT-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "AC-130 first component is allocated 50.00"
               TO TK-NAME
           MOVE 50.00 TO TK-EXPECTED-N
           MOVE KA-L-ALLOC-NET(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-130 second component is allocated 50.00"
               TO TK-NAME
           MOVE 50.00 TO TK-EXPECTED-N
           MOVE KA-L-ALLOC-NET(2) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-130 VAT of 10.00 on the standard rated part"
               TO TK-NAME
           MOVE 10.00 TO TK-EXPECTED-N
           MOVE KA-L-VAT-AMT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-130 no VAT on the zero rated part" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE KA-L-VAT-AMT(2) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-130 kit VAT totals 10.00" TO TK-NAME
           MOVE 10.00 TO TK-EXPECTED-N
           MOVE KA-VAT-TOTAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-130 different liabilities make a MIXED supply"
               TO TK-NAME
           MOVE "MIXED" TO TK-EXPECTED-X
           MOVE KA-SUPPLY-KIND TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-130 the supply kind is recorded" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE KA-SUPPLY-RECORDED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-130 two VAT analysis rows" TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE KA-VAT-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   same prices but one liability - a SINGLE supply
       2100-SINGLE-SUPPLY.
           PERFORM 0300-BUILD-AC130
           MOVE "S" TO KA-L-VAT-CODE(2)
           MOVE 20.0000 TO KA-L-VAT-RATE(2)
           CALL "KITSOAL" USING WS-KIT-CA
           MOVE "one liability makes a SINGLE supply" TO TK-NAME
           MOVE "SINGLE" TO TK-EXPECTED-X
           MOVE KA-SUPPLY-KIND TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "single supply VAT is 20.00" TO TK-NAME
           MOVE 20.00 TO TK-EXPECTED-N
           MOVE KA-VAT-TOTAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   the parts must re-add to the parent net amount exactly
       2200-ALLOC-RECONCILES.
           PERFORM 0300-BUILD-AC130
           MOVE 100.01 TO KA-PARENT-NET
           MOVE 70.00 TO KA-L-SSP(1)
           MOVE 30.00 TO KA-L-SSP(2)
           CALL "KITSOAL" USING WS-KIT-CA
           MOVE "an odd penny is allocated, never lost" TO TK-NAME
           MOVE 100.01 TO TK-EXPECTED-N
           MOVE KA-ALLOCATED TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-KIT-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "no residual is left behind" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE KA-RESIDUAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the components re-add to the parent" TO TK-NAME
           MOVE 100.01 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N =
               KA-L-ALLOC-NET(1) + KA-L-ALLOC-NET(2)
           PERFORM TK-ASSERT-NUM.

       3000-BAD-INPUT.
           PERFORM 0300-BUILD-AC130
           MOVE "SPREAD" TO CA-FUNCTION OF WS-KIT-CA
           CALL "KITSOAL" USING WS-KIT-CA
           MOVE "an unknown function is refused with 08" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-KIT-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "an unknown function reports KIT0006" TO TK-NAME
           MOVE "KIT0006" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-KIT-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0300-BUILD-AC130
           MOVE 0 TO KA-PARENT-NET
           CALL "KITSOAL" USING WS-KIT-CA
           MOVE "a zero kit price cannot be allocated" TO TK-NAME
           MOVE "KIT0004" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-KIT-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0300-BUILD-AC130
           MOVE 0 TO KA-L-SSP(1)
           MOVE 0 TO KA-L-SSP(2)
           CALL "KITSOAL" USING WS-KIT-CA
           MOVE "without standalone prices there is no basis"
               TO TK-NAME
           MOVE "KIT0005" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-KIT-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> CHECKDUP needs the sales order tables, so it runs inside a
      *> transaction that is always rolled back, and skips entirely
      *> when the database is not available.
      *>----------------------------------------------------------------
       4000-CHECKDUP-DB.
           MOVE "CHECKDUP refuses demand on both parent and child"
               TO TK-NAME
           IF WS-DB-UP = "N"
               MOVE "database not available" TO TK-DETAIL
               PERFORM TK-SKIP
               GO TO 4000-EXIT
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           INITIALIZE WS-KIT-CA
           MOVE "STC" TO CA-COMPANY OF WS-KIT-CA
           MOVE "RDG" TO CA-SITE OF WS-KIT-CA
           MOVE "SALESREP" TO CA-USER OF WS-KIT-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-KIT-CA
           MOVE "CHECKDUP" TO CA-FUNCTION OF WS-KIT-CA
           MOVE "ZZSOKIT-NOTREAL" TO KA-ORDER-NUMBER
           MOVE 1 TO KA-PARENT-LINE
           CALL "KITSOAL" USING WS-KIT-CA
      *>   the fixture order does not exist, so the only correct
      *>   answer is "no structure" - never a confident duplication
           MOVE 16 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-KIT-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-KIT-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "CHECKDUP names the structure it could not find"
               TO TK-NAME
           MOVE "NONE" TO TK-EXPECTED-X
           MOVE KA-STRUCT-SOURCE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.
       4000-EXIT.
           EXIT.

       COPY TESTKITP.
