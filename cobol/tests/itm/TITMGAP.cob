      *>================================================================
      *> PROGRAM-ID : TITMGAP
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE UNIT TESTS
      *> PURPOSE    : THE THREE ITEM RULES THAT HAD NO TEST OF THEIR
      *>              OWN.
      *>              ITM-008  SUPPLY RULES - APPROVED MAKE OR BUY,
      *>                       LEAD TIME, MINIMUM ORDER, PURCHASE
      *>                       MULTIPLE AND SITE PLANNING PARAMETERS.
      *>                       PROCUREMENT PACK ROUNDING IS REPORTED
      *>                       AS EXCESS SUPPLY AND NEVER CHANGES THE
      *>                       ENGINEERING USAGE THAT ASKED FOR IT
      *>                       (AC-071).
      *>              ITM-021  COPY AND BULK MAINTENANCE - THE PREVIEW
      *>                       TELLS THE TRUTH ABOUT WHAT WOULD BE
      *>                       CREATED AND WHAT IS LEFT BEHIND; THE
      *>                       COPY CARRIES THE MASTER DATA AND NEVER
      *>                       THE BALANCES, THE MOVEMENT HISTORY, THE
      *>                       APPROVALS OR THE SERIAL IDENTITIES; A
      *>                       MIXED BULK SELECTION IS PARTLY APPLIED
      *>                       AND FULLY EXPLAINED (AC-069).
      *>              ITM-027  PRODUCT COMPLIANCE - THE DANGEROUS
      *>                       GOODS CLASSIFICATION A DESPATCH NOTE
      *>                       MUST SHOW, AND PACKAGING WEIGHT BY
      *>                       MATERIAL WITH RECYCLED CONTENT FOR THE
      *>                       PACKAGING DATA REPORT (AC-132).
      *>              EVERY FIXTURE IS PREFIXED ZZITMGAP AND IS
      *>              REMOVED BEFORE AND AFTER THE RUN, SO THE SUITE
      *>              IS REPEATABLE.  WITH NO DATABASE THE SUITE SKIPS
      *>              RATHER THAN FAILS.
      *> OUTPUT     : TAP - "ok n - text" / "not ok n - text"
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2026-10-07 STERP ORIGINAL - ITM-008, ITM-021, ITM-027
      *>================================================================
      *> REQ: ITM-008, ITM-021, ITM-027
      *> AC: AC-069, AC-071, AC-132
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TITMGAP.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY ITEMENQCA REPLACING ==DFHCOMMAREA== BY ==WS-ENQ-CA==.
       COPY ITEMCOPYCA REPLACING ==DFHCOMMAREA== BY ==WS-CPY-CA==.
       COPY ITMCOMPLCA REPLACING ==DFHCOMMAREA== BY ==WS-CMP-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-BAD                  PIC S9(4) VALUE 0.
           05  WS-COUNT                PIC S9(9) VALUE 0.
           05  WS-SRC-ROWS             PIC S9(9) VALUE 0.
           05  WS-TGT-ROWS             PIC S9(9) VALUE 0.
           05  WS-NUM-ED               PIC -(8)9.
       01  WS-NAMES.
           05  WS-SOURCE-ITEM          PIC X(40) VALUE
               "BATTERY-36V-14AH".
           05  WS-COPY-ITEM            PIC X(40) VALUE
               "ZZITMGAP-COPY1".
           05  WS-GHOST-ITEM           PIC X(40) VALUE
               "ZZITMGAP-NOSUCH".
           05  WS-PLAN-ITEM            PIC X(40) VALUE
               "LINK-QUICK-11S".
           05  WS-PKG-ITEM             PIC X(40) VALUE
               "HUB-REAR-148-BOOST-XD".
           05  WS-COPY-DESC            PIC X(100) VALUE
               "ZZ copy of the battery pack for the copy tests".
           05  WS-BULK-DESC            PIC X(100) VALUE
               "ZZ bulk maintained description".
           05  WS-TABLE                PIC X(24) VALUE SPACES.
           05  WS-WHERE-ITEM           PIC X(40) VALUE SPACES.
       01  WS-GROUP-TAB.
           05  WS-GROUP-ENT OCCURS 7 TIMES PIC X(24).
       01  WS-ROW.
           05  WS-COL1                 PIC X(100).
           05  WS-COL2                 PIC X(100).
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           MOVE "TITMGAP supply, copy, compliance" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0050-NAME-THE-GROUPS
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 0150-CLEAN-UP
               PERFORM 1000-SUPPLY-RULES
               PERFORM 1100-PACK-ROUNDING
               PERFORM 1200-MINIMUM-ORDER
               PERFORM 1300-EXACT-MULTIPLE
               PERFORM 1400-SITE-SPECIFIC
               PERFORM 2000-COPY-PREVIEW
               PERFORM 2100-COPY-CARRIES-ACROSS
               PERFORM 2200-COPY-LEAVES-BEHIND
               PERFORM 2300-IDENTITY-NOT-REUSED
               PERFORM 2400-BULK-FIELD-GUARD
               PERFORM 2500-BULK-MIXED-SELECTION
               PERFORM 3000-DANGEROUS-GOODS
               PERFORM 3100-NOT-DANGEROUS-GOODS
               PERFORM 3200-PACKAGING-WEIGHTS
               PERFORM 3300-PACKAGING-BY-MATERIAL
               PERFORM 3400-COMPLIANCE-DATA
               PERFORM 3500-COPY-NEEDS-OWN-EVIDENCE
               PERFORM 0150-CLEAN-UP
           END-IF
           PERFORM TK-END
           STOP RUN.

       0050-NAME-THE-GROUPS.
           MOVE "inv.ItemUom" TO WS-GROUP-ENT(1)
           MOVE "inv.ItemSite" TO WS-GROUP-ENT(2)
           MOVE "inv.ItemVatClass" TO WS-GROUP-ENT(3)
           MOVE "inv.ItemCustoms" TO WS-GROUP-ENT(4)
           MOVE "inv.ItemPackaging" TO WS-GROUP-ENT(5)
           MOVE "inv.ItemPartner" TO WS-GROUP-ENT(6)
           MOVE "inv.ItemSubstitute" TO WS-GROUP-ENT(7).

      *>   one harmless read tells us whether the item catalogue is
      *>   deployed on this machine
       0100-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) ItemCode, '' FROM inv.Item WHERE "
                  "CompanyCode = 'STC' AND ItemCode = "
                  "'BATTERY-36V-14AH'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 100 TO SQLIO-COL-WIDTH(1)
           MOVE 100 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-DB-UP
           END-IF.

       0900-SKIP-EVERYTHING.
           MOVE "item supply, copy and compliance rules" TO TK-NAME
           MOVE "the item catalogue is not deployed here"
               TO TK-DETAIL
           PERFORM TK-SKIP.

      *>   remove anything an earlier run of this suite left behind.
      *>   The children go first because every detail table has a
      *>   foreign key to inv.Item; the audit trail goes last because
      *>   deleting the item writes to it.
       0150-CLEAN-UP.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; DELETE inv.ItemSubstitute WHERE "
                  "CompanyCode = 'STC' AND (ItemCode LIKE "
                  "'ZZITMGAP%' OR SubstituteItemCode LIKE "
                  "'ZZITMGAP%'); DELETE inv.ItemPartner WHERE "
                  "CompanyCode = 'STC' AND ItemCode LIKE "
                  "'ZZITMGAP%'; DELETE inv.ItemPackaging WHERE "
                  "CompanyCode = 'STC' AND ItemCode LIKE "
                  "'ZZITMGAP%'; DELETE inv.ItemCustoms WHERE "
                  "CompanyCode = 'STC' AND ItemCode LIKE "
                  "'ZZITMGAP%'; DELETE inv.ItemVatClass WHERE "
                  "CompanyCode = 'STC' AND ItemCode LIKE "
                  "'ZZITMGAP%'; DELETE inv.ItemSite WHERE "
                  "CompanyCode = 'STC' AND ItemCode LIKE "
                  "'ZZITMGAP%'; DELETE inv.ItemUom WHERE "
                  "CompanyCode = 'STC' AND ItemCode LIKE "
                  "'ZZITMGAP%'; DELETE inv.Item WHERE CompanyCode = "
                  "'STC' AND ItemCode LIKE 'ZZITMGAP%'; DELETE "
                  "core.AuditLog WHERE EntityKey LIKE 'ZZITMGAP%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

      *>----------------------------------------------------------------
      *> 1000  ITM-008 - THE APPROVED SUPPLY RULES FOR A SITE
      *>----------------------------------------------------------------
      *>   REQ: ITM-008
       1000-SUPPLY-RULES.
           PERFORM 9000-RESET-ENQ
           MOVE "SUPPLY" TO CA-FUNCTION OF WS-ENQ-CA
           MOVE WS-PLAN-ITEM TO IE-ITEM-CODE
           MOVE 0 TO IE-ORDER-QTY
           CALL "ITEMENQ" USING WS-ENQ-CA
           MOVE "the supply rules of a stocked part are returned"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-ENQ-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-ENQ-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the approved make or buy choice is BUY" TO TK-NAME
           MOVE "BUY" TO TK-EXPECTED-X
           MOVE IE-MAKE-BUY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the lead time at Reading is 14 days" TO TK-NAME
           MOVE 14 TO TK-EXPECTED-N
           MOVE IE-LEAD-TIME-DAYS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the minimum order quantity is 25" TO TK-NAME
           MOVE 25 TO TK-EXPECTED-N
           MOVE IE-MIN-ORDER-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the purchase multiple is a pack of 25" TO TK-NAME
           MOVE 25 TO TK-EXPECTED-N
           MOVE IE-PURCH-MULTIPLE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the site planning targets are reported together"
               TO TK-NAME
           IF IE-REORDER-POINT = 100 AND IE-SAFETY-STOCK = 50
              AND IE-MAX-STOCK = 1000 AND IE-ABC-CLASS = "B"
               PERFORM TK-PASS
           ELSE
               MOVE "reorder, safety, maximum or ABC is wrong"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "an approved source is named, not guessed at"
               TO TK-NAME
           MOVE "S0025" TO TK-EXPECTED-X
           MOVE IE-PREFERRED-SUPP TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-008   AC: AC-071
      *>   65 pieces of engineering demand buy three packs of 25.
      *>   The ten spare pieces are excess supply and are reported as
      *>   such; the engineering usage is still 65.
       1100-PACK-ROUNDING.
           PERFORM 9000-RESET-ENQ
           MOVE "SUPPLY" TO CA-FUNCTION OF WS-ENQ-CA
           MOVE WS-PLAN-ITEM TO IE-ITEM-CODE
           MOVE 65 TO IE-ORDER-QTY
           CALL "ITEMENQ" USING WS-ENQ-CA
           MOVE "65 pieces round up to three packs, that is 75"
               TO TK-NAME
           MOVE 75 TO TK-EXPECTED-N
           MOVE IE-ROUNDED-QTY TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-ENQ-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the ten spare pieces are reported as excess supply"
               TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE IE-EXCESS-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "pack rounding does not change engineering usage"
               TO TK-NAME
           MOVE 65 TO TK-EXPECTED-N
           MOVE IE-ORDER-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the excess is a warning the planner must see"
               TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-ENQ-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the rounding rule is named" TO TK-NAME
           MOVE "ENQ0004" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-ENQ-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-008   AC: AC-071
       1200-MINIMUM-ORDER.
           PERFORM 9000-RESET-ENQ
           MOVE "SUPPLY" TO CA-FUNCTION OF WS-ENQ-CA
           MOVE WS-PLAN-ITEM TO IE-ITEM-CODE
           MOVE 10 TO IE-ORDER-QTY
           CALL "ITEMENQ" USING WS-ENQ-CA
           MOVE "ten pieces still buy the minimum order of 25"
               TO TK-NAME
           MOVE 25 TO TK-EXPECTED-N
           MOVE IE-ROUNDED-QTY TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-ENQ-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the minimum order creates 15 of excess supply"
               TO TK-NAME
           MOVE 15 TO TK-EXPECTED-N
           MOVE IE-EXCESS-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-008   AC: AC-071
       1300-EXACT-MULTIPLE.
           PERFORM 9000-RESET-ENQ
           MOVE "SUPPLY" TO CA-FUNCTION OF WS-ENQ-CA
           MOVE WS-PLAN-ITEM TO IE-ITEM-CODE
           MOVE 100 TO IE-ORDER-QTY
           CALL "ITEMENQ" USING WS-ENQ-CA
           MOVE "a demand of four whole packs is not rounded up"
               TO TK-NAME
           MOVE 100 TO TK-EXPECTED-N
           MOVE IE-ROUNDED-QTY TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-ENQ-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "an exact pack quantity creates no excess supply"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE IE-EXCESS-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "nothing has to be warned about" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-ENQ-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-008 - the planning parameters belong to the site.
      *>   Birmingham holds no planning record for this part, so it
      *>   has no pack rule of its own and nothing is invented for it.
       1400-SITE-SPECIFIC.
           PERFORM 9000-RESET-ENQ
           MOVE "SUPPLY" TO CA-FUNCTION OF WS-ENQ-CA
           MOVE WS-PLAN-ITEM TO IE-ITEM-CODE
           MOVE "BHM" TO IE-SITE
           MOVE 65 TO IE-ORDER-QTY
           CALL "ITEMENQ" USING WS-ENQ-CA
           MOVE "a site without a planning record has no pack rule"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE IE-PURCH-MULTIPLE TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-ENQ-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the Reading pack size is not applied at Birmingham"
               TO TK-NAME
           MOVE 65 TO TK-EXPECTED-N
           MOVE IE-ROUNDED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the company make or buy choice still answers"
               TO TK-NAME
           MOVE "BUY" TO TK-EXPECTED-X
           MOVE IE-MAKE-BUY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 2000  ITM-021 - COPY AND BULK MAINTENANCE
      *>----------------------------------------------------------------
      *>   REQ: ITM-021   AC: AC-069
       2000-COPY-PREVIEW.
           PERFORM 9100-RESET-COPY
           MOVE "PREVIEW" TO CA-FUNCTION OF WS-CPY-CA
           CALL "ITEMCOPY" USING WS-CPY-CA
           MOVE "a copy can be previewed before anything is written"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CPY-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CPY-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the master record and seven groups are previewed"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE IC-PREVIEW-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the preview names the item master first" TO TK-NAME
           MOVE "Item master" TO TK-EXPECTED-X
           MOVE IC-P-GROUP(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the preview counts the records that would be made"
               TO TK-NAME
           MOVE "inv.ItemPartner" TO WS-TABLE
           MOVE WS-SOURCE-ITEM TO WS-WHERE-ITEM
           PERFORM 9400-COUNT-ROWS
           MOVE WS-COUNT TO TK-EXPECTED-N
           MOVE IC-P-ROWS(7) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "six kinds of record are shown as excluded"
               TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE IC-EXCLUDE-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "a serial identity is named as never duplicated"
               TO TK-NAME
           MOVE "Lots and serials" TO TK-EXPECTED-X
           MOVE IC-X-GROUP(4) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the preview writes nothing at all" TO TK-NAME
           MOVE "inv.Item" TO WS-TABLE
           MOVE WS-COPY-ITEM TO WS-WHERE-ITEM
           PERFORM 9400-COUNT-ROWS
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-COUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-021   AC: AC-069 - what the preview promised is
      *>   exactly what the copy creates, group by group.
       2100-COPY-CARRIES-ACROSS.
           PERFORM 9100-RESET-COPY
           MOVE "ZZITMGAP-COPY-001" TO CA-CORRELATION-ID OF WS-CPY-CA
           MOVE "COPY" TO CA-FUNCTION OF WS-CPY-CA
           CALL "ITEMCOPY" USING WS-CPY-CA
           MOVE "the copy is accepted" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CPY-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CPY-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "one item is reported as copied" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE IC-COPIED-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the copy is a new identity in its own right"
               TO TK-NAME
           PERFORM 9550-READ-COPY-LIFECYCLE
           MOVE WS-COPY-ITEM TO TK-EXPECTED-X
           MOVE WS-COL1 TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the copy starts as a draft, not as released"
               TO TK-NAME
           MOVE "DRAFT" TO TK-EXPECTED-X
           MOVE WS-COL2 TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "every previewed group is created on the copy"
               TO TK-NAME
           MOVE 0 TO WS-BAD
           MOVE SPACES TO TK-DETAIL
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 7
               MOVE WS-GROUP-ENT(WS-IX) TO WS-TABLE
               MOVE WS-SOURCE-ITEM TO WS-WHERE-ITEM
               PERFORM 9400-COUNT-ROWS
               MOVE WS-COUNT TO WS-SRC-ROWS
               MOVE WS-GROUP-ENT(WS-IX) TO WS-TABLE
               MOVE WS-COPY-ITEM TO WS-WHERE-ITEM
               PERFORM 9400-COUNT-ROWS
               MOVE WS-COUNT TO WS-TGT-ROWS
               IF WS-SRC-ROWS NOT = WS-TGT-ROWS
                   ADD 1 TO WS-BAD
                   PERFORM 9600-REPORT-GROUP-GAP
               END-IF
           END-PERFORM
           IF WS-BAD = 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE "the copy is written to the audit trail for review"
               TO TK-NAME
           PERFORM 9700-COUNT-AUDIT-COPY
           IF WS-COUNT > 0
               PERFORM TK-PASS
           ELSE
               MOVE "no COPY row was written to core.AuditLog"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-021   AC: AC-069 - a copy has no past.  The source
      *>   holds stock and serial identities; the copy holds neither.
       2200-COPY-LEAVES-BEHIND.
           MOVE "the item that was copied really does hold stock"
               TO TK-NAME
           MOVE "inv.StockBalance" TO WS-TABLE
           MOVE WS-SOURCE-ITEM TO WS-WHERE-ITEM
           PERFORM 9400-COUNT-ROWS
           IF WS-COUNT > 0
               PERFORM TK-PASS
           ELSE
               MOVE "the fixture has no balance to leave behind"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "and really does carry serial identities" TO TK-NAME
           MOVE "inv.Serial" TO WS-TABLE
           MOVE WS-SOURCE-ITEM TO WS-WHERE-ITEM
           PERFORM 9400-COUNT-ROWS
           IF WS-COUNT > 0
               PERFORM TK-PASS
           ELSE
               MOVE "the fixture has no serial to leave behind"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "the copy inherits no stock balance" TO TK-NAME
           MOVE "inv.StockBalance" TO WS-TABLE
           MOVE WS-COPY-ITEM TO WS-WHERE-ITEM
           PERFORM 9400-COUNT-ROWS
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-COUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the copy inherits no movement history" TO TK-NAME
           MOVE "inv.StockMovement" TO WS-TABLE
           MOVE WS-COPY-ITEM TO WS-WHERE-ITEM
           PERFORM 9400-COUNT-ROWS
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-COUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the copy inherits no serial identity" TO TK-NAME
           MOVE "inv.Serial" TO WS-TABLE
           MOVE WS-COPY-ITEM TO WS-WHERE-ITEM
           PERFORM 9400-COUNT-ROWS
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-COUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the copy inherits no reservation" TO TK-NAME
           MOVE "inv.StockReservation" TO WS-TABLE
           MOVE WS-COPY-ITEM TO WS-WHERE-ITEM
           PERFORM 9400-COUNT-ROWS
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-COUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-021 - an identity is never reused
       2300-IDENTITY-NOT-REUSED.
           PERFORM 9100-RESET-COPY
           MOVE "ZZITMGAP-COPY-002" TO CA-CORRELATION-ID OF WS-CPY-CA
           MOVE "COPY" TO CA-FUNCTION OF WS-CPY-CA
           CALL "ITEMCOPY" USING WS-CPY-CA
           MOVE "copying onto a code that exists is refused"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CPY-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CPY-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the refusal quotes the identity rule" TO TK-NAME
           MOVE "CPY0002" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CPY-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-021 - bulk maintenance is for descriptive and
      *>   analysis attributes; identity, units and traceability are
      *>   changed one item at a time
       2400-BULK-FIELD-GUARD.
           PERFORM 9100-RESET-COPY
           MOVE "BULKPREV" TO CA-FUNCTION OF WS-CPY-CA
           MOVE "STOCKUOMCODE" TO IC-FIELD-NAME
           MOVE "KG" TO IC-NEW-VALUE
           MOVE 1 TO IC-TARGET-CNT
           MOVE WS-COPY-ITEM TO IC-T-ITEM(1)
           CALL "ITEMCOPY" USING WS-CPY-CA
           MOVE "the stock unit may not be changed in bulk"
               TO TK-NAME
           MOVE "CPY0004" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CPY-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CPY-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "a refused bulk change touches nothing" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE IC-COPIED-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 9100-RESET-COPY
           MOVE "BULKPREV" TO CA-FUNCTION OF WS-CPY-CA
           MOVE "DESCRIPTION" TO IC-FIELD-NAME
           MOVE SPACES TO IC-NEW-VALUE
           MOVE 1 TO IC-TARGET-CNT
           MOVE WS-COPY-ITEM TO IC-T-ITEM(1)
           CALL "ITEMCOPY" USING WS-CPY-CA
           MOVE "a description may not be blanked in bulk"
               TO TK-NAME
           MOVE "CPY0005" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CPY-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CPY-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-021   AC: AC-069 - a mixed valid and invalid
      *>   selection is partly applied and every target explains
      *>   itself; the result is reviewable afterwards
       2500-BULK-MIXED-SELECTION.
           PERFORM 9100-RESET-COPY
           MOVE "ZZITMGAP-BULK-001" TO CA-CORRELATION-ID OF WS-CPY-CA
           MOVE "BULKPREV" TO CA-FUNCTION OF WS-CPY-CA
           MOVE "DESCRIPTION" TO IC-FIELD-NAME
           MOVE WS-BULK-DESC TO IC-NEW-VALUE
           MOVE 2 TO IC-TARGET-CNT
           MOVE WS-COPY-ITEM TO IC-T-ITEM(1)
           MOVE WS-GHOST-ITEM TO IC-T-ITEM(2)
           CALL "ITEMCOPY" USING WS-CPY-CA
           MOVE "the preview shows the value each target holds now"
               TO TK-NAME
           MOVE WS-COPY-DESC TO TK-EXPECTED-X
           MOVE IC-T-OLD-VALUE(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the preview changes nothing yet" TO TK-NAME
           PERFORM 9500-READ-COPY-DESC
           MOVE WS-COPY-DESC TO TK-EXPECTED-X
           MOVE WS-COL2 TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 9100-RESET-COPY
           MOVE "ZZITMGAP-BULK-002" TO CA-CORRELATION-ID OF WS-CPY-CA
           MOVE "BULKAPPLY" TO CA-FUNCTION OF WS-CPY-CA
           MOVE "DESCRIPTION" TO IC-FIELD-NAME
           MOVE WS-BULK-DESC TO IC-NEW-VALUE
           MOVE 2 TO IC-TARGET-CNT
           MOVE WS-COPY-ITEM TO IC-T-ITEM(1)
           MOVE WS-GHOST-ITEM TO IC-T-ITEM(2)
           CALL "ITEMCOPY" USING WS-CPY-CA
           MOVE "the valid target is applied" TO TK-NAME
           MOVE "APPLIED" TO TK-EXPECTED-X
           MOVE IC-T-OUTCOME(1) TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CPY-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "the target that does not exist is rejected"
               TO TK-NAME
           MOVE "REJECT" TO TK-EXPECTED-X
           MOVE IC-T-OUTCOME(2) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the rejected target says why" TO TK-NAME
           IF IC-T-REASON(2) NOT = SPACES
               MOVE IC-T-REASON(2) TO TK-DETAIL
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE "one target of two was accepted" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE IC-COPIED-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "one target of two was rejected" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE IC-REJECTED-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "a partly applied run is a warning, not a failure"
               TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CPY-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the accepted target really did change" TO TK-NAME
           PERFORM 9500-READ-COPY-DESC
           MOVE WS-BULK-DESC TO TK-EXPECTED-X
           MOVE WS-COL2 TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the bulk change leaves a reviewable history"
               TO TK-NAME
           PERFORM 9800-COUNT-AUDIT-BULK
           IF WS-COUNT > 0
               PERFORM TK-PASS
           ELSE
               MOVE "no BULKCHANGE row was written to core.AuditLog"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 3000  ITM-027 - PRODUCT COMPLIANCE AND PACKAGING DATA
      *>----------------------------------------------------------------
      *>   REQ: ITM-027   AC: AC-132 - a despatch note for dangerous
      *>   goods must carry the UN number, the class and the packing
      *>   group, and they come from the item, not from a typist
       3000-DANGEROUS-GOODS.
           PERFORM 9200-RESET-COMPL
           MOVE "DGCHECK" TO CA-FUNCTION OF WS-CMP-CA
           MOVE WS-SOURCE-ITEM TO CO-ITEM-CODE
           CALL "ITMCOMPL" USING WS-CMP-CA
           MOVE "a dangerous goods item can be despatched" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CMP-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CMP-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the despatch note shows UN number UN3480" TO TK-NAME
           MOVE "UN3480" TO TK-EXPECTED-X
           MOVE CO-DG-UN-NUMBER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the despatch note shows class 9" TO TK-NAME
           MOVE "9" TO TK-EXPECTED-X
           MOVE CO-DG-CLASS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the despatch note shows packing group II" TO TK-NAME
           MOVE "II" TO TK-EXPECTED-X
           MOVE CO-DG-PACK-GROUP TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the classification is offered as one legend"
               TO TK-NAME
           MOVE "UN3480, class 9, packing group II" TO TK-EXPECTED-X
           MOVE CA-MESSAGE OF WS-CMP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the dangerous goods data is complete" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE CO-VALID TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-027 - an ordinary part is not labelled dangerous
       3100-NOT-DANGEROUS-GOODS.
           PERFORM 9200-RESET-COMPL
           MOVE "DGCHECK" TO CA-FUNCTION OF WS-CMP-CA
           MOVE WS-PLAN-ITEM TO CO-ITEM-CODE
           CALL "ITMCOMPL" USING WS-CMP-CA
           MOVE "a part that is not dangerous goods says so"
               TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CMP-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CMP-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "and carries no invented classification" TO TK-NAME
           IF CO-DG-UN-NUMBER = SPACES AND CO-DG-CLASS = SPACES
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-027   AC: AC-132 - 1,000 units despatched, each
      *>   with 50 g of plastic packaging at 30 per cent recycled
      *>   content, are 50,000 g (50 kg) of plastic of which 15,000 g
      *>   (15 kg) is recycled.  This is the figure RPT-019 reports.
       3200-PACKAGING-WEIGHTS.
           PERFORM 9200-RESET-COMPL
           MOVE "PACKAGE" TO CA-FUNCTION OF WS-CMP-CA
           MOVE WS-PKG-ITEM TO CO-ITEM-CODE
           MOVE 1000 TO CO-DESPATCH-QTY
           CALL "ITMCOMPL" USING WS-CMP-CA
           MOVE "the packaging composition is held per unit"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CMP-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CMP-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the material is reported as plastic" TO TK-NAME
           MOVE "PLASTIC" TO TK-EXPECTED-X
           MOVE CO-L-MATERIAL(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "1,000 units carry 50,000 g of plastic" TO TK-NAME
           MOVE 50000 TO TK-EXPECTED-N
           MOVE CO-PKG-PLASTIC-G TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "15,000 g of that plastic is recycled content"
               TO TK-NAME
           MOVE 15000 TO TK-EXPECTED-N
           MOVE CO-PKG-RECYCLED-G TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the packaging total agrees with the material line"
               TO TK-NAME
           MOVE 50000 TO TK-EXPECTED-N
           MOVE CO-PKG-TOTAL-G TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the plastic is liable to Plastic Packaging Tax"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE CO-PPT-LIABLE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-027 - weights are reported by material, so paper
      *>   is never counted as plastic and never taxed as plastic
       3300-PACKAGING-BY-MATERIAL.
           PERFORM 9200-RESET-COMPL
           MOVE "PACKAGE" TO CA-FUNCTION OF WS-CMP-CA
           MOVE WS-PLAN-ITEM TO CO-ITEM-CODE
           MOVE 1000 TO CO-DESPATCH-QTY
           CALL "ITMCOMPL" USING WS-CMP-CA
           MOVE "1,000 units carry 3,000 g of paper packaging"
               TO TK-NAME
           MOVE 3000 TO TK-EXPECTED-N
           MOVE CO-PKG-TOTAL-G TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CMP-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "none of it is counted as plastic" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CO-PKG-PLASTIC-G TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "and none of it is liable to the plastic tax"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE CO-PPT-LIABLE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-027 - the compliance data the company is obliged
      *>   to hold is returned together for the enabled flags
       3400-COMPLIANCE-DATA.
           PERFORM 9200-RESET-COMPL
           MOVE "COMPLY" TO CA-FUNCTION OF WS-CMP-CA
           MOVE WS-SOURCE-ITEM TO CO-ITEM-CODE
           CALL "ITMCOMPL" USING WS-CMP-CA
           MOVE "the compliance record of the battery is complete"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CO-FAULT-CNT TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CMP-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "a UKCA marked item names its conformity document"
               TO TK-NAME
           IF CO-UKCA-MARKED = "Y" AND CO-CONFORMITY-REF NOT = SPACES
               MOVE CO-CONFORMITY-REF TO TK-DETAIL
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE "the WEEE category is held" TO TK-NAME
           MOVE "6" TO TK-EXPECTED-X
           MOVE CO-WEEE-CATEGORY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the battery category is held" TO TK-NAME
           MOVE "INDUSTRIAL" TO TK-EXPECTED-X
           MOVE CO-BATTERY-CATEGORY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-021, ITM-027 - a declaration of conformity belongs
      *>   to the product it was issued for.  The copy carries the
      *>   UKCA flag but no evidence of its own, so compliance reports
      *>   the gap instead of letting the copy trade on the original's
      *>   paperwork.
       3500-COPY-NEEDS-OWN-EVIDENCE.
           PERFORM 9200-RESET-COMPL
           MOVE "COMPLY" TO CA-FUNCTION OF WS-CMP-CA
           MOVE WS-COPY-ITEM TO CO-ITEM-CODE
           CALL "ITMCOMPL" USING WS-CMP-CA
           MOVE "a copy does not inherit the conformity evidence"
               TO TK-NAME
           MOVE "CMP0008" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CMP-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CMP-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "the missing evidence is counted as a fault"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE CO-FAULT-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the copy is not reported as compliant" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE CO-VALID TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 9000  SHARED SET UP AND DATABASE HELPERS
      *>----------------------------------------------------------------
       9000-RESET-ENQ.
           INITIALIZE WS-ENQ-CA
           MOVE "STC" TO CA-COMPANY OF WS-ENQ-CA
           MOVE "RDG" TO CA-SITE OF WS-ENQ-CA
           MOVE "BUYER" TO CA-USER OF WS-ENQ-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-ENQ-CA
           MOVE "RDG" TO IE-SITE.

       9100-RESET-COPY.
           INITIALIZE WS-CPY-CA
           MOVE "STC" TO CA-COMPANY OF WS-CPY-CA
           MOVE "RDG" TO CA-SITE OF WS-CPY-CA
           MOVE "ENGINEER" TO CA-USER OF WS-CPY-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-CPY-CA
           MOVE WS-SOURCE-ITEM TO IC-SOURCE-ITEM
           MOVE WS-COPY-ITEM TO IC-TARGET-ITEM
           MOVE WS-COPY-DESC TO IC-TARGET-DESC
           MOVE "Y" TO IC-INCLUDE-UOM IC-INCLUDE-SITE
           MOVE "Y" TO IC-INCLUDE-VAT IC-INCLUDE-CUSTOMS
           MOVE "Y" TO IC-INCLUDE-PACKAGE IC-INCLUDE-PARTNER
           MOVE "Y" TO IC-INCLUDE-SUBST.

       9200-RESET-COMPL.
           INITIALIZE WS-CMP-CA
           MOVE "STC" TO CA-COMPANY OF WS-CMP-CA
           MOVE "RDG" TO CA-SITE OF WS-CMP-CA
           MOVE "WHSE" TO CA-USER OF WS-CMP-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-CMP-CA.

      *>   the table name is one of this program's own literals, so
      *>   nothing a user typed ever reaches the statement
       9400-COUNT-ROWS.
           MOVE 0 TO WS-COUNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT(*) FROM " FUNCTION TRIM(WS-TABLE)
                  " WHERE CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE "STC" TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-WHERE-ITEM TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 100 TO SQLIO-COL-WIDTH(1)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-OK
               COMPUTE WS-COUNT = FUNCTION NUMVAL(WS-COL1)
           END-IF.

       9500-READ-COPY-DESC.
           MOVE SPACES TO WS-COL1 WS-COL2
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ItemCode, Description FROM inv.Item WHERE"
                  " CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE "STC" TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-COPY-ITEM TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 100 TO SQLIO-COL-WIDTH(1)
           MOVE 100 TO SQLIO-COL-WIDTH(2)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       9550-READ-COPY-LIFECYCLE.
           MOVE SPACES TO WS-COL1 WS-COL2
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ItemCode, LifecycleStatus FROM inv.Item "
                  "WHERE CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE "STC" TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-COPY-ITEM TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 100 TO SQLIO-COL-WIDTH(1)
           MOVE 100 TO SQLIO-COL-WIDTH(2)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       9600-REPORT-GROUP-GAP.
           MOVE SPACES TO TK-DETAIL
           MOVE WS-SRC-ROWS TO WS-NUM-ED
           STRING FUNCTION TRIM(WS-GROUP-ENT(WS-IX)) " previewed "
                  FUNCTION TRIM(WS-NUM-ED) " row(s) but the copy got "
               DELIMITED BY SIZE INTO TK-DETAIL
           MOVE WS-TGT-ROWS TO WS-NUM-ED
           STRING FUNCTION TRIM(TK-DETAIL) " "
                  FUNCTION TRIM(WS-NUM-ED)
               DELIMITED BY SIZE INTO TK-DETAIL.

       9700-COUNT-AUDIT-COPY.
           MOVE 0 TO WS-COUNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT(*) FROM core.AuditLog WHERE "
                  "EntityType = 'inv.Item' AND EntityKey = ? AND "
                  "Action = 'COPY'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-COPY-ITEM TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 100 TO SQLIO-COL-WIDTH(1)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-OK
               COMPUTE WS-COUNT = FUNCTION NUMVAL(WS-COL1)
           END-IF.

       9800-COUNT-AUDIT-BULK.
           MOVE 0 TO WS-COUNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT(*) FROM core.AuditLog WHERE "
                  "EntityType = 'inv.Item' AND EntityKey = ? AND "
                  "Action = 'BULKCHANGE' AND NewValue = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-COPY-ITEM TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-BULK-DESC TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 100 TO SQLIO-COL-WIDTH(1)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-OK
               COMPUTE WS-COUNT = FUNCTION NUMVAL(WS-COL1)
           END-IF.

       COPY TESTKITP.
